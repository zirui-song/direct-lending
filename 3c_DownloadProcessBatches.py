#!/usr/bin/env python3
"""
3c_DownloadProcessBatches.py

Downloads completed OpenAI batch results listed in upload_summary.json,
parses the JSONL responses, and writes a consolidated CSV with extracted
Borrower Name, Lender Name, and Lender Type for each contract.

Inputs:
- ../Data/Raw/batch_jobs/upload_summary.json (from 3b_UploadBatches.py)

Outputs:
- Raw outputs saved under ../Data/Raw/batch_jobs/outputs/<batch_id>.jsonl
- Consolidated CSV: ../Data/Raw/batch_jobs/processed/batch_results.csv
"""

import os
import csv
import json
import time
from pathlib import Path
from typing import List, Dict, Any, Optional

import requests
from dotenv import load_dotenv


API_BASE = "https://api.openai.com/v1"


def load_api_key() -> str:
    try:
        load_dotenv()
    except Exception:
        pass
    api_key = os.getenv("OPENAI_API_KEY", "").strip()
    if not api_key:
        raise RuntimeError("OPENAI_API_KEY not found. Set it in .env or environment.")
    return api_key


def read_upload_summary(summary_path: Path) -> List[Dict[str, Any]]:
    if not summary_path.exists():
        raise FileNotFoundError(f"upload_summary.json not found at {summary_path}")
    data = json.loads(summary_path.read_text())
    return data.get("results", [])


def get_batch_status(api_key: str, batch_id: str) -> Dict[str, Any]:
    url = f"{API_BASE}/batches/{batch_id}"
    headers = {"Authorization": f"Bearer {api_key}"}
    resp = requests.get(url, headers=headers, timeout=60)
    if resp.status_code != 200:
        raise RuntimeError(f"Failed to fetch batch {batch_id}: {resp.status_code} {resp.text}")
    return resp.json()


def download_file_content(api_key: str, file_id: str) -> str:
    url = f"{API_BASE}/files/{file_id}/content"
    headers = {"Authorization": f"Bearer {api_key}"}
    resp = requests.get(url, headers=headers, timeout=300)
    if resp.status_code != 200:
        raise RuntimeError(f"Failed to download file {file_id}: {resp.status_code} {resp.text}")
    return resp.text


def safe_get(d: Dict[str, Any], *keys, default=None):
    cur = d
    for k in keys:
        if not isinstance(cur, dict) or k not in cur:
            return default
        cur = cur[k]
    return cur


def parse_chat_content_to_fields(content: str) -> Dict[str, Optional[str]]:
    """Parse Borrower Name, Lender Name, and Lender Type from assistant content.

    Expected shape per line (case-insensitive), with Markdown line breaks:
      Borrower Name: <value>  \n
      Lender Name: <value>  \n
      Lender Type: <value>
    """
    import re

    def _norm(s: Optional[str]) -> Optional[str]:
        if s is None:
            return None
        v = s.strip().strip('\u200b').strip()  # trim, remove zero-width
        # Normalize common not-available markers to None
        low = v.lower().strip(" []<>")
        if low in {"n/a", "na", "not applicable", "not specified", "not applicable.", "not specified.", "n a"}:
            return None
        return v

    if not content:
        return {"borrower_name": None, "lender_name": None, "lender_type": None}

    # Normalize newlines and markdown line-breaks (two spaces before \n)
    text = content.replace("\r\n", "\n").replace("\r", "\n").replace("\xa0", " ")
    text = re.sub(r"\s{2,}\n", "\n", text)  # convert double-space breaks to newline

    # Generic extractor: capture up to end-of-line
    def extract_after(label: str) -> Optional[str]:
        pattern = rf"{label}\s*:\s*(.+?)\s*$"
        m = re.search(pattern, text, flags=re.IGNORECASE | re.MULTILINE)
        return _norm(m.group(1)) if m else None

    borrower = extract_after("borrower\s*name")
    lender = extract_after("lender\s*name")
    ltype = extract_after("lender\s*type")

    # Fallback: scan line-by-line and partition on first ':'
    if borrower is None or lender is None or ltype is None:
        for line in text.split("\n"):
            s = line.strip()
            if not s:
                continue
            if ":" not in s:
                continue
            key, _, val = s.partition(":")
            key_l = key.strip().lower()
            val_n = _norm(val)
            if borrower is None and key_l.startswith("borrower"):
                borrower = val_n
            elif lender is None and (key_l.startswith("lender name") or key_l == "lender"):
                lender = val_n
            elif ltype is None and key_l.startswith("lender type"):
                ltype = val_n

    return {"borrower_name": borrower, "lender_name": lender, "lender_type": ltype}


def parse_batch_output_jsonl(raw_jsonl: str) -> List[Dict[str, Any]]:
    rows: List[Dict[str, Any]] = []
    for ln in raw_jsonl.splitlines():
        ln = ln.strip()
        if not ln:
            continue
        try:
            obj = json.loads(ln)
        except Exception:
            rows.append({"error": "invalid_json", "raw": ln})
            continue

        custom_id = obj.get("custom_id")
        # OpenAI batch output typically nests the completion under 'response'
        # Actual OpenAI batch response puts completion under response.body.choices[0].message.content
        text_out: Optional[str] = safe_get(
            obj, "response", "body", "choices", 0, "message", "content"
        )
        if text_out is None:
            # Fallbacks for older/alternate shapes
            text_out = safe_get(obj, "response", "choices", 0, "message", "content")
        if text_out is None:
            text_out = safe_get(obj, "response", "body", "message", "content")
        if text_out is None:
            # Final defensive fallback using explicit indexing
            try:
                text_out = obj["response"]["body"]["choices"][0]["message"]["content"]
            except Exception:
                text_out = None

        parsed = parse_chat_content_to_fields(text_out or "")

        accession = None
        # our custom_id was set to loan_{index}_{accession}; parse if present
        if isinstance(custom_id, str) and custom_id.count("_") >= 2:
            parts = custom_id.split("_")
            accession = "_".join(parts[2:])  # in case accession has underscores (unlikely)

        rows.append({
            "custom_id": custom_id,
            "accession": accession,
            "borrower_name": parsed.get("borrower_name"),
            "lender_name": parsed.get("lender_name"),
            "lender_type": parsed.get("lender_type"),
            "raw_text": text_out,
        })
    return rows


def write_csv(rows: List[Dict[str, Any]], csv_path: Path) -> None:
    if not rows:
        csv_path.write_text("")
        return
    fieldnames = [
        "custom_id",
        "accession",
        "borrower_name",
        "lender_name",
        "lender_type",
        "raw_text",
    ]
    with open(csv_path, "w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=fieldnames)
        writer.writeheader()
        for r in rows:
            writer.writerow({k: r.get(k, None) for k in fieldnames})


def main():
    print("=" * 80)
    print("3c_DownloadProcessBatches.py - Download and process completed batch outputs")
    print("=" * 80)

    script_dir = Path(__file__).parent
    base_dir = script_dir / ".." / "Data" / "Raw" / "batch_jobs"
    summary_path = base_dir / "upload_summary.json"
    outputs_dir = base_dir / "outputs"
    processed_dir = base_dir / "processed"
    outputs_dir.mkdir(parents=True, exist_ok=True)
    processed_dir.mkdir(parents=True, exist_ok=True)

    try:
        api_key = load_api_key()
    except Exception as e:
        print(f"Error: {e}")
        return

    try:
        records = read_upload_summary(summary_path)
    except Exception as e:
        print(f"Error loading upload summary: {e}")
        return

    if not records:
        print("No uploaded batches found in summary.")
        return

    all_rows: List[Dict[str, Any]] = []
    for i, rec in enumerate(records, 1):
        batch_id = rec.get("batch_id")
        filename = rec.get("filename")
        if not batch_id:
            print(f"[{i}/{len(records)}] Skipping (no batch_id): {filename}")
            continue

        print(f"[{i}/{len(records)}] Checking batch: {batch_id} ({filename})")
        try:
            status = get_batch_status(api_key, batch_id)
        except Exception as e:
            print(f"  !! Failed to fetch status: {e}")
            continue

        state = status.get("status") or status.get("state")  # API may use 'status'
        output_file_id = status.get("output_file_id")
        if state != "completed" or not output_file_id:
            print(f"  -> Not completed yet (status={state}, output_file_id={output_file_id})")
            continue

        out_path = outputs_dir / f"{batch_id}.jsonl"
        if out_path.exists():
            print(f"  -> Output already downloaded: {out_path.name}")
            raw_jsonl = out_path.read_text(encoding="utf-8")
        else:
            print(f"  -> Downloading output file: {output_file_id}")
            try:
                raw_jsonl = download_file_content(api_key, output_file_id)
                out_path.write_text(raw_jsonl, encoding="utf-8")
            except Exception as e:
                print(f"  !! Failed to download: {e}")
                continue

        # Parse and accumulate
        batch_rows = parse_batch_output_jsonl(raw_jsonl)
        print(f"  -> Parsed {len(batch_rows)} responses")
        all_rows.extend(batch_rows)

    if not all_rows:
        print("No completed batch outputs to process.")
        return

    csv_path = processed_dir / "batch_results.csv"
    write_csv(all_rows, csv_path)
    print("\n" + "=" * 80)
    print("DOWNLOAD & PROCESS COMPLETE")
    print("=" * 80)
    print(f"Wrote consolidated CSV: {csv_path}")


if __name__ == "__main__":
    main()


