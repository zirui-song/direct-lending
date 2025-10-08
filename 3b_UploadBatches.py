#!/usr/bin/env python3
"""
3b_UploadBatches.py

Uploads generated JSONL batch input files to the OpenAI API and creates
batch jobs. Reads OPENAI_API_KEY from .env (if present) or environment.

Inputs:
- Looks for JSONL files in ../Data/Raw/batch_jobs/

Outputs:
- Writes an upload summary JSON with file_ids and batch_ids
  to ../Data/Raw/batch_jobs/upload_summary.json

Note:
- Each JSONL file is first uploaded via /v1/files (purpose=batch), then a
  batch is created via /v1/batches with completion_window=24h.
"""

import os
import json
import time
from pathlib import Path
from datetime import datetime

import requests
from dotenv import load_dotenv


API_BASE = "https://api.openai.com/v1"


def load_api_key() -> str:
    """Load OPENAI_API_KEY from .env or environment."""
    try:
        load_dotenv()
    except Exception:
        pass
    api_key = os.getenv("OPENAI_API_KEY", "").strip()
    if not api_key:
        raise RuntimeError("OPENAI_API_KEY not found. Set it in .env or environment.")
    return api_key


def list_batch_files(batch_dir: Path) -> list[Path]:
    files = sorted(batch_dir.glob("batch_*.jsonl"))
    return [f for f in files if f.is_file()]


def upload_file(api_key: str, file_path: Path) -> str:
    """Upload a JSONL file to OpenAI files API with purpose=batch. Return file_id."""
    url = f"{API_BASE}/files"
    headers = {"Authorization": f"Bearer {api_key}"}
    with open(file_path, "rb") as fh:
        files = {
            "file": (file_path.name, fh, "application/jsonl")
        }
        data = {"purpose": "batch"}
        resp = requests.post(url, headers=headers, files=files, data=data, timeout=120)
    if resp.status_code != 200:
        raise RuntimeError(f"Upload failed for {file_path.name}: {resp.status_code} {resp.text}")
    payload = resp.json()
    file_id = payload.get("id")
    if not file_id:
        raise RuntimeError(f"No file id returned for {file_path.name}: {payload}")
    return file_id


def create_batch(api_key: str, input_file_id: str, completion_window: str = "24h") -> str:
    """Create a batch job for a previously uploaded file. Return batch_id."""
    url = f"{API_BASE}/batches"
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json",
    }
    body = {
        "input_file_id": input_file_id,
        "endpoint": "/v1/chat/completions",
        "completion_window": completion_window,
    }
    resp = requests.post(url, headers=headers, data=json.dumps(body), timeout=60)
    if resp.status_code != 200:
        raise RuntimeError(f"Batch creation failed for file {input_file_id}: {resp.status_code} {resp.text}")
    payload = resp.json()
    batch_id = payload.get("id")
    if not batch_id:
        raise RuntimeError(f"No batch id returned for file {input_file_id}: {payload}")
    return batch_id


def main():
    print("=" * 80)
    print("3b_UploadBatches.py - Upload JSONL batches and create OpenAI batch jobs")
    print("=" * 80)

    script_dir = Path(__file__).parent
    batch_dir = script_dir / ".." / "Data" / "Raw" / "batch_jobs"
    summary_file = batch_dir / "upload_summary.json"

    if not batch_dir.exists():
        print(f"Error: batch directory does not exist: {batch_dir}")
        return

    try:
        api_key = load_api_key()
    except Exception as e:
        print(f"Error: {e}")
        return

    batch_files = list_batch_files(batch_dir)
    if not batch_files:
        print(f"No batch_*.jsonl files found in {batch_dir}")
        return

    # Start fresh - create new upload_summary.json each time
    results = []
    seen_files = {}

    print(f"Found {len(batch_files)} batch files. Starting upload...")
    for idx, file_path in enumerate(batch_files, 1):
        rel_name = file_path.name

        print(f"[{idx}/{len(batch_files)}] Uploading: {rel_name}")
        try:
            file_id = upload_file(api_key, file_path)
            print(f"  -> file_id: {file_id}")
            time.sleep(0.5)
            batch_id = create_batch(api_key, file_id)
            print(f"  -> batch_id: {batch_id}")
            record = {
                "filename": rel_name,
                "file_id": file_id,
                "batch_id": batch_id,
                "uploaded_at": datetime.utcnow().isoformat() + "Z",
            }
            results.append(record)
            # Save after each success
            with open(summary_file, "w") as fh:
                json.dump({
                    "upload_started": datetime.utcnow().isoformat() + "Z",
                    "upload_last_updated": datetime.utcnow().isoformat() + "Z",
                    "results": results,
                }, fh, indent=2)
        except Exception as e:
            print(f"  !! Failed: {e}")
            # Continue with next file
            continue

    print("\n" + "=" * 80)
    print("UPLOAD COMPLETE")
    print("=" * 80)
    print(f"Summary written to: {summary_file}")


if __name__ == "__main__":
    main()


