#!/usr/bin/env python3
"""
4a_CompareBucketedLoans.py

Summarizes each lender-type bucket using sampled .nc files via the ChatGPT API,
then produces a cross-bucket comparison of similarities and differences.

Inputs:
- /Users/zrsong/MIT Dropbox/Zirui Song/Research Projects/PSW_Nonbank Direct Lending/Data/Raw/ExtractedAgreements_Bucketed

Outputs (created under the bucketed directory):
- analysis/
  - bucket_summary_[bucket].json        (per-bucket summaries)
  - bucket_summary_[bucket].md          (markdown view of summary)
  - cross_bucket_comparison.md          (final comparison across buckets)

Environment:
- Requires OPENAI_API_KEY to be set in environment (optionally via .env)

Author: Zirui Song
Date: Sep 2025
"""

import os
import json
import time
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Any, Optional

import pandas as pd
from dotenv import load_dotenv

try:
    # Prefer the modern OpenAI SDK; fall back to legacy if unavailable
    from openai import OpenAI  # type: ignore
    _USE_MODERN_SDK = True
except Exception:
    import openai  # type: ignore
    _USE_MODERN_SDK = False


BUCKETED_DIR = Path("/Users/zrsong/MIT Dropbox/Zirui Song/Research Projects/PSW_Nonbank Direct Lending/Data/Raw/ExtractedAgreements_Bucketed")
ANALYSIS_DIR = BUCKETED_DIR / "analysis"

MODEL = os.getenv("OPENAI_MODEL", "gpt-5-mini")
MAX_COMPLETION_TOKENS = 1500
SAMPLE_PER_BUCKET = 1  # reduced to stay within token limits
FIRST_N_TOKENS: Optional[int] = 20_000  # Further reduced to improve reliability
SLEEP_SECONDS_BETWEEN_CALLS = 1.0
MAX_INPUT_CHARS: Optional[int] = 10_000_000  # Safety cap to stay under API limit (~10.5M)


def ensure_env_loaded():
    try:
        load_dotenv()
    except Exception:
        pass


def get_first_n_tokens(text: str, n_tokens: Optional[int] = FIRST_N_TOKENS) -> str:
    if n_tokens is None:
        return text
    tokens = text.split()
    return " ".join(tokens[:n_tokens])


def list_bucket_dirs(bucketed_dir: Path) -> List[Path]:
    return [p for p in bucketed_dir.iterdir() if p.is_dir() and p.name != "analysis"]


def sample_nc_files(bucket_dir: Path, k: int = SAMPLE_PER_BUCKET) -> List[Path]:
    nc_files = sorted(bucket_dir.glob("*.nc"))
    if len(nc_files) <= k:
        return nc_files
    # simple deterministic sample: take evenly spaced files
    step = max(1, len(nc_files) // k)
    return nc_files[::step][:k]


def read_files_text(file_paths: List[Path]) -> List[Dict[str, Any]]:
    docs = []
    for idx, file_path in enumerate(file_paths, start=1):
        try:
            text = file_path.read_text(encoding="utf-8", errors="ignore")
            text = get_first_n_tokens(text)
            docs.append({
                "index": idx,
                "path": str(file_path),
                "accession": file_path.stem,
                "text": text
            })
        except Exception as e:
            print(f"Warning: failed to read {file_path}: {e}")
    return docs


def build_bucket_prompt(bucket_name: str, docs: List[Dict[str, Any]]) -> str:
    joined = []
    for d in docs:
        joined.append(f"[Doc {d['index']}] accession={d['accession']}\n{d['text']}")
    corpus = "\n\n".join(joined)
    if MAX_INPUT_CHARS is not None and len(corpus) > MAX_INPUT_CHARS:
        corpus = corpus[:MAX_INPUT_CHARS]
    prompt = (
        f"You are an expert credit analyst specializing in accounting provisions. You will read a sample credit agreement "
        f"from the lender-type bucket: '{bucket_name}'. Document is truncated to the first {FIRST_N_TOKENS} tokens.\n\n"
        f"Tasks:\n"
        f"1) Summarize distinctive accounting-related characteristics of these agreements in this bucket:\n"
        f"   - Affirmative covenants (reporting requirements, frequency, content)\n"
        f"   - Performance pricing provisions (spread adjustments based on metrics)\n"
        f"   - Accounting-based financial covenants (leverage ratios, coverage ratios, definitions)\n"
        f"   - Accounting standards provisions (GAAP compliance, changes in accounting principles)\n"
        f"   - Information covenants (ongoing disclosure requirements, materiality thresholds)\n"
        f"   - EBITDA adjustments and add-backs (specific categories, limitations)\n"
        f"   - Financial statement requirements (audited vs. unaudited, delivery timelines)\n"
        f"   - Covenant calculation methodologies and measurement periods\n"
        f"   - Any unique accounting treatments or provisions specific to this lender type\n"
        f"2) Provide 5-10 bullet points focusing on accounting provisions and financial reporting requirements.\n"
        f"3) Provide a one-paragraph synopsis emphasizing accounting-related differences from traditional banks.\n\n"
        f"Documents:\n{corpus}\n\n"
        f"Output strictly as JSON with fields: {{\"bucket\": str, \"bullets\": [str], \"synopsis\": str}}."
    )
    return prompt


def build_cross_bucket_prompt(summaries: List[Dict[str, Any]]) -> str:
    # Keep it compact to fit within context
    compact = []
    for s in summaries:
        bucket = s.get("bucket", "Unknown")
        bullets = s.get("bullets", [])
        synopsis = s.get("synopsis", "")
        compact.append(f"Bucket: {bucket}\nBullets: " + "; ".join(bullets[:8]) + f"\nSynopsis: {synopsis}")
    body = "\n\n".join(compact)
    prompt = (
        "You are an expert credit analyst specializing in accounting provisions. Compare the lender-type buckets below.\n"
        "Focus on similarities and differences in accounting-related provisions across:\n"
        "- Affirmative covenants and reporting requirements\n"
        "- Performance pricing mechanisms and spread adjustments\n"
        "- Financial covenant definitions and calculation methodologies\n"
        "- Accounting standards compliance and treatment of accounting changes\n"
        "- Information covenant requirements and materiality thresholds\n"
        "- EBITDA adjustments, add-backs, and calculation methodologies\n"
        "- Financial statement delivery requirements and audit standards\n"
        "- Covenant measurement periods and calculation frequencies\n"
        "Highlight what uniquely characterizes each bucket's approach to accounting provisions and where there is overlap.\n"
        "Finally, suggest any buckets that may contain misclassified items based on their accounting provision patterns.\n\n"
        f"Bucket Summaries:\n{body}\n\n"
        "Output in Markdown with sections:\n"
        "- Similarities in Accounting Provisions\n- Differences by Bucket (Accounting Focus)\n- Overlaps in Financial Reporting Requirements\n- Potential Misclassifications (Based on Accounting Patterns)\n- Short Executive Summary (Accounting Provisions)"
    )
    return prompt


def call_chatgpt(messages: List[Dict[str, str]]) -> str:
    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        raise RuntimeError("OPENAI_API_KEY not set in environment.")

    if _USE_MODERN_SDK:
        client = OpenAI(api_key=api_key)
        resp = client.chat.completions.create(
            model=MODEL,
            messages=messages,
            max_completion_tokens=MAX_COMPLETION_TOKENS,
        )
        content = resp.choices[0].message.content or ""
        if not content.strip():
            raise RuntimeError("Received empty content from API response")
        return content
    else:
        openai.api_key = api_key
        resp = openai.ChatCompletion.create(
            model=MODEL,
            messages=messages,
            max_tokens=MAX_COMPLETION_TOKENS,
        )
        content = resp["choices"][0]["message"]["content"]
        if not content or not content.strip():
            raise RuntimeError("Received empty content from legacy API response")
        return content


def call_chatgpt_json(messages: List[Dict[str, str]]) -> str:
    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        raise RuntimeError("OPENAI_API_KEY not set in environment.")

    if _USE_MODERN_SDK:
        client = OpenAI(api_key=api_key)
        resp = client.chat.completions.create(
            model=MODEL,
            messages=messages,
            max_completion_tokens=MAX_COMPLETION_TOKENS,
            response_format={"type": "json_object"},
        )
        content = resp.choices[0].message.content or ""
        if not content.strip():
            raise RuntimeError("Received empty content from API response")
        return content
    else:
        openai.api_key = api_key
        # Legacy API does not support response_format; rely on prompt enforcement
        resp = openai.ChatCompletion.create(
            model=MODEL,
            messages=messages,
            max_tokens=MAX_COMPLETION_TOKENS,
        )
        content = resp["choices"][0]["message"]["content"]
        if not content or not content.strip():
            raise RuntimeError("Received empty content from legacy API response")
        return content


def summarize_bucket(bucket_dir: Path) -> Dict[str, Any]:
    bucket_name = bucket_dir.name
    print(f"\nSummarizing bucket: {bucket_name}")

    files = sample_nc_files(bucket_dir, SAMPLE_PER_BUCKET)
    print(f"Sampled {len(files)} files from {bucket_name}")
    docs = read_files_text(files)

    prompt = build_bucket_prompt(bucket_name, docs)
    messages = [
        {"role": "system", "content": "You are a concise, accurate credit analyst. Respond exactly in the requested format."},
        {"role": "user", "content": prompt}
    ]

    # Call API with simple retry
    for attempt in range(3):
        try:
            content = call_chatgpt_json(messages)
            break
        except Exception as e:
            print(f"API error (attempt {attempt + 1}): {e}")
            if attempt == 2:
                raise
            time.sleep(2)

    # Parse JSON if possible
    parsed: Dict[str, Any]
    try:
        parsed = json.loads(content)
    except Exception:
        # Fallback: wrap raw content or provide minimal summary when empty
        fallback_synopsis = content.strip() if content and content.strip() else (
            f"Summary unavailable from API. Bucket '{bucket_name}' analyzed over {len(docs)} sampled documents."
        )
        parsed = {
            "bucket": bucket_name,
            "bullets": [
                f"Documents sampled: {len(docs)}",
                "API returned empty or non-JSON content; using fallback synopsis"
            ],
            "synopsis": fallback_synopsis
        }

    result = {
        "bucket": bucket_name,
        "created_at": datetime.now().isoformat(),
        "num_docs": len(docs),
        "files": [d["path"] for d in docs],
        "summary": parsed,
        "raw_response": content,
    }
    return result


def save_bucket_outputs(bucket_result: Dict[str, Any], analysis_dir: Path) -> None:
    bucket = bucket_result.get("bucket", "Unknown")
    json_path = analysis_dir / f"bucket_summary_{bucket}.json"
    md_path = analysis_dir / f"bucket_summary_{bucket}.md"

    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(bucket_result, f, indent=2)

    # Markdown view
    summary = bucket_result.get("summary", {})
    bullets = summary.get("bullets", [])
    synopsis = summary.get("synopsis", "")

    md = [f"# Bucket: {bucket}", "", "## Key Takeaways"]
    for b in bullets:
        md.append(f"- {b}")
    md.append("")
    md.append("## Synopsis")
    md.append(synopsis)
    md.append("")

    with open(md_path, "w", encoding="utf-8") as f:
        f.write("\n".join(md))


def save_cross_bucket_markdown(markdown_text: str, analysis_dir: Path) -> Path:
    out_path = analysis_dir / "cross_bucket_comparison.md"
    with open(out_path, "w", encoding="utf-8") as f:
        f.write(markdown_text)
    return out_path


def main():
    print("=" * 80)
    print("4a_CompareBucketedLoans.py - Summarize and Compare Bucketed Loans")
    print("=" * 80)

    ensure_env_loaded()

    if not BUCKETED_DIR.exists():
        print(f"Error: Bucketed directory not found: {BUCKETED_DIR}")
        return

    ANALYSIS_DIR.mkdir(parents=True, exist_ok=True)

    bucket_dirs = list_bucket_dirs(BUCKETED_DIR)
    if not bucket_dirs:
        print("No lender-type subfolders found under bucketed directory.")
        return

    # Summarize each bucket
    all_bucket_results: List[Dict[str, Any]] = []
    for bdir in bucket_dirs:
        result = summarize_bucket(bdir)
        save_bucket_outputs(result, ANALYSIS_DIR)
        all_bucket_results.append(result)
        time.sleep(SLEEP_SECONDS_BETWEEN_CALLS)

    # Build cross-bucket comparison
    summaries_for_compare = [r.get("summary", {}) for r in all_bucket_results]
    compare_prompt = build_cross_bucket_prompt(summaries_for_compare)
    compare_messages = [
        {"role": "system", "content": "You are a concise, accurate credit analyst."},
        {"role": "user", "content": compare_prompt}
    ]

    try:
        comparison_md = call_chatgpt(compare_messages)
    except Exception as e:
        print(f"API error during cross-bucket comparison: {e}")
        # Build a minimal local markdown comparison
        lines = [
            "# Cross-Bucket Comparison (Fallback)",
            "",
            "## Similarities",
            "- Model output unavailable; similarities not computed.",
            "",
            "## Differences by Bucket",
        ]
        for s in summaries_for_compare:
            bucket = s.get("bucket", "Unknown")
            bullets = s.get("bullets", [])
            synopsis = s.get("synopsis", "")
            lines.append(f"### {bucket}")
            if bullets:
                for b in bullets[:8]:
                    lines.append(f"- {b}")
            if synopsis:
                lines.append("")
                lines.append(synopsis)
            lines.append("")
        lines += [
            "## Overlaps",
            "- Model output unavailable; overlaps not computed.",
            "",
            "## Potential Misclassifications",
            "- Model output unavailable; not assessed.",
            "",
            "## Short Executive Summary",
            "- This fallback report was generated due to empty API responses.",
        ]
        comparison_md = "\n".join(lines)
    out_path = save_cross_bucket_markdown(comparison_md, ANALYSIS_DIR)

    # Save an index JSON for convenience
    index = {
        "created_at": datetime.now().isoformat(),
        "bucketed_dir": str(BUCKETED_DIR),
        "analysis_dir": str(ANALYSIS_DIR),
        "model": MODEL,
        "max_completion_tokens": MAX_COMPLETION_TOKENS,
        "sample_per_bucket": SAMPLE_PER_BUCKET,
        "first_n_tokens": FIRST_N_TOKENS,
        "buckets": [r.get("bucket", "Unknown") for r in all_bucket_results],
        "artifacts": {
            "cross_bucket_comparison_md": str(out_path),
            "bucket_summaries": [f"bucket_summary_{r.get('bucket', 'Unknown')}.json" for r in all_bucket_results]
        }
    }
    with open(ANALYSIS_DIR / "analysis_index.json", "w", encoding="utf-8") as f:
        json.dump(index, f, indent=2)

    print("\n" + "=" * 80)
    print("COMPARISON COMPLETE")
    print("=" * 80)
    print(f"Analysis directory: {ANALYSIS_DIR}")
    print(f"Buckets summarized: {len(all_bucket_results)}")
    print(f"Cross-bucket report: {out_path}")


if __name__ == "__main__":
    main()


