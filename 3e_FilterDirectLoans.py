#!/usr/bin/env python3
"""
3e_FilterDirectLoans.py

Scans .nc files in the ExtractedAgreements folder and flags loans as
direct if the first 1000 characters (or up to "table of contents") do NOT contain any of the following
keywords (case-insensitive):
  - "syndication"
  - "syndicated"
  - "documentation"
  - "lead arranger"
  - "bookrunner" / "book runner"
  - "lenders"
  - "banks"

Inputs:
- ../Data/Raw/ExtractedAgreements/*.nc

Outputs:
- ../Data/Intermediate/3e_DirectLoans_FromNC.csv
  Contains one row per direct loan with:
    - accession
    - direct_from_text (1 if no specified keywords in first 1000 chars or up to "table of contents")
    - first_1000_chars (first 1000 characters or up to "table of contents", whitespace normalized)
    - sec_url

Author: Zirui Song
Date: Dec 2025
"""

from pathlib import Path
import pandas as pd
import re


def normalize_whitespace(text: str) -> str:
    """
    Normalize whitespace in text by replacing multiple spaces, newlines,
    tabs, and other whitespace characters with a single space.
    Also strips leading and trailing whitespace.
    """
    if not text:
        return ""
    # Replace all whitespace characters (spaces, newlines, tabs, etc.) with a single space
    normalized = re.sub(r'\s+', ' ', text)
    # Strip leading and trailing whitespace
    return normalized.strip()


def extract_first_chars(file_path: Path, num_chars: int = 1000) -> str:
    """
    Read text from file, stopping at either:
    1. First num_chars characters, OR
    2. When "table of contents" is encountered (case-insensitive)
    
    Returns the extracted text snippet with normalized whitespace.
    """
    try:
        with file_path.open("r", encoding="utf-8", errors="ignore") as f:
            # Read text character by character or in chunks to find "table of contents"
            text = ""
            chunk_size = 100
            toc_pattern = "table of contents"
            toc_found = False
            
            while len(text) < num_chars:
                chunk = f.read(chunk_size)
                if not chunk:
                    break
                
                text += chunk
                text_lower = text.lower()
                
                # Check if "table of contents" appears in the accumulated text
                if toc_pattern in text_lower:
                    # Find the position where "table of contents" starts
                    toc_pos = text_lower.find(toc_pattern)
                    # Stop at the start of "table of contents"
                    text = text[:toc_pos]
                    toc_found = True
                    break
            
            # If we didn't find TOC, truncate to num_chars
            if not toc_found and len(text) > num_chars:
                text = text[:num_chars]
            
            # Normalize whitespace (replace multiple spaces/newlines with single space)
            text = normalize_whitespace(text)
            
            return text
    except Exception as e:
        print(f"  Warning: Failed to read {file_path.name}: {e}")
        return ""


def is_direct_loan(text: str) -> bool:
    """
    Return True if `text` does NOT contain any of the specified
    syndicated/arranged-loan keywords (case-insensitive).
    
    Note: We look for plural forms and specific syndication-related terms
    to avoid false positives from common words like "documentation" or
    singular "lender" which appear in direct loans too.
    """
    text_lower = (text or "").lower()

    keywords = [
        "syndication",
        "syndicated",
        "syndicate",
        "lead arranger",
        "bookrunner",
        "book runner",
        "arranger",
        # Use plural forms to avoid matching singular "lender" in direct loans
        "lenders",  # e.g., "the lenders", "among the lenders"
        "banks",    # e.g., "the banks", "among the banks"
        # More specific syndication terms
        "syndication agent",
        "administrative agent",
        "co-arranger",
        "coarranger",
    ]

    return not any(kw in text_lower for kw in keywords)


def generate_sec_url(accession: str) -> str:
    """
    Generate SEC EDGAR URL from accession number.
    Mirrors logic in 3d_FilterNonbankLoans.py.
    """
    if accession is None or accession == "":
        return ""

    accession_str = str(accession).strip()
    if len(accession_str) >= 10:
        # Extract CIK from first 10 digits and strip leading zeros
        cik = accession_str[:10].lstrip("0")
        if cik == "":
            cik = "0"
        return f"https://www.sec.gov/Archives/edgar/data/{cik}/{accession_str}/{accession_str}-index.html"
    return ""


def filter_direct_loans_from_loan_terms(loan_terms_csv: Path) -> pd.DataFrame:
    """
    Filter direct loans from loan_terms_cleaned_all_20251007.csv based on:
    - Single lender (no ";" separator)
    - Single lead_arranger (empty or no ";")
    - No mentions of "lenders", "banks", "syndicat" in text fields
    
    Returns DataFrame with direct loans identified from structured data.
    """
    print("\n" + "=" * 80)
    print("Method 2: Filtering direct loans from loan terms CSV")
    print("=" * 80)
    
    if not loan_terms_csv.exists():
        print(f"Error: Loan terms file not found: {loan_terms_csv}")
        return pd.DataFrame()
    
    print(f"Loading loan terms from: {loan_terms_csv}")
    df_loan_terms = pd.read_csv(loan_terms_csv)
    print(f"  Loaded {len(df_loan_terms)} loan records")
    
    # Convert columns to string for processing
    df_loan_terms = df_loan_terms.copy()
    
    # Check for single lender (no ";" separator) or empty
    if 'lender' in df_loan_terms.columns:
        df_loan_terms['lender_str'] = df_loan_terms['lender'].astype(str).fillna('')
        # Replace 'nan' string with empty string
        df_loan_terms['lender_str'] = df_loan_terms['lender_str'].replace('nan', '')
        single_lender = (~df_loan_terms['lender_str'].str.contains(';', na=False)) | \
                        (df_loan_terms['lender_str'] == '')
        print(f"  Loans with single/empty lender: {single_lender.sum()}")
    else:
        print("  Warning: 'lender' column not found")
        single_lender = pd.Series([True] * len(df_loan_terms))
    
    # Check for single lead_arranger (empty or no ";")
    if 'lead_arranger' in df_loan_terms.columns:
        df_loan_terms['lead_arranger_str'] = df_loan_terms['lead_arranger'].astype(str).fillna('')
        # Replace 'nan' string with empty string
        df_loan_terms['lead_arranger_str'] = df_loan_terms['lead_arranger_str'].replace('nan', '')
        # Empty or no ";" separator
        single_lead_arranger = (df_loan_terms['lead_arranger_str'] == '') | \
                              (~df_loan_terms['lead_arranger_str'].str.contains(';', na=False))
        print(f"  Loans with single/empty lead_arranger: {single_lead_arranger.sum()}")
    else:
        print("  Warning: 'lead_arranger' column not found")
        single_lead_arranger = pd.Series([True] * len(df_loan_terms))
    
    # Check if both lender and lead_arranger are empty
    if 'lender' in df_loan_terms.columns and 'lead_arranger' in df_loan_terms.columns:
        both_empty = (df_loan_terms['lender_str'] == '') & (df_loan_terms['lead_arranger_str'] == '')
        print(f"  Loans with both lender and lead_arranger empty: {both_empty.sum()}")
        # If both are empty, consider it direct
        single_lender = single_lender | both_empty
        single_lead_arranger = single_lead_arranger | both_empty
    
    # Check for absence of keywords in text fields
    # Combine relevant text columns
    text_columns = ['lender', 'lead_arranger', 'borrower']
    available_text_cols = [col for col in text_columns if col in df_loan_terms.columns]
    
    if available_text_cols:
        # Combine all text fields into one string for keyword search
        combined_text = df_loan_terms[available_text_cols].astype(str).fillna('').agg(' '.join, axis=1).str.lower()
        
        # Check for absence of keywords
        no_lenders = ~combined_text.str.contains('lenders', na=False, regex=False)
        no_banks = ~combined_text.str.contains('banks', na=False, regex=False)
        no_syndicat = ~combined_text.str.contains('syndicat', na=False, regex=False)
        
        no_keywords = no_lenders & no_banks & no_syndicat
        print(f"  Loans without 'lenders', 'banks', 'syndicat' keywords: {no_keywords.sum()}")
    else:
        print("  Warning: No text columns found for keyword search")
        no_keywords = pd.Series([True] * len(df_loan_terms))
    
    # Combine all conditions
    is_direct = single_lender & single_lead_arranger & no_keywords
    
    df_direct = df_loan_terms[is_direct].copy()
    print(f"\n  Total direct loans identified: {len(df_direct)}")
    
    # Create output with required columns (keep lender and lead_arranger)
    output_cols = ['accession']
    if 'lender' in df_direct.columns:
        output_cols.append('lender')
    if 'lead_arranger' in df_direct.columns:
        output_cols.append('lead_arranger')
    
    result = df_direct[output_cols].copy()
    result['direct_from_loan_terms'] = 1
    
    # Generate sec_url
    result['sec_url'] = result['accession'].apply(generate_sec_url)
    
    # Reorder columns: accession, lender, lead_arranger, direct_from_loan_terms, sec_url
    final_cols = ['accession']
    if 'lender' in result.columns:
        final_cols.append('lender')
    if 'lead_arranger' in result.columns:
        final_cols.append('lead_arranger')
    final_cols.extend(['direct_from_loan_terms', 'sec_url'])
    result = result[final_cols]
    
    return result


def main():
    print("=" * 80)
    print("3e_FilterDirectLoans.py - Identify direct loans from .nc text")
    print("=" * 80)

    script_dir = Path(__file__).parent
    nc_dir = script_dir / ".." / "Data" / "Raw" / "ExtractedAgreements"
    output_dir = script_dir / ".." / "Data" / "Intermediate"
    output_file = output_dir / "3e_DirectLoans_FromNC.csv"
    loan_terms_csv = script_dir / ".." / "Data" / "Intermediate" / "loan_terms_cleaned_all_20251007.csv"

    # Check input directory
    if not nc_dir.exists():
        print(f"Error: NC files directory not found: {nc_dir}")
        return

    nc_files = sorted(nc_dir.glob("*.nc"))
    if not nc_files:
        print(f"No .nc files found in: {nc_dir}")
        return

    print(f"Found {len(nc_files)} .nc files to scan")

    records = []
    total_files = 0
    direct_count = 0

    for nc_path in nc_files:
        total_files += 1
        text = extract_first_chars(nc_path, num_chars=1000)
        
        if not text:
            continue

        direct_flag = is_direct_loan(text)

        if direct_flag:
            direct_count += 1
            accession = nc_path.stem  # filename without .nc
            records.append(
                {
                    "accession": accession,
                    "direct_from_text": 1,
                    "first_1000_chars": text,
                    "sec_url": generate_sec_url(accession),
                }
            )

        if total_files % 100 == 0:
            print(f"  Processed {total_files} files... Direct so far: {direct_count}")

    print(f"\nFinished scanning {total_files} files.")
    print(f"Identified {direct_count} direct loans (no syndication keywords in first 1000 characters or up to 'table of contents').")

    # Ensure output directory exists
    output_dir.mkdir(parents=True, exist_ok=True)

    # Method 1: From .nc files
    df_nc = None
    if records:
        df_nc = pd.DataFrame(records)
        df_nc.to_csv(output_file, index=False)
        print(f"Saved direct loans CSV (from .nc files) to: {output_file}")
    else:
        print("No direct loans identified from .nc files; no CSV written.")
    
    # Method 2: From loan terms CSV
    df_loan_terms = filter_direct_loans_from_loan_terms(loan_terms_csv)
    
    if not df_loan_terms.empty:
        output_file_loan_terms = output_dir / "3e_DirectLoans_FromLoanTerms.csv"
        df_loan_terms.to_csv(output_file_loan_terms, index=False)
        print(f"Saved direct loans CSV (from loan terms) to: {output_file_loan_terms}")
        
        # Merge both methods if both exist
        if df_nc is not None and not df_nc.empty:
            # Merge on accession, keeping lender and lead_arranger from loan terms
            merge_cols = ['accession', 'direct_from_loan_terms']
            if 'lender' in df_loan_terms.columns:
                merge_cols.append('lender')
            if 'lead_arranger' in df_loan_terms.columns:
                merge_cols.append('lead_arranger')
            
            df_merged = df_nc.merge(
                df_loan_terms[merge_cols], 
                on='accession', 
                how='outer'
            )
            # Fill missing values
            df_merged['direct_from_text'] = df_merged['direct_from_text'].fillna(0).astype(int)
            df_merged['direct_from_loan_terms'] = df_merged['direct_from_loan_terms'].fillna(0).astype(int)
            # Create combined direct flag (1 if either method identifies as direct)
            df_merged['direct'] = ((df_merged['direct_from_text'] == 1) | 
                                   (df_merged['direct_from_loan_terms'] == 1)).astype(int)
            
            # Ensure sec_url is present (prefer from NC method, fallback to loan terms)
            if 'sec_url' not in df_merged.columns or df_merged['sec_url'].isna().any():
                df_merged['sec_url'] = df_merged['sec_url'].fillna(
                    df_merged['accession'].apply(generate_sec_url)
                )
            
            # Reorder columns: accession, lender, lead_arranger, then other columns
            col_order = ['accession']
            if 'lender' in df_merged.columns:
                col_order.append('lender')
            if 'lead_arranger' in df_merged.columns:
                col_order.append('lead_arranger')
            # Add remaining columns
            remaining_cols = [c for c in df_merged.columns if c not in col_order]
            col_order.extend(remaining_cols)
            df_merged = df_merged[col_order]
            
            output_file_merged = output_dir / "3e_DirectLoans_Combined.csv"
            df_merged.to_csv(output_file_merged, index=False)
            print(f"Saved combined direct loans CSV to: {output_file_merged}")
            print(f"  Total unique direct loans (either method): {len(df_merged[df_merged['direct'] == 1])}")
            print(f"  From .nc files only: {len(df_merged[(df_merged['direct_from_text'] == 1) & (df_merged['direct_from_loan_terms'] == 0)])}")
            print(f"  From loan terms only: {len(df_merged[(df_merged['direct_from_text'] == 0) & (df_merged['direct_from_loan_terms'] == 1)])}")
            print(f"  From both methods: {len(df_merged[(df_merged['direct_from_text'] == 1) & (df_merged['direct_from_loan_terms'] == 1)])}")

    print("=" * 80)
    print("DONE")
    print("=" * 80)


if __name__ == "__main__":
    main()


