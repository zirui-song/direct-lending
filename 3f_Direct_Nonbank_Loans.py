#!/usr/bin/env python3
"""
3f_Direct_Nonbank_Loans.py

Merges direct loans identified from .nc text analysis with filtered nonbank loan results.

Inputs:
- ../Data/Intermediate/3e_DirectLoans_FromNC.csv
- ../Data/Raw/ExtractedAgreements_Bucketed/filtered_batch_results_manual_check.csv

Outputs:
- ../Data/Intermediate/3f_Direct_Nonbank_Loans.csv
  Contains merged data with direct loan flags and nonbank loan information.
  Uses outer join to keep all observations from both datasets (matched and unmatched).

Author: Zirui Song
Date: Dec 2025
"""

import pandas as pd
from pathlib import Path


def main():
    print("=" * 80)
    print("3f_Direct_Nonbank_Loans.py - Merge Direct Loans with Nonbank Loan Data")
    print("=" * 80)
    
    # Set up paths
    script_dir = Path(__file__).parent
    direct_loans_csv = script_dir / ".." / "Data" / "Intermediate" / "3e_DirectLoans_FromNC.csv"
    filtered_results_csv = script_dir / ".." / "Data" / "Raw" / "ExtractedAgreements_Bucketed" / "filtered_batch_results_manual_check.csv"
    output_csv = script_dir / ".." / "Data" / "Intermediate" / "3f_Direct_Nonbank_Loans.csv"
    
    # Check if input files exist
    if not direct_loans_csv.exists():
        print(f"Error: Direct loans CSV not found: {direct_loans_csv}")
        return
    
    if not filtered_results_csv.exists():
        print(f"Error: Filtered batch results CSV not found: {filtered_results_csv}")
        return
    
    # Load direct loans data
    print("\nLoading direct loans data...")
    try:
        df_direct = pd.read_csv(direct_loans_csv)
        print(f"Loaded {len(df_direct)} records from direct loans CSV")
        print(f"Columns: {list(df_direct.columns)}")
    except Exception as e:
        print(f"Error loading direct loans CSV: {e}")
        return
    
    # Load filtered batch results
    print("\nLoading filtered batch results...")
    try:
        df_filtered = pd.read_csv(filtered_results_csv)
        print(f"Loaded {len(df_filtered)} records from filtered batch results")
        print(f"Columns: {list(df_filtered.columns)}")
    except Exception as e:
        print(f"Error loading filtered batch results CSV: {e}")
        return
    
    # Check for accession column in both datasets
    if 'accession' not in df_direct.columns:
        print(f"Error: 'accession' column not found in direct loans CSV")
        print(f"Available columns: {list(df_direct.columns)}")
        return
    
    if 'accession' not in df_filtered.columns:
        print(f"Error: 'accession' column not found in filtered batch results CSV")
        print(f"Available columns: {list(df_filtered.columns)}")
        return
    
    # Merge on accession (outer join to keep all observations)
    print("\nMerging datasets on accession (outer join)...")
    df_merged = df_filtered.merge(
        df_direct,
        on='accession',
        how='outer',
        suffixes=('', '_direct')
    )
    
    print(f"Merged dataset: {len(df_merged)} records")
    
    # Show merge statistics
    print(f"\nMerge statistics:")
    print(f"  Direct loans records: {len(df_direct)}")
    print(f"  Filtered batch results records: {len(df_filtered)}")
    print(f"  Total merged records (outer join): {len(df_merged)}")
    
    # Count matched vs unmatched
    matched = df_merged['accession'].notna().sum()  # All should have accession
    only_direct = df_merged[df_merged['borrower_name'].isna()].shape[0] if 'borrower_name' in df_merged.columns else 0
    only_filtered = df_merged[df_merged['direct_from_text'].isna()].shape[0] if 'direct_from_text' in df_merged.columns else 0
    both = len(df_merged) - only_direct - only_filtered
    
    print(f"  Matched records (in both): {both}")
    print(f"  Only in direct loans CSV: {only_direct}")
    print(f"  Only in filtered batch results CSV: {only_filtered}")
    
    # Handle sec_url column conflict (both datasets have sec_url, merge creates sec_url and sec_url_direct)
    if 'sec_url_direct' in df_merged.columns:
        # Combine sec_url columns: prefer sec_url_direct (from direct loans) if available, otherwise use sec_url (from filtered results)
        df_merged['sec_url'] = df_merged['sec_url_direct'].fillna(df_merged.get('sec_url', ''))
        # Drop the sec_url_direct column
        df_merged = df_merged.drop(columns=['sec_url_direct'])
        print(f"\nCombined sec_url columns into single sec_url column")
    
    # Drop first_3000_chars column if it exists
    if 'first_3000_chars' in df_merged.columns:
        df_merged = df_merged.drop(columns=['first_3000_chars'])
        print(f"Dropped first_3000_chars column")
    
    # Reorder columns to put sec_url last
    if 'sec_url' in df_merged.columns:
        cols = [col for col in df_merged.columns if col != 'sec_url'] + ['sec_url']
        df_merged = df_merged[cols]
        print(f"Moved sec_url to last column")
    
    # Ensure output directory exists
    output_csv.parent.mkdir(parents=True, exist_ok=True)
    
    # Save merged dataset
    df_merged.to_csv(output_csv, index=False)
    print(f"\nSaved merged dataset to: {output_csv}")
    
    # Show summary of direct loans in merged data
    if 'direct_from_text' in df_merged.columns:
        direct_count = df_merged['direct_from_text'].sum() if df_merged['direct_from_text'].dtype in ['int64', 'float64'] else (df_merged['direct_from_text'] == 1).sum()
        print(f"\nDirect loans in merged dataset: {direct_count}")
    
    print("=" * 80)
    print("DONE")
    print("=" * 80)


if __name__ == "__main__":
    main()

