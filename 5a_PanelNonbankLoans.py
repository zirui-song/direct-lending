#!/usr/bin/env python3
"""
5a_PanelNonbankLoans.py

Creates a panel dataset by merging filtered nonbank loans with loan terms data.

Inputs:
- ../Data/Raw/ExtractedAgreements_Bucketed/filtered_batch_results.csv
- ../Data/Intermediate/loan_terms_cleaned_all_20251007.csv
- ../Data/Cleaned/agreements_comp_crsp_merged.csv

Outputs:
- ../Data/Intermediate/5a_PanelNonbankLoans.csv

Author: Zirui Song
Date: Sep 2025
"""

import pandas as pd
import numpy as np
from pathlib import Path
import os


def main():
    print("=" * 80)
    print("5a_PanelNonbankLoans.py - Create Panel Dataset of Nonbank Loans")
    print("=" * 80)
    
    # Set up paths
    script_dir = Path(__file__).parent
    filtered_csv = script_dir / ".." / "Data" / "Raw" / "ExtractedAgreements_Bucketed" / "filtered_batch_results_manual_check.csv"
    loan_terms_csv = script_dir / ".." / "Data" / "Intermediate" / "loan_terms_cleaned_all_20251007.csv"
    firm_financials_csv = script_dir / ".." / "Data" / "Cleaned" / "agreements_comp_crsp_merged.csv"
    output_csv = script_dir / ".." / "Data" / "Intermediate" / "5a_PanelNonbankLoans.csv"
    
    # Check if input files exist
    if not filtered_csv.exists():
        print(f"Error: Filtered CSV not found: {filtered_csv}")
        return
    
    if not loan_terms_csv.exists():
        print(f"Error: Loan terms CSV not found: {loan_terms_csv}")
        return
    
    if not firm_financials_csv.exists():
        print(f"Error: Firm financials CSV not found: {firm_financials_csv}")
        return
    
    # Load filtered batch results
    print("Loading filtered batch results...")
    try:
        df_filtered = pd.read_csv(filtered_csv)
        print(f"Loaded {len(df_filtered)} records from filtered batch results")
        print(f"Columns: {list(df_filtered.columns)}")
    except Exception as e:
        print(f"Error loading filtered CSV: {e}")
        return
    
    # Load loan terms data
    print("\nLoading loan terms data...")
    try:
        df_loan_terms = pd.read_csv(loan_terms_csv)
        print(f"Loaded {len(df_loan_terms)} records from loan terms data")
        print(f"Columns: {list(df_loan_terms.columns)}")
    except Exception as e:
        print(f"Error loading loan terms CSV: {e}")
        return
    
    # Load firm financials data
    print("\nLoading firm financials data...")
    try:
        df_firm_financials = pd.read_csv(firm_financials_csv)
        print(f"Loaded {len(df_firm_financials)} records from firm financials data")
        print(f"Columns: {list(df_firm_financials.columns)}")
    except Exception as e:
        print(f"Error loading firm financials CSV: {e}")
        return
    
    # Check if accession column exists in both datasets
    if 'accession' not in df_filtered.columns:
        print("Error: 'accession' column not found in filtered CSV")
        return
    
    if 'accession' not in df_loan_terms.columns:
        print("Error: 'accession' column not found in loan terms CSV")
        return
    
    # Perform inner merge on accession
    print(f"\nMerging datasets on accession...")
    df_merged = df_filtered.merge(df_loan_terms, on='accession', how='inner')
    print(f"Merged dataset: {len(df_merged)} records")
    
    # Extract year from deal_active_date for merging with firm financials
    if 'deal_active_date' in df_merged.columns:
        print("Extracting year from deal_active_date...")
        df_merged['deal_active_date'] = pd.to_datetime(df_merged['deal_active_date'], errors='coerce')
        df_merged['year'] = df_merged['deal_active_date'].dt.year
        print(f"Year range: {df_merged['year'].min()} - {df_merged['year'].max()}")
    
    # Merge with firm financials using accession -> filing_id
    print(f"\nMerging with firm financials using accession -> filing_id...")
    
    # Check if filing_id exists in firm financials for merging
    if 'filing_id' not in df_firm_financials.columns:
        print("Error: filing_id column not found in firm financials")
        print(f"Available columns: {list(df_firm_financials.columns)}")
        print("Using loan terms data only...")
        df_final = df_merged
    else:
        print("Merging with firm financials using accession -> filing_id...")
        df_final = df_merged.merge(df_firm_financials, left_on='accession', right_on='filing_id', how='inner')
        print(f"Final merged dataset: {len(df_final)} records")
    
    if len(df_final) == 0:
        print("Warning: No matching records found after all merges")
        print("Checking merge steps...")
        
        print(f"Records after loan terms merge: {len(df_merged)}")
        print(f"Records after firm financials merge: {len(df_final)}")
        
        # Check gvkey and year availability
        if len(df_merged) > 0:
            gvkey_missing = df_merged['gvkey'].isna().sum()
            year_missing = df_merged['year'].isna().sum()
            print(f"Missing gvkey: {gvkey_missing}/{len(df_merged)}")
            print(f"Missing year: {year_missing}/{len(df_merged)}")
        return
    
    # Show merge statistics
    print(f"\nMerge Statistics:")
    print(f"Records in filtered CSV: {len(df_filtered)}")
    print(f"Records in loan terms CSV: {len(df_loan_terms)}")
    print(f"Records in firm financials CSV: {len(df_firm_financials)}")
    print(f"Records after loan terms merge: {len(df_merged)}")
    print(f"Records after firm financials merge: {len(df_final)}")
    print(f"Overall match rate: {len(df_final)/len(df_filtered)*100:.1f}%")
    
    # Show lender type distribution in final data
    print(f"\nLender type distribution in final dataset:")
    if 'lender_type' in df_final.columns:
        lender_counts = df_final['lender_type'].value_counts()
        for lender_type, count in lender_counts.items():
            print(f"  {lender_type}: {count}")
    
    # Show column information
    print(f"\nFinal dataset columns ({len(df_final.columns)} total):")
    for i, col in enumerate(df_final.columns, 1):
        print(f"  {i:2d}. {col}")
    
    # Save final dataset
    print(f"\nSaving final dataset to: {output_csv}")
    try:
        df_final.to_csv(output_csv, index=False)
        print("Dataset saved successfully!")
    except Exception as e:
        print(f"Error saving dataset: {e}")
        return
    
    # Create summary statistics
    summary = {
        "merge_date": pd.Timestamp.now().isoformat(),
        "input_files": {
            "filtered_csv": str(filtered_csv),
            "loan_terms_csv": str(loan_terms_csv),
            "firm_financials_csv": str(firm_financials_csv)
        },
        "merge_stats": {
            "filtered_records": len(df_filtered),
            "loan_terms_records": len(df_loan_terms),
            "firm_financials_records": len(df_firm_financials),
            "after_loan_terms_merge": len(df_merged),
            "final_merged_records": len(df_final),
            "overall_match_rate_pct": round(len(df_final)/len(df_filtered)*100, 1)
        },
        "output_file": str(output_csv)
    }
    
    # Save summary
    summary_file = script_dir / ".." / "Data" / "Intermediate" / "5a_PanelNonbankLoans_summary.json"
    import json
    with open(summary_file, 'w') as f:
        json.dump(summary, f, indent=2)
    
    print(f"\n" + "=" * 80)
    print("PANEL DATASET CREATION COMPLETE")
    print("=" * 80)
    print(f"Output file: {output_csv}")
    print(f"Summary file: {summary_file}")
    print(f"Total records: {len(df_final)}")
    print(f"Total columns: {len(df_final.columns)}")


if __name__ == "__main__":
    main()
