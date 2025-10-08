#!/usr/bin/env python3
"""
3d_FilterNonbankLoans.py

Filters batch_results.csv to exclude traditional banks and missing borrower/lender names,
then organizes the remaining loans by lender type into subfolders.

Inputs:
- ../Data/Raw/batch_jobs/processed/batch_results.csv

Outputs:
- ../Data/Raw/ExtractedAgreements_Bucketed/
  - [lender_type]/ (subfolders for each lender type)
    - [accession].nc (copied .nc files)

Lender Types (from 3a_ExtractLenderNames.py):
- Traditional Banks (EXCLUDED - filtered out)
- Finance Companies (Bank-Affiliated)
- Finance Companies (Nonbank)
- Investment Companies
- Insurance Companies
- Business Development Companies (BDCs)
- Private Equity / Venture Capital Credit Arms
- Hedge Funds
- Investment Managers / Asset Managers
- Specialty Direct Lenders / Private Credit

Author: Zirui Song
Date: Sep 2025
"""

import pandas as pd
import os
import shutil
from pathlib import Path
import re


def clean_lender_type(lender_type):
    """Clean lender type string for use as folder name"""
    if pd.isna(lender_type) or lender_type == "":
        return "Unknown"
    
    # Remove special characters and replace spaces with underscores
    clean_type = re.sub(r'[^\w\s-]', '', str(lender_type))
    clean_type = re.sub(r'\s+', '_', clean_type.strip())
    return clean_type


def generate_sec_url(accession):
    """Generate SEC EDGAR URL from accession number"""
    if pd.isna(accession) or accession == "":
        return ""
    
    # Extract CIK from accession number (first 10 digits)
    # Format: 0000883237-21-000233 -> CIK: 883237
    accession_str = str(accession).strip()
    if len(accession_str) >= 10:
        cik = accession_str[:10].lstrip('0')  # Remove leading zeros
        if cik == "":
            cik = "0"
        return f"https://www.sec.gov/Archives/edgar/data/{cik}/{accession_str}/{accession_str}-index.html"
    return ""


def main():
    print("=" * 80)
    print("3d_FilterNonbankLoans.py - Filter and organize loans by lender type")
    print("=" * 80)
    
    # Set up paths
    script_dir = Path(__file__).parent
    results_file = script_dir / ".." / "Data" / "Raw" / "batch_jobs" / "processed" / "batch_results.csv"
    source_nc_dir = script_dir / ".." / "Data" / "Raw" / "ExtractedAgreements_FinalMapped"
    output_dir = script_dir / ".." / "Data" / "Raw" / "ExtractedAgreements_Bucketed"
    
    # Check if input files exist
    if not results_file.exists():
        print(f"Error: Results file not found: {results_file}")
        return
    
    if not source_nc_dir.exists():
        print(f"Error: Source NC files directory not found: {source_nc_dir}")
        return
    
    # Load batch results
    print("Loading batch results...")
    try:
        df = pd.read_csv(results_file)
        print(f"Loaded {len(df)} records from batch results")
    except Exception as e:
        print(f"Error loading batch results: {e}")
        return
    
    # Check required columns
    required_cols = ['accession', 'borrower_name', 'lender_name', 'lender_type']
    missing_cols = [col for col in required_cols if col not in df.columns]
    if missing_cols:
        print(f"Error: Missing required columns: {missing_cols}")
        print(f"Available columns: {list(df.columns)}")
        return
    
    # Filter out traditional banks and missing data
    print("\nFiltering data...")
    initial_count = len(df)
    
    # Remove traditional banks
    traditional_banks = ['Traditional Banks']
    df_filtered = df[~df['lender_type'].isin(traditional_banks)]
    traditional_removed = initial_count - len(df_filtered)
    print(f"Removed {traditional_removed} traditional bank loans")
    
    # Remove missing borrower, lender names, or lender type
    df_filtered = df_filtered.dropna(subset=['borrower_name', 'lender_name', 'lender_type'])
    df_filtered = df_filtered[(df_filtered['borrower_name'] != '') & (df_filtered['lender_name'] != '') & (df_filtered['lender_type'] != '')]
    missing_removed = len(df) - traditional_removed - len(df_filtered)
    print(f"Removed {missing_removed} loans with missing borrower/lender names or lender type")
    
    print(f"Final filtered dataset: {len(df_filtered)} loans")
    
    if len(df_filtered) == 0:
        print("No loans remaining after filtering!")
        return
    
    # Show lender type distribution
    print("\nLender type distribution:")
    lender_type_counts = df_filtered['lender_type'].value_counts()
    for lender_type, count in lender_type_counts.items():
        print(f"  {lender_type}: {count}")
    
    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Group by lender type and create subfolders
    print(f"\nOrganizing loans by lender type...")
    organized_count = 0
    missing_nc_count = 0
    
    for lender_type, group in df_filtered.groupby('lender_type'):
        # Clean lender type for folder name
        folder_name = clean_lender_type(lender_type)
        lender_dir = output_dir / folder_name
        lender_dir.mkdir(exist_ok=True)
        
        print(f"Processing {lender_type} -> {folder_name}/ ({len(group)} loans)")
        
        for _, row in group.iterrows():
            accession = row['accession']
            source_nc_file = source_nc_dir / f"{accession}.nc"
            dest_nc_file = lender_dir / f"{accession}.nc"
            
            if source_nc_file.exists():
                try:
                    shutil.copy2(source_nc_file, dest_nc_file)
                    organized_count += 1
                except Exception as e:
                    print(f"  Warning: Failed to copy {accession}.nc: {e}")
            else:
                missing_nc_count += 1
                print(f"  Warning: Source file not found: {source_nc_file}")
    
    # Create summary
    summary = {
        "filtering_summary": {
            "initial_loans": initial_count,
            "traditional_banks_removed": traditional_removed,
            "missing_data_removed": missing_removed,
            "final_loans": len(df_filtered)
        },
        "organization_summary": {
            "loans_organized": organized_count,
            "missing_nc_files": missing_nc_count,
            "lender_types": lender_type_counts.to_dict()
        }
    }
    
    # Save summary
    summary_file = output_dir / "filtering_summary.json"
    import json
    with open(summary_file, 'w') as f:
        json.dump(summary, f, indent=2)
    
    # Add SEC URL column
    print("Adding SEC EDGAR URLs...")
    df_filtered['sec_url'] = df_filtered['accession'].apply(generate_sec_url)
    
    # Save filtered CSV (sorted by lender_type and then lender_name)
    filtered_csv = output_dir / "filtered_batch_results.csv"
    df_filtered_sorted = df_filtered.sort_values(['lender_type', 'lender_name'])
    df_filtered_sorted.to_csv(filtered_csv, index=False)
    
    print(f"\n" + "=" * 80)
    print("FILTERING AND ORGANIZATION COMPLETE")
    print("=" * 80)
    print(f"Output directory: {output_dir}")
    print(f"Loans organized: {organized_count}")
    print(f"Missing NC files: {missing_nc_count}")
    print(f"Summary file: {summary_file}")
    print(f"Filtered CSV: {filtered_csv}")
    print(f"\nSubfolders created:")
    for subfolder in sorted(output_dir.iterdir()):
        if subfolder.is_dir():
            nc_count = len(list(subfolder.glob("*.nc")))
            print(f"  {subfolder.name}/ ({nc_count} files)")


if __name__ == "__main__":
    main()
