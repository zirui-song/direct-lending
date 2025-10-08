#!/usr/bin/env python3
"""
6b_MergeInfoCov.py

Merge aggregated information covenants with panel data from 5c_PanelAllLoans.csv.

Inner joins the information covenants data with the panel data on accession
to create a comprehensive dataset with both loan characteristics and covenant requirements.

Inputs:
- ../Data/Intermediate/6a_InformationCovenants_Aggregated.csv (covenant data)
- ../Data/Intermediate/5c_PanelAllLoans.csv (panel data)

Outputs:
- ../Data/Intermediate/6b_PanelWithInfoCovenants.csv (merged panel with covenants)

Author: Zirui Song
Date: Oct 2025
"""

import pandas as pd
import numpy as np
from pathlib import Path


def merge_covenants_with_panel(covenants_file, panel_file, output_file):
    """
    Merge information covenants with panel data.
    
    Args:
        covenants_file: Path to aggregated covenants CSV
        panel_file: Path to panel data CSV
        output_file: Path to save merged data
    """
    print("Loading aggregated information covenants...")
    df_covenants = pd.read_csv(covenants_file)
    print(f"  Covenants data: {len(df_covenants)} rows, {len(df_covenants.columns)} columns")
    
    print("Loading panel data...")
    df_panel = pd.read_csv(panel_file)
    print(f"  Panel data: {len(df_panel)} rows, {len(df_panel.columns)} columns")
    
    # Check for accession column in both datasets
    if 'accession' not in df_covenants.columns:
        print("Error: 'accession' column not found in covenants data")
        return None
        
    if 'accession' not in df_panel.columns:
        print("Error: 'accession' column not found in panel data")
        return None
    
    print(f"\nMerging on accession column (left join)...")
    
    # Perform left join on accession (keep all panel observations)
    df_merged = df_panel.merge(df_covenants, on='accession', how='left')
    
    # Fill missing covenant values with 0 (no covenants found)
    covenant_cols = ['monthly_fs', 'projected_fs', 'lender_meeting', 'total_info_covenants']
    for col in covenant_cols:
        if col in df_merged.columns:
            df_merged[col] = df_merged[col].fillna(0).astype(int)
    
    # Fill missing context columns with empty strings
    context_cols = ['monthly_context', 'projected_context', 'lender_meeting_context']
    for col in context_cols:
        if col in df_merged.columns:
            df_merged[col] = df_merged[col].fillna('')
    
    print(f"  Merged data: {len(df_merged)} rows, {len(df_merged.columns)} columns")
    print(f"  Panel rows kept: {len(df_merged)}/{len(df_panel)} ({len(df_merged)/len(df_panel)*100:.1f}%)")
    print(f"  Covenant rows matched: {len(df_covenants)}/{len(df_covenants)} ({len(df_covenants)/len(df_covenants)*100:.1f}%)")
    
    # Summary statistics for merged data
    print(f"\nMerged Dataset Summary:")
    print(f"  Total observations: {len(df_merged)}")
    
    # Check nonbank vs bank distribution
    if 'nonbank_lender' in df_merged.columns:
        nonbank_count = df_merged['nonbank_lender'].sum()
        bank_count = len(df_merged) - nonbank_count
        print(f"  Nonbank loans: {nonbank_count} ({nonbank_count/len(df_merged)*100:.1f}%)")
        print(f"  Bank loans: {bank_count} ({bank_count/len(df_merged)*100:.1f}%)")
    
    # Covenant statistics by lender type
    if 'nonbank_lender' in df_merged.columns:
        print(f"\nCovenant Requirements by Lender Type:")
        
        # Nonbank loans
        nonbank_data = df_merged[df_merged['nonbank_lender'] == 1]
        if len(nonbank_data) > 0:
            print(f"  Nonbank loans ({len(nonbank_data)} observations):")
            print(f"    Monthly FS requirements: {nonbank_data['monthly_fs'].sum()} ({nonbank_data['monthly_fs'].mean()*100:.1f}%)")
            print(f"    Projected FS requirements: {nonbank_data['projected_fs'].sum()} ({nonbank_data['projected_fs'].mean()*100:.1f}%)")
            print(f"    Lender meeting requirements: {nonbank_data['lender_meeting'].sum()} ({nonbank_data['lender_meeting'].mean()*100:.1f}%)")
            print(f"    Any covenant: {nonbank_data['total_info_covenants'].gt(0).sum()} ({nonbank_data['total_info_covenants'].gt(0).mean()*100:.1f}%)")
            print(f"    No covenants: {(nonbank_data['total_info_covenants'] == 0).sum()} ({(nonbank_data['total_info_covenants'] == 0).mean()*100:.1f}%)")
        
        # Bank loans
        bank_data = df_merged[df_merged['nonbank_lender'] == 0]
        if len(bank_data) > 0:
            print(f"  Bank loans ({len(bank_data)} observations):")
            print(f"    Monthly FS requirements: {bank_data['monthly_fs'].sum()} ({bank_data['monthly_fs'].mean()*100:.1f}%)")
            print(f"    Projected FS requirements: {bank_data['projected_fs'].sum()} ({bank_data['projected_fs'].mean()*100:.1f}%)")
            print(f"    Lender meeting requirements: {bank_data['lender_meeting'].sum()} ({bank_data['lender_meeting'].mean()*100:.1f}%)")
            print(f"    Any covenant: {bank_data['total_info_covenants'].gt(0).sum()} ({bank_data['total_info_covenants'].gt(0).mean()*100:.1f}%)")
            print(f"    No covenants: {(bank_data['total_info_covenants'] == 0).sum()} ({(bank_data['total_info_covenants'] == 0).mean()*100:.1f}%)")
    
    # Overall covenant statistics
    print(f"\nOverall Covenant Statistics:")
    print(f"  Monthly FS requirements: {df_merged['monthly_fs'].sum()} ({df_merged['monthly_fs'].mean()*100:.1f}%)")
    print(f"  Projected FS requirements: {df_merged['projected_fs'].sum()} ({df_merged['projected_fs'].mean()*100:.1f}%)")
    print(f"  Lender meeting requirements: {df_merged['lender_meeting'].sum()} ({df_merged['lender_meeting'].mean()*100:.1f}%)")
    print(f"  Any information covenant: {df_merged['total_info_covenants'].gt(0).sum()} ({df_merged['total_info_covenants'].gt(0).mean()*100:.1f}%)")
    print(f"  No information covenants: {(df_merged['total_info_covenants'] == 0).sum()} ({(df_merged['total_info_covenants'] == 0).mean()*100:.1f}%)")
    
    # Covenant intensity distribution
    print(f"\nCovenant Intensity Distribution:")
    for i in range(4):
        count = (df_merged['total_info_covenants'] == i).sum()
        pct = count / len(df_merged) * 100
        print(f"  {i} covenants: {count} ({pct:.1f}%)")
    
    # Save merged data
    df_merged.to_csv(output_file, index=False)
    print(f"\nMerged data saved to: {output_file}")
    
    return df_merged


def main():
    print("=" * 80)
    print("6b_AggregateInfoCov.py - Merge Information Covenants with Panel Data")
    print("=" * 80)

    # Set up paths
    script_dir = Path(__file__).parent
    covenants_file = script_dir / ".." / "Data" / "Intermediate" / "6a_InformationCovenants_Aggregated.csv"
    panel_file = script_dir / ".." / "Data" / "Intermediate" / "5c_PanelAllLoans.csv"
    output_file = script_dir / ".." / "Data" / "Intermediate" / "6b_PanelWithInfoCovenants.csv"

    # Check if input files exist
    if not covenants_file.exists():
        print(f"Error: Covenants file not found: {covenants_file}")
        print("Please run 6a_AggregateInfoCov.py first to generate the aggregated covenants.")
        return
        
    if not panel_file.exists():
        print(f"Error: Panel file not found: {panel_file}")
        print("Please run 5c_PanelAllLoans.py first to generate the panel data.")
        return

    print(f"Covenants file: {covenants_file}")
    print(f"Panel file: {panel_file}")
    print(f"Output file: {output_file}")

    # Create output directory if it doesn't exist
    output_file.parent.mkdir(parents=True, exist_ok=True)

    # Merge the data
    merged_df = merge_covenants_with_panel(covenants_file, panel_file, output_file)

    if merged_df is not None:
        print("\nDone.")
    else:
        print("\nFailed to merge data.")


if __name__ == "__main__":
    main()
