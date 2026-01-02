#!/usr/bin/env python3
"""
6b_MergeInfoCov.py

Merge aggregated information covenants with panel data from multiple panel files.

Inner joins the information covenants data with the panel data on accession
to create a comprehensive dataset with both loan characteristics and covenant requirements.

Inputs:
- ../Data/Intermediate/6a_InformationCovenants_Aggregated.csv (covenant data)
- ../Data/Intermediate/5a_PanelNonbankLoans.csv (panel data)
- ../Data/Intermediate/5c_PanelAllLoans.csv (panel data)
- ../Data/Intermediate/5d_MatchedPanelLoans.csv (matched panel data)
- ../Data/Intermediate/5d_MatchedPanelLoans_Direct.csv (matched direct panel data)

Outputs:
- ../Data/Intermediate/6b_PanelNonbankWithInfoCovenants.csv
- ../Data/Intermediate/6b_PanelWithInfoCovenants.csv
- ../Data/Intermediate/6b_MatchedPanelWithInfoCovenants.csv
- ../Data/Intermediate/6b_MatchedPanelDirectWithInfoCovenants.csv

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
    df_covenants = pd.read_csv(covenants_file)
    df_panel = pd.read_csv(panel_file)
    
    # Check for accession column in both datasets
    if 'accession' not in df_covenants.columns:
        print("Error: 'accession' column not found in covenants data")
        return None
        
    # Handle different panel file structures
    if 'accession' in df_panel.columns:
        # Standard panel file with single accession column
        df_merged = df_panel.merge(df_covenants, on='accession', how='left')
        
    elif 'accession_nonbank' in df_panel.columns and 'accession_bank' in df_panel.columns:
        # Matched panel file with separate accession columns - convert to long format
        # Create nonbank loans dataframe
        nonbank_cols = [col for col in df_panel.columns if col.endswith('_nonbank') or col in ['ff12', 'term_loan', 'ps_distance']]
        nonbank_df = df_panel[nonbank_cols].copy()
        
        # Rename columns to remove _nonbank suffix
        rename_dict = {col: col.replace('_nonbank', '') for col in nonbank_df.columns if col.endswith('_nonbank')}
        nonbank_df = nonbank_df.rename(columns=rename_dict)
        nonbank_df['lender_type'] = 'nonbank'
        nonbank_df['accession'] = df_panel['accession_nonbank']
        
        # Create bank loans dataframe  
        bank_cols = [col for col in df_panel.columns if col.endswith('_bank') or col in ['ff12', 'term_loan', 'ps_distance']]
        bank_df = df_panel[bank_cols].copy()
        
        # Rename columns to remove _bank suffix
        rename_dict = {col: col.replace('_bank', '') for col in bank_df.columns if col.endswith('_bank')}
        bank_df = bank_df.rename(columns=rename_dict)
        bank_df['lender_type'] = 'bank'
        bank_df['accession'] = df_panel['accession_bank']
        
        # Combine into long format and merge
        df_panel_long = pd.concat([nonbank_df, bank_df], ignore_index=True)
        df_merged = df_panel_long.merge(df_covenants, on='accession', how='left')
        
    elif 'accession_direct' in df_panel.columns and 'accession_nondirect' in df_panel.columns:
        # Matched panel file with direct/nondirect accession columns - convert to long format
        # Create direct loans dataframe
        direct_cols = [col for col in df_panel.columns if col.endswith('_direct') or col in ['ff12', 'term_loan', 'ps_distance']]
        direct_df = df_panel[direct_cols].copy()
        
        # Rename columns to remove _direct suffix
        rename_dict = {col: col.replace('_direct', '') for col in direct_df.columns if col.endswith('_direct')}
        direct_df = direct_df.rename(columns=rename_dict)
        direct_df['loan_type'] = 'direct'
        direct_df['accession'] = df_panel['accession_direct']
        
        # Create nondirect loans dataframe  
        nondirect_cols = [col for col in df_panel.columns if col.endswith('_nondirect') or col in ['ff12', 'term_loan', 'ps_distance']]
        nondirect_df = df_panel[nondirect_cols].copy()
        
        # Rename columns to remove _nondirect suffix
        rename_dict = {col: col.replace('_nondirect', '') for col in nondirect_df.columns if col.endswith('_nondirect')}
        nondirect_df = nondirect_df.rename(columns=rename_dict)
        nondirect_df['loan_type'] = 'nondirect'
        nondirect_df['accession'] = df_panel['accession_nondirect']
        
        # Combine into long format and merge
        df_panel_long = pd.concat([direct_df, nondirect_df], ignore_index=True)
        df_merged = df_panel_long.merge(df_covenants, on='accession', how='left')
        
    else:
        print("Error: No accession column found in panel data")
        return None
    
    # Fill missing covenant values with 0 (no covenants found)
    covenant_cols = ['monthly_fs', 'projected_fs', 'lender_meeting', 'total_info_covenants']
    context_cols = ['monthly_context', 'projected_context', 'lender_meeting_context']
    
    for col in covenant_cols:
        if col in df_merged.columns:
            df_merged[col] = df_merged[col].fillna(0).astype(int)
    
    for col in context_cols:
        if col in df_merged.columns:
            df_merged[col] = df_merged[col].fillna('')
    
    # Print summary statistics
    print(f"Merged: {len(df_merged)} observations")
    print(f"  Any covenant: {df_merged['total_info_covenants'].gt(0).sum()} ({df_merged['total_info_covenants'].gt(0).mean()*100:.1f}%)")
    print(f"  No covenants: {(df_merged['total_info_covenants'] == 0).sum()} ({(df_merged['total_info_covenants'] == 0).mean()*100:.1f}%)")
    
    # Save merged data
    df_merged.to_csv(output_file, index=False)
    
    return df_merged


def main():
    print("=" * 80)
    print("6b_MergeInfoCov.py - Merge Information Covenants with Panel Data")
    print("=" * 80)

    # Set up paths
    script_dir = Path(__file__).parent
    covenants_file = script_dir / ".." / "Data" / "Intermediate" / "6a_InformationCovenants_Aggregated.csv"
    
    # Define panel files to merge with
    panel_files = [
        ("5a_PanelNonbankLoans.csv", "6b_PanelNonbankWithInfoCovenants.csv"),
        ("5c_PanelAllLoans.csv", "6b_PanelWithInfoCovenants.csv"),
        ("5d_MatchedPanelLoans.csv", "6b_MatchedPanelWithInfoCovenants.csv"),
        ("5d_MatchedPanelLoans_Direct.csv", "6b_MatchedPanelDirectWithInfoCovenants.csv")
    ]

    # Check if covenants file exists
    if not covenants_file.exists():
        print(f"Error: Covenants file not found: {covenants_file}")
        return
        
    # Process each panel file
    for panel_filename, output_filename in panel_files:
        panel_file = script_dir / ".." / "Data" / "Intermediate" / panel_filename
        output_file = script_dir / ".." / "Data" / "Intermediate" / output_filename
        
        print(f"\nProcessing: {panel_filename}")
        
        # Check if panel file exists
        if not panel_file.exists():
            print(f"  Warning: {panel_filename} not found, skipping")
            continue

        # Create output directory if it doesn't exist
        output_file.parent.mkdir(parents=True, exist_ok=True)

        # Merge the data
        merged_df = merge_covenants_with_panel(covenants_file, panel_file, output_file)

        if merged_df is not None:
            print(f"  Saved to: {output_file}")
        else:
            print(f"  Failed to merge {panel_filename}")

    print(f"\nAll merging operations completed.")


if __name__ == "__main__":
    main()
