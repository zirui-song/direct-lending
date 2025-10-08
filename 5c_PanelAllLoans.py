#!/usr/bin/env python3
"""
5c_PanelAllLoans.py

Build a comprehensive panel of all agreements by:
1) Inner joining firm financials (agreements_comp_crsp_merged.csv) with loan terms
   (loan_terms_cleaned_all_20251007.csv) using accession ↔ filing_id
2) Left joining filtered nonbank results (filtered_batch_results.csv) on accession
   to create an indicator column: nonbank_lender = 1 if matched, else 0

Inputs:
- ../Data/Cleaned/agreements_comp_crsp_merged.csv
- ../Data/Intermediate/loan_terms_cleaned_all_20251007.csv
- ../Data/Raw/ExtractedAgreements_Bucketed/filtered_batch_results.csv

Output:
- ../Data/Intermediate/5c_PanelAllLoans.csv

Author: Zirui Song
Date: Oct 2025
"""

import pandas as pd
import numpy as np
from pathlib import Path
from scipy.stats import mstats


def winsorize_continuous_variables(df: pd.DataFrame, limits: tuple = (0.01, 0.01)) -> pd.DataFrame:
    """
    Winsorize continuous variables used in matching at specified percentiles.
    
    Args:
        df: DataFrame to winsorize
        limits: Tuple of (lower_percentile, upper_percentile) to winsorize at
    
    Returns:
        DataFrame with winsorized continuous variables
    """
    print(f"\nWinsorizing continuous variables at {limits[0]*100}% and {limits[1]*100}%...")
    
    # Define continuous variables to winsorize
    continuous_vars = [
        'clean_interest_spread', 'interest_spread', 'maturity_months', 'facility_amount',
        'at', 'dltt', 'dlc', 'lt', 'ebitda', 'market_to_book'
    ]
    
    df_winsorized = df.copy()
    winsorized_count = 0
    
    for var in continuous_vars:
        if var in df_winsorized.columns:
            # Convert to numeric, handling any non-numeric values
            numeric_series = pd.to_numeric(df_winsorized[var], errors='coerce')
            
            # Only winsorize if we have non-null values
            if numeric_series.notna().sum() > 0:
                # Store original values for comparison
                original_values = numeric_series.copy()
                
                # Winsorize
                winsorized_values = mstats.winsorize(
                    original_values.dropna(), 
                    limits=limits
                )
                
                # Create new series with winsorized values
                new_series = numeric_series.copy()
                valid_mask = original_values.notna()
                new_series.loc[valid_mask] = winsorized_values
                
                # Count how many values changed
                changed = (original_values != new_series).sum()
                
                df_winsorized[var] = new_series
                print(f"  {var}: {changed} values winsorized")
                winsorized_count += changed
            else:
                print(f"  {var}: No valid numeric values to winsorize")
        else:
            print(f"  {var}: Column not found in dataset")
    
    print(f"Total values winsorized: {winsorized_count}")
    return df_winsorized


def main():
    print("=" * 80)
    print("5c_PanelAllLoans.py - Build All-Agreements Panel with Nonbank Indicator")
    print("=" * 80)

    # Paths
    script_dir = Path(__file__).parent
    firm_financials_csv = script_dir / ".." / "Data" / "Cleaned" / "agreements_comp_crsp_merged.csv"
    loan_terms_csv = script_dir / ".." / "Data" / "Intermediate" / "loan_terms_cleaned_all_20251007.csv"
    filtered_csv = script_dir / ".." / "Data" / "Raw" / "ExtractedAgreements_Bucketed" / "filtered_batch_results.csv"
    output_csv = script_dir / ".." / "Data" / "Intermediate" / "5c_PanelAllLoans.csv"

    # Check inputs
    for p in [firm_financials_csv, loan_terms_csv, filtered_csv]:
        if not p.exists():
            print(f"Error: Missing input file: {p}")
            return

    # Load data
    print("Loading firm financials data...")
    df_fin = pd.read_csv(firm_financials_csv)
    print(f"  firm financials rows: {len(df_fin)}; columns: {len(df_fin.columns)}")

    print("Loading loan terms data...")
    df_terms = pd.read_csv(loan_terms_csv)
    print(f"  loan terms rows: {len(df_terms)}; columns: {len(df_terms.columns)}")

    print("Loading filtered nonbank results...")
    df_filtered = pd.read_csv(filtered_csv)
    print(f"  filtered results rows: {len(df_filtered)}; columns: {len(df_filtered.columns)}")

    # Validate join keys
    if "accession" not in df_terms.columns:
        print("Error: 'accession' not found in loan terms")
        return
    if "filing_id" not in df_fin.columns:
        print("Error: 'filing_id' not found in firm financials")
        return

    # Step 1: Inner join firm financials with loan terms
    print("\nJoining firm financials with loan terms (inner on accession ↔ filing_id)...")
    df_all = df_terms.merge(df_fin, left_on="accession", right_on="filing_id", how="inner")
    print(f"  merged rows: {len(df_all)}")
    
    # Add unique loan_id for each row
    df_all["loan_id"] = range(1, len(df_all) + 1)
    print(f"  added loan_id column (1 to {len(df_all)})")

    # Step 2: Left join to filtered results by accession, create indicator
    print("Left joining filtered results to add nonbank indicator...")
    keep_cols = ["accession", "lender_type"]
    present_cols = [c for c in keep_cols if c in df_filtered.columns]
    df_filtered_small = df_filtered[present_cols].drop_duplicates()

    df_panel = df_all.merge(df_filtered_small, on="accession", how="left")
    df_panel["nonbank_lender"] = np.where(df_panel["lender_type"].notna(), 1, 0)

    # Step 3: Winsorize continuous variables used in matching
    df_panel = winsorize_continuous_variables(df_panel, limits=(0.01, 0.01))

    # Optional: sort for readability
    sort_cols = [c for c in ["nonbank_lender", "lender_type", "accession"] if c in df_panel.columns]
    if sort_cols:
        df_panel = df_panel.sort_values(sort_cols).reset_index(drop=True)

    # Save
    print(f"\nSaving panel to: {output_csv}")
    df_panel.to_csv(output_csv, index=False)
    print("Saved.")

    # Report
    total = len(df_panel)
    nonbank = int(df_panel["nonbank_lender"].sum())
    bank = total - nonbank
    print("\nPanel Summary:")
    print(f"  total rows: {total}")
    print(f"  nonbank_lender=1: {nonbank}")
    print(f"  nonbank_lender=0: {bank}")

    print("\nColumns (first 30 shown):")
    for i, col in enumerate(df_panel.columns[:30], 1):
        print(f"  {i:2d}. {col}")

    print("\nDone.")


if __name__ == "__main__":
    main()


