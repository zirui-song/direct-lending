#!/usr/bin/env python3
"""
6e_CleanMatchedPanel.py

Cleans the matched panel with information covenants by verifying firm financials
from agreements_comp_crsp_merged.csv using gvkey and year (fyear).
Uses inner join to ensure all observations have complete firm financial data.

Inputs:
- ../Data/Intermediate/6b_MatchedPanelWithInfoCovenants.csv (matched panel with covenants)
- ../Data/Cleaned/agreements_comp_crsp_merged.csv (firm financials)

Output:
- ../Data/Intermediate/6e_CleanMatchedPanel.csv (cleaned matched panel with firm financials)

Author: Zirui Song
Date: Oct 2025
"""

import pandas as pd
import numpy as np
from pathlib import Path


def load_data():
    """Load the matched panel and firm financials data"""
    script_dir = Path(__file__).parent
    matched_file = script_dir / ".." / "Data" / "Intermediate" / "6b_MatchedPanelWithInfoCovenants.csv"
    firm_financials_file = script_dir / ".." / "Data" / "Cleaned" / "agreements_comp_crsp_merged.csv"
    
    df_matched = pd.read_csv(matched_file)
    df_firm = pd.read_csv(firm_financials_file)
    
    print(f"Loaded {len(df_matched)} matched panel observations")
    print(f"Loaded {len(df_firm)} firm financial observations")
    
    return df_matched, df_firm


def prepare_matching_variables(df_matched, df_firm):
    """Prepare variables for matching on gvkey and year"""
    # Ensure we have the required columns
    if 'gvkey' not in df_matched.columns:
        print("Error: gvkey column not found in matched panel")
        return None, None
    
    if 'gvkey' not in df_firm.columns:
        print("Error: gvkey column not found in firm financials")
        return None, None
    
    # Use the appropriate year column from firm financials
    firm_year_col = 'fyear' if 'fyear' in df_firm.columns else 'year'
    if firm_year_col not in df_firm.columns:
        print(f"Error: {firm_year_col} column not found in firm financials")
        return None, None
    
    # Prepare matched panel
    df_matched_clean = df_matched.copy()
    
    # Handle year in matched panel - use existing year column
    if 'year' not in df_matched.columns:
        print("Error: No year column found in matched panel")
        return None, None
    
    # Prepare firm financials
    df_firm_clean = df_firm.copy()
    df_firm_clean['year'] = df_firm_clean[firm_year_col]
    
    # Convert year to numeric
    df_matched_clean['year'] = pd.to_numeric(df_matched_clean['year'], errors='coerce')
    df_firm_clean['year'] = pd.to_numeric(df_firm_clean['year'], errors='coerce')
    
    # Convert gvkey to string for consistent matching
    df_matched_clean['gvkey'] = df_matched_clean['gvkey'].astype(str)
    df_firm_clean['gvkey'] = df_firm_clean['gvkey'].astype(str)
    
    return df_matched_clean, df_firm_clean


def merge_with_firm_financials(df_matched, df_firm):
    """Merge matched panel with firm financials using inner join"""
    # Remove duplicates from firm financials based on gvkey-year
    # Keep the first occurrence if there are duplicates
    df_firm_unique = df_firm.drop_duplicates(subset=['gvkey', 'year'], keep='first')
    
    if len(df_firm_unique) < len(df_firm):
        print(f"Removed {len(df_firm) - len(df_firm_unique)} duplicate gvkey-year entries from firm financials")
    
    # Perform inner join to keep only observations with matching firm financials
    df_merged = df_matched.merge(
        df_firm_unique,
        on=['gvkey', 'year'],
        how='inner',
        suffixes=('', '_firm')
    )
    
    print(f"Inner join results:")
    print(f"  Before merge: {len(df_matched)} observations")
    print(f"  After merge: {len(df_merged)} observations")
    
    return df_merged


def clean_and_validate_data(df):
    """Clean and validate the merged dataset"""
    # Summary statistics
    print(f"Final dataset: {len(df)} observations")
    
    if 'lender_type' in df.columns:
        lender_counts = df['lender_type'].value_counts()
        print(f"  Lender types: ", end="")
        print(", ".join([f"{lender}={count}" for lender, count in lender_counts.items()]))
    
    if 'total_info_covenants' in df.columns:
        cov_count = (df['total_info_covenants'] > 0).sum()
        print(f"  With covenants: {cov_count} ({cov_count/len(df)*100:.1f}%)")
    
    return df


def main():
    print("=" * 80)
    print("6e_CleanMatchedPanel.py - Clean Matched Panel with Firm Financials")
    print("=" * 80)
    
    # Set up paths
    script_dir = Path(__file__).parent
    output_file = script_dir / ".." / "Data" / "Intermediate" / "6e_CleanMatchedPanel.csv"
    
    # Load data
    df_matched, df_firm = load_data()
    
    # Prepare matching variables
    df_matched_clean, df_firm_clean = prepare_matching_variables(df_matched, df_firm)
    
    if df_matched_clean is None or df_firm_clean is None:
        print("Error: Failed to prepare matching variables")
        return
    
    # Merge with firm financials
    df_merged = merge_with_firm_financials(df_matched_clean, df_firm_clean)
    
    # Clean and validate
    df_final = clean_and_validate_data(df_merged)
    
    # Save results
    df_final.to_csv(output_file, index=False)
    print(f"Saved to: {output_file}")
    print("=" * 80)


if __name__ == "__main__":
    main()
