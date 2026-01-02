#!/usr/bin/env python3
"""
6f_CleanNonbankPanel.py

Cleans the nonbank panel with information covenants by verifying firm financials
from agreements_comp_crsp_merged.csv using gvkey and year (fyear).
Uses inner join to ensure all observations have complete firm financial data.

Inputs:
- ../Data/Intermediate/6b_PanelNonbankWithInfoCovenants.csv (nonbank panel with covenants)
- ../Data/Cleaned/agreements_comp_crsp_merged.csv (firm financials)

Output:
- ../Data/Intermediate/6f_CleanNonbankPanel.csv (cleaned nonbank panel with firm financials)

Author: Zirui Song
Date: Oct 2025
"""

import pandas as pd
import numpy as np
from pathlib import Path


def load_data():
    """Load the nonbank panel and firm financials data"""
    script_dir = Path(__file__).parent
    nonbank_file = script_dir / ".." / "Data" / "Intermediate" / "6b_PanelNonbankWithInfoCovenants.csv"
    firm_financials_file = script_dir / ".." / "Data" / "Cleaned" / "agreements_comp_crsp_merged.csv"
    
    df_nonbank = pd.read_csv(nonbank_file)
    df_firm = pd.read_csv(firm_financials_file)
    
    print(f"Loaded {len(df_nonbank)} nonbank panel observations")
    print(f"Loaded {len(df_firm)} firm financial observations")
    
    return df_nonbank, df_firm


def prepare_matching_variables(df_nonbank, df_firm):
    """Prepare variables for matching on gvkey and year"""
    # Ensure we have the required columns
    if 'gvkey' not in df_nonbank.columns:
        print("Error: gvkey column not found in nonbank panel")
        return None, None
    
    if 'gvkey' not in df_firm.columns:
        print("Error: gvkey column not found in firm financials")
        return None, None
    
    # Use the appropriate year column from firm financials
    firm_year_col = 'fyear' if 'fyear' in df_firm.columns else 'year'
    if firm_year_col not in df_firm.columns:
        print(f"Error: {firm_year_col} column not found in firm financials")
        return None, None
    
    # Prepare nonbank panel
    df_nonbank_clean = df_nonbank.copy()
    
    # Handle year in nonbank panel - check multiple possible year columns
    year_col = None
    for col in ['year', 'fyear', 'year_x', 'year_y']:
        if col in df_nonbank.columns:
            year_col = col
            break
    
    if year_col is None:
        print("Error: No year column found in nonbank panel")
        return None, None
    
    df_nonbank_clean['year'] = df_nonbank_clean[year_col]
    
    # Prepare firm financials
    df_firm_clean = df_firm.copy()
    df_firm_clean['year'] = df_firm_clean[firm_year_col]
    
    # Convert year to numeric
    df_nonbank_clean['year'] = pd.to_numeric(df_nonbank_clean['year'], errors='coerce')
    df_firm_clean['year'] = pd.to_numeric(df_firm_clean['year'], errors='coerce')
    
    # Convert gvkey to string for consistent matching
    df_nonbank_clean['gvkey'] = df_nonbank_clean['gvkey'].astype(str)
    df_firm_clean['gvkey'] = df_firm_clean['gvkey'].astype(str)
    
    return df_nonbank_clean, df_firm_clean


def merge_with_firm_financials(df_nonbank, df_firm):
    """Merge nonbank panel with firm financials using inner join"""
    # Remove duplicates from firm financials based on gvkey-year
    # Keep the first occurrence if there are duplicates
    df_firm_unique = df_firm.drop_duplicates(subset=['gvkey', 'year'], keep='first')
    
    if len(df_firm_unique) < len(df_firm):
        print(f"Removed {len(df_firm) - len(df_firm_unique)} duplicate gvkey-year entries from firm financials")
    
    # Perform inner join to keep only observations with matching firm financials
    df_merged = df_nonbank.merge(
        df_firm_unique,
        on=['gvkey', 'year'],
        how='inner',
        suffixes=('', '_firm')
    )
    
    print(f"Inner join results:")
    print(f"  Before merge: {len(df_nonbank)} observations")
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
    
    if 'lender_type_detail' in df.columns:
        lender_detail_counts = df['lender_type_detail'].value_counts()
        print(f"  Lender details: ", end="")
        print(", ".join([f"{lender}={count}" for lender, count in lender_detail_counts.items()]))
    
    if 'total_info_covenants' in df.columns:
        cov_count = (df['total_info_covenants'] > 0).sum()
        print(f"  With covenants: {cov_count} ({cov_count/len(df)*100:.1f}%)")
    
    # Print year range
    if 'year' in df.columns:
        year_min = df['year'].min()
        year_max = df['year'].max()
        print(f"  Year range: {year_min:.0f} to {year_max:.0f}")
    
    return df


def main():
    print("=" * 80)
    print("6f_CleanNonbankPanel.py - Clean Nonbank Panel with Firm Financials")
    print("=" * 80)
    
    # Set up paths
    script_dir = Path(__file__).parent
    output_file = script_dir / ".." / "Data" / "Intermediate" / "6f_CleanNonbankPanel.csv"
    
    # Load data
    df_nonbank, df_firm = load_data()
    
    # Prepare matching variables
    df_nonbank_clean, df_firm_clean = prepare_matching_variables(df_nonbank, df_firm)
    
    if df_nonbank_clean is None or df_firm_clean is None:
        print("Error: Failed to prepare matching variables")
        return
    
    # Merge with firm financials
    df_merged = merge_with_firm_financials(df_nonbank_clean, df_firm_clean)
    
    # Clean and validate
    df_final = clean_and_validate_data(df_merged)
    
    # Save results
    df_final.to_csv(output_file, index=False)
    print(f"Saved to: {output_file}")
    print("=" * 80)


if __name__ == "__main__":
    main()

