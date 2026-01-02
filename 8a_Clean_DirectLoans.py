#!/usr/bin/env python3
"""
8a_Clean_DirectLoans.py

Reads the ChatGPT-processed direct lending panel and creates a lender-count
measure at the accession level.

Input:
- ../Data/Cleaned/loan_officer_final_panel_chatgpt_nolinkedin_cleaned.csv

Output:
- ../Data/Cleaned/loan_officer_final_panel_chatgpt_nolinkedin_with_num_lenders.csv
  (same data plus num_lenders)
"""

import pandas as pd
from pathlib import Path


def main():
    # Set up paths relative to this script
    script_dir = Path(__file__).parent
    input_file = script_dir / ".." / "Data" / "Cleaned" / "loan_officer_final_panel_chatgpt_nolinkedin_cleaned.csv"
    output_file = script_dir / ".." / "Data" / "Cleaned" / "loan_officer_final_panel_chatgpt_nolinkedin_with_num_lenders.csv"
    direct_only_file = script_dir / ".." / "Data" / "Cleaned" / "loan_officer_final_panel_chatgpt_nolinkedin_direct_only.csv"

    print("=" * 80)
    print("8a_Clean_DirectLoans.py - Compute number of distinct lenders per accession")
    print("=" * 80)
    print(f"Reading: {input_file}")

    # Read input
    df = pd.read_csv(input_file)
    print(f"Loaded {len(df)} rows")

    # Basic checks
    required_cols = ["accession", "standardized_bank_name"]
    missing = [c for c in required_cols if c not in df.columns]
    if missing:
        raise ValueError(f"Missing required columns in input file: {missing}")

    # Compute number of distinct standardized_bank_name per accession
    lender_counts = (
        df.groupby("accession")["standardized_bank_name"]
        .nunique(dropna=True)
        .rename("num_lenders")
    )

    # Merge back to full dataframe
    df = df.merge(lender_counts, on="accession", how="left")

    # Generate direct dummy: 1 if exactly one lender, 0 otherwise
    df["direct"] = (df["num_lenders"] == 1).astype(int)

    # Simple summary
    print("num_lenders summary:")
    print(df["num_lenders"].describe())
    print("direct dummy value counts:")
    print(df["direct"].value_counts(dropna=False))

    # Save full output
    df.to_csv(output_file, index=False)
    print(f"Saved with num_lenders and direct dummy to: {output_file}")

    # Save direct-only sample (direct == 1)
    df_direct = df[df["direct"] == 1].copy()
    df_direct.to_csv(direct_only_file, index=False)
    print(f"Saved direct-only sample (direct == 1) to: {direct_only_file}")
    print("=" * 80)


if __name__ == "__main__":
    main()


