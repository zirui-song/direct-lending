#!/usr/bin/env python3
"""
6a_AggregateInfoCov.py

Aggregate information covenants to accession level from detailed extraction.

Converts the detailed covenant extraction (multiple rows per accession) 
into a single row per accession with binary indicators.

Inputs:
- ../Data/Intermediate/6_InformationCovenants.csv (detailed extraction)

Outputs:
- ../Data/Intermediate/6a_InformationCovenants_Aggregated.csv (accession-level)

Author: Zirui Song
Date: Oct 2025
"""

import pandas as pd
from pathlib import Path


def aggregate_info_covenants(input_file, output_file):
    """
    Aggregate information covenants to accession level.
    
    Args:
        input_file: Path to detailed covenant extraction CSV
        output_file: Path to save aggregated CSV
    """
    print("Loading detailed covenant extraction...")
    df = pd.read_csv(input_file)
    
    print(f"Original data: {len(df)} rows, {df['accession'].nunique()} unique accessions")
    
    # Aggregate to accession level
    # Create binary indicators: 1 if any covenant of that type found, 0 otherwise
    aggregated = df.groupby('accession').agg({
        'monthly_fs': 'max',  # 1 if any monthly FS covenant found
        'projected_fs': 'max',  # 1 if any projected FS covenant found  
        'lender_meeting': 'max',  # 1 if any lender meeting covenant found
        'monthly_context': lambda x: '; '.join(x.dropna().unique()) if x.notna().any() else '',
        'projected_context': lambda x: '; '.join(x.dropna().unique()) if x.notna().any() else '',
        'lender_meeting_context': lambda x: '; '.join(x.dropna().unique()) if x.notna().any() else ''
    }).reset_index()
    
    # Clean up context fields (remove empty strings and duplicates)
    for col in ['monthly_context', 'projected_context', 'lender_meeting_context']:
        aggregated[col] = aggregated[col].str.strip()
        aggregated.loc[aggregated[col] == '', col] = None
    
    # Add summary statistics
    total_covenants = (aggregated['monthly_fs'] + 
                      aggregated['projected_fs'] + 
                      aggregated['lender_meeting'])
    
    aggregated['total_info_covenants'] = total_covenants
    
    # Reorder columns
    column_order = [
        'accession', 'monthly_fs', 'projected_fs', 'lender_meeting', 
        'total_info_covenants', 'monthly_context', 'projected_context', 'lender_meeting_context'
    ]
    aggregated = aggregated[column_order]
    
    # Save aggregated data
    aggregated.to_csv(output_file, index=False)
    
    print(f"\nAggregated data saved to: {output_file}")
    print(f"Aggregated data: {len(aggregated)} rows (one per accession)")
    
    # Summary statistics
    monthly_count = aggregated['monthly_fs'].sum()
    projected_count = aggregated['projected_fs'].sum()
    meeting_count = aggregated['lender_meeting'].sum()
    any_covenant = (aggregated['total_info_covenants'] > 0).sum()
    
    print(f"\nSummary (accession level):")
    print(f"  Total accessions: {len(aggregated)}")
    print(f"  Accessions with any information covenant: {any_covenant} ({any_covenant/len(aggregated)*100:.1f}%)")
    print(f"  Accessions with monthly FS requirements: {monthly_count} ({monthly_count/len(aggregated)*100:.1f}%)")
    print(f"  Accessions with projected FS requirements: {projected_count} ({projected_count/len(aggregated)*100:.1f}%)")
    print(f"  Accessions with lender meeting requirements: {meeting_count} ({meeting_count/len(aggregated)*100:.1f}%)")
    
    # Covenant combinations
    print(f"\nCovenant combinations:")
    both_fs = ((aggregated['monthly_fs'] == 1) & (aggregated['projected_fs'] == 1)).sum()
    all_three = (aggregated['total_info_covenants'] == 3).sum()
    print(f"  Both monthly and projected FS: {both_fs}")
    print(f"  All three covenant types: {all_three}")
    
    return aggregated


def main():
    print("=" * 80)
    print("6a_AggregateInfoCov.py - Aggregate Information Covenants to Accession Level")
    print("=" * 80)

    # Set up paths
    script_dir = Path(__file__).parent
    input_file = script_dir / ".." / "Data" / "Intermediate" / "6_InformationCovenants.csv"
    output_file = script_dir / ".." / "Data" / "Intermediate" / "6a_InformationCovenants_Aggregated.csv"

    # Check if input file exists
    if not input_file.exists():
        print(f"Error: Input file not found: {input_file}")
        print("Please run 6_ExtractInfoCov.py first to generate the detailed extraction.")
        return

    print(f"Input file: {input_file}")
    print(f"Output file: {output_file}")

    # Aggregate the data
    aggregated_df = aggregate_info_covenants(input_file, output_file)

    print("\nDone.")


if __name__ == "__main__":
    main()
