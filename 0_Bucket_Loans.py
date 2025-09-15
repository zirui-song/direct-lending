#!/usr/bin/env python3
"""
0_Bucket_Loans.py

This script filters the agreements_mm_clean202507.csv file to:
1. Keep columns from 'accession' to 'form_type' (inclusive)
2. Keep 'private_credit_lender' and 'nonbank_lender' columns
3. Filter for instances where nonbank_lender == 1

Author: Generated for PSW Nonbank Direct Lending Research
Date: 2025
"""

import pandas as pd
import numpy as np
import os
from pathlib import Path

def main():
    """
    Main function to process the loan agreements data.
    """
    # Set up paths
    script_dir = Path(__file__).parent
    data_dir = script_dir / ".." / "Data" / "Cleaned"
    loans_full_dir = script_dir / ".." / "Data" / "LoansFull"
    output_dir = loans_full_dir / "bucketed_loans"
    input_file = data_dir / "agreements_mm_clean202507.csv"
    contracts_file = loans_full_dir / "cleaned_loancontracts.csv"
    output_file = output_dir / "bucketed_loans.csv"
    
    print("=" * 60)
    print("0_Bucket_Loans.py - Filtering Loan Agreements Data")
    print("=" * 60)
    
    # Check if input files exist
    if not input_file.exists():
        print(f"Error: Input file not found: {input_file}")
        print("Please ensure the file exists in the correct location.")
        return
    
    if not contracts_file.exists():
        print(f"Error: Contracts file not found: {contracts_file}")
        print("Please ensure the file exists in the correct location.")
        return
    
    # Create output directory if it doesn't exist
    output_dir.mkdir(parents=True, exist_ok=True)
    print(f"Output directory: {output_dir}")
    
    print(f"Reading data from: {input_file}")
    
    try:
        # Read the CSV file
        df = pd.read_csv(input_file)
        print(f"Original dataset shape: {df.shape}")
        
        # Define columns to keep
        # Columns from 'accession' to 'form_type' (inclusive)
        accession_to_form_type = [
            'accession', 'filename', 'type_filing', 'type_attachment', 'date', 'form_type'
        ]
        
        # Additional columns to keep
        additional_columns = ['private_credit_lender', 'nonbank_lender']
        
        # Combine all columns to keep
        columns_to_keep = accession_to_form_type + additional_columns
        
        # Check which columns exist in the dataset
        existing_columns = [col for col in columns_to_keep if col in df.columns]
        missing_columns = [col for col in columns_to_keep if col not in df.columns]
        
        if missing_columns:
            print(f"Warning: The following columns are missing from the dataset:")
            for col in missing_columns:
                print(f"  - {col}")
            print()
        
        print(f"Keeping {len(existing_columns)} columns:")
        for i, col in enumerate(existing_columns, 1):
            print(f"  {i:2d}. {col}")
        print()
        
        # Filter the dataset to keep only the specified columns
        df_filtered = df[existing_columns].copy()
        print(f"After column filtering: {df_filtered.shape}")
        
        # Check the distribution of nonbank_lender before filtering
        if 'nonbank_lender' in df_filtered.columns:
            nonbank_counts = df_filtered['nonbank_lender'].value_counts()
            print(f"\nNonbank lender distribution before filtering:")
            for value, count in nonbank_counts.items():
                print(f"  nonbank_lender = {value}: {count:,} records")
            
            # Filter for nonbank_lender == 1
            df_final = df_filtered[df_filtered['nonbank_lender'] == 1].copy()
            print(f"\nAfter filtering for nonbank_lender == 1: {df_final.shape}")
            
            if len(df_final) == 0:
                print("Warning: No records found with nonbank_lender == 1")
                return
            
            # Drop duplicates
            print(f"\nDropping duplicates...")
            df_final = df_final.drop_duplicates()
            print(f"After dropping duplicates: {df_final.shape}")
            
        else:
            print("Warning: 'nonbank_lender' column not found. No filtering applied.")
            df_final = df_filtered

        
        # Load contracts data for merging
        print(f"\nLoading contracts data from: {contracts_file}")
        print("Reading only accession, text, and type_filing columns...")
        
        # Read only the required columns from contracts file
        contracts_df = pd.read_csv(contracts_file, usecols=['accession', 'text', 'type_filing'])
        print(f"Contracts data shape: {contracts_df.shape}")
        
        # Merge with contracts data
        print("\nMerging with contracts data...")
        df_merged = df_final.merge(
            contracts_df, 
            on=['accession', 'type_filing'], 
            how='left'
        )
        
        # Check merge results
        merged_count = df_merged['text'].notna().sum()
        print(f"Successfully merged text for {merged_count:,} out of {len(df_merged):,} records")
        print(f"Final dataset shape after merge: {df_merged.shape}")
        
        # Save the merged dataset
        print(f"\nSaving merged data to: {output_file}")
        df_merged.to_csv(output_file, index=False)
        
        # Create a version without text column
        output_file_no_text = output_dir / "bucketed_loans_no_text.csv"
        df_no_text = df_merged.drop(columns=['text'])
        print(f"Saving data without text to: {output_file_no_text}")
        df_no_text.to_csv(output_file_no_text, index=False)
        
        # Summary statistics
        print(f"\n" + "=" * 60)
        print("SUMMARY")
        print("=" * 60)
        print(f"Original dataset: {df.shape[0]:,} rows × {df.shape[1]} columns")
        print(f"After column filtering: {df_filtered.shape[0]:,} rows × {df_filtered.shape[1]} columns")
        if 'nonbank_lender' in df_filtered.columns:
            nonbank_filtered = df_filtered[df_filtered['nonbank_lender'] == 1]
            print(f"After nonbank_lender filtering: {nonbank_filtered.shape[0]:,} rows × {nonbank_filtered.shape[1]} columns")
            print(f"After dropping duplicates: {df_final.shape[0]:,} rows × {df_final.shape[1]} columns")
            duplicates_removed = nonbank_filtered.shape[0] - df_final.shape[0]
            if duplicates_removed > 0:
                print(f"Duplicates removed: {duplicates_removed:,} rows")
        else:
            print(f"Final dataset: {df_final.shape[0]:,} rows × {df_final.shape[1]} columns")
        print(f"After merging with contracts: {df_merged.shape[0]:,} rows × {df_merged.shape[1]} columns")
        print(f"Text successfully merged: {merged_count:,} records ({merged_count/len(df_merged)*100:.1f}%)")
        print(f"Total reduction: {((df.shape[0] - df_merged.shape[0]) / df.shape[0] * 100):.1f}% of rows removed")
        print(f"Output files:")
        print(f"  - With text: {output_file}")
        print(f"  - Without text: {output_file_no_text}")
        
        # Additional analysis
        if 'private_credit_lender' in df_merged.columns and 'nonbank_lender' in df_merged.columns:
            pc_nonbank = df_merged[df_merged['private_credit_lender'] == True]
            print(f"Private credit lenders in final dataset: {len(pc_nonbank):,} ({len(pc_nonbank)/len(df_merged)*100:.1f}%)")
        
        print("\nProcessing completed successfully!")
        
    except Exception as e:
        print(f"Error processing the data: {str(e)}")
        print("Please check the input file format and try again.")
        return

if __name__ == "__main__":
    main()
