#!/usr/bin/env python3
"""
6_ExtractInfoCov.py

Extract information covenants from .nc files in ExtractedAgreements_FinalMapped folder.

This script extracts three types of information covenants:
1. Monthly financial statement requirements
2. Projected financial statement requirements  
3. Lender meeting requirements

Inputs:
- ../Data/Raw/ExtractedAgreements_FinalMapped/*.nc files

Outputs:
- ../Data/Intermediate/6_InformationCovenants.csv

Author: Zirui Song
Date: Oct 2025
"""

import os
import pandas as pd
import re
from pathlib import Path
import glob


def search_after_phrase(content, phrase, offset, statement_keyword, fiscal_month_keyword, 
                       fiscal_year_keyword, projected_keyword, borrower_keyword):
    """Search for monthly and projected financial statement requirements after a phrase."""
    content = re.sub(r'\s+', ' ', content).strip()

    occurrences = [i for i in range(len(content)) if content.startswith(phrase, i)]
    monthly_fs_sentences = []
    projected_fs_sentences = []

    for start_index in occurrences:
        start_index += len(phrase)
        end_index = start_index + offset
        snippet = content[start_index:end_index]

        sentences = re.split(r'(?<=[.;]) +', snippet)

        for sentence in sentences:
            if (any(keyword in sentence for keyword in borrower_keyword) and 
                any(keyword in sentence for keyword in fiscal_month_keyword) and 
                any(keyword in sentence for keyword in statement_keyword)):
                monthly_fs_sentences.append(sentence)

            if (any(keyword in sentence for keyword in projected_keyword) and 
                any(keyword in sentence for keyword in fiscal_year_keyword) and 
                any(keyword in sentence for keyword in borrower_keyword)):
                projected_fs_sentences.append(sentence)

    return monthly_fs_sentences, projected_fs_sentences


def search_after_phrase_meeting(content, phrase, offset, meeting_keyword):
    """Search for lender meeting requirements after a phrase."""
    # Normalize whitespace in the content
    content = re.sub(r'\s+', ' ', content).strip()
    
    # Find all occurrences of the phrase in the content
    occurrences = [i for i in range(len(content)) if content.startswith(phrase, i)]
    
    # Initialize lists to store the extracted sentences
    lender_meeting_sentences = []

    # Loop through each occurrence of the phrase
    for start_index in occurrences:
        # Move the start index to just after the phrase
        start_index += len(phrase)
        
        # Define the range to extract from (start_index to start_index + offset)
        end_index = start_index + offset
        snippet = content[start_index:end_index]

        # Split the snippet into sentences
        sentences = re.split(r'(?<=[.;]) +', snippet)
        
        # Search each sentence for keywords and extract only sentence
        for sentence in sentences:
            if (any(keyword in sentence for keyword in meeting_keyword)):
                lender_meeting_sentences.append(sentence)

    return lender_meeting_sentences


def extract_accession_from_filename(filename):
    """Extract accession number from .nc filename."""
    # Remove .nc extension and any path
    basename = os.path.basename(filename).replace('.nc', '')
    return basename


def process_nc_files(input_dir, output_file):
    """Process all .nc files and extract information covenants."""
    
    # Define keywords for different types of information covenants
    statement_keyword = [
        "balance sheet", "income statement", "cash flow", "certificate", 
        "statement of operations", "financial statement", "financial report"
    ]

    fiscal_month_keyword = [
        "fiscal month", "monthly", "calendar month"
    ]

    fiscal_year_keyword = [
        "fiscal year", "annually", "annual", "yearly", "calendar year"
    ]

    projected_keyword = [
        "forecast", "operating plan", "business plan", "projection", 
        "budget", "projected", "budgeted"
    ]

    borrower_keyword = [
        "borrower", "borrowing", "agent", "lender", "parent", 
        "holdings", "guarantor", "collateral property"
    ]

    meeting_keyword = [
        "lender meeting", "lender call", "conference call", "conference meeting"
    ]

    # Get all .nc files
    nc_files = glob.glob(os.path.join(input_dir, "*.nc"))
    
    if not nc_files:
        print(f"No .nc files found in {input_dir}")
        return

    print(f"Found {len(nc_files)} .nc files to process")

    # Initialize results list
    results = []

    # Process each .nc file
    for i, nc_file in enumerate(nc_files):
        if (i + 1) % 100 == 0:
            print(f"Processing file {i + 1}/{len(nc_files)}: {os.path.basename(nc_file)}")

        try:
            # Extract accession from filename
            accession = extract_accession_from_filename(nc_file)
            
            # Read file content
            with open(nc_file, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read().lower()

            # Search for monthly financial statement requirements
            monthly_fs_sentences, projected_fs_sentences = search_after_phrase(
                content, 'covenants', 10000, statement_keyword, fiscal_month_keyword, 
                fiscal_year_keyword, projected_keyword, borrower_keyword
            )

            # Search for lender meeting requirements
            lender_meeting_sentences = search_after_phrase_meeting(
                content, 'covenants', 50000, meeting_keyword
            )

            # Add monthly financial statement results
            for sentence in monthly_fs_sentences:
                results.append({
                    'accession': accession,
                    'monthly_fs': 1,
                    'monthly_context': sentence,
                    'projected_fs': 0,
                    'projected_context': '',
                    'lender_meeting': 0,
                    'lender_meeting_context': ''
                })

            # Add projected financial statement results
            for sentence in projected_fs_sentences:
                results.append({
                    'accession': accession,
                    'monthly_fs': 0,
                    'monthly_context': '',
                    'projected_fs': 1,
                    'projected_context': sentence,
                    'lender_meeting': 0,
                    'lender_meeting_context': ''
                })

            # Add lender meeting results
            for sentence in lender_meeting_sentences:
                results.append({
                    'accession': accession,
                    'monthly_fs': 0,
                    'monthly_context': '',
                    'projected_fs': 0,
                    'projected_context': '',
                    'lender_meeting': 1,
                    'lender_meeting_context': sentence
                })

        except Exception as e:
            print(f"Error processing {nc_file}: {e}")
            continue

    # Convert results to DataFrame
    if results:
        df_results = pd.DataFrame(results)
        
        # Save results
        df_results.to_csv(output_file, index=False)
        print(f"\nResults saved to: {output_file}")
        print(f"Total information covenant instances found: {len(df_results)}")
        
        # Summary statistics
        unique_accessions = df_results['accession'].nunique()
        monthly_count = df_results['monthly_fs'].sum()
        projected_count = df_results['projected_fs'].sum()
        meeting_count = df_results['lender_meeting'].sum()
        
        print(f"\nSummary:")
        print(f"  Unique accessions with information covenants: {unique_accessions}")
        print(f"  Monthly financial statement requirements: {monthly_count}")
        print(f"  Projected financial statement requirements: {projected_count}")
        print(f"  Lender meeting requirements: {meeting_count}")
        
    else:
        print("No information covenants found in any files")


def main():
    print("=" * 80)
    print("6_ExtractInfoCov.py - Extract Information Covenants from .nc Files")
    print("=" * 80)

    # Set up paths
    script_dir = Path(__file__).parent
    input_dir = script_dir / ".." / "Data" / "Raw" / "ExtractedAgreements_FinalMapped"
    output_file = script_dir / ".." / "Data" / "Intermediate" / "6_InformationCovenants.csv"

    # Check if input directory exists
    if not input_dir.exists():
        print(f"Error: Input directory not found: {input_dir}")
        return

    # Create output directory if it doesn't exist
    output_file.parent.mkdir(parents=True, exist_ok=True)

    print(f"Input directory: {input_dir}")
    print(f"Output file: {output_file}")

    # Process files
    process_nc_files(str(input_dir), str(output_file))

    print("\nDone.")


if __name__ == "__main__":
    main()
