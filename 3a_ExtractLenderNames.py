#!/usr/bin/env python3
"""
1_ExtractLenderNames.py

This script processes .nc files from ExtractedAgreements_FinalMapped folder
and creates batch job inputs for ChatGPT API to extract lender and borrower
information (no dependency on bucketed_loans).

Author: Zirui Song
Date: Sep 2025
"""

import pandas as pd
import numpy as np
import os
import json
import time
from pathlib import Path
import re
from datetime import datetime
from dotenv import load_dotenv

def get_first_800_tokens(text):
    """Tokenizing the text to get the first 800 tokens"""
    tokens = text.split()  # Split the text by spaces to approximate tokens
    return ' '.join(tokens[:800])  # Join back the first 800 tokens

def create_batch_prompt(text):
    """Create the prompt for batch processing"""
    # Limit text to first 800 tokens
    text = get_first_800_tokens(text)
    
    prompt = f"""You are an AI language model designed to extract key parties from corporate credit agreements and classify the lender type.

Tasks:
1) Extract Borrower Name
2) Extract Lender Name (Administrative Agent(s) preferred; if not present, use Lead Arrangers)
   - Exclude generic phrases such as: "various lenders", "various financial institutions", "certain financial institutions", "lenders from time to time party hereto".
   - If Borrower or Lender is not found, leave the value empty after the colon (e.g., "Lender Name: ").
3) Classify the Lender Type using the categories below (choose the single best category):

Description:
We are classifying lenders and financial institutions into categories commonly found in credit agreements. The categories are:
• Traditional Banks: Large global or regional commercial/investment banks (e.g., JPMorgan, Citi, Barclays).
• Finance Companies (Bank-Affiliated): Securities broker-dealers and capital markets arms of banks (e.g., J.P. Morgan Securities, Merrill Lynch).
• Finance Companies (Nonbank): Standalone finance and leasing firms, often linked to industrial companies (e.g., GE Capital, Ford Motor Credit).
• Investment Companies: Mutual funds and registered investment companies (e.g., Fidelity, Vanguard).
• Insurance Companies: Life and property-casualty insurers and their investment subsidiaries (e.g., MetLife, Allianz).
• Business Development Companies (BDCs): SEC-registered closed-end investment funds specializing in lending to middle-market firms (e.g., Ares Capital, Golub Capital BDC).
• Private Equity / Venture Capital Credit Arms: Credit or lending arms of private equity/VC firms (e.g., Apollo Credit, Blackstone Credit).
• Hedge Funds: Alternative investment funds with flexible, opportunistic strategies (e.g., Elliott, King Street).
• Investment Managers / Asset Managers: Institutional asset managers overseeing third-party capital across multiple strategies (e.g., PIMCO, Wellington).
• Specialty Direct Lenders / Private Credit: Nonbank lenders focused on direct loans to private companies (e.g., Antares, HPS).

If an institution does not appear in the reference list, assign it to the closest matching category based on its primary function (banking, insurance, asset management, private credit, etc.).

Categories and examples:
- Traditional Banks: JPMorgan Chase; Citibank; Bank of America; Wells Fargo; Goldman Sachs; Morgan Stanley; Barclays; HSBC; Deutsche Bank; BNP Paribas; Crédit Agricole; Société Générale; UBS; Mizuho; MUFG; Sumitomo Mitsui Banking Corporation; Royal Bank of Canada; Toronto-Dominion Bank; Banco Santander; BBVA; Standard Chartered
- Finance Companies (Bank-Affiliated): Citigroup Global Markets Inc.; SG Americas Securities, LLC; J.P. Morgan Securities Inc.; merrill lynch, pierce, fenner smith, incorporated; PNC Capital Markets LLC
- Finance Companies (Nonbank): GE Capital; CIT Group / CIT Finance LLC; Ford Motor Credit Company; GM Financial Company; DLL Finance LLC; John Deere Capital Corporation
- Investment Companies: Fidelity Management & Research; BlackRock Funds; T. Rowe Price Funds; Vanguard Group funds
- Insurance Companies: MetLife; Prudential Insurance Company of America; New York Life Insurance Company; MassMutual; Aegon; Allianz; AXA; Sun Life Financial
- Business Development Companies (BDCs): Ares Capital Corporation (ARCC); FS KKR Capital Corp.; Owl Rock Capital Corporation (Blue Owl); Golub Capital BDC, Inc.; Hercules Capital, Inc.; Main Street Capital Corporation
- Private Equity / Venture Capital Credit Arms: KKR Credit Advisors; Apollo Global Management (Apollo Credit); Blackstone Credit (GSO); Carlyle Global Credit; Bain Capital Credit; TPG Sixth Street Partners; Oaktree Capital Management
- Hedge Funds: Anchorage Capital Group; Canyon Partners; Elliott Management Corporation; King Street Capital; Silver Point Capital; York Capital Management
- Investment Managers / Asset Managers: BlackRock; PIMCO; Invesco; Neuberger Berman; Wellington Management; TCW Group; Barings LLC
- Specialty Direct Lenders / Private Credit: Ares Management; Golub Capital; Antares Capital; HPS Investment Partners; Monroe Capital; MidCap Financial; Twin Brook Capital Partners

If the Lender Name is empty, also leave Lender Type empty (do not guess). Do not output placeholders like "Not Found" or "N/A".

Text:
{text}

Output (each on its own line):
Borrower Name: <value>
Lender Name: <value>
Lender Type: <one of the categories above>"""
    
    return prompt

def list_nc_files(nc_folder: Path):
    """Enumerate all .nc files in the folder and return simple descriptors"""
    print(f"\nEnumerating .nc files in: {nc_folder}")
    nc_files = sorted(nc_folder.glob("*.nc"))
    print(f"Found {len(nc_files)} .nc files")
    matched = []
    for idx, nc_file in enumerate(nc_files):
        accession = nc_file.stem
        matched.append({
            'index': idx,
            'accession': accession,
            'nc_file': nc_file
        })
    return matched

def find_matching_nc_files(df, nc_folder):
    # Deprecated in folder-only mode; kept for backward compatibility
    return list_nc_files(nc_folder)

def read_nc_file(nc_file_path):
    """Read content from .nc file"""
    try:
        with open(nc_file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
        return content
    except Exception as e:
        print(f"Error reading {nc_file_path}: {e}")
        return None

def create_batch_job(matched_loans, output_dir, max_files_per_batch=100):
    """Create batch job files for ChatGPT API"""
    print(f"\nCreating batch job files...")
    
    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Split into batches
    num_batches = (len(matched_loans) + max_files_per_batch - 1) // max_files_per_batch
    print(f"Creating {num_batches} batch files with max {max_files_per_batch} files each")
    
    batch_files = []
    
    for batch_num in range(num_batches):
        start_idx = batch_num * max_files_per_batch
        end_idx = min((batch_num + 1) * max_files_per_batch, len(matched_loans))
        batch_loans = matched_loans[start_idx:end_idx]
        
        # Create batch file
        batch_file = output_dir / f"batch_{batch_num + 1:03d}.jsonl"
        batch_files.append(batch_file)
        
        print(f"Processing batch {batch_num + 1}/{num_batches}: {len(batch_loans)} files")
        
        with open(batch_file, 'w', encoding='utf-8') as f:
            for i, loan in enumerate(batch_loans):
                # Read .nc file content
                content = read_nc_file(loan['nc_file'])
                if content is None:
                    continue
                
                # Create prompt
                prompt = create_batch_prompt(content)
                
                # Create batch request
                batch_request = {
                    "custom_id": f"loan_{loan['index']}_{loan['accession']}",
                    "method": "POST",
                    "url": "/v1/chat/completions",
                    "body": {
                        "model": "gpt-5-mini",
                        "messages": [
                            {
                                "role": "user",
                                "content": prompt
                            }
                        ],
                        "max_completion_tokens": 2000
                    }
                }
                
                # Write to JSONL file
                f.write(json.dumps(batch_request) + '\n')
        
        print(f"Created batch file: {batch_file}")
    
    return batch_files

# Note: upload script generation removed per user request (manual upload of JSONL files).

def create_processing_summary(matched_loans, batch_files, output_dir):
    """Create a summary of the processing"""
    summary = {
        "processing_date": datetime.now().isoformat(),
        "total_contracts_processed": len(matched_loans),
        "total_batch_files": len(batch_files),
        "batch_files": [str(f) for f in batch_files]
    }
    
    summary_file = output_dir / "processing_summary.json"
    with open(summary_file, 'w') as f:
        json.dump(summary, f, indent=2)
    
    return summary_file

def main():
    """Main function to process .nc files and create batch jobs"""
    print("=" * 80)
    print("3_ExtractLenderNames.py - Process .nc Files and Create Batch Jobs")
    print("=" * 80)
    
    # Set up paths
    script_dir = Path(__file__).parent
    nc_folder = Path("/Users/zrsong/MIT Dropbox/Zirui Song/Research Projects/PSW_Nonbank Direct Lending/Data/Raw/ExtractedAgreements_FinalMapped")
    output_dir = script_dir / ".." / "Data" / "Raw" / "batch_jobs"

    # Load .env to populate OPENAI_API_KEY for any direct API calls (future-proof)
    try:
        load_dotenv()
    except Exception:
        pass
    
    # Check if nc folder exists
    if not nc_folder.exists():
        print(f"Error: NC files folder not found: {nc_folder}")
        return
    
    # Load all contracts directly from the folder (no CSV dependency)
    matched_loans = list_nc_files(nc_folder)
    if not matched_loans:
        print("No .nc files found to process!")
        return
    
    # Create batch job files
    batch_files = create_batch_job(matched_loans, output_dir, max_files_per_batch=100)
    
    # Create processing summary
    summary_file = create_processing_summary(matched_loans, batch_files, output_dir)
    
    # Print summary
    print(f"\n" + "=" * 80)
    print("PROCESSING COMPLETE")
    print("=" * 80)
    print(f"Total contracts processed: {len(matched_loans)}")
    print(f"Batch files created: {len(batch_files)}")
    print(f"Output directory: {output_dir}")
    # Upload script omitted; upload the JSONL files manually
    print(f"Summary file: {summary_file}")
    print("\nNext steps:")
    print("1. Set your OpenAI API key in the upload script")
    print("2. Run the upload script to submit batches to OpenAI")
    print("3. Monitor batch progress using the OpenAI API")

if __name__ == "__main__":
    main()
