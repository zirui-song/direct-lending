import pandas as pd
import numpy as np
import os
import shutil
import re
from pathlib import Path

def get_project_root():
    """
    Automatically detect the project root directory.
    Returns the directory containing this script.
    """
    return Path(__file__).parent.absolute()

def extract_filing_id(file_name):
    """
    Extract the filing ID from the file name.
    Example: from '2011/2011_1/20110329.nc/0001193125-11-081452.nc' 
    extract '0001193125-11-081452'
    """
    # Extract the last part before .nc
    parts = file_name.split('/')
    if len(parts) >= 1:
        last_part = parts[-1]
        # Remove .nc extension
        filing_id = last_part.replace('.nc', '')
        return filing_id
    return None

def main():
    """
    Main function to merge extracted agreements with Compustat/CRSP data.
    """
    print("Starting merge of extracted agreements with Compustat/CRSP data...")
    print("=" * 60)
    
    # Get project root and set up paths
    project_root = get_project_root()
    raw_data_path = os.path.join(project_root, '..', 'Data', 'Raw')
    clean_data_path = os.path.join(project_root, '..', 'Data', 'Cleaned')
    intermediate_data_path = os.path.join(project_root, '..', 'Data', 'Intermediate')
    
    print(f"Raw data path: {raw_data_path}")
    print(f"Clean data path: {clean_data_path}")
    print(f"Intermediate data path: {intermediate_data_path}")
    
    # Ensure directories exist
    os.makedirs(clean_data_path, exist_ok=True)
    os.makedirs(intermediate_data_path, exist_ok=True)
    
    # Step 1: Load extraction results
    print("\n1. Loading extraction results...")
    extraction_results_path = os.path.join(raw_data_path, 'extraction_results.csv')
    
    if not os.path.exists(extraction_results_path):
        print(f"Error: {extraction_results_path} not found!")
        return
    
    extraction_df = pd.read_csv(extraction_results_path)
    print(f"Loaded {len(extraction_df)} extracted agreements")
    print(f"Columns: {list(extraction_df.columns)}")
    
    # Step 2: Extract filing IDs from File Name
    print("\n2. Extracting filing IDs from file names...")
    extraction_df['filing_id'] = extraction_df['File Name'].apply(extract_filing_id)
    
    # Remove rows where filing_id extraction failed
    extraction_df = extraction_df.dropna(subset=['filing_id'])
    print(f"After filing ID extraction: {len(extraction_df)} agreements")
    
    # Save intermediate dataset
    extraction_with_filing_id_path = os.path.join(intermediate_data_path, 'extraction_with_filing_id.csv')
    extraction_df.to_csv(extraction_with_filing_id_path, index=False)
    print(f"Saved extraction with filing IDs to: {extraction_with_filing_id_path}")
    
    # Step 3: Load SEC filing mapping (filter for years >= 1994)
    print("\n3. Loading SEC filing mapping (years >= 1994)...")
    sec_mapping_path = os.path.join(raw_data_path, 'sec_filing_mapping_95to24.csv')
    
    if not os.path.exists(sec_mapping_path):
        print(f"Error: {sec_mapping_path} not found!")
        return
    
    sec_mapping_df = pd.read_csv(sec_mapping_path)
    print(f"Original SEC mapping: {len(sec_mapping_df)} records")
    
    # Extract year from FDATE and filter for years >= 1994
    sec_mapping_df['FDATE'] = pd.to_datetime(sec_mapping_df['FDATE'])
    sec_mapping_df['year'] = sec_mapping_df['FDATE'].dt.year
    sec_mapping_df = sec_mapping_df[sec_mapping_df['year'] >= 1994]
    print(f"After filtering for years >= 1994: {len(sec_mapping_df)} records")
    
    # Extract filing ID from FName
    def extract_filing_id_from_fname(fname):
        """Extract filing ID from FName like 'edgar/data/3/0000934850-95-001755.txt'"""
        if pd.isna(fname):
            return None
        # Extract the filing ID part
        match = re.search(r'/(\d{10}-\d{2}-\d+)\.txt$', fname)
        if match:
            return match.group(1)
        return None
    
    sec_mapping_df['filing_id'] = sec_mapping_df['FName'].apply(extract_filing_id_from_fname)
    sec_mapping_df = sec_mapping_df.dropna(subset=['filing_id'])
    print(f"After filing ID extraction: {len(sec_mapping_df)} records")
    
    # Save intermediate dataset
    sec_mapping_filtered_path = os.path.join(intermediate_data_path, 'sec_mapping_filtered_1994to24.csv')
    sec_mapping_df.to_csv(sec_mapping_filtered_path, index=False)
    print(f"Saved filtered SEC mapping to: {sec_mapping_filtered_path}")
    
    # Step 4: Merge extraction results with SEC mapping
    print("\n4. Merging extraction results with SEC mapping...")
    merged_df = extraction_df.merge(sec_mapping_df, on='filing_id', how='inner')
    print(f"After merging with SEC mapping: {len(merged_df)} records")
    
    # Save intermediate dataset
    extraction_sec_merged_path = os.path.join(intermediate_data_path, 'extraction_sec_merged.csv')
    merged_df.to_csv(extraction_sec_merged_path, index=False)
    print(f"Saved extraction-SEC merged data to: {extraction_sec_merged_path}")
    
    # Step 5: Load CIK-GVKEY link table and ensure one-to-one mapping
    print("\n5. Loading CIK-GVKEY link table...")
    cik_gvkey_path = os.path.join(raw_data_path, 'gvkey_cik_linktable.csv')
    
    if not os.path.exists(cik_gvkey_path):
        print(f"Error: {cik_gvkey_path} not found!")
        return
    
    cik_gvkey_df = pd.read_csv(cik_gvkey_path, encoding='latin-1')
    print(f"Original CIK-GVKEY mapping: {len(cik_gvkey_df)} records")
    
    # For duplicate CIKs, keep the GVKEY mapping based on a specific source hierarchy
    # Define the priority order for sources
    source_priority = {
        "CRSP/Compustat Merged": 1,
        "Capital IQ": 2,
        "Compustat Company": 3,
        "Compustat Security": 4
    }
    
    # Assign a numerical priority to each row based on its source
    # Default to a lower priority (higher number) if source is not in the defined list
    cik_gvkey_df['source_rank'] = cik_gvkey_df['source'].map(source_priority).fillna(5).astype(int)
    
    # Sort by CIK and then by source_rank to prioritize mappings
    # Added gvkey to sort for deterministic tie-breaking if multiple GVKEYs have the same highest priority source
    cik_gvkey_df = cik_gvkey_df.sort_values(by=['cik', 'source_rank', 'gvkey'], ascending=[True, True, True])
    
    # Keep only the first (highest priority) GVKEY for each CIK
    cik_gvkey_df = cik_gvkey_df.drop_duplicates(subset=['cik'], keep='first')
    
    # Drop the temporary source_rank column
    cik_gvkey_df = cik_gvkey_df.drop(columns=['source_rank'])
    print(f"After keeping CIK-GVKEY mapping based on source hierarchy: {len(cik_gvkey_df)} records")
    
    # Save intermediate dataset
    cik_gvkey_filtered_path = os.path.join(intermediate_data_path, 'cik_gvkey_most_common.csv')
    cik_gvkey_df.to_csv(cik_gvkey_filtered_path, index=False)
    print(f"Saved filtered CIK-GVKEY mapping to: {cik_gvkey_filtered_path}")
    
    # Step 6: Merge with CIK-GVKEY mapping
    print("\n6. Merging with CIK-GVKEY mapping...")
    # Check if CIK column exists (case insensitive)
    cik_col = None
    for col in merged_df.columns:
        if col.lower() == 'cik':
            cik_col = col
            break
    
    if cik_col is None:
        print("Warning: CIK column not found in merged data. Please check column names.")
        print(f"Available columns: {list(merged_df.columns)}")
        return
    
    # Rename CIK column to lowercase for consistency
    merged_df = merged_df.rename(columns={cik_col: 'cik'})
    
    # Merge with CIK-GVKEY mapping
    temp_merged = merged_df.merge(cik_gvkey_df, on='cik', how='inner')
    print(f"After merging with CIK-GVKEY mapping: {len(temp_merged)} records")
    
    # Keep only one record per unique filing (filing_id)
    final_df = temp_merged.drop_duplicates(subset=['filing_id'], keep='first')
    print(f"After removing duplicate filings: {len(final_df)} records")
    
    # Save intermediate dataset
    agreements_with_gvkey_path = os.path.join(intermediate_data_path, 'agreements_with_gvkey.csv')
    final_df.to_csv(agreements_with_gvkey_path, index=False)
    print(f"Saved agreements with GVKEY to: {agreements_with_gvkey_path}")
    # Step 7: Load Compustat/CRSP merged data
    print("\n7. Loading Compustat/CRSP merged data...")
    comp_crsp_path = os.path.join(clean_data_path, 'comp_crspa_merged.csv')
    
    if not os.path.exists(comp_crsp_path):
        print(f"Warning: {comp_crsp_path} not found. Skipping Compustat/CRSP merge.")
        # Save the current merged data
        output_path = os.path.join(clean_data_path, 'agreements_sec_mapping.csv')
        final_df.to_csv(output_path, index=False)
        print(f"Saved agreements with SEC mapping to: {output_path}")
        return
    
    comp_crsp_df = pd.read_csv(comp_crsp_path)
    print(f"Compustat/CRSP data: {len(comp_crsp_df)} records")
    
    # Step 8: Final merge with Compustat/CRSP data (only for filing year)
    print("\n8. Final merge with Compustat/CRSP data (filing year only)...")
    # Convert gvkey to same type for merging
    final_df['gvkey'] = final_df['gvkey'].astype(str)
    comp_crsp_df['gvkey'] = comp_crsp_df['gvkey'].astype(str)
    
    # Extract filing year from FDATE
    final_df['filing_year'] = pd.to_datetime(final_df['FDATE']).dt.year
    
    # Merge on both gvkey and year (filing year)
    complete_df = final_df.merge(comp_crsp_df, left_on=['gvkey', 'filing_year'], right_on=['gvkey', 'fyear'], how='inner')
    print(f"Final merged dataset: {len(complete_df)} records")
    
    # Step 9: Save the final dataset
    print("\n9. Saving final merged dataset...")
    output_path = os.path.join(clean_data_path, 'agreements_comp_crsp_merged.csv')
    complete_df.to_csv(output_path, index=False)
    print(f"Final dataset saved to: {output_path}")

    # Step 9.5: Create a folder with only final mapped contracts (with Compustat/CRSP)
    print("\n9.5. Creating folder of final mapped contracts...")
    extracted_dir = os.path.join(raw_data_path, 'ExtractedAgreements')
    final_mapped_dir = os.path.join(raw_data_path, 'ExtractedAgreements_FinalMapped')
    os.makedirs(final_mapped_dir, exist_ok=True)

    final_copied_count = 0
    final_missing_count = 0
    for filing_id in complete_df['filing_id'].dropna().unique():
        src_path = os.path.join(extracted_dir, f"{filing_id}.nc")
        dst_path = os.path.join(final_mapped_dir, f"{filing_id}.nc")
        if os.path.exists(src_path):
            try:
                shutil.copy2(src_path, dst_path)
                final_copied_count += 1
            except Exception as e:
                print(f"Warning: failed to copy {src_path} -> {dst_path}: {e}")
        else:
            final_missing_count += 1
    print(f"Copied {final_copied_count} final mapped contracts to: {final_mapped_dir}")
    if final_missing_count:
        print(f"Warning: {final_missing_count} final mapped filings did not have a matching .nc file in {extracted_dir}")
    
    # Print summary statistics
    print("\n" + "=" * 60)
    print("MERGE SUMMARY:")
    print(f"Original extracted agreements: {len(extraction_df)}")
    print(f"After SEC mapping merge: {len(merged_df)}")
    print(f"After CIK-GVKEY mapping: {len(final_df)}")
    print(f"Final with Compustat/CRSP: {len(complete_df)}")
    print(f"Success rate: {len(complete_df)/len(extraction_df)*100:.1f}%")
    
    # Show sample of final data
    print(f"\nSample of final merged data:")
    print(complete_df[['File Name', 'filing_id', 'cik', 'gvkey', 'fyear']].head())
    
    print("\nMerge completed successfully!")

if __name__ == "__main__":
    main()
