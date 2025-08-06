import os
import json
import csv
import glob
from pathlib import Path

def combine_jsonl_to_csv():
    """
    Combine all five JSONL files from the loan_extractions folder into a single CSV file.
    """
    
    # Define paths
    loan_extractions_dir = os.path.join("..", "Data", "LoansFull", "loan_extractions")
    output_csv_path = os.path.join("..", "Data", "Intermediate", "combined_loan_extractions.csv")
    
    # Create output directory if it doesn't exist
    os.makedirs(os.path.dirname(output_csv_path), exist_ok=True)
    
    # Find all JSONL files in the loan_extractions directory
    jsonl_pattern = os.path.join(loan_extractions_dir, "batch_*_output.jsonl")
    jsonl_files = glob.glob(jsonl_pattern)
    
    if not jsonl_files:
        print(f"No JSONL files found in {loan_extractions_dir}")
        print(f"Expected pattern: {jsonl_pattern}")
        return
    
    print(f"Found {len(jsonl_files)} JSONL files:")
    for file in jsonl_files:
        print(f"  - {os.path.basename(file)}")
    
    # Process all JSONL files and extract data
    all_rows = []
    file_summary = {}
    
    for jsonl_file in sorted(jsonl_files):
        file_name = os.path.basename(jsonl_file)
        print(f"\nProcessing {file_name}...")
        
        file_count = 0
        error_count = 0
        
        with open(jsonl_file, 'r', encoding='utf-8') as f:
            for line_num, line in enumerate(f, 1):
                try:
                    # Parse JSON line
                    data = json.loads(line.strip())
                    
                    # Extract basic information
                    custom_id = data.get("custom_id") or data.get("id", "")
                    
                    # Split custom_id into accession and type_attachment
                    accession = ""
                    type_attachment = ""
                    if custom_id and "_EX-" in custom_id:
                        parts = custom_id.split("_EX-", 1)
                        accession = parts[0]
                        type_attachment = "EX-" + parts[1] if len(parts) > 1 else ""
                    else:
                        accession = custom_id
                    
                    # Extract response content
                    response = data.get("response", {})
                    content = ""
                    
                    if response and "body" in response and "choices" in response["body"]:
                        content = response["body"]["choices"][0]["message"]["content"]
                    else:
                        content = str(response)
                    
                    # Parse the content JSON if it's valid JSON
                    extracted_data = {}
                    try:
                        # Remove code block markers if present
                        if content.strip().startswith("```json"):
                            content = content.strip().split("\n", 1)[1].rsplit("```", 1)[0]
                        elif content.strip().startswith("```"):
                            content = content.strip().split("\n", 1)[1].rsplit("```", 1)[0]
                        
                        extracted_data = json.loads(content)
                    except (json.JSONDecodeError, IndexError) as e:
                        # If JSON parsing fails, store the raw content
                        extracted_data = {"raw_content": content}
                    
                    # Create row with all extracted fields
                    row = {
                        "accession": accession,
                        "type_attachment": type_attachment,
                        "lender": extracted_data.get("lender", ""),
                        "borrower": extracted_data.get("borrower", ""),
                        "lead_arranger": extracted_data.get("lead_arranger", ""),
                        "deal_active_date": extracted_data.get("DealActiveDate", ""),
                        "multiple_facilities": extracted_data.get("multiple_facilities", ""),
                        "facility_type": "",
                        "facility_active_date": "",
                        "facility_maturity_date": "",
                        "facility_amount": "",
                        "maturity": "",
                        "secured": "",
                        "base_rate": "",
                        "interest_spread": "",
                        "interest_spread_lowest": "",
                        "interest_spread_highest": "",
                        "raw_content": extracted_data.get("raw_content", "")
                    }
                    
                    # Extract facility information
                    facilities = extracted_data.get("facilities", {})
                    if isinstance(facilities, dict):
                        # Single facility as dict
                        row.update({
                            "facility_type": facilities.get("Facility_type", facilities.get("Original_facility_type", "")),
                            "facility_active_date": facilities.get("Facility_active_date", ""),
                            "facility_maturity_date": facilities.get("Facility_maturity_date", ""),
                            "facility_amount": facilities.get("Facility_amount", ""),
                            "maturity": facilities.get("Maturity", ""),
                            "secured": facilities.get("Secured", ""),
                            "base_rate": facilities.get("Base_rate", ""),
                            "interest_spread": facilities.get("Interest_Spread", ""),
                            "interest_spread_lowest": facilities.get("Interest_Spread_Lowest", ""),
                            "interest_spread_highest": facilities.get("Interest_Spread_Highest", "")
                        })
                        all_rows.append(row)
                    elif isinstance(facilities, list) and len(facilities) > 0:
                        # Multiple facilities as list - create a row for each facility
                        for facility_idx, facility in enumerate(facilities):
                            if isinstance(facility, dict):
                                # Create a new row for each facility, copying the base info
                                facility_row = row.copy()
                                facility_row.update({
                                    "facility_type": facility.get("Facility_type", ""),
                                    "facility_active_date": facility.get("Facility_active_date", ""),
                                    "facility_maturity_date": facility.get("Facility_maturity_date", ""),
                                    "facility_amount": facility.get("Facility_amount", ""),
                                    "maturity": facility.get("Maturity", ""),
                                    "secured": facility.get("Secured", ""),
                                    "base_rate": facility.get("Base_rate", ""),
                                    "interest_spread": facility.get("Interest_Spread", ""),
                                    "interest_spread_lowest": facility.get("Interest_Spread_Lowest", ""),
                                    "interest_spread_highest": facility.get("Interest_Spread_Highest", "")
                                })
                                all_rows.append(facility_row)
                    else:
                        # No facilities found, add the row as is
                        all_rows.append(row)
                    file_count += 1
                    
                except Exception as e:
                    error_count += 1
                    print(f"  Error processing line {line_num} in {file_name}: {e}")
                    continue
        
        file_summary[file_name] = {"processed": file_count, "errors": error_count}
        print(f"  Processed {file_count} records, {error_count} errors")
    
    # Write to CSV
    if all_rows:
        with open(output_csv_path, 'w', newline='', encoding='utf-8') as csvfile:
            fieldnames = all_rows[0].keys()
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(all_rows)
        
        print(f"\nSuccessfully combined {len(all_rows)} records into {output_csv_path}")
        
        # Print summary
        print("\nFile Summary:")
        total_processed = 0
        total_errors = 0
        for file_name, stats in file_summary.items():
            print(f"  {file_name}: {stats['processed']} processed, {stats['errors']} errors")
            total_processed += stats['processed']
            total_errors += stats['errors']
        
        print(f"\nTotal: {total_processed} records processed, {total_errors} errors")
        
        # Show sample data
        print(f"\nSample data (first 3 records):")
        for i, row in enumerate(all_rows[:3]):
            print(f"Record {i+1}:")
            print(f"  Accession: {row['accession']}")
            print(f"  Type Attachment: {row['type_attachment']}")
            print(f"  Borrower: {row['borrower']}")
            print(f"  Lead Arranger: {row['lead_arranger']}")
            print(f"  Facility Type: {row['facility_type']}")
            print(f"  Facility Amount: {row['facility_amount']}")
            print()
    else:
        print("No data found to write to CSV")

if __name__ == "__main__":
    combine_jsonl_to_csv() 