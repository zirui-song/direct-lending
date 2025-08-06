import pandas as pd
import os
import json
import math

old_extraction_path = "../Data/Intermediate/old_extraction_unmerged.csv"
combined_loancontracts_path = "../Data/LoansFull/combined_loancontracts_mm.csv"

# Load accession and type_attachment from old_extraction_unmerged.csv
old_extraction_df = pd.read_csv(old_extraction_path, dtype=str)

# Load combined_loancontracts_mm.csv
combined_loancontracts_df = pd.read_csv(combined_loancontracts_path, dtype=str)

# Strip whitespace from accession and type_attachment columns
old_extraction_df['accession'] = old_extraction_df['accession'].str.strip()
combined_loancontracts_df['accession'] = combined_loancontracts_df['accession'].str.strip()
if 'type_attachment' in old_extraction_df.columns and 'type_attachment' in combined_loancontracts_df.columns:
    old_extraction_df['type_attachment'] = old_extraction_df['type_attachment'].str.strip()
    combined_loancontracts_df['type_attachment'] = combined_loancontracts_df['type_attachment'].str.strip()
else:
    # If type_attachment is missing in either, fill with empty string for join
    old_extraction_df['type_attachment'] = old_extraction_df.get('type_attachment', pd.Series(['']*len(old_extraction_df)))
    combined_loancontracts_df['type_attachment'] = combined_loancontracts_df.get('type_attachment', pd.Series(['']*len(combined_loancontracts_df)))

# Filter by both accession and type_attachment using a set of tuples for fast lookup
old_keys = set(zip(old_extraction_df['accession'], old_extraction_df['type_attachment']))
filtered_df = combined_loancontracts_df[
    combined_loancontracts_df.apply(
        lambda row: (row['accession'], row['type_attachment']) in old_keys, axis=1
    )
]
# save the filtered_df to a csv file
filtered_df.to_csv("../Data/LoansFull/filtered_df.csv", index=False)

def estimate_token_count(text):
    # Roughly 4 characters per token for English text, but contract text may be more dense.
    # We'll use 4.2 as a conservative estimate.
    return int(len(text) / 4.2)

if not filtered_df.empty:
    prompt = """You are tasked with analyzing a contract from an SEC exhibit 10 filing. Your goal is to pull out the deal-level and facility-level terms. Here's how to proceed:

1. Review the following contract text carefully and 

2. Extract the names of the lender, borrower and lead arranger from the contract:
    - A lender is defined as the financial institution providing the loan to a corporate entity.
    - A borrower is defined as the corporate entity receiving the loan from the lender.
    
3. Extract the DealActiveDate from the contract:
    - DealActiveDate refers to the origination date of the loan (format: YYYY-MM-DD).
    
4. Extract facility-level information:
    - Determine whether the contract includes multiple facilities.
    - Create a binary variable called multiple_facilities:
        - Set the value to 0 if the contract has only one facility.
        - Set the value to 1 if the contract has multiple facilities.
    - For each facility in the contract, extract the following information:
        - **Facility_type**: Classify to one of these types: Term Loan A; Term Loan B; Term Loan C; Revolver/Line >= 1 Yr; Term Loan; Revolver/Line < 1 Yr; Term Loan D; Term Loan E; Term Loan F; Term Loan G; Term Loan H; Term Loan I; Term Loan J; Term Loan K
        - **Facility_active_date**: format: YYYY-MM-DD
        - **Facility_maturity_date**: format: YYYY-MM-DD
        - **Facility_amount**: Loan amount in millions (e.g., 5,000,000 should be recorded as 5)
        - **Maturity**: Number of months between signing and expiration
        - **Secured**: Yes/No value indicating if the loan is secured
        - **Base_rate**: The reference interest rate (e.g., LIBOR, SOFR)
        - **Interest_Spread**: The applicable rate or margin for the loan
        
5. Interest Spread examples and formatting:
    - **Interest_Spread**: The applicable rate or margin for the loan, which is often stated as a spread over a benchmark rate (e.g., LIBOR, SOFR), such as 'LIBOR + 2%' or 'SOFR + 1.5%'. In this case, report LIBOR + 2% or SOFR + 1.5% for instance.
    A few more examples include: 
        - 1. "Applicable Rate" means, for any day, (i) 4.50% per annum, in the case of an ABR Loan or (ii) 5.50% per annum, in the case of a Eurodollar Loan. In this case, you should report LIBOR + 5.50%. 
        - 2. Interest on Loans.  Subject to the provisions of Section 2.07, the Loans shall bear interest (computed on the basis of the actual number of days elapsed over a year of 360 days) at a rate per annum equal to 9.00% on the unpaid principal amount thereof through the date such Loan is paid in full in cash (whether upon final maturity, prepayment, acceleration or otherwise). In this case, you should report 9.00%.
    - **Interest_Spread_Lowest**: If the applicable rate varies, report the lowest rate (e.g., "LIBOR + 2.5%")
    - **Interest_Spread_Highest**: If the applicable rate varies, report the highest rate (e.g., "LIBOR + 4.0%")
    - If there is only one rate, leave Interest_Spread_Lowest and Interest_Spread_Highest empty or set to the same value as Interest_Spread    
        
6. Output Format:
    - Return a JSON object with the following structure:
    {
        "lender": "lender name",
        "borrower": "borrower name", 
        "lead_arranger": "lead arranger name",
        "DealActiveDate": "YYYY-MM-DD",
        "multiple_facilities": 0 or 1,
        "facilities": [
            {
                "Facility_type": "facility type",
                "Facility_active_date": "YYYY-MM-DD",
                "Facility_maturity_date": "YYYY-MM-DD", 
                "Facility_amount": amount,
                "Maturity": months,
                "Secured": "Yes/No",
                "Base_rate": "base rate",
                "Interest_Spread": "interest spread",
                "Interest_Spread_Lowest": "lowest rate if varies, otherwise empty",
                "Interest_Spread_Highest": "highest rate if varies, otherwise empty"
            }
        ]
    }
    - If there are multiple facilities, include all of them in the facilities array
    - Please ensure all the deal amounts and facility amounts are in the same format
    - Please ensure the JSON format is correct and can be parsed properly
    - Please don't add any comments in the response
"""

    # Estimate prompt token count
    prompt_token_count = estimate_token_count(prompt)
    # Set a safe token limit for the model (gpt-4o-mini: 128k)
    MODEL_TOKEN_LIMIT = 128000 - 1000  # Subtracting an additional buffer for safety
    # Reserve tokens for response and system/user message structure
    RESPONSE_TOKEN_RESERVE = 1024
    MESSAGE_STRUCTURE_RESERVE = 100  # for JSON, roles, etc.

    batch_inputs = []
    for idx, row in filtered_df.iterrows():
        contract_text = row.get('text', '')
        accession = row.get('accession', f"row_{idx}")
        type_attachment = row.get('type_attachment', '')
        input_id = f"{accession}_{type_attachment}" if type_attachment else accession

        # Estimate contract text token count
        contract_token_count = estimate_token_count(contract_text)
        total_tokens = prompt_token_count + contract_token_count + RESPONSE_TOKEN_RESERVE + MESSAGE_STRUCTURE_RESERVE

        # If contract is too long, truncate it to fit within token limit
        if total_tokens > MODEL_TOKEN_LIMIT:
            allowed_contract_tokens = MODEL_TOKEN_LIMIT - prompt_token_count - RESPONSE_TOKEN_RESERVE - MESSAGE_STRUCTURE_RESERVE
            # Estimate allowed characters
            allowed_chars = int(allowed_contract_tokens * 4.2)
            contract_text = contract_text[:allowed_chars]
            # Recalculate contract_token_count for truncated text
            contract_token_count = estimate_token_count(contract_text)
            total_tokens = prompt_token_count + contract_token_count + RESPONSE_TOKEN_RESERVE + MESSAGE_STRUCTURE_RESERVE

        batch_inputs.append({
            "custom_id": input_id,
            "method": "POST",
            "url": "/v1/chat/completions",
            "body": {
                "model": "gpt-4o-mini",
                "messages": [
                    {"role": "system", "content": prompt},
                    {"role": "user", "content": contract_text}
                ],
                "max_tokens": RESPONSE_TOKEN_RESERVE,
                "temperature": 0.0
            }
        })

    # Split batch_inputs into 5 equally sized batches
    num_batches = 5
    total = len(batch_inputs)
    batch_size = math.ceil(total / num_batches)
    output_dir = "../Data/LoansFull/loan_extractions"
    os.makedirs(output_dir, exist_ok=True)
    batch_paths = []

    for i in range(num_batches):
        start_idx = i * batch_size
        end_idx = min((i + 1) * batch_size, total)
        batch = batch_inputs[start_idx:end_idx]

        # Remove duplicates by custom_id within this batch
        seen_ids = set()
        unique_batch = []
        for item in batch:
            custom_id = item.get("custom_id")
            if custom_id not in seen_ids:
                unique_batch.append(item)
                seen_ids.add(custom_id)
            # else: skip duplicate

        batch_input_path = os.path.join(output_dir, f"batch_input_part{i+1}.jsonl")
        with open(batch_input_path, "w", encoding="utf-8") as f:
            for item in unique_batch:
                f.write(json.dumps(item) + "\n")
        batch_paths.append(batch_input_path)
        print(f"Batch input file written to {batch_input_path}. Entries written: {len(unique_batch)}")

    print(f"Total entries: {total}. Split into {num_batches} batches.")
