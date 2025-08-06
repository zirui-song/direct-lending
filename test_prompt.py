import os
import json
import pandas as pd
import openai
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Set up OpenAI client
client = openai.OpenAI(api_key=os.getenv("OPENAI_API_KEY"))

def test_prompt():
    """
    Test the prompt from Extract_Loan_Terms.py with a sample contract
    """
    
    # The same prompt from Extract_Loan_Terms.py
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

    # Load the filtered_df.csv file
    try:
        filtered_df_path = "../Data/LoansFull/filtered_df.csv"
        filtered_df = pd.read_csv(filtered_df_path, dtype=str)
        print(f"✅ Loaded {len(filtered_df)} contracts from {filtered_df_path}")
        
        # Get a random sample of 10 contracts
        test_contracts = filtered_df.sample(n=5, random_state=42)  # random_state for reproducibility
        print(f"Testing with random sample of {len(test_contracts)} contracts...")
        
    except FileNotFoundError:
        print(f"❌ Could not find {filtered_df_path}")
        print("Please run Extract_Loan_Terms.py first to generate the filtered_df.csv file")
        return
    except Exception as e:
        print(f"❌ Error loading filtered_df.csv: {e}")
        return

    # Test each contract
    for idx, (_, contract_row) in enumerate(test_contracts.iterrows(), 1):
        contract_text = contract_row.get('text', '')
        accession = contract_row.get('accession', f'contract_{idx}')
        type_attachment = contract_row.get('type_attachment', '')
        
        print(f"\n{'='*80}")
        print(f"TESTING CONTRACT {idx}: {accession}_{type_attachment}")
        print(f"{'='*80}")
        print(f"Contract text length: {len(contract_text)} characters")
        
        # Truncate if too long (for testing purposes)
        if len(contract_text) > 400000:
            contract_text = contract_text[:400000] + "... [TRUNCATED]"
            print(f"Truncated to {len(contract_text)} characters for testing")
        
        try:
            # Make API call
            response = client.chat.completions.create(
                model="gpt-4o-mini",
                messages=[
                    {"role": "system", "content": prompt},
                    {"role": "user", "content": contract_text}
                ],
                max_tokens=1024,
                temperature=0.0
            )
            
            # Get the response content
            content = response.choices[0].message.content
            print("\nAPI Response:")
            print(content)
            
            # Try to parse the JSON response
            try:
                parsed_response = json.loads(content)
                print("\n✅ Successfully parsed JSON response!")
                print("\nParsed structure:")
                print(json.dumps(parsed_response, indent=2))
                
                # Check if it has the expected structure
                expected_fields = ["lender", "borrower", "lead_arranger", "DealActiveDate", "multiple_facilities", "facilities"]
                missing_fields = [field for field in expected_fields if field not in parsed_response]
                
                if missing_fields:
                    print(f"\n⚠️  Missing fields: {missing_fields}")
                else:
                    print("\n✅ All expected fields present!")
                    
                # Check facilities structure
                if "facilities" in parsed_response:
                    facilities = parsed_response["facilities"]
                    if isinstance(facilities, list):
                        print(f"\n✅ Facilities is a list with {len(facilities)} facility(ies)")
                        for i, facility in enumerate(facilities):
                            print(f"  Facility {i+1}: {facility.get('Facility_type', 'N/A')} - ${facility.get('Facility_amount', 'N/A')}M")
                    else:
                        print("\n⚠️  Facilities is not a list")
                        
            except json.JSONDecodeError as e:
                print(f"\n❌ Failed to parse JSON: {e}")
                print("Raw response content:")
                print(content)
                
        except Exception as e:
            print(f"\n❌ API call failed: {e}")
        
        print(f"\n{'='*80}")
        print(f"END OF CONTRACT {idx}")
        print(f"{'='*80}\n")

if __name__ == "__main__":
    test_prompt() 