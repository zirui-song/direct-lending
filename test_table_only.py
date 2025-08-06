#!/usr/bin/env python3
"""
Test script for table generation only
"""

import pandas as pd
import numpy as np
import os

# Set-up
script_dir = os.getcwd()
tab_dir = '/Users/zrsong/MIT Dropbox/Zirui Song/Apps/Overleaf/Information Covenants of Nonbank Direct Lending/Tables'

# Import data from Data/Cleaned folder
data = pd.read_csv('../Data/Cleaned/final_regression_sample.csv')

print("Data loaded successfully!")
print(f"Data shape: {data.shape}")
print(f"ff_12 unique values: {sorted(data['ff_12'].unique())}")
print(f"nonbank_lender unique values: {sorted(data['nonbank_lender'].unique())}")

# Industry names mapping - using the actual text values from your data
industry_names = {
    "Consumer NonDurables": "Consumer Nondurables",
    "Consumer Durables": "Consumer Durables", 
    "Manufacturing": "Manufacturing",
    "Oil, Gas, and Coal Extraction and Products": "Oil, Gas, and Coal Extraction and Products",
    "Chemicals and Allied Products": "Chemicals and Allied Products",
    "Business Equipment": "Business Equipment",
    "Telephone and Television Transmission": "Telephone and Television Transmission",
    "Utilities": "Utilities",
    "Wholesale, Retail, and Some Services": "Wholesale, Retail, and Some Services",
    "Healthcare, Medical Equipment, and Drugs": "Healthcare, Medical Equipment, and Drugs",
    "Finance": "Finance",
    "Other": "Other"
}

# Create cross-tabulation
cross_tab = pd.crosstab(data['ff_12'], data['nonbank_lender'], margins=True, margins_name='Total')

print(f"Cross-tabulation shape: {cross_tab.shape}")
print(f"Cross-tabulation index: {cross_tab.index.tolist()}")
print(f"Cross-tabulation:\n{cross_tab}")

# Calculate totals
bank_total = cross_tab[0].iloc[-1]
nonbank_total = cross_tab[1].iloc[-1]
grand_total = cross_tab['Total'].iloc[-1]

print(f"Bank total: {bank_total}")
print(f"Nonbank total: {nonbank_total}")
print(f"Grand total: {grand_total}")

# Test the table generation
latex_content = []
latex_content.append(r"\begin{tabular}{lccc}")
latex_content.append(r"\hline")
latex_content.append(r"& \multicolumn{2}{c}{Nonbank Lender} & \\")
latex_content.append(r"\cline{2-3}")
latex_content.append(r"& 0 & 1 & P-value \\")
latex_content.append(r"\hline")

# Add total row
bank_pct = (bank_total / grand_total) * 100
nonbank_pct = (nonbank_total / grand_total) * 100
latex_content.append(f"N & {bank_total:,} ({bank_pct:.1f}\\%) & {nonbank_total:,} ({nonbank_pct:.1f}\\%) & - \\\\")
latex_content.append(r"\hline")
latex_content.append(r"\hline")

# Add industry rows
for industry_code in range(1, 13):
    industry_name = industry_names.get(industry_code, f"Industry {industry_code}")
    
    if industry_code in cross_tab.index:
        bank_count = cross_tab.loc[industry_code, 0]
        nonbank_count = cross_tab.loc[industry_code, 1]
    else:
        bank_count = 0
        nonbank_count = 0
    
    bank_pct = (bank_count / bank_total) * 100 if bank_total > 0 else 0
    nonbank_pct = (nonbank_count / nonbank_total) * 100 if nonbank_total > 0 else 0
    
    print(f"Industry {industry_code} ({industry_name}): Banks={bank_count}, Nonbanks={nonbank_count}")
    
    latex_content.append(f"  {industry_name} & {bank_count:,} ({bank_pct:.1f}\\%) & {nonbank_count:,} ({nonbank_pct:.1f}\\%) & 0.000 \\\\")

latex_content.append(r"\hline")
latex_content.append(r"\hline")
latex_content.append(r"\end{tabular}")

# Write to file
output_file = f"{tab_dir}/test_table.tex"
with open(output_file, 'w') as f:
    f.write('\n'.join(latex_content))

print(f"Test table saved to: {output_file}")
print("Done!") 