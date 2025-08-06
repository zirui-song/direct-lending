#!/usr/bin/env python3
"""
Analyze Merged Covenants
This script analyzes covenant data from the merged dataset, including deal counts, 
information covenant usage, and lender type analysis.
"""

import pandas as pd
import numpy as np
import os
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend to prevent display
import matplotlib.pyplot as plt

# Set-up
# Get the current working directory
script_dir = os.getcwd()
fig_dir = '/Users/zrsong/MIT Dropbox/Zirui Song/Apps/Overleaf/Information Covenants of Nonbank Direct Lending/Figures'

# Set the working directory to the current script's directory (which in this case is already the working directory)
os.chdir(script_dir)

print(f"Working directory is set to: {script_dir}")

# Import data from Data/Cleaned folder
data = pd.read_csv('../Data/Cleaned/final_regression_sample.csv')

# =============================================================================
# Deal Counts and Shares
# =============================================================================

# Group by year and nonbank_lender, then count the number of deals
deals_by_year = data.groupby(['year', 'nonbank_lender']).size().unstack()

# Plot the data with different line styles and markers
plt.figure(figsize=(10, 6))
plt.plot(deals_by_year.index, deals_by_year[0], marker='o', linestyle='-', linewidth=2, label='Banks')
plt.plot(deals_by_year.index, deals_by_year[1], marker='s', linestyle='--', linewidth=2, label='Nonbanks')
plt.xlabel('Year')
plt.ylabel('Number of Deals')
plt.title('Number of Deals by Banks and Nonbanks Over Years')
plt.legend()
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)  # Enable grid with lighter shade

# Remove the boxes around the plot
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)

# save to fig_dir as png
plt.savefig(f"{fig_dir}/dealcounts_by_year.png", dpi=300, bbox_inches='tight')
plt.close()

# Filter data for banks, private credit lenders, and other nonbanks
banks = data[data['nonbank_lender'] == 0]
private_credit = data[data['private_credit_lender'] == 1]
other_nonbanks = data[data['other_nonbank_lender'] == 1]

# Group by year and count the number of deals for each group
deals_by_year_banks = banks.groupby('year').size()
deals_by_year_private_credit = private_credit.groupby('year').size()
deals_by_year_other_nonbanks = other_nonbanks.groupby('year').size()

# Create a DataFrame to hold the counts
deals_by_year_combined = pd.DataFrame({
    'Banks': deals_by_year_banks,
    'Private Credit': deals_by_year_private_credit,
    'Other Nonbanks': deals_by_year_other_nonbanks
})

# Plot the data with different line styles and markers
plt.figure(figsize=(10, 6))
plt.plot(deals_by_year_combined.index, deals_by_year_combined['Banks'], marker='o', linestyle='-', linewidth=2, label='Banks')
plt.plot(deals_by_year_combined.index, deals_by_year_combined['Private Credit'], marker='s', linestyle='--', linewidth=2, label='Private Credit')
plt.plot(deals_by_year_combined.index, deals_by_year_combined['Other Nonbanks'], marker='^', linestyle=':', linewidth=2, label='Other Nonbanks')
plt.xlabel('Year')
plt.ylabel('Number of Deals')
plt.title('Number of Deals by Banks, Private Credit, and Other Nonbanks Over Years')
plt.legend()
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)  # Enable grid with lighter shade

# Remove the boxes around the plot
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)

# Save to fig_dir as png
plt.savefig(f"{fig_dir}/dealcounts_by_lender_type.png", dpi=300, bbox_inches='tight')
plt.close()

# Calculate the 3-year moving average of the share of deals by nonbanks
# Calculate the share of deals by nonbanks
deals_by_year['Nonbank_Share'] = deals_by_year[1] / (deals_by_year[0] + deals_by_year[1])

# Calculate the 3-year moving average of the share of deals by nonbanks
deals_by_year['Nonbank_Share_MA'] = deals_by_year['Nonbank_Share'].rolling(window=3).mean()

# Plot the 3-year moving average of the share of deals by nonbanks over the years
plt.figure(figsize=(10, 6))
plt.plot(deals_by_year.index, deals_by_year['Nonbank_Share_MA'], marker='o', linestyle='-', linewidth=2)
plt.xlabel('Year')
plt.ylabel('Share of Deals by Nonbanks')
plt.title('3-Year Moving Average Share of Deals by Nonbanks')
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)  # Enable grid with lighter shade
# Remove the boxes around the plot
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)

# save to fig_dir as png
plt.savefig(f"{fig_dir}/dealshares_by_year.png", dpi=300, bbox_inches='tight')
plt.close()

# Calculate the share of deals for private credit, and other nonbanks
deals_by_year_combined['Private_Credit_Share'] = deals_by_year_combined['Private Credit'] / deals_by_year_combined.sum(axis=1)
deals_by_year_combined['Other_Nonbank_Share'] = deals_by_year_combined['Other Nonbanks'] / deals_by_year_combined.sum(axis=1)

# Calculate the 3-year moving average of the share of deals for private credit, and other nonbanks
deals_by_year_combined['Private_Credit_Share_MA'] = deals_by_year_combined['Private_Credit_Share'].rolling(window=3).mean()
deals_by_year_combined['Other_Nonbank_Share_MA'] = deals_by_year_combined['Other_Nonbank_Share'].rolling(window=3).mean()

# Plot the 3-year moving average of the share of deals for banks, private credit, and other nonbanks over the years
plt.figure(figsize=(10, 6))
plt.plot(deals_by_year_combined.index, deals_by_year_combined['Private_Credit_Share_MA'], marker='s', linestyle='--', linewidth=2, label='Private Credit')
plt.plot(deals_by_year_combined.index, deals_by_year_combined['Other_Nonbank_Share_MA'], marker='^', linestyle=':', linewidth=2, label='Other Nonbanks')
plt.xlabel('Year')
plt.ylabel('Share of Deals')
plt.title('3-Year Moving Average Share of Deals by Nonbank Lender Type')
plt.legend()
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)  # Enable grid with lighter shade
# Remove the boxes around the plot
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)

# Save to fig_dir as png
plt.savefig(f"{fig_dir}/dealshares_by_lender_type.png", dpi=300, bbox_inches='tight')
plt.close()

# =============================================================================
# Information Covenant Usage
# =============================================================================

# rename monthly_fs to monthly_financial_statement and projected_fs to projected_financial_statement
data.rename(columns={'monthly_fs': 'monthly_financial_statement', 'projected_fs': 'projected_financial_statement'}, inplace=True)

# Filter data for banks and nonbanks
banks = data[data['nonbank_lender'] == 0]
nonbanks = data[data['nonbank_lender'] == 1]

# Define the three types of information covenants
info_covenants = ['monthly_financial_statement', 'projected_financial_statement', 'lender_meeting']

# Group by year and count the occurrences of each type of information covenant for banks
banks_info_covenants = banks.groupby('year')[info_covenants].mean()

# Group by year and count the occurrences of each type of information covenant for nonbanks
nonbanks_info_covenants = nonbanks.groupby('year')[info_covenants].mean()

# Plot the data for banks with different line styles and markers
plt.figure(figsize=(10, 6))
plt.plot(banks_info_covenants.index, banks_info_covenants['monthly_financial_statement'], marker='o', linestyle='-', linewidth=2, label='Monthly FS')
plt.plot(banks_info_covenants.index, banks_info_covenants['projected_financial_statement'], marker='s', linestyle='--', linewidth=2, label='Projected FS')
plt.plot(banks_info_covenants.index, banks_info_covenants['lender_meeting'], marker='^', linestyle=':', linewidth=2, label='Lender Meeting')
plt.xlabel('Year')
plt.ylabel('Number of Information Covenants')
plt.title('Use of Information Covenants by Banks Over Years')
plt.legend()
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)
plt.savefig(f"{fig_dir}/info_covenants_by_banks.png", dpi=300, bbox_inches='tight')
plt.close()

# Plot the data for nonbanks with different line styles and markers
plt.figure(figsize=(10, 6))
plt.plot(nonbanks_info_covenants.index, nonbanks_info_covenants['monthly_financial_statement'], marker='o', linestyle='-', linewidth=2, label='Monthly FS')
plt.plot(nonbanks_info_covenants.index, nonbanks_info_covenants['projected_financial_statement'], marker='s', linestyle='--', linewidth=2, label='Projected FS')
plt.plot(nonbanks_info_covenants.index, nonbanks_info_covenants['lender_meeting'], marker='^', linestyle=':', linewidth=2, label='Lender Meeting')
plt.xlabel('Year')
plt.ylabel('Number of Information Covenants')
plt.title('Use of Information Covenants by Nonbanks Over Years')
plt.legend()
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)
plt.savefig(f"{fig_dir}/info_covenants_by_nonbanks.png", dpi=300, bbox_inches='tight')
plt.close()

# Plot individual covenant types by banks and nonbanks with different line styles and markers
for covenant in info_covenants:
    plt.figure(figsize=(10, 6))
    plt.plot(banks_info_covenants.index, banks_info_covenants[covenant], marker='o', linestyle='-', linewidth=2, label='Banks')
    plt.plot(nonbanks_info_covenants.index, nonbanks_info_covenants[covenant], marker='s', linestyle='--', linewidth=2, label='Nonbanks')

    plt.xlabel('Year')
    plt.ylabel(f"Share of Deals with {' '.join([word.capitalize() for word in covenant.split('_')])} Covenant")
    plt.title(f"Use of {' '.join([word.capitalize() for word in covenant.split('_')])} Covenant by Banks and Nonbanks")
    plt.legend()
    plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)
    plt.gca().spines['top'].set_visible(False)
    plt.gca().spines['right'].set_visible(False)

    # Save to fig_dir as png
    plt.savefig(f"{fig_dir}/{covenant}_use_by_banks_and_nonbanks.png", dpi=300, bbox_inches='tight')
    plt.close()

# Calculate the 3-year moving average for banks and nonbanks
banks_info_covenants_ma = banks_info_covenants.rolling(window=3).mean()
nonbanks_info_covenants_ma = nonbanks_info_covenants.rolling(window=3).mean()

# Plot the 3-year moving average for each covenant type by banks and nonbanks with different line styles and markers
for covenant in info_covenants:
    plt.figure(figsize=(10, 6))
    plt.plot(banks_info_covenants_ma.index, banks_info_covenants_ma[covenant], marker='o', linestyle='-', linewidth=2, label='Banks')
    plt.plot(nonbanks_info_covenants_ma.index, nonbanks_info_covenants_ma[covenant], marker='s', linestyle='--', linewidth=2, label='Nonbanks')

    plt.xlabel('Year')
    plt.ylabel(f"Share of Deals with {' '.join([word.capitalize() for word in covenant.split('_')])} Covenant")
    plt.title(f"3-Year Moving Average of {' '.join([word.capitalize() for word in covenant.split('_')])} Covenant by Banks and Nonbanks")
    plt.legend()
    plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)
    plt.gca().spines['top'].set_visible(False)
    plt.gca().spines['right'].set_visible(False)

    # Save to fig_dir as png
    plt.savefig(f"{fig_dir}/{covenant}_use_by_banks_and_nonbanks_ma.png", dpi=300, bbox_inches='tight')
    plt.close()

# Calculate the 3-year moving average for banks, private credit, and other nonbanks
banks_info_covenants_ma = banks_info_covenants.rolling(window=3).mean()

# Create copies and rename columns in private_credit and other_nonbanks
private_credit_copy = private_credit.copy()
private_credit_copy.rename(columns={'monthly_fs': 'monthly_financial_statement', 'projected_fs': 'projected_financial_statement'}, inplace=True)
other_nonbanks_copy = other_nonbanks.copy()
other_nonbanks_copy.rename(columns={'monthly_fs': 'monthly_financial_statement', 'projected_fs': 'projected_financial_statement'}, inplace=True)

# Plot the 3-year moving average for each covenant type by banks, private credit, and other nonbanks with different line styles and markers
for covenant in info_covenants:
    private_credit_info_covenants_ma = private_credit_copy.groupby('year')[info_covenants].mean().rolling(window=3).mean()
    other_nonbanks_info_covenants_ma = other_nonbanks_copy.groupby('year')[info_covenants].mean().rolling(window=3).mean()
    plt.figure(figsize=(10, 6))
    plt.plot(banks_info_covenants_ma.index, banks_info_covenants_ma[covenant], marker='o', linestyle='-', linewidth=2, label='Banks')
    plt.plot(private_credit_info_covenants_ma.index, private_credit_info_covenants_ma[covenant], marker='s', linestyle='--', linewidth=2, label='Private Credit')
    plt.plot(other_nonbanks_info_covenants_ma.index, other_nonbanks_info_covenants_ma[covenant], marker='^', linestyle=':', linewidth=2, label='Other Nonbanks')

    plt.xlabel('Year')
    plt.ylabel(f"Share of Deals with {' '.join([word.capitalize() for word in covenant.split('_')])} Covenant")
    plt.title(f"3-Year Moving Average of {' '.join([word.capitalize() for word in covenant.split('_')])} by Lender Type")
    plt.legend()
    plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)
    plt.gca().spines['top'].set_visible(False)
    plt.gca().spines['right'].set_visible(False)

    # Save to fig_dir as png
    plt.savefig(f"{fig_dir}/{covenant}_use_by_lender_type_ma.png", dpi=300, bbox_inches='tight')
    plt.close()

# =============================================================================
# Panel A: Number of Deals By Industry (Fama-French 12)
# =============================================================================

# Define the tabulation directory
tab_dir = '/Users/zrsong/MIT Dropbox/Zirui Song/Apps/Overleaf/Information Covenants of Nonbank Direct Lending/Tables'

# Function to create LaTeX table for industry distribution comparison
def create_industry_comparison_table(data, output_file, title):
    """
    Create a LaTeX table showing deal distribution by Fama-French 12 industry 
    comparing banks vs nonbanks with percentages and totals.
    """
    
    # Create cross-tabulation
    cross_tab = pd.crosstab(data['ff_12'], data['nonbank_lender'], margins=True, margins_name='Total')
    
    # Calculate totals
    bank_total = cross_tab[0].iloc[-1]
    nonbank_total = cross_tab[1].iloc[-1]
    grand_total = cross_tab['Total'].iloc[-1]
    
    # Create LaTeX table
    latex_content = []
    latex_content.append(r"\begin{tabular}{lccc}")
    latex_content.append(r"\hline")
    latex_content.append(r"\hline")
    latex_content.append(r"& \multicolumn{2}{c}{Nonbank Lender} & \\")
    latex_content.append(r"\cline{2-3}")
    latex_content.append(r"& No & Yes & P-value \\")
    latex_content.append(r"\hline")
    
    # Add total row
    bank_pct = (bank_total / grand_total) * 100
    nonbank_pct = (nonbank_total / grand_total) * 100
    latex_content.append(f"Fama-French 12 Industry & {bank_total:,} ({bank_pct:.1f}\\%) & {nonbank_total:,} ({nonbank_pct:.1f}\\%) & - \\\\")
    latex_content.append(r"\hline")
    # Add industry rows
    for industry_name in cross_tab.index[:-1]:  # Exclude 'Total' row
        # Get counts
        bank_count = cross_tab.loc[industry_name, 0]
        nonbank_count = cross_tab.loc[industry_name, 1]
        
        # Calculate percentages
        bank_pct = (bank_count / bank_total) * 100
        nonbank_pct = (nonbank_count / nonbank_total) * 100
        
        # Perform t-test
        from scipy import stats
        industry_indicator = (data['ff_12'] == industry_name).astype(int)
        lender_indicator = data['nonbank_lender']
        
        try:
            t_stat, p_value = stats.ttest_ind(
                industry_indicator[lender_indicator == 0],  # Banks
                industry_indicator[lender_indicator == 1]   # Nonbanks
            )
            
            # Add significance stars
            if p_value < 0.01:
                p_value_str = f"{p_value:.3f}\\^{{***}}"
            elif p_value < 0.05:
                p_value_str = f"{p_value:.3f}\\^{{**}}"
            elif p_value < 0.10:
                p_value_str = f"{p_value:.3f}\\^{{*}}"
            else:
                p_value_str = f"{p_value:.3f}"
        except:
            p_value_str = "-"
        
        latex_content.append(f"  {industry_name} & {bank_count:,} ({bank_pct:.1f}\\%) & {nonbank_count:,} ({nonbank_pct:.1f}\\%) & {p_value_str} \\\\")
    
    latex_content.append(r"\hline")
    latex_content.append(r"\hline")
    latex_content.append(r"\end{tabular}")
    
    # Write to file
    with open(output_file, 'w') as f:
        f.write('\n'.join(latex_content))
    
    print(f"Table saved to: {output_file}")

# Create comparison table for banks vs nonbanks
print("Creating industry comparison table...")

# Table comparing banks vs nonbanks
create_industry_comparison_table(
    data=data,
    output_file=f"{tab_dir}/tabulation_ff12_comparison.tex",
    title="Panel A: Industry Distribution by Lender Type"
)

print("Industry comparison table completed!")