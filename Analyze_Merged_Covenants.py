# Remove unused imports
import pandas as pd
import os
import matplotlib.pyplot as plt


# Get the current working directory
script_dir = os.getcwd()
fig_dir = '/Users/zrsong/MIT Dropbox/Zirui Song/Apps/Overleaf/Information Covenants of Nonbank Direct Lending/Figures'

# Set the working directory to the current script's directory (which in this case is already the working directory)
os.chdir(script_dir)

print(f"Working directory is set to: {script_dir}")

# import from Data/Cleaned folder
data = pd.read_csv('../Data/Cleaned/final_regression_sample.csv')
# Loan Counts and Shares
# Group by year and nonbank_lender, then count the number of loans
loans_by_year = data.groupby(['year', 'nonbank_lender']).size().unstack()

# Plot the data
# Plot the data
fig, ax = plt.subplots()
ax.plot(loans_by_year.index, loans_by_year[0], marker='o', linestyle='-', label='Banks')
ax.plot(loans_by_year.index, loans_by_year[1], marker='s', linestyle='--', label='Nonbanks')

plt.xlabel('Year')
plt.ylabel('Number of Loans')
plt.legend()
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)

# Remove the boxes around the plot
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)

# save to fig_dir as png
plt.savefig(f"{fig_dir}/loancounts_by_year.png", dpi=300, bbox_inches='tight')

# Filter data for banks, private credit lenders, and other nonbanks
banks = data[data['nonbank_lender'] == 0]
private_credit = data[data['private_credit_lender'] == 1]
other_nonbanks = data[data['other_nonbank_lender'] == 1]

# Group by year and count the number of loans for each group
loans_by_year_banks = banks.groupby('year').size()
loans_by_year_private_credit = private_credit.groupby('year').size()
loans_by_year_other_nonbanks = other_nonbanks.groupby('year').size()

# Create a DataFrame to hold the counts
loans_by_year_combined = pd.DataFrame({
    'Banks': loans_by_year_banks,
    'Private Credit': loans_by_year_private_credit,
    'Other Nonbanks': loans_by_year_other_nonbanks
})

# Plot the data with different line styles
fig, ax = plt.subplots()
ax.plot(loans_by_year_combined.index, loans_by_year_combined['Banks'], marker='o', linestyle='-')
ax.plot(loans_by_year_combined.index, loans_by_year_combined['Private Credit'], marker='s', linestyle='--')
ax.plot(loans_by_year_combined.index, loans_by_year_combined['Other Nonbanks'], marker='^', linestyle=':')

plt.xlabel('Year')
plt.ylabel('Number of Loans')
plt.legend(['Banks', 'Private Credit', 'Other Nonbanks'])
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)

# Remove the boxes around the plot
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)

# Save to fig_dir as png
plt.savefig(f"{fig_dir}/loancounts_by_lender_type.png", dpi=300, bbox_inches='tight')

# Calculate the share of loans by nonbanks
loans_by_year['Nonbank_Share'] = loans_by_year[1] / (loans_by_year[0] + loans_by_year[1])

# Calculate the 3-year moving average of the share of loans by nonbanks
loans_by_year['Nonbank_Share_MA'] = loans_by_year['Nonbank_Share'].rolling(window=3).mean()

# Plot the 3-year moving average of the share of loans by nonbanks over the years
fig, ax = plt.subplots()
ax.plot(loans_by_year.index, loans_by_year['Nonbank_Share_MA'], marker='o')
plt.xlabel('Year')
plt.ylabel('Share of Loans by Nonbanks')
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)
# Remove the boxes around the plot
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)

# save to fig_dir as png
plt.savefig(f"{fig_dir}/loanshares_by_year.png", dpi=300, bbox_inches='tight')
plt.show()  # Add this to display the figure

# Calculate the share of loans for private credit, and other nonbanks
loans_by_year_combined['Private_Credit_Share'] = loans_by_year_combined['Private Credit'] / loans_by_year_combined.sum(axis=1)
loans_by_year_combined['Other_Nonbank_Share'] = loans_by_year_combined['Other Nonbanks'] / loans_by_year_combined.sum(axis=1)

# Calculate the 3-year moving average of the share of loans for private credit, and other nonbanks
loans_by_year_combined['Private_Credit_Share_MA'] = loans_by_year_combined['Private_Credit_Share'].rolling(window=3).mean()
loans_by_year_combined['Other_Nonbank_Share_MA'] = loans_by_year_combined['Other_Nonbank_Share'].rolling(window=3).mean()

# Plot with different line styles
fig, ax = plt.subplots()
ax.plot(loans_by_year_combined.index, loans_by_year_combined['Private_Credit_Share_MA'], marker='o', linestyle='-')
ax.plot(loans_by_year_combined.index, loans_by_year_combined['Other_Nonbank_Share_MA'], marker='s', linestyle='--')

plt.xlabel('Year')
plt.ylabel('Share of Loans')
plt.legend(['Private Credit', 'Other Nonbanks'])
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)

# Remove the boxes around the plot
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)

# Save to fig_dir as png
plt.savefig(f"{fig_dir}/loanshares_by_lender_type.png", dpi=300, bbox_inches='tight')

# Information Covenant Usage
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

# Plot the data for banks with different line styles
fig, ax = plt.subplots()
ax.plot(banks_info_covenants.index, banks_info_covenants['monthly_financial_statement'], marker='o', linestyle='-')
ax.plot(banks_info_covenants.index, banks_info_covenants['projected_financial_statement'], marker='s', linestyle='--')
ax.plot(banks_info_covenants.index, banks_info_covenants['lender_meeting'], marker='^', linestyle=':')

plt.xlabel('Year')
plt.ylabel('Number of Information Covenants')
plt.legend(['Monthly FS', 'Projected FS', 'Lender Meeting'])
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)
plt.savefig(fig_dir + '/info_covenants_by_banks.png', dpi=300, bbox_inches='tight')
plt.show()

# Plot the data for nonbanks with different line styles
fig, ax = plt.subplots()
ax.plot(nonbanks_info_covenants.index, nonbanks_info_covenants['monthly_financial_statement'], marker='o', linestyle='-')
ax.plot(nonbanks_info_covenants.index, nonbanks_info_covenants['projected_financial_statement'], marker='s', linestyle='--')
ax.plot(nonbanks_info_covenants.index, nonbanks_info_covenants['lender_meeting'], marker='^', linestyle=':')

plt.xlabel('Year')
plt.ylabel('Number of Information Covenants')
plt.legend(['Monthly FS', 'Projected FS', 'Lender Meeting'])
plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)
plt.savefig(f"{fig_dir}/info_covenants_by_nonbanks.png", dpi=300, bbox_inches='tight')

for covenant in info_covenants:
    plt.figure(figsize=(10, 6))
    plt.plot(banks_info_covenants.index, banks_info_covenants[covenant], marker='o', label='Banks', linestyle='-')
    plt.plot(nonbanks_info_covenants.index, nonbanks_info_covenants[covenant], marker='s', label='Nonbanks', linestyle='--')

    plt.xlabel('Year')
    plt.ylabel(f"Share of Loans with {' '.join([word.capitalize() for word in covenant.split('_')])} Covenant")
    plt.legend()
    plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)
    plt.gca().spines['top'].set_visible(False)
    plt.gca().spines['right'].set_visible(False)

    # Save to fig_dir as png
    plt.savefig(f"{fig_dir}/{covenant}_use_by_banks_and_nonbanks.png", dpi=300, bbox_inches='tight')
    plt.show()

# Calculate the 3-year moving average for banks and nonbanks
banks_info_covenants_ma = banks_info_covenants.rolling(window=3).mean()
nonbanks_info_covenants_ma = nonbanks_info_covenants.rolling(window=3).mean()

# Plot the 3-year moving average for each covenant type by banks and nonbanks
for covenant in info_covenants:
    plt.figure(figsize=(10, 6))
    plt.plot(banks_info_covenants_ma.index, banks_info_covenants_ma[covenant], marker='o', label='Banks', linestyle='-')
    plt.plot(nonbanks_info_covenants_ma.index, nonbanks_info_covenants_ma[covenant], marker='s', label='Nonbanks', linestyle='--')

    plt.xlabel('Year')
    plt.ylabel(f"Share of Loans with {' '.join([word.capitalize() for word in covenant.split('_')])} Covenant")
    plt.legend()
    plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)
    plt.gca().spines['top'].set_visible(False)
    plt.gca().spines['right'].set_visible(False)

    # Save to fig_dir as png
    plt.savefig(f"{fig_dir}/{covenant}_use_by_banks_and_nonbanks_ma.png", dpi=300, bbox_inches='tight')
    plt.show()

# Calculate the 3-year moving average for banks, private credit, and other nonbanks
banks_info_covenants_ma = banks_info_covenants.rolling(window=3).mean()

# Create copies and rename columns in private_credit and other_nonbanks
private_credit_copy = private_credit.copy()
private_credit_copy.rename(columns={'monthly_fs': 'monthly_financial_statement', 'projected_fs': 'projected_financial_statement'}, inplace=True)
other_nonbanks_copy = other_nonbanks.copy()
other_nonbanks_copy.rename(columns={'monthly_fs': 'monthly_financial_statement', 'projected_fs': 'projected_financial_statement'}, inplace=True)

# Plot the 3-year moving average for each covenant type by banks, private credit, and other nonbanks
for covenant in info_covenants:
    private_credit_info_covenants_ma = private_credit_copy.groupby('year')[info_covenants].mean().rolling(window=3).mean()
    other_nonbanks_info_covenants_ma = other_nonbanks_copy.groupby('year')[info_covenants].mean().rolling(window=3).mean()
    
    plt.figure(figsize=(10, 6))
    plt.plot(banks_info_covenants_ma.index, banks_info_covenants_ma[covenant], marker='o', label='Banks', linestyle='-')
    plt.plot(private_credit_info_covenants_ma.index, private_credit_info_covenants_ma[covenant], marker='s', label='Private Credit', linestyle='--')
    plt.plot(other_nonbanks_info_covenants_ma.index, other_nonbanks_info_covenants_ma[covenant], marker='^', label='Other Nonbanks', linestyle=':')

    plt.xlabel('Year')
    plt.ylabel(f"Share of Loans with {' '.join([word.capitalize() for word in covenant.split('_')])} Covenant")
    plt.legend()
    plt.grid(True, color='gray', linestyle='-', linewidth=0.5, alpha=0.3)
    plt.gca().spines['top'].set_visible(False)
    plt.gca().spines['right'].set_visible(False)

    # Save to fig_dir as png
    plt.savefig(f"{fig_dir}/{covenant}_use_by_lender_type_ma.png", dpi=300, bbox_inches='tight')
    plt.show()