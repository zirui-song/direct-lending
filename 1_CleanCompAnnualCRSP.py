import wrds
import os
import pandas as pd
import numpy as np
import datetime
import time
import re 

# Get the current working directory

script_dir = os.getcwd()

# Set the working directory to the current script's directory (which in this case is already the working directory)
os.chdir(script_dir)

print(f"Working directory is set to: {script_dir}")

# Connect to WRDS
db = wrds.Connection(wrds_username='zrsong')

# Define the start and end dates
start_date = '2010-01-01'
end_date = '2024-06-30'

# overleaf_dir = "/Users/zrsong/MIT Dropbox/Zirui Song/Apps/Overleaf/Contract Innovations"

# Compustat/CRSP

fund_table = 'funda'

varlist = ['conm', 'tic', 'cusip','fyear', 'fyr', 'at','capx', 'ceq', 'cogs', 'csho', 'dlc', 'dlcch','dltt', 'dp', 'ib', 'itcb', 
           'lt', 'mib', 'naicsh', 'ni', 'prstkcc', 'pstk', 'pstkl', 'pstkrv', 're', 'revt', 'sale', 'ebitda', 'dpc', 'oiadp', 'oibdp',
           'seq', 'sich', 'txdb', 'txdi', 'txditc', 'wcapch', 'xint', 'xlr', 'xrd', 'xsga', 'ppegt', 'xrd', 'ebit', 'aqc',
           'act', 'che', 'dltis', 'dltr', 'dvc', 'idit', 'intan', 'lct', 'dclo', 'oancf', 'pi', 'pifo', 'ppent', 'prcc_f', 'tlcf', 'txfo',
           'txdba', 'txdbca', 'txndb']

query = """SELECT gvkey, datadate, {}
           FROM comp.{}
           WHERE datafmt = 'STD'
           AND popsrc = 'D'
           AND indfmt = 'INDL'
           AND consol = 'C'
           AND fyear>=1994;""".format(", ".join(varlist), fund_table)

compa = db.raw_sql(query, date_cols=['datadate'])

del(fund_table, varlist, query)

# Import SIC codes from comp.company
sic_table = 'company'
query = "SELECT gvkey, sic, ipodate FROM comp.company"
sic_codes = db.raw_sql(query)

# Merge SIC codes back to compa dataframe
compa = compa.merge(sic_codes, how='left', on='gvkey')

# all colnames of compa
# check if sic exists
# for each gvkey fyear, keep the one with the highest at
compa = compa.sort_values(['gvkey', 'fyear', 'at'], ascending=[True, True, False])
compa = compa.drop_duplicates(subset=['gvkey', 'fyear'], keep='first')

# change ipodate to date format
compa['ipodate'] = pd.to_datetime(compa['ipodate'])

# drop if at is missing
compa = compa.dropna(subset=['at'])

# drop if xint is missing or negative
compa = compa.dropna(subset=['xint'])
compa = compa[compa['xint'] > 0]

# missing values of ebitda (due to missing dp/oiabp)
compa['ebitda'] = compa['ebitda'].fillna(compa['ebit'] + compa['dp'])
# replace ebitda = pi + xint - idit + dp if ebitda is still missing
compa['ebitda'] = compa['ebitda'].fillna(compa['pi'] + compa['xint'] - compa['idit'] + compa['dp'])
# replace ebit = pi + xint - idit if ebit is still missing
compa['ebit'] = compa['ebit'].fillna(compa['pi'] + compa['xint'] - compa['idit'])

# drop if ebitda is missing
compa = compa.dropna(subset=['ebitda']) 

# Remove duplicate columns
compa = compa.loc[:, ~compa.columns.duplicated()]

compa['dclo'] = compa['dclo'].fillna(0)
compa['idit'] = compa['idit'].fillna(0)

# Debt
compa['debt'] = compa['dltt'] + compa['dlc'] - compa['dclo']

# Dividend_payer — 1 if dvc>0, 0 if dvc≤0, <NA> if dvc is missing
compa['dividend_payer'] = (compa['dvc'] > 0).astype('Int64')

# 1) “Financial deficit”: 1 if oancf – capx – dvc < 0, 0 otherwise, <NA> if any input missing
cond_deficit = (compa['oancf'] - compa['capx'] - compa['dvc']) < 0
compa['financial_deficit'] = cond_deficit.astype('Int64')

# 2) “Immediate depletion”: 1 if che + oancf – capx – dvc < 0, 0 otherwise, <NA> if any input missing
cond_immediate = (compa['che'] + compa['oancf'] - compa['capx'] - compa['dvc']) < 0
compa['immediate_depletion'] = cond_immediate.astype('Int64')

# Investment
compa['investment'] = compa['aqc'] + compa['capx'] + compa['xrd']

# Loss before interest expense
compa['loss_before_interest_expense'] = ((compa['pi'] + compa['idit']) < 0).astype('Int64')

# Market to book
compa['market_to_book'] = (compa['debt'] + compa['pstk'] + (compa['prcc_f'] * compa['csho'])) / compa['at']

# MNC (indicator = 1 if pifo or txfo not missing or zero)
cond_mnc = (compa['pifo'] != 0) | (compa['txfo'] != 0)
compa['mnc'] = cond_mnc.astype('Int64')

# Net interest
compa['net_interest'] = compa['xint'] - compa['idit']

# NOL 
compa['nol'] = (compa['tlcf'] > 0).astype('Int64')

# Sales growth
compa['sales_growth'] = (compa['sale'] - compa['sale'].shift(1)) / compa['sale'].shift(1)
compa['sales_growth'] = compa['sales_growth'].replace([np.inf, -np.inf], 0)
compa['sales_growth'] = compa['sales_growth'].clip(-1, 1)

# Z-score
compa['z_score'] = (3.3 * compa['pi'] + 1.0 * compa['sale'] + 1.4 * compa['re'] + 1.2 * (compa['act'] - compa['lct'])) / compa['at']

# Delta_DCF
compa['delta_dcf'] = compa['dltis'] - compa['dltr']

# CRSP 

# Define the variables to be imported
crsp_vars = ['cusip', 'permco', 'permno', 'date', 'ret', 'vol', 'shrout', 'prc']

# Define the query to get the annual returns of North American firms
crsp_query = f"""
    SELECT {', '.join(crsp_vars)}
    FROM crsp.msf
    WHERE date >= '{start_date}' AND date <= '{end_date}'
"""

# Execute the query and fetch the data
crspm = db.raw_sql(crsp_query, date_cols=['date'])

# Display the first few rows of the dataframe
print(crspm.head())

# header information from the CRSP file
crsp_hdr_query = """
    SELECT *
    FROM crsp.dsfhdr
"""

# Execute the query and fetch the data
crsp_hdr = db.raw_sql(crsp_hdr_query, date_cols=['date'])

# Display the first few rows of the dataframe
print(crsp_hdr.head())

# merge crspm and crsp_hdr with permno
crspm = crspm.merge(crsp_hdr[['permno', 'dlstcd']], on='permno', how='left')

# sort by permno date
crspm = crspm.sort_values(['permno', 'date'])

# Aggregate the data by permno and year and calculate the buy and hold return over the year as well as the volatility
crspm['year'] = crspm['date'].dt.year

# Display the first few rows of the dataframe
print(crspm.head())

std_ret = crspm.groupby(['permno', 'year'])['ret'].std().reset_index()
buy_and_hold_return = crspm.groupby(['permno', 'year'])['ret'].apply(lambda x: (1 + x).prod() - 1).reset_index()
# merge the buy and hold return and the volatility to the crspm dataframe
crspm = crspm.merge(buy_and_hold_return, on=['permno', 'year'], suffixes=('', '_buy_and_hold'))
crspm = crspm.merge(std_ret, on=['permno', 'year'], suffixes=('', '_vol'))

# aggregate to permno and year level (keep ret_buy_and_hold and ret_vol and dlstcd)
crspa = crspm.groupby(['permno', 'year']).agg({
    'ret_buy_and_hold': 'first',
    'ret_vol': 'first',
    'dlstcd': 'first'
}).reset_index()

# Compustat/CRSP Link Table
ccm_query = """
    SELECT gvkey, lpermno, linktype, linkprim, linkdt, linkenddt
    FROM crsp.ccmxpf_linktable
"""

# Execute the query and fetch the data
ccm = db.raw_sql(ccm_query, date_cols=['linkdt', 'linkenddt'])

# Display the first few rows of the dataframe
print(ccm.head())

# merge crspa and ccm
crspac = crspa.merge(ccm, left_on='permno', right_on='lpermno', how='left')

# keep only the rows where the link date is before the year and the link end date is after the year
# change linkenddt to 2024-12-31 if it is NaT
crspac['linkenddt'] = crspac['linkenddt'].fillna(pd.Timestamp('2024-12-31'))
crspac = crspac[(crspac['year'] >= crspac['linkdt'].dt.year) & (crspac['year'] <= crspac['linkenddt'].dt.year)]

# merge crspac with compa on gvkey (keep everything)
comp_crspa_merged = compa.merge(crspac, left_on=['gvkey', 'fyear'], right_on=['gvkey', 'year'], how='inner')
# drop year
comp_crspa_merged = comp_crspa_merged.drop(columns='year')

# change gvkey to int
comp_crspa_merged['gvkey'] = comp_crspa_merged['gvkey'].astype(int)

# for each gvkey fyear, sort by ret_buy_and_hold and ret_vol and keep the first one 
comp_crspa_merged = comp_crspa_merged.sort_values(['gvkey', 'fyear', 'ret_buy_and_hold', 'ret_vol'], ascending=[True, True, False, False])
comp_crspa_merged = comp_crspa_merged.drop_duplicates(subset=['gvkey', 'fyear'], keep='first')

# output csv. format
comp_crspa_merged.to_csv("../Data/Cleaned/comp_crspa_merged.csv", index=False)