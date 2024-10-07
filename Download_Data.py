import wrds
import pandas as pd
import os
import sys

# Get the directory of the current script
script_dir = os.path.dirname(os.path.abspath(__file__))

# Set the working directory to the script's directory
os.chdir(script_dir)

# Now the working directory is set to the script's directory
print("Current Working Directory:", os.getcwd())

# Connect to WRDS
db = wrds.Connection(wrds_username='zrsong')

# Define the start and end dates
start_date = '1994-01-01'
end_date = '2024-06-30'

'''
# Use S&P ratings data as Compustat ratings data were discontinued in 2017
# Query the S&P ratings data
query = f"""
    SELECT gvkey, ratingdate, ratingsymbol
    FROM ciq_ratings.wrds_erating 
    WHERE ratingdate >= '{start_date}' AND ratingdate <= '{end_date}'
"""

# Execute the query and fetch the data
ratings_date = db.raw_sql(query)

# Save the data to a CSV file
ratings_date.to_csv('../Data/ratings_data.csv', index=False)
'''

# obtain Dealscan legacy lenders data (to use 2018 JF bank name link)
query = f"""
    SELECT *
    FROM tr_dealscan.lendershares
"""
lendershares = db.raw_sql(query)

# output csv. format
lendershares.to_csv("../Data/Raw/lendershares.csv", index=False)

# Annual Compustat data
fund_table = 'funda'

# use the varlist above to query quarterly compustat data (don't use join(varlist) as it will return a string)
query = f"""
    SELECT *
    FROM compa.{fund_table}
    WHERE datadate >= '{start_date}' AND datadate <= '{end_date}'
"""

compa = db.raw_sql(query)

# output csv. format
compa.to_csv("../Data/Raw/compustat_annual.csv", index=False)

# Quarterly Compustat data
fund_table = 'fundq'

varlist = ['conm', 'tic', 'cusip','fyearq', 'fqtr', 'fyr', 'atq','capxy', 'ceqq', 'cogsq', 
           'cshoq', 'dlcq', 'dlcchy','dlttq', 'dpq', 'ibq', 'itccy', 'fic',
           'ltq', 'mibq', 'niq', 'prstkccy', 'pstkq', 'req', 'revtq', 'saleq',
           'seqq', 'txdbq', 'txdiq', 'txditcq', 'wcapchy', 'xinty', 'xrdq', 'xsgaq',
           'mkvaltq', 'epspxq', 'epsfxq', 'ajexq', 'prccq', 'oancfy', 'ivncfy', 'rdq']
# use the varlist above to query quarterly compustat data (don't use join(varlist) as it will return a string)
query = f"""
    SELECT *
    FROM compa.{fund_table}
    WHERE datadate >= '{start_date}' AND datadate <= '{end_date}'
"""

compq = db.raw_sql(query)

# output csv. format
compq.to_csv("../Data/Raw/compustat_quarterly.csv", index=False)


# Query the dealscan syndicated loan data
query = f"""
    SELECT *
    FROM tr_dealscan.dealscan
    WHERE deal_active_date >= '{start_date}' AND deal_active_date <= '{end_date}'
"""

# Execute the query and fetch the data
dealscan_data = db.raw_sql(query)

# Save the data to a CSV file
dealscan_data.to_csv('../Data/Raw/dealscan_data.csv', index=False)

# Covenant levels are then merged with accounting data available from Compustat using a link file provided by Michael Roberts and Sudheer Chava (as used in Chava and Roberts (2008))
# Query the link file


# Disconnect from WRDS
db.close()




