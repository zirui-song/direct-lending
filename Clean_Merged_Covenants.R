library(data.table)
library(dplyr)
library(haven)
library(stringr)
library(mvtnorm)
library(tidyr)
library(readxl)
library(ggplot2)

options(scipen = 999)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list = ls())

##################################################
  # Section 0: Clean combined_loancontracts and df_merged_crsp
##################################################
#df_merged_crsp <- fread("../Data/LoansFull/df_merged_crsp.csv")
# check if accession if unique by accession and type_attachment
#df_merged_clean <- df_merged_crsp %>%
#  group_by(accession, type_filing, type_attachment) %>%
#  filter(n() == 1)

#combined_loancontracts <- fread("../Data/LoansFull/combined_loancontracts.csv")
# check if obs is unique by accession and type_attachment
#combined_loancontracts_clean <- combined_loancontracts %>%
#  group_by(accession, type_filing, type_attachment) %>%
#  filter(n() == 1)

#cleaned_loancontracts <- inner_join(combined_loancontracts_clean, df_merged_clean, by = c("accession", "type_filing", "type_attachment"))
# save as csv
#write.csv(cleaned_loancontracts, "../Data/LoansFull/cleaned_loancontracts.csv", row.names = FALSE)

##################################################
  # Section 1: Data Cleaning (Agreements + SEC filing mapping)
##################################################

# Load credit agreements with information covenants
info_cov_combined_loancontracts <- fread("../Data/LoansFull/combined_loancontracts_info_cov.csv")
# check if there are duplicated rows
info_cov_combined_loancontracts <- info_cov_combined_loancontracts %>%
  distinct()
# keep only the needed columns
info_cov_loancontracts_clean <- info_cov_combined_loancontracts %>%
  select(accession, monthly_fs, projected_fs, lender_meeting)
# keep only the maximum value of the three variables for each filename 
info_cov_loancontracts_clean <- info_cov_loancontracts_clean %>%
  group_by(accession) %>%
  summarise(monthly_fs = max(monthly_fs), projected_fs = max(projected_fs), lender_meeting = max(lender_meeting))

# merge back to all agreements with lender names extracted as valid from ChatGPT
agreements <- fread("../Data/LoansFull/loancontracts_with_extracted_lendernames.csv")
agreements <- agreements %>%
  left_join(info_cov_loancontracts_clean, by = "accession")
# fill NA with 0
agreements <- agreements %>%
  mutate(monthly_fs = ifelse(is.na(monthly_fs), 0, monthly_fs),
         projected_fs = ifelse(is.na(projected_fs), 0, projected_fs),
         lender_meeting = ifelse(is.na(lender_meeting), 0, lender_meeting))

# keep only needed variables 
agreements <- agreements %>%
  select(accession, type_filing, type_attachment, filename.x, date.x, gvkey, cik, lpermno, coname, total_assets, borrower_name, lender_name, monthly_fs, projected_fs, lender_meeting)
# rename filename.x and date.x to filename and date
colnames(agreements) <- c("accession", "type_filing", "type_attachment", "filename", "date", "gvkey", "cik", "lpermno", "coname", "total_assets",
                          "borrower_name", "lender_name", "monthly_fs", "projected_fs", "lender_meeting")

##################################################
# Section 2: Clean Lender Names
##################################################

# drop those that don't have a lender name in column lender or contain string "Not Found"

# change the lender_name to lower case
agreements <- agreements %>%
  mutate(lender_name = tolower(lender_name))
agreements <- agreements %>%
  filter(!is.na(lender_name) & !str_detect(lender_name, "not found"))
# rename lender and borrower names to be more informative 
agreements <- agreements %>%
  rename(lender_chatgpt = lender_name, borrower_chatgpt = borrower_name)
# generate lead_arranger as the first string before the first comma
agreements <- agreements %>%
  mutate(lead_arranger = str_extract(lender_chatgpt, "^[^,]+"))

# manual check for lead_arranger names 
# change banc to bank 
agreements <- agreements %>%
  mutate(lead_arranger = str_replace(lead_arranger, "banc", "bank")) %>%
  mutate(lead_arranger = str_replace(lead_arranger, "banque", "bank"))
  
# get a list of banks 
non_regulated_ib_fcs <- c("brown brothers harriman & co.", "calyon", "cantor fitzgerald securities", "chase securities", 
                          "federal home loan mortgage corporation", "jefferies", "salomon smith barney", "swk", 
                          "td securities", "the cit group/business credit", "wachovia", "wilmington")
banks <- c("barclays", "bayerische", "bbva", "bear", "bmo", "bnp paribas", "bofa", "capital one", "cibc", "citicorp", "citigroup",
"citizens", "credit lyonnais", "credit suisse", "goldman", "hsbc", "j.p. morgan", "jpmorgan", "jp morgan", "lehman brothers", 
"merrill", "morgan stanley", "pnc", "rbc", "rbs", "societe generale", "suntrust", "toronto dominion", "ubs", "wells fargo", "société générale")
# generate a variable that indicates whether the lender is a bank if "bank" appears in the name or strings in banks above appear in the name
# Collapse the bank names into a single pattern
bank_pattern <- str_c(banks, collapse = "|")

# Generate a variable that indicates whether the lender is a bank
agreements <- agreements %>%
  mutate(lender_is_bank = ifelse(str_detect(lead_arranger, "bank")
                                 | str_detect(lead_arranger, regex(bank_pattern, ignore_case = TRUE)), 1, 0))

### Time Series Plots of Use of Covenants
# generate year from date
agreements <- agreements %>%
  mutate(year = year(date),
         quarter = quarter(date))

# time series plots of the number of credit agreements by year by lender_is_bank
agreements %>%
  group_by(year, lender_is_bank) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n, color = factor(lender_is_bank))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Number of Credit Agreements by Year",
       x = "Year",
       y = "Number of Credit Agreements",
       color = "Lender is Bank") +
  theme(legend.position = c(0.1, 0.8),    # Position legend inside the plot
        legend.background = element_blank(),  # Remove legend background fill
        legend.box.background = element_blank(),  # Remove legend border color
        legend.text = element_text(size = 6),  # Smaller text inside the legend
        legend.title = element_text(size = 6),  # Smaller title inside the legend
        legend.key.size = unit(0.5, "cm"),  # Smaller keys in the legend
        legend.spacing.y = unit(0.2, "cm"),  # Smaller vertical spacing
        legend.spacing.x = unit(0.2, "cm")) +  # Smaller horizontal spacing
  guides(color = guide_legend(title = "Lender is Bank"))
# save figure as pdf
ggsave("../Results/Figures/Agreements_by_year.pdf")

# time series plots of the three variables (monthly_fs, projected_fs, lender_meeting) by year for banks and nonbanks on the same plot
agreements %>%
  group_by(year, lender_is_bank) %>%
  summarise(monthly_fs = mean(monthly_fs), projected_fs = mean(projected_fs), lender_meeting = mean(lender_meeting)) %>%
  gather(key = "variable", value = "value", -year, -lender_is_bank) %>%
  ggplot(aes(x = year, y = value, color = variable, linetype = factor(lender_is_bank))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Monthly FS, Projected FS, and Lender Meeting by Year",
       x = "Year",
       y = "Frequency",
       color = "Variable",
       linetype = "Lender is Bank") +
  theme(legend.position = c(0.1, 0.8),    # Position legend inside the plot
        legend.background = element_blank(),  # Remove legend background fill
        legend.box.background = element_blank(),  # Remove legend border color
        legend.text = element_text(size = 6),  # Smaller text inside the legend
        legend.title = element_text(size = 6),  # Smaller title inside the legend
        legend.key.size = unit(0.5, "cm"),  # Smaller keys in the legend
        legend.spacing.y = unit(0.2, "cm"),  # Smaller vertical spacing
        legend.spacing.x = unit(0.2, "cm")) +  # Smaller horizontal spacing
  guides(color = guide_legend(title = "Variable"), 
         linetype = guide_legend(title = "Lender is Bank"))
# save figure as pdf 
ggsave("../Results/Figures/Info_Cov_94to23_full.pdf")

##################################################
# Section 3: Merge with Compustat Quarterly (Annual) Data
##################################################

# import compustat quarterly data
compq <- fread("../Data/Raw/compustat_quarterly.csv")
# use slice(1) to keep only the first row of each gvkey-year pair
compq <- compq %>%
  group_by(gvkey, fyearq, fqtr) %>%
  slice(1)
# merge with filing data
agreements_compq_merged <- agreements %>%
  left_join(compq %>% select(gvkey, fyearq, fqtr, atq, revtq, niq, ibq, ltq, xrdq, ppegtq), by = c("gvkey" = "gvkey", "year" = "fyearq", "quarter" = "fqtr"))
# calculate ROA
agreements_compq_merged <- agreements_compq_merged %>%
  mutate(roa = ibq / atq)
# calculate leverage
agreements_compq_merged <- agreements_compq_merged %>%
  mutate(leverage = ltq / atq)
# replace xrd ppegt to be divided by at
agreements_compq_merged <- agreements_compq_merged %>%
  mutate(xrdq = xrdq / atq,
         ppegtq = ppegtq / atq)

# keep only those firms with revenue between 10,000,000 and 1,000,000,000
agreements_mm <- agreements_compq_merged %>%
  filter(revtq >= 10 & revtq <= 1000)

# time series plots of the number of credit agreements by year by lender_is_bank
agreements_mm %>%
  group_by(year, lender_is_bank) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n, color = factor(lender_is_bank))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Number of Credit Agreements by Year",
       x = "Year",
       y = "Number of Credit Agreements",
       color = "Lender is Bank") +
  theme(legend.position = c(0.1, 0.8),    # Position legend inside the plot
        legend.background = element_blank(),  # Remove legend background fill
        legend.box.background = element_blank(),  # Remove legend border color
        legend.text = element_text(size = 6),  # Smaller text inside the legend
        legend.title = element_text(size = 6),  # Smaller title inside the legend
        legend.key.size = unit(0.5, "cm"),  # Smaller keys in the legend
        legend.spacing.y = unit(0.2, "cm"),  # Smaller vertical spacing
        legend.spacing.x = unit(0.2, "cm")) +  # Smaller horizontal spacing
  guides(color = guide_legend(title = "Lender is Bank"))
# save figure as pdf
ggsave("../Results/Figures/Agreements_by_year_mm.pdf")


# plot the frequencies of the three variables (monthly_fs, projected_fs, lender_meeting) by year 
agreements_mm %>%
  group_by(year, lender_is_bank) %>%
  summarise(monthly_fs = mean(monthly_fs), projected_fs = mean(projected_fs), lender_meeting = mean(lender_meeting)) %>%
  gather(key = "variable", value = "value", -year, -lender_is_bank) %>%
  ggplot(aes(x = year, y = value, color = variable, linetype = factor(lender_is_bank))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Monthly FS, Projected FS, and Lender Meeting by Year",
       x = "Year",
       y = "Frequency",
       color = "Variable",
       linetype = "Lender is Bank") +
  theme(legend.position = c(0.1, 0.8),    # Position legend inside the plot
        legend.background = element_blank(),  # Remove legend background fill
        legend.box.background = element_blank(),  # Remove legend border color
        legend.text = element_text(size = 6),  # Smaller text inside the legend
        legend.title = element_text(size = 6),  # Smaller title inside the legend
        legend.key.size = unit(0.5, "cm"),  # Smaller keys in the legend
        legend.spacing.y = unit(0.2, "cm"),  # Smaller vertical spacing
        legend.spacing.x = unit(0.2, "cm")) +  # Smaller horizontal spacing
  guides(color = guide_legend(title = "Variable"), 
         linetype = guide_legend(title = "Lender is Bank"))
# save figure as pdf 
ggsave("../Results/Figures/Info_Cov_94to23_mm.pdf")

### inner join agreements_mm with combined_loancontracts to get text so that I can use to extract deal information
combined_loancontracts <- fread("../Data/LoansFull/combined_loancontracts.csv")
combined_loancontracts_mm <- combined_loancontracts %>%
  inner_join(agreements_mm %>% select(accession, type_filing, type_attachment), by = c("accession", "type_filing", "type_attachment"))

# save as csv
write.csv(combined_loancontracts_mm, "../Data/LoansFull/combined_loancontracts_mm.csv", row.names = FALSE)

##################################################
# Section 3: Table 1/2 (Summary Statistics) and Main Regression
##################################################

### Table 1: Summary Statistics of full sample of 3446 loan contracts from 2010 to 2024 by lender_is_bank
# summary table of filing_data_filtered
# make sure filing_data_filtered is a df
filing_data_filtered <- as.data.frame(filing_data_filtered)
variable_labels <- c("monthly_fs" = "Monthly FS",
                     "projected_fs" = "Projected FS",
                     "lender_meeting" = "Lender Meeting")
variable_labels_firm <- c("Size", "ROA", "Leverage", "Revenue")

stargazer(filing_data_filtered[, c("monthly_fs","projected_fs","lender_meeting")], 
          type = "text", summary.stat = c("mean", "min", "p25", "median", "p75", "max"), 
          digits = 2, covariate.labels = variable_labels, title = "Summary", out = "../Results/Table1_PanelA.txt")

stargazer(filing_data_filtered[, c("atq","roa","leverage", "revtq")],
          type = "text", summary.stat = c("mean", "min", "p25", "median", "p75", "max"), 
          digits = 2, covariate.labels = variable_labels_firm, title = "Summary", out = "../Results/Table1_PanelB.txt")

#stargazer(filing_data_filtered[, c("xrdq","ppegtq")],
#          type = "text", summary.stat = c("mean", "min", "p25", "median", "p75", "max"), 
#          digits = 2, title = "Summary", out = "../Results/Table1_PanelB.txt")

### Merge with Dealscan 
dealscan_compq_matched <- read_dta("../Data/Cleaned/dealscan_compustat_matched.dta")
# keep only needed variables
dealscan_compq_matched <- dealscan_compq_matched %>%
  select(gvkey, year, quarter, lead_arranger, deal_amount, deal_amount_converted, 
         deal_purpose, tranche_active_date, tranche_maturity_date, seniority_type, 
         secured, covenants, base_reference_rate, margin_bps, all_base_rate_spread_margin,
         base_rate_margin_bps, floor_bps)

# merge with filing data
filing_data_merged <- filing_data_filtered %>%
  inner_join(dealscan_compq_matched, by = c("gvkey" = "gvkey", "year" = "year", "quarter" = "quarter"))

### Merge with Pitchbook (Nonbanks) ###
pitchbook <- fread("../Data/Cleaned/PitchBook_Cleaned.csv")
pitchbook <- pitchbook %>%
  select("Issue Date", "Lenders", "Company", "Ticker", "Deal Size", "Deal Type 1", "Spread/Interest Rate")
# generate year and quarter from filing date
pitchbook <- pitchbook %>%
  mutate(year = year(`Issue Date`), quarter = quarter(`Issue Date`))

# Output list of loans by nonbank lenders (generate the list to use in chatgpt so that we have deal information about them)
# in a first pass regression
nonbank_loan <- filing_data_filtered %>%
  filter(lender_is_bank == 0)

# merge with combined_loan
combined_loancontracts <- fread("../Data/LoansFull/combined_loancontracts.csv")
nonbank_contracts_mm <- combined_loancontracts %>%
  inner_join(nonbank_loan, by = c("accession" = "file_name"))
nonbank_contracts_mm <- nonbank_contracts_mm %>%
  select(accession, text)
# save as csv
write.csv(nonbank_contracts_mm, "../Data/Intermediate/nonbank_contracts_mm.csv", row.names = FALSE)
# re-read the extracted contracts
nonbank_contracts_extracted <- fread("../Data/Intermediate/updated_data_with_extracted_info.csv")
nonbank_contracts_extracted <- nonbank_contracts_extracted %>%
  select(accession, deal_amount, interest_spread, maturity)
### clean the variables 
  # keep only the number after $
  nonbank_contracts_extracted$deal_amount <- as.numeric(str_extract(nonbank_contracts_extracted$deal_amount, "\\d+"))
  # keep the number (including numbers with decimal points) after "LIBOR + "
  nonbank_contracts_extracted$interest_spread1 <- as.numeric(str_extract(nonbank_contracts_extracted$interest_spread, "\\d+\\.?\\d*"))
  # generate a variable that indicates whether the interest rate is fixed or floating (LIBOR, base, prime, etc.)
  nonbank_contracts_extracted$interest_type <- ifelse(str_detect(nonbank_contracts_extracted$interest_spread, "LIBOR|ABR|Prime|Base"), "floating", "fixed")
  # generate a date variable for maturity (format is November 15, 2013)
  nonbank_contracts_extracted$maturity1 <- as.Date(nonbank_contracts_extracted$maturity, format = "%B %d, %Y")
  # drop if any of the above variables are NA
  nonbank_contracts_extracted <- nonbank_contracts_extracted %>%
    filter(!is.na(deal_amount) & !is.na(interest_spread1) & !is.na(maturity1))
  
### merge back to nonbank_loan
nonbank_loan_with_deal_info <- nonbank_loan %>%
  inner_join(nonbank_contracts_extracted, by = c("file_name" = "accession"))

### merge with filing_data_merged
all_merged_deals <- bind_rows(filing_data_merged, nonbank_loan_with_deal_info)
# format tranche dates as date format
all_merged_deals <- all_merged_deals %>%
  mutate(tranche_active_date = as.Date(tranche_active_date, format = "%Y-%m-%d"),
         tranche_maturity_date = as.Date(tranche_maturity_date, format = "%Y-%m-%d"))

# replace tranche active date with fdate if it's NA
all_merged_deals <- all_merged_deals %>%
  mutate(tranche_active_date = ifelse(is.na(tranche_active_date), fdate, tranche_active_date))
# replace tranche maturity date with maturity1 if it's NA
all_merged_deals <- all_merged_deals %>%
  mutate(tranche_maturity_date = ifelse(is.na(tranche_maturity_date), maturity1, tranche_maturity_date))
# replace margin_bps with interest_spread1*100 if it's NA
all_merged_deals <- all_merged_deals %>%
  mutate(margin_bps = ifelse(is.na(margin_bps), interest_spread1*100, margin_bps))
# generate maturity = tranche_maturity_date - tranche_active_date
all_merged_deals <- all_merged_deals %>%
  mutate(maturity =  as.numeric(tranche_maturity_date - tranche_active_date) / 365)

# keep only variables useful for analysis 
all_merged_deals <- all_merged_deals %>%
  select(fdate, gvkey, tic, coname, lender_chatgpt, lender_is_bank, year, quarter, monthly_fs, projected_fs, lender_meeting, 
         atq, revtq, niq, ibq, ltq, xrdq, ppegtq, roa, leverage, lead_arranger, deal_amount, 
         tranche_active_date, tranche_maturity_date, maturity, margin_bps)

# save in Cleaned Data folder
write.csv(all_merged_deals, "../Data/Cleaned/all_merged_deals.csv", row.names = FALSE)

all_merged_deals %>%
  group_by(year, lender_is_bank) %>%
  summarise(monthly_fs = mean(monthly_fs), projected_fs = mean(projected_fs), lender_meeting = mean(lender_meeting)) %>%
  gather(key = "variable", value = "value", -year, -lender_is_bank) %>%
  ggplot(aes(x = year, y = value, color = variable, linetype = factor(lender_is_bank))) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Monthly FS, Projected FS, and Lender Meeting by Year",
       x = "Year",
       y = "Frequency",
       color = "Variable",
       linetype = "Lender is Bank") +
  theme(legend.position = c(0.1, 0.8),    # Position legend inside the plot
        legend.background = element_blank(),  # Remove legend background fill
        legend.box.background = element_blank(),  # Remove legend border color
        legend.text = element_text(size = 4),  # Smaller text inside the legend
        legend.title = element_text(size = 4),  # Smaller title inside the legend
        legend.key.size = unit(0.5, "cm"),  # Smaller keys in the legend
        legend.spacing.y = unit(0.2, "cm"),  # Smaller vertical spacing
        legend.spacing.x = unit(0.2, "cm")) +  # Smaller horizontal spacing
  guides(color = guide_legend(title = "Variable"), 
         linetype = guide_legend(title = "Lender is Bank"))
# save figure as pdf 
ggsave("../Results/Figure1_all_merged.pdf")

