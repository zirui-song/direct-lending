library(data.table)
library(dplyr)
library(haven)
library(stringr)
library(mvtnorm)
library(tidyr)
library(readxl)

options(scipen = 999)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list = ls())

# Load data
filing_data_matched <- fread("../Data/Intermediate/filing_data_matched.csv", col.names = tolower)
# generate year and quarter from filing date
filing_data_matched <- filing_data_matched %>% 
  select(filename, fdate, cik, gvkey, monthly_fs, projected_fs, lender_meeting)
filing_data_matched <- filing_data_matched %>% 
  mutate(year = year(fdate), quarter = quarter(fdate))
# drop if NA in gvkey
filing_data_matched <- filing_data_matched %>% 
  filter(!is.na(gvkey))

# time series plots of the three variables (monthly_fs, projected_fs, lender_meeting) by year

filing_data_matched %>%
  group_by(year) %>%
  summarise(monthly_fs = sum(monthly_fs), projected_fs = sum(projected_fs), lender_meeting = sum(lender_meeting)) %>%
  gather(key = "variable", value = "value", -year) %>%
  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Monthly FS, Projected FS, and Lender Meeting by Year",
       x = "Year",
       y = "Count") +
  theme(legend.position = "bottom")

# time series plots of number of observations by year
filing_data_matched %>%
  group_by(year) %>%
  summarise(n_obs = n()) %>%
  ggplot(aes(x = year, y = n_obs)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Number of Observations by Year",
       x = "Year",
       y = "Count") +
  theme(legend.position = "bottom")


# import compustat quarterly data
compq <- fread("../Data/Raw/compustat_quarterly.csv")
# use slice(1) to keep only the first row of each gvkey-year pair
compq <- compq %>%
  group_by(gvkey, fyearq, fqtr) %>%
  slice(1)
# merge with filing data
filing_data_matched <- filing_data_matched %>%
  left_join(compq, by = c("gvkey" = "gvkey", "year" = "fyearq", "quarter" = "fqtr"))
# keep only needed variables
filing_data_filtered <- filing_data_matched %>%
  select(fdate, filename, gvkey, fdate, year, quarter, monthly_fs, projected_fs, lender_meeting, atq, revtq, niq, ibq, ltq, xrdq, ppegtq)
# calculate ROA
filing_data_filtered <- filing_data_filtered %>%
  mutate(roa = ibq / atq)
# calculate leverage
filing_data_filtered <- filing_data_filtered %>%
  mutate(leverage = ltq / atq)
# replace xrd ppegt to be divided by at
filing_data_filtered <- filing_data_filtered %>%
  mutate(xrdq = xrdq / atq,
         ppegtq = ppegtq / atq)

# keep only those firms with revenue between 10,000,000 and 1,000,000,000
filing_data_filtered <- filing_data_filtered %>%
  filter(revtq >= 10 & revtq <= 1000)

# make the three plots as above but using this filtered data
filing_data_filtered %>%
  group_by(year) %>%
  summarise(monthly_fs = mean(monthly_fs), projected_fs = mean(projected_fs), lender_meeting = mean(lender_meeting)) %>%
  gather(key = "variable", value = "value", -year) %>%
  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Monthly FS, Projected FS, and Lender Meeting by Year",
       x = "Year",
       y = "Count") +
  theme(legend.position = "bottom")

filing_data_filtered %>%
  group_by(year) %>%
  summarise(n_obs = n()) %>%
  ggplot(aes(x = year, y = n_obs)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Number of Observations by Year",
       x = "Year",
       y = "Count") +
  theme(legend.position = "bottom")

# import dealscan
dealscan_compq_matched <- read_dta("../Data/Cleaned/dealscan_compustat_matched.dta")
# keep only needed variables
dealscan_compq_matched <- dealscan_compq_matched %>%
  select(gvkey, year, quarter)

# merge with filing data
filing_data_merged <- filing_data_filtered %>%
  inner_join(dealscan_compq_matched, by = c("gvkey" = "gvkey", "year" = "year", "quarter" = "quarter"))

# plot the frequencies of the three variables (monthly_fs, projected_fs, lender_meeting) by year
filing_data_merged %>%
  group_by(year) %>%
  summarise(monthly_fs = mean(monthly_fs), projected_fs = mean(projected_fs), lender_meeting = mean(lender_meeting)) %>%
  gather(key = "variable", value = "value", -year) %>%
  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Time Series of Monthly FS, Projected FS, and Lender Meeting by Year",
       x = "Year",
       y = "Count") +
  theme(legend.position = "bottom")

# summary table of filing_data_merged 
# make sure filing_data_merged is a df
filing_data_merged <- as.data.frame(filing_data_merged)
variable_labels <- c("monthly_fs" = "Monthly FS",
                     "projected_fs" = "Projected FS",
                     "lender_meeting" = "Lender Meeting")
variable_labels_firm <- c("Size", "ROA", "Leverage", "Revenue", "R&D", "Tangibility")

stargazer(filing_data_merged[, c("monthly_fs","projected_fs","lender_meeting")], 
          type = "text", summary.stat = c("mean", "min", "p25", "median", "p75", "max"), 
          digits = 2, covariate.labels = variable_labels, title = "Summary", out = "../Results/test.txt")

stargazer(filing_data_merged[, c("atq","roa","leverage","revtq")], 
          type = "text", summary.stat = c("mean", "min", "p25", "median", "p75", "max"), 
          digits = 2, title = "Summary", out = "../Results/test.txt")

stargazer(filing_data_merged[, c("xrdq","ppegtq")], 
          type = "text", summary.stat = c("mean", "min", "p25", "median", "p75", "max"), 
          digits = 2, title = "Summary", out = "../Results/test.txt")


stargazer(filing_data_filtered[, c("monthly_fs","projected_fs","lender_meeting")], 
          type = "text", summary.stat = c("mean", "min", "p25", "median", "p75", "max"), 
          digits = 2, covariate.labels = variable_labels, title = "Summary", out = "../Results/test.txt")

stargazer(filing_data_filtered[, c("atq","roa","leverage","revtq")], 
          type = "text", summary.stat = c("mean", "min", "p25", "median", "p75", "max"), 
          digits = 2, title = "Summary", out = "../Results/test.txt")

stargazer(filing_data_filtered[, c("xrdq","ppegtq")], 
          type = "text", summary.stat = c("mean", "min", "p25", "median", "p75", "max"), 
          digits = 2, title = "Summary", out = "../Results/test.txt")