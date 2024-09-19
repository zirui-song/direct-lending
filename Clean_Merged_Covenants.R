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
  select(fdate, cik, gvkey, monthly_fs, projected_fs, lender_meeting)
filing_data_matched <- filing_data_matched %>% 
  mutate(year = year(fdate), quarter = quarter(fdate))

# time series plots of the three variables (monthly_fs, projected_fs, lender_meeting) by year
filing_data_matched %>%
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

# Merge with compq data
compq <- fread("../Data/Raw/compustat_quarterly.csv")
# make sure that gvkey-fyearq-fqtr is unique
compq <- compq %>%
  group_by(gvkey, fyearq, fqtr) %>%
  filter(n() == 1)

filing_data_matched <- filing_data_matched %>%
  left_join(compq, by = c("gvkey" = "gvkey", "year" = "fyearq", "quarter" = "fqtr"))

# import dealscan