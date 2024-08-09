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

# Read the dataset in the current folder (Excel)
data <- read_excel("../Data/Raw/PitchBook_All_Columns_DirectLending_2024_07_10.xlsx", skip = 8, col_names = TRUE)

# Split the Companies column into two columns based on the last ( and remove the last )
data <- data %>%
  separate(Companies, into = c("Company", "Company2"), sep = "\\(", remove = TRUE) %>%
  mutate(Company2 = str_remove(Company2, "\\)"))
# Order the dataset by Companies Company Company2
data <- data %>%
  select(Company, Company2, everything())
# In the column Company2, keep only the characters after : and remove the rest
data <- data %>%
  mutate(Company2 = str_remove(Company2, ".*:"))
# trim white spaces and then keep only the entries with less than or equal to 4 characters in Company2
data <- data %>%
  mutate(Company2 = str_trim(Company2)) %>%
  filter(nchar(Company2) <= 4)
# rename Company2 as Ticker
names(data)[2] <- "Ticker"
# Move variable Issue Date to the front
data <- data %>%
  select("Issue Date", everything())
# change issue date (numeric) to date format from Excel numbers using as.Date and first convert NA to 0
data <- data %>%
  mutate(`Issue Date` = ifelse(is.na(`Issue Date`), 0, `Issue Date`)) %>%
  mutate(`Issue Date` = as.Date(`Issue Date`, origin = "1899-12-30"))
# order Lenders, Net Income, and EBITDA to the front 
data <- data %>%
  select(Lenders, `Net Income`, EBITDA, `Deal Type 1`, everything())

# keep only US companies (those with company country/territory/region as United States)
data <- data %>%
  filter(`Company Country/Territory/Region` == "United States")

# keep only the unique borrower name, ticker, and date of issue
deals <- data %>%
  distinct(Lenders, Company, Ticker, `Issue Date`)
write.csv(deals, "../Data/Cleaned/PitchBook_Cleaned.csv", row.names = FALSE)





