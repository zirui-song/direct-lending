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

### Clean the PitchBook Data ###
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
  distinct(Lenders, Company, Ticker, `Deal Size`, `Issue Date`, .keep_all = TRUE)
write.csv(deals, "../Data/Cleaned/PitchBook_Cleaned.csv", row.names = FALSE)

### Import and Analyze preliminary sample of 100 contracts ###
# Read the dataset in the current folder (Excel)
test_sample <- read_excel("../Data/Cleaned/Pitchbook_Cleaned_Aug7.xlsx", col_names = TRUE)

# change all variables besides the first 3 to be categorical (factor) variables that take 1 as long as the value is not NA
test_sample <- test_sample %>%
  mutate(across(-c(1:3), ~ ifelse(is.na(.), 0, 1)))

# generate Hard_N and Soft_N and Info_N that is the sum of monthly financial and budget forecast, sum of lender meetings and inspection, and sum of all respectively
test_sample <- test_sample %>%
  mutate(Hard_N = rowSums(select(test_sample, `Monthly Financial`:`Budget/Forecast`)),
         Soft_N = rowSums(select(test_sample, `Lender Meetings`:`Inspection`)),
         Info_N = rowSums(select(test_sample, `Monthly Financial`:`Inspection`)))

# generate the summary statistics for the sample 
summary(test_sample)
# what's the correlation between the last 4 variables
cor(test_sample[, 4:7])

# number of zeros in each variable
colSums(test_sample == 0)
