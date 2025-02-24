library(data.table)
library(dplyr)
library(haven)
library(stringr)
library(mvtnorm)
library(tidyr)
library(readxl)
library(ggplot2)
library(fuzzyjoin)
library(stringdist)
library(zoo)
library(readxl)

options(scipen = 999)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list = ls())

# Define the base figure path
figure_path <- "/Users/zrsong/MIT Dropbox/Zirui Song/Apps/Overleaf/Information Covenants of Nonbank Direct Lending/Figures"

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

### inner join agreements_mm with combined_loancontracts to get text so that I can use to extract deal information
#cleaned_loancontracts <- fread("../Data/LoansFull/cleaned_loancontracts.csv")
# check if contracts have amendments (include words such as 'amendment', 'amended', 'amends', 'restatement', 'restated', 'restates') in the first 1000 characters
#cleaned_loancontracts <- cleaned_loancontracts %>%
#  mutate(is_amendment = ifelse(str_detect(str_sub(text, 1, 1000), "amendment|amended|amends|restatement|restated|restates"), 1, 0))
#cleaned_loancontracts_mm <- cleaned_loancontracts %>%
#  inner_join(agreements_mm %>% select(accession, type_filing, type_attachment), by = c("accession", "type_filing", "type_attachment"))
# keep only non-amendment contracts
#cleaned_loancontracts_mm <- cleaned_loancontracts_mm %>%
#  filter(is_amendment == 0)
# save as csv
#write.csv(cleaned_loancontracts_mm, "../Data/LoansFull/combined_loancontracts_mm.csv", row.names = FALSE)
# read in cleaned_loancontracts_mm

#cleaned_loancontracts_mm <- fread("../Data/LoansFull/combined_loancontracts_mm.csv")
# check for unique years and number of observations by year
# Count number of observations by year
#cleaned_loancontracts_mm <- cleaned_loancontracts_mm %>%
#  group_by(year) %>%
#  summarise(count = n())

##################################################
  # Section 0.1: Clean Preqin Fund Names a
##################################################

pattern <- "\\s*(\\b(?:llc|lp|ag|limited|plc|iv|co|gp|spv|inc)\\b)$"

preqin <- read_excel("../Data/Raw/Preqin_deals_export-10-23-2024-1857.xlsx")
names(preqin) <- tolower(names(preqin))

# keep only the needed columns
preqin_names <- preqin[, c("portfolio company", "portfolio company city", "portfolio company state/ county", "debt provider id",
                           "debt provider name", "debt provider city", "debt provider state/ county", "industry classification")]
# rename the above columns
colnames(preqin_names) <- c("company_name", "company_city", "company_state", "lender_id", "lender_name", "lender_city", "lender_state", "industry")
# change lender_name to lower case
preqin_names <- preqin_names %>%
  mutate(lender_name = tolower(lender_name))
# Clean lender_names using the pattern above as well
preqin_names <- preqin_names %>%
  mutate(lender_name_cleaned = str_replace_all(lender_name, "\\.", "")) %>%
  mutate(lender_name_cleaned = str_trim(str_remove_all(lender_name_cleaned, pattern), side = "both"))
# generate top 3 most common industry within each lender_name
preqin_names <- preqin_names %>%
  group_by(lender_name_cleaned, industry) %>% 
  summarise(count = n(), .groups = 'drop') %>%   # Count occurrences of each industry per lender
  arrange(lender_name_cleaned, desc(count)) %>%   # Arrange by lender and descending count
  group_by(lender_name_cleaned) %>% 
  slice_max(order_by = count, n = 3, with_ties = FALSE) %>% # Select top 3 industries
  mutate(rank = row_number()) %>%   # Rank the top industries as 1, 2, 3
  ungroup() %>%
  select(-count) # Optionally remove count column if not needed
# Reshape to wide format
preqin_names <- preqin_names %>%
  pivot_wider(names_from = rank, values_from = industry, names_prefix = "industry_")
# check unique industries
preqin_names %>%
  summarise(n_distinct(industry_1), n_distinct(industry_2), n_distinct(industry_3))
# generate lender_names as all lender_name in preqin_names
lender_names <- preqin_names %>%
  select(lender_name_cleaned) %>%
  distinct()

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
# change banc to bank 
agreements <- agreements %>%
  mutate(lender_chatgpt = str_replace(lender_chatgpt, "banc", "bank")) %>%
  mutate(lender_chatgpt = str_replace(lender_chatgpt, "banque", "bank"))
# generate lead_arranger as the first string before the first comma
agreements <- agreements %>%
  mutate(lead_arranger = str_extract(lender_chatgpt, "^[^,]+"))

# manual check for lead_arranger names 
  
# get a list of banks and non-banks

private_credit_entities <- c("ares", "abc", "alter domus", "apollo", "athene", "athyrium", "bain", "biopharma", 
                             "blackrock", "blackstone", "blue ridge", "blue torch", "brookfield", 
                             "Canada pension plan investment board", "cap 1 llc", "carter", "centre lane", 
                             "cerberus", "cf capital llc", "cf equipment loans", "cf turul llc", 
                             "chambers energy management", "chatham credit", "churchill", "colfin", 
                             "comvest", "cortland capital market", "cortland products", "cpp", "cyan partners", 
                             "db realty", "db structured", "deerfield", "eclipse business", "eig ", "ej funds", 
                             "elliott ", "encina ", "enervest ", "enhanced capital ", "ept ski properties", 
                             "escalate", "evergreen solar", "evolve transition", "fiera private", "fifth street finance", 
                             "five mile capital", "fleet capital corporation", "fleet retail", "fortress", 
                             "four winds funding", "gacp", "gci capital", "goldentree", "golub", "gpc partners", 
                             "gso ", "guggenheim ", "hercules technology", "hersha mezz", "high street capital", 
                             "highbridge ", "hps investment", "hv capital investors", "inter-americas investment corporation", 
                             "whitney mezzanine ", "jcf affm debt", "jha ", "kayne anderson", "kkr ", "laminar direct capital", 
                             "lasalle business", "laurus master fund", "lion capital", "loan core capital", "isb funding llc", 
                             "macquarie capital ", "madison capital funding", "madison dearborn ", "mclarity capital ", 
                             "egg investment group lp", "mid islandi sf.", "midcap", "monroe capital ", "newstar ", 
                             "northwest farm credit services", "oaktree ", "ocm ", "obsidian ", "orion energy ", "owl rock ", 
                             "pathlight ", "pdl biopharma", "patriot capital ", "pepi capital", "praesidian ", "rhône", 
                             "riverstone ", "rock", "sales capital partners", "siena lending group", "silver lake", "silver point", 
                             "sixth street", "solar capital", "squadron capital", "standard general ", "swk ", "tangshan caofeidian ", 
                             "tc ", "tcf ", "tcw", "tennenbaum", "thermo ", "third eye capital corporation", "thl", "tpg specialty", 
                             "victory park management", "wynnefield capital", "z investment", "sell credit opportunities")
non_regulated_ib_fcs <- c("brown brothers harriman & co.", "calyon", "cantor fitzgerald securities", "chase securities", 
                          "federal home loan mortgage corporation", "jefferies", "salomon smith barney", "swk", 
                          "td securities", "the cit group/business credit", "wachovia", "wilmington", "ge capital", "ing capital",
                          "walker & dunlop", "truist")
banks <- c("barclays", "bayerische", "bbva", "bear", "bmo", "bnp paribas", "bofa", "capital one", "cibc", "citicorp", "citigroup",
           "citizens", "credit lyonnais", "credit suisse", "goldman", "hsbc", "j.p. morgan", "j. p. morgan", "jpmorgan", "jp morgan", "lehman brothers", 
           "merrill", "morgan stanley", "pnc", "rbc", "rbs", "societe generale", "suntrust", "toronto dominion", "ubs", "wells fargo", 
           "société générale", "harris", "westlb")
# generate a variable that indicates whether the lender is a bank if "bank" appears in the name or strings in banks above appear in the name
# Collapse the bank names into a single pattern
bank_pattern <- str_c(banks, collapse = "|")
non_regulated_ib_fcs_pattern <- str_c(non_regulated_ib_fcs, collapse = "|")
private_credit_entities_pattern <- str_c(private_credit_entities, collapse = "|")

# Generate a variable that indicates whether the lender is a bank
agreements <- agreements %>%
  mutate(lender_is_nonbank = ifelse(str_detect(lead_arranger, "bank")
                                 | str_detect(lead_arranger, regex(bank_pattern, ignore_case = TRUE)), 0, 1))
# generate a variable indicating whether the lender is a non-regulated investment bank or financial company
agreements <- agreements %>%
  mutate(lender_is_non_regulated_ib_fc = ifelse(str_detect(lead_arranger, regex(non_regulated_ib_fcs_pattern, ignore_case = TRUE)), 1, 0))
# generate a variable indicating whether the lender is a private credit entity
agreements <- agreements %>%
  mutate(lender_is_private_credit_entity = ifelse(str_detect(lead_arranger, regex(private_credit_entities_pattern, ignore_case = TRUE)), 1, 0))

# check the number of observations for each lender type
agreements %>%
  group_by(lender_is_nonbank) %>%
  summarise(n = n())


###############################################################################################
# NEW FROM OCT 23RD (Use Pitchbook lender_name fuzzy matched to lead_arranger to identify private credit entities))

# Define a regular expression pattern to match unwanted suffixes at the end of the string
agreements <- agreements %>%
  mutate(lead_arranger_cleaned = str_replace_all(lead_arranger, "\\.", ""))

# Clean the 'lead_arranger_cleaned' column by removing the unwanted suffixes
agreements <- agreements %>%
  mutate(lead_arranger_cleaned = str_trim(str_remove_all(lead_arranger_cleaned, pattern), side = "both"))

# output all unique lender names from agreements and merge with all lender_names
unique_lender_names <- agreements %>%
  select(lead_arranger_cleaned) %>%
  distinct()

###
fuzzy_matched_lenders <- stringdist_join(
  unique_lender_names, 
  lender_names, 
  by = c("lead_arranger_cleaned" = "lender_name_cleaned"),
  mode = "left",       # Keep all from agreements (left side)
  method = "jw",       # Jaro-Winkler distance
  max_dist = 0.05,      # Maximum allowed distance (adjustable)
  distance_col = "dist" # Add a distance column to see the match score
)
# drop if NA
fuzzy_matched_lenders <- fuzzy_matched_lenders %>%
  filter(!is.na(lender_name_cleaned))
# Sort by distance to check the closest matches
fuzzy_matched_lenders <- fuzzy_matched_lenders %>%
  arrange(lead_arranger_cleaned, dist)
# keep only the smallest matches in terms of distance
fuzzy_matched_lenders <- fuzzy_matched_lenders %>%
  group_by(lead_arranger_cleaned) %>%
  filter(dist == min(dist))
# make sure each lead_arranger_cleaned is unique
fuzzy_matched_lenders <- fuzzy_matched_lenders %>%
  distinct(lead_arranger_cleaned, .keep_all = TRUE)

# merge this list back to agreements
agreements <- agreements %>%
  left_join(fuzzy_matched_lenders, by = c("lead_arranger_cleaned" = "lead_arranger_cleaned"))
# merge the lender_name_cleaned back to preqin_names to get industry
agreements <- agreements %>%
  left_join(preqin_names, by = c("lender_name_cleaned" = "lender_name_cleaned"))

# generate a variable that indicates whether the lender is a private credit entity (dist != NA and not a bank)
agreements <- agreements %>%
  mutate(lender_is_private_credit_entity_pitchbook = ifelse(!is.na(dist) & lender_is_nonbank == 1, 1, 0))
# change the lender_is_private_credit_entity to 0 if == lender_is_non_regulated_ib_fc = 0
agreements <- agreements %>%
  mutate(lender_is_private_credit_entity_pitchbook = ifelse(lender_is_non_regulated_ib_fc == 1, 0, lender_is_private_credit_entity_pitchbook))

# generate a variable that is equal to 1 if lender_is_private_credit_entity == 1 or lender_is_private_credit_entity_pitchbook == 1, and 0 if lender_is_nonbank == 0
agreements <- agreements %>%
  mutate(lender_is_private_credit = case_when(
    lender_is_private_credit_entity == 1 | lender_is_private_credit_entity_pitchbook == 1 ~ 1,
    lender_is_nonbank == 0 ~ 0,
    TRUE ~ NA_real_  # This sets the value to NA for all other cases
  ))

# arrange by lender_is_nonbank, lender_is_non_regulated_ib_fc, lender_is_private_credit_entity, lender_is_private_credit_entity_pitchbook, lender_is_private_credit
agreements <- agreements %>%
  arrange(lender_is_nonbank, lender_is_non_regulated_ib_fc, lender_is_private_credit_entity, lender_is_private_credit_entity_pitchbook, lender_is_private_credit)

agreements <- agreements %>%
  mutate(year = year(date),
         quarter = quarter(date))

##################################################
# time series plots of the number of credit agreements by year by lender_is_nonbank
plot_agreements_ts <- function(data, lender_var) {
  lender_var_name <- deparse(substitute(lender_var))  # Get the name of lender_var as a string
  data %>%
    group_by(year, {{ lender_var }}) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = year, y = n, color = factor({{ lender_var }}))) +
    geom_line() +
    theme_minimal() +
    labs(title = "Time Series of Number of Credit Agreements by Year",
         x = "Year",
         y = "Number of Credit Agreements",
         color = "Lender is Bank") +
    theme(legend.position = c(0.2, 0.8),    # Position legend inside the plot
          legend.background = element_blank(),  # Remove legend background fill
          legend.box.background = element_blank(),  # Remove legend border color
          legend.text = element_text(size = 10),  # Smaller text inside the legend
          legend.title = element_text(size = 10),  # Smaller title inside the legend
          legend.key.size = unit(0.5, "cm"),  # Smaller keys in the legend
          legend.spacing.y = unit(0.2, "cm"),  # Smaller vertical spacing
          legend.spacing.x = unit(0.2, "cm")) +  # Smaller horizontal spacing
    guides(color = guide_legend(title = lender_var_name))
}
# save figure as pdf
plot_agreements_ts(agreements, lender_is_nonbank)
ggsave(file.path(figure_path, "Agreements_by_year_bank.pdf"))
plot_agreements_ts(agreements, lender_is_private_credit)
ggsave(file.path(figure_path, "Agreements_by_year_private_credit.pdf"))

##################################################
# time series plots of the three variables (monthly_fs, projected_fs, lender_meeting) by year for banks and nonbanks on the same plot

plot_infocov_ts <- function(data, lender_var) {
  lender_var_name <- deparse(substitute(lender_var))  # Get the name of lender_var as a string
  data %>%
    group_by(year, {{ lender_var }}) %>%
    summarise(
      monthly_fs = mean(monthly_fs, na.rm = TRUE),
      projected_fs = mean(projected_fs, na.rm = TRUE),
      lender_meeting = mean(lender_meeting, na.rm = TRUE)
    ) %>%
    # Apply 3-year moving average smoothing using rollmean from zoo package
    mutate(
      monthly_fs = rollmean(monthly_fs, 3, fill = NA, align = "right", partial = TRUE),
      projected_fs = rollmean(projected_fs, 3, fill = NA, align = "right", partial = TRUE),
      lender_meeting = rollmean(lender_meeting, 3, fill = NA, align = "right", partial = TRUE)
    ) %>%
    # Reshape the data from wide to long format for easier plotting
    gather(key = "variable", value = "value", -year, -{{ lender_var }}) %>%
    ggplot(aes(x = year, y = value, color = variable, linetype = factor({{ lender_var }}))) +
    geom_line() +
    theme_minimal() +
    labs(title = "3-Year Moving Average of Monthly FS, Projected FS, and Lender Meeting Covenants",
         x = "Year",
         y = "Proportion of Agreements with Information Covenant",
         color = "Variable",
         linetype = "Lender is Bank") +
    # Manually set linetypes: solid for 1, dashed for 0
    scale_linetype_manual(values = c("1" = "solid", "0" = "dashed")) + 
    theme(
      legend.position = c(0.2, 0.8),  # Position legend inside the plot
      legend.background = element_blank(),  # Remove legend background fill
      legend.box.background = element_blank(),  # Remove legend border color
      legend.text = element_text(size = 8),  # Smaller text inside the legend
      legend.title = element_text(size = 8),  # Smaller title inside the legend
      legend.key.size = unit(0.5, "cm"),  # Smaller keys in the legend
      legend.spacing.y = unit(0.2, "cm"),  # Smaller vertical spacing
      legend.spacing.x = unit(0.2, "cm")   # Smaller horizontal spacing
    ) +
    guides(
      color = guide_legend(title = "Variable"), 
      linetype = guide_legend(title = lender_var_name)
    )
}

plot_infocov_ts2 <- function(data, lender_var) {
  lender_var_name <- deparse(substitute(lender_var))  # Get the name of lender_var as a string
  data %>%
    group_by(year, {{ lender_var }}) %>%
    summarise(
      monthly_fs = mean(monthly_fs, na.rm = TRUE),
      projected_fs = mean(projected_fs, na.rm = TRUE),
      lender_meeting = mean(lender_meeting, na.rm = TRUE)
    ) %>%
    # Reshape the data from wide to long format for easier plotting
    gather(key = "variable", value = "value", -year, -{{ lender_var }}) %>%
    ggplot(aes(x = year, y = value, color = variable, linetype = factor({{ lender_var }}))) +
    geom_line() +
    theme_minimal() +
    labs(title = "Time Series of Monthly FS, Projected FS, and Lender Meeting Covenants",
         x = "Year",
         y = "Proportion of Agreements with Information Covenant",
         color = "Variable",
         linetype = "Lender is Bank") +
    # Manually set linetypes: solid for 1, dashed for 0
    scale_linetype_manual(values = c("1" = "solid", "0" = "dashed")) + 
    theme(
      legend.position = c(0.2, 0.8),  # Position legend inside the plot
      legend.background = element_blank(),  # Remove legend background fill
      legend.box.background = element_blank(),  # Remove legend border color
      legend.text = element_text(size = 8),  # Smaller text inside the legend
      legend.title = element_text(size = 8),  # Smaller title inside the legend
      legend.key.size = unit(0.5, "cm"),  # Smaller keys in the legend
      legend.spacing.y = unit(0.2, "cm"),  # Smaller vertical spacing
      legend.spacing.x = unit(0.2, "cm"),  # Smaller horizontal spacing
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
    ) +
    guides(
      color = guide_legend(title = "Variable"), 
      linetype = guide_legend(title = lender_var_name)
    )
}
plot_infocov_ts2(agreements, lender_is_nonbank)
ggsave(file.path(figure_path, "Info_Cov2_94to23_full.pdf"))

# tab number of obs per year for each year (# ℹ Use `print(n = ...)` to see more rows)
obs <- agreements %>%
  group_by(year) %>%
  summarise(n = n())

plot_infocov_ts(agreements, lender_is_nonbank)
ggsave(file.path(figure_path, "Info_Cov_94to23_full.pdf"))
plot_infocov_ts(agreements, lender_is_private_credit_entity)
ggsave(file.path(figure_path, "Info_Cov_94to23_private_credit.pdf"))

##################################################
# Section 3: Merge with Compustat Quarterly (Annual) Data
##################################################

compq = fread("../Data/Cleaned/crsp_compustat_quarterly_cleaned.csv")
# merge with filing data
agreements_compq_merged <- agreements %>%
  left_join(compq, by = c("gvkey" = "gvkey", "year" = "fyearq", "quarter" = "fqtr"))

# generate the max and min of last_year_revenue and last_year_ebitda for lender_is_nonbank == 1 and lender_is_nonbank == 0
agreements_compq_merged %>%
  group_by(lender_is_nonbank) %>%
  summarise(min_last_year_revenue = min(last_year_revenue, na.rm = TRUE),
            max_last_year_revenue = max(last_year_revenue, na.rm = TRUE),
            min_last_year_ebitda = min(last_year_ebitda, na.rm = TRUE),
            max_last_year_ebitda = max(last_year_ebitda, na.rm = TRUE))

# keep only those firms with revenue between 10,000,000 and 1,000,000,000
agreements_mm <- agreements_compq_merged %>%
  filter(last_year_revenue >= 10 & last_year_revenue <= 1000)

# time series plots of the number of credit agreements by year by lender_is_nonbank
plot_agreements_ts(agreements_mm, lender_is_nonbank)
ggsave(file.path(figure_path, "Agreements_by_year_bank_mm.pdf"))
plot_agreements_ts(agreements_mm, lender_is_private_credit_entity)
ggsave(file.path(figure_path, "Agreements_by_year_private_credit_mm.pdf"))

# plot the frequencies of the three variables (monthly_fs, projected_fs, lender_meeting) by year 
plot_infocov_ts(agreements_mm, lender_is_nonbank)
ggsave(file.path(figure_path, "Info_Cov_94to23_mm.pdf"))
plot_infocov_ts(agreements_mm, lender_is_private_credit_entity)
ggsave(file.path(figure_path, "Info_Cov_94to23_private_credit_mm.pdf"))

# save as csv
write.csv(agreements_mm, "../Data/Cleaned/agreements_mm.csv", row.names = FALSE)

##################################################
# Section 4: Merge back to LoanContracts with Deal Information
##################################################

### merge agreements_mm with deal information to arrive at the final dataset that includes only new contracts with deal information
new_loancontracts_mm_dealinfo <- fread("../Data/LoansFull/loancontracts_with_extracted_dealinfo_oct26_update.csv")
# number of observations each year
new_loancontracts_mm_dealinfo_count <- new_loancontracts_mm_dealinfo %>%
  group_by(year) %>%
  summarise(count = n())

new_loancontracts_mm_dealinfo <- new_loancontracts_mm_dealinfo %>%
  select(accession, type_filing, type_attachment, deal_amount, interest_spread, maturity)
### clean the variables 
# drop "," in the deal_amount variable
new_loancontracts_mm_dealinfo$deal_amount1 <- str_replace_all(new_loancontracts_mm_dealinfo$deal_amount, ",", "")
new_loancontracts_mm_dealinfo$deal_amount1 <- as.numeric(str_extract(new_loancontracts_mm_dealinfo$deal_amount1, "\\d+")) / 1e6 # in million
# multiply deal_amount1 by 1e6 if "million" appears in the deal_amount variable
new_loancontracts_mm_dealinfo$deal_amount1 <- ifelse(str_detect(new_loancontracts_mm_dealinfo$deal_amount, "million"), 
                                                     new_loancontracts_mm_dealinfo$deal_amount1 * 1e6, new_loancontracts_mm_dealinfo$deal_amount1)
# keep the number (including numbers with decimal points) after "LIBOR + "
new_loancontracts_mm_dealinfo$interest_spread1 <- as.numeric(str_extract(new_loancontracts_mm_dealinfo$interest_spread, "\\d+\\.?\\d*"))
# generate a variable that indicates whether the interest rate is fixed or floating (LIBOR, base, prime, etc.)
new_loancontracts_mm_dealinfo$interest_type <- ifelse(str_detect(new_loancontracts_mm_dealinfo$interest_spread, "LIBOR|ABR|Prime|Base"), "floating", "fixed")
# generate a date variable for maturity (format is November 15, 2013)
new_loancontracts_mm_dealinfo$maturity1 <- as.Date(new_loancontracts_mm_dealinfo$maturity, format = "%B %d, %Y")
# drop if any of the above variables are NA
#new_loancontracts_mm_dealinfo <- new_loancontracts_mm_dealinfo %>%
# filter(!is.na(deal_amount) & !is.na(interest_spread1) & !is.na(maturity1))
# merge with agreements_mm
agreements_mm_dealinfo <- agreements_mm %>%
  inner_join(new_loancontracts_mm_dealinfo, by = c("accession", "type_filing", "type_attachment"))
# save as csv
write.csv(agreements_mm_dealinfo, "../Data/Cleaned/agreements_mm_dealinfo.csv", row.names = FALSE)

# plot with agreements_mm_dealinfo
plot_infocov_ts(agreements_mm_dealinfo, lender_is_nonbank)
ggsave(file.path(figure_path, "Info_Cov_94to23_mm_dealinfo.pdf"))
plot_infocov_ts(agreements_mm_dealinfo, lender_is_private_credit_entity)
ggsave(file.path(figure_path, "Info_Cov_94to23_private_credit_mm_dealinfo.pdf"))

plot_infocov_ts2(agreements_mm_dealinfo, lender_is_nonbank)
ggsave(file.path(figure_path, "Info_Cov2_94to23_mm_dealinfo.pdf"))

plot_agreements_ts(agreements_mm_dealinfo, lender_is_nonbank)
ggsave(file.path(figure_path, "Agreements_by_year_bank_mm_dealinfo.pdf"))
plot_agreements_ts(agreements_mm_dealinfo, lender_is_private_credit_entity)
ggsave(file.path(figure_path, "Agreements_by_year_private_credit_mm_dealinfo.pdf"))

##################################################
# Section 5: Clean LIBOR Rate Datasets
##################################################
libor1 <- fread("../Data/Raw/LIOR3M.csv")
colnames(libor1) <- c("date", "rate")
libor2 <- fread("../Data/Raw/3-month-libor-rate-historical-chart.csv", skip = 15, fill=TRUE)
colnames(libor2) <- c("date", "rate")
libor3 <- fread("../Data/Raw/AMERIBOR.csv")
colnames(libor3) <- c("date", "rate")
# append libor1 and libor3 first
libor <- rbind(libor1, libor3)
libor <- libor %>%
  mutate(year = year(date),
         quarter = quarter(date))

# generate year and quarter for libor2
libor2 <- libor2 %>%
  mutate(year = year(date),
         quarter = quarter(date))
# keep only year-quarter from 2016Q4 to 2019Q4
libor2 <- libor2 %>%
  filter(year >= 2017 & year <= 2019)
# for each year-quarter keep only the first observation
libor2 <- libor2 %>%
  group_by(year, quarter) %>%
  slice(1)
# drop 2019Q4
libor2 <- libor2 %>%
  filter(!(year == 2019 & quarter == 4))
# append with libor
libor <- rbind(libor, libor2)
# change rate to numeric
libor <- libor %>%
  mutate(rate = as.numeric(rate))
# remove date
libor <- libor %>%
  select(-date)
# save as csv
write.csv(libor, "../Data/Cleaned/libor3_rates.csv", row.names = FALSE)

# merge with agreements_mm_dealinfo
agreements_mm_dealinfo_libor <- agreements_mm_dealinfo %>%
  left_join(libor, by = c("year" = "year", "quarter" = "quarter"))
write.csv(agreements_mm_dealinfo_libor, "../Data/Cleaned/agreements_mm_dealinfo.csv", row.names = FALSE)
