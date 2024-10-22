/* This file uses the all_merged_cleaned.csv to analyze and output regression and summary tables
*/

*** Generate a list of globals (path) for future references
global repodir "/Users/zrsong/Dropbox (MIT)/Research Projects/Direct Lending"
global datadir "$repodir/Data"
global rawdir "$datadir/Raw"
global intdir "$datadir/Intermediate"
global cleandir "$datadir/Cleaned"
global tabdir "$repodir/Results/Tables"
global figdir "$repodir/Results/Figures"

/**************
	Summary Table
	***************/

import delimited "$cleandir/agreements_mm_dealinfo.csv", clear
keep if year >= 2010
destring atq-prev_ebitda sich deal_amount1 interest_spread1, replace ignore("NA")
drop if atq >= .
* generate maturity
replace maturity1 = "" if maturity1 == "NA"
gen maturity_date = date(maturity1, "YMD")
gen start_date = date(date, "YMD")
gen maturity_year = (maturity_date - start_date)/365
drop if maturity_year < 0
* generate margin 
gen margin_bps = interest_spread1*100
replace deal_amount1 = deal_amount1/1000

* winsorize at 1%-99%s
winsor2 atq-leverage prev_ebitda deal_amount1 interest_spread1 maturity_year margin_bps , cuts(1 99) replace
sum lender_is_nonbank lender_is_private_credit_entity monthly_fs-lender_meeting atq-leverage prev_ebitda deal_amount1 maturity_year margin_bps

* generate FF-12 Industry
sicff sich, ind(12)

label define ff_12_lab 1 "Consumer NonDurables" 2 "Consumer Durables" 3 "Manufacturing" 4 "Oil, Gas, and Coal Extraction and Products" 5 "Chemicals and Allied Products" 6 "Business Equipment" 7 "Telephone and Television Transmission" 8 "Utilities" 9 "Wholesale, Retail, and Some Services" 10 "Healthcare, Medical Equipment, and Drugs" 11 "Finance" 12 "Other"
label values ff_12 ff_12_lab

* generate private_credit vs pure bank comparison
gen lender_is_private_credit = 1 if lender_is_private_credit_entity == 1
replace lender_is_private_credit = 0 if lender_is_nonbank == 0

save "$intdir/intermediate_data_after_main_regression.dta", replace

*** Table 1: Descriptives *** 

* Panel A (Number of Deals By Industry)
dtable i.ff_12, by(lender_is_private_credit) export("$tabdir/tabulation_ff12_nonbank.tex", tableonly replace) note("Panel A: Number of Deals By Industry")

la var lender_is_nonbank "Nonbank Lender"
la var lender_is_private_credit "Private Credit Lender"
la var monthly_fs "Monthly Financial Statement"
la var projected_fs "Annual Budget/Projection"
la var lender_meeting "Lender Meeting"
la var atq "Total Assets (Million USD)"
la var revtq "Revenue (Million USD)"
la var niq "Net Income (Million USD)"
la var ibq "Income Before Taxes (Million USD)"
la var ltq "Long-Term Debt (Million USD)"
la var xrdq "R\&D"
la var ppegtq "Tangibility"
la var roa "ROA"
la var leverage "Leverage Ratio"
la var prev_ebitda "Previous Year EBITDA"
la var maturity_year "Maturity (Years)"
la var margin_bps "Floating Interest Margin (Basis Points)"
la var deal_amount1 "Deal Amount (Billion USD)"
la var interest_spread1 "Interest Margin (Percentage)"


* Panel C (Summary Statistics by Banks and Nonbanks Respectively)
estpost sum monthly_fs-lender_meeting atq-leverage prev_ebitda if lender_is_nonbank == 0, de
esttab using "$tabdir/summary_table_by_bank.tex", replace ///
    cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) p50(fmt(%9.2f)) max(fmt(%9.2f))") noobs nonum collabels(Count Mean "Std. Dev." Min Median Max) title(": Summary Statistics for Banks") label
	
estpost sum monthly_fs-lender_meeting atq-leverage prev_ebitda if lender_is_nonbank == 1, de
esttab using "$tabdir/summary_table_by_nonbank.tex", replace ///
     cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) p50(fmt(%9.2f))  max(fmt(%9.2f))") noobs nonum collabels(Count Mean "Std. Dev." Min Median Max) title(": Summary Statistics for Nonbanks") label

estpost sum monthly_fs-lender_meeting atq-leverage prev_ebitda if lender_is_private_credit_entity == 1, de
esttab using "$tabdir/summary_table_by_private_credit.tex", replace ///
     cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) p50(fmt(%9.2f))  max(fmt(%9.2f))") noobs nonum collabels(Count Mean "Std. Dev." Min Median Max) title(": Summary Statistics for Private Credit") label


*** Table 2: Borrowers with both ***
gsort gvkey year
order lender_is_nonbank coname gvkey year date lead_arranger atq revtq prev_ebitda ff_12 monthly_fs-lender_meeting

/**************
	Determinant Tables
	***************/
	
* keep only those with floating rates
keep if interest_type == "floating"
replace prev_ebitda = prev_ebitda/atq
replace atq = log(atq)
	
gen prev_ebitda_dummy = 1 if prev_ebitda > 0 
replace prev_ebitda_dummy = 0 if prev_ebitda_dummy == .
	
reg lender_is_nonbank prev_ebitda_dummy prev_ebitda atq roa leverage i.ff_12 i.year
outreg2 using "$tabdir/determinant_nonbank_lending.xls", excel replace
reg lender_is_private_credit_entity prev_ebitda_dummy prev_ebitda atq roa leverage i.ff_12 i.year
outreg2 using "$tabdir/determinant_nonbank_lending.xls", excel append

reg monthly_fs prev_ebitda atq roa leverage i.ff_12 i.year
outreg2 using "$tabdir/determinant_info_cov.xls", excel replace
reg projected_fs prev_ebitda atq roa leverage i.ff_12 i.year
outreg2 using "$tabdir/determinant_info_cov.xls", excel append
reg lender_meeting prev_ebitda atq roa leverage i.ff_12 i.year
outreg2 using "$tabdir/determinant_info_cov.xls", excel append

egen hard_info = rowmax(monthly_fs projected_fs)
egen back_info = rowmax(monthly_fs lender_meeting)
egen all_info = rowmin(monthly_fs projected_fs lender_meeting)

reg hard_info prev_ebitda atq roa leverage i.ff_12 i.year
outreg2 using "$tabdir/determinant_info_cov.xls", excel append

reg back_info prev_ebitda atq roa leverage i.ff_12 i.year
outreg2 using "$tabdir/determinant_info_cov.xls", excel append

reg all_info prev_ebitda atq roa leverage i.ff_12 i.year
outreg2 using "$tabdir/determinant_info_cov.xls", excel append


*** Determinant Regressions (Table III)

local borr_vars "prev_ebitda atq roa leverage"
local deal_vars "deal_amount1 maturity_year interest_spread1"	
local y_vars "lender_is_nonbank lender_is_private_credit" 
local info_vars "monthly_fs projected_fs lender_meeting hard_info back_info all_info"
	
foreach var of varlist `y_vars' {
	eststo: reghdfe `var' `borr_vars' `deal_vars', absorb(ff_12 year)
}	
esttab using "$tabdir/Table3_treatment.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear


foreach var of varlist `info_vars' {
	eststo: reghdfe `var' `borr_vars' `deal_vars', absorb(ff_12 year)
}
esttab using "$tabdir/Table3_infocov.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear

/**************
	Main Regression Table 
	***************/
	
	save "$cleandir/final_regression_sample.dta", replace

reg monthly_fs lender_is_private_credit_entity `borr_vars' `deal_vars' i.ff_12 i.year, vce(cluster year)
outreg2 using "$tabdir/main_regression.xls", excel replace
reg projected_fs lender_is_private_credit_entity `borr_vars' `deal_vars'  i.ff_12 i.year, vce(cluster year)
outreg2 using "$tabdir/main_regression.xls", excel append

reg lender_meeting lender_is_private_credit_entity `borr_vars' `deal_vars'  i.ff_12 i.year, vce(cluster year)
outreg2 using "$tabdir/main_regression.xls", excel append

reg hard_info lender_is_private_credit_entity `borr_vars' `deal_vars' i.ff_12 i.year, vce(cluster year)
outreg2 using "$tabdir/main_regression.xls", excel append

reg back_info lender_is_private_credit_entity `borr_vars' `deal_vars' i.ff_12 i.year, vce(cluster year)
outreg2 using "$tabdir/main_regression.xls", excel append

reg all_info lender_is_private_credit_entity `borr_vars' `deal_vars' i.ff_12 i.year, vce(cluster year)
outreg2 using "$tabdir/main_regression.xls", excel append

*** Main (Table IV)

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' lender_is_private_credit `borr_vars' `deal_vars', absorb(ff_12 year)
}
esttab using "$tabdir/Table4_main_regression.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear

*** Propensity Score Matching
local borr_vars "prev_ebitda atq roa leverage"
local deal_vars "deal_amount1 maturity_year interest_spread1"	
local y_vars "lender_is_nonbank lender_is_private_credit" 
local info_vars "monthly_fs projected_fs lender_meeting hard_info back_info all_info"

psmatch2 lender_is_private_credit_entity `borr_vars' i.ff_12 `deal_vars', out(monthly_fs) logit n(3) ai(3) common caliper(0.01)
esttab using "$tabdir/Table5_main_psm.tex", replace nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
psmatch2 lender_is_private_credit_entity `borr_vars' i.ff_12 `deal_vars', out(projected_fs) logit n(3) ai(3) common caliper(0.01)
psmatch2 lender_is_private_credit_entity `borr_vars' i.ff_12 `deal_vars', out(lender_meeting) logit n(3) ai(3) common caliper(0.01)
psmatch2 lender_is_private_credit_entity `borr_vars' i.ff_12 `deal_vars', out(hard_info) logit n(3) ai(3) common caliper(0.01)
psmatch2 lender_is_private_credit_entity `borr_vars' i.ff_12 `deal_vars', out(back_info) logit n(3) ai(3) common caliper(0.01)
psmatch2 lender_is_private_credit_entity `borr_vars' i.ff_12 `deal_vars', out(all_info) logit n(3) ai(3) common caliper(0.01)
pstest `borr_vars' `deal_vars', graph both


/**************
	Regression Discontinuity Design Around prev_ebitda == 0
	***************/
	
* check for share of nonbank loans for over prev_ebitda
egen prev_ebitda_bins = cut(prev_ebitda), at(-50 -20 -10 -1 10 20 50 100 200)
preserve
	collapse (mean) lender_is_private_credit_entity (count) gvkey, by(prev_ebitda_bins)
	scatter lender_is_private_credit_entity prev_ebitda_bins
	*scatter gvkey prev_ebitda_bins
restore

*keep if year > 2013
*keep if margin_bps > 200
* keep only those with prev_ebitda <= +- 25
* keep if abs(prev_ebitda) < 50
histogram prev_ebitda, bin(100) normal kdensity freq

rddensity prev_ebitda, c(0) plot
rdrobust lender_is_nonbank prev_ebitda, c(0)
rdrobust monthly_fs prev_ebitda, c(0) fuzzy(lender_is_nonbank)

/**************
	DiD in 2018 from SNC increase from $20 to $100 million
	***************/
	
use "$intdir/intermediate_data_after_main_regression.dta", clear

*** generate treat and post variables 

gen treat = 1 if inrange(deal_amount1, 20/1000, 100/1000)
replace treat = 0 if treat == .

gen post = 1 if year >= 2018
replace post = 0 if year == .

gen treat_post = treat * post

*** TWFE

reghdfe lender_is_private_credit treat_post maturity_year interest_spread1, absorb(year gvkey)


/**************
	Cross-Sectional Tests
	***************/

*** Large Versus Small Private Credit Lenders
	use "$cleandir/final_regression_sample.dta", replace
	
* generate large and small private credit lenders	
bysort lead_arranger: gen deal_count = _N
tab deal_count if lender_is_private_credit == 1	
gen big_pc = 1 if lender_is_private_credit == 1 & deal_count >= 4
replace big_pc = 0 if big_pc ==. 
gen big_bank = 1 if lender_is_private_credit == 0 & deal_count >= 93
replace big_bank = 0 if big_bank ==. 

keep if lender_is_private_credit == 1

* manually clean some names 
replace big_pc = 1 if strpos(lead_arranger, "apollo") > 0
replace big_pc = 1 if strpos(lead_arranger, "ares") > 0
replace big_pc = 1 if strpos(lead_arranger, "cortland") > 0
replace big_pc = 1 if strpos(lead_arranger, "kkr") > 0
replace big_pc = 1 if strpos(lead_arranger, "oaktree") > 0

drop if lead_arranger == "wilmington trust"

* regression	
local borr_vars "prev_ebitda atq roa leverage"
local deal_vars "deal_amount1 maturity_year interest_spread1"	
local y_vars "lender_is_nonbank lender_is_private_credit" 
local info_vars "monthly_fs projected_fs lender_meeting hard_info back_info all_info"

foreach var of varlist `info_vars' {
	reghdfe `var' big_pc `borr_vars' `deal_vars', absorb(year ff_12)
}
