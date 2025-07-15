/* This file uses the all_merged_cleaned.csv to analyze and output regression and summary tables
*/

*** Generate a list of globals (path) for future references
global repodir "/Users/zrsong/MIT Dropbox/Zirui Song/Research Projects/Direct Lending"
global datadir "$repodir/Data"
global rawdir "$datadir/Raw"
global intdir "$datadir/Intermediate"
global cleandir "$datadir/Cleaned"
*global tabdir "$repodir/Results/Tables"
global tabdir "/Users/zrsong/MIT Dropbox/Zirui Song/Apps/Overleaf/Information Covenants of Nonbank Direct Lending/Tables"
global figdir "/Users/zrsong/MIT Dropbox/Zirui Song/Apps/Overleaf/Information Covenants of Nonbank Direct Lending/Figures"
global logdir "$repodir/Code/LogFiles"

*log using "$logdir/Analyze_Merged_Covenants.log", text replace

/**************
	Data Cleaning
	***************/

use "$cleandir/agreements_mm_clean202503.dta", clear

* generate margin 
gen margin_bps = clean_rate*100

* generate FF-12 Industry
sicff sic, ind(12)

label define ff_12_lab 1 "Consumer NonDurables" 2 "Consumer Durables" 3 "Manufacturing" 4 "Oil, Gas, and Coal Extraction and Products" 5 "Chemicals and Allied Products" 6 "Business Equipment" 7 "Telephone and Television Transmission" 8 "Utilities" 9 "Wholesale, Retail, and Some Services" 10 "Healthcare, Medical Equipment, and Drugs" 11 "Finance" 12 "Other"
label values ff_12 ff_12_lab

egen hard_info = rowmax(monthly_fs projected_fs)
gen info_n = monthly_fs + projected_fs + lender_meeting
egen all_info = rowmin(monthly_fs projected_fs lender_meeting)

gen other_nonbank_lender = 1 if nonbank_lender == 1 & private_credit_lender != 1
replace other_nonbank_lender = 0 if other_nonbank_lender == .

gen scaled_ebitda = last_year_ebitda/assets
la var scaled_ebitda "EBITDA (Scaled)"

gen ln_assets = log(assets)
la var ln_assets "Ln(Total Assets)"
	
gen prev_ebitda_dummy = 1 if last_year_ebitda < 0 
replace prev_ebitda_dummy = 0 if prev_ebitda_dummy == .
la var prev_ebitda_dummy "EBITDA < 0"

winsor2 debt_to_ebitda, cuts(5 95) replace

gen debt_to_ebitda_gr6 = 1 if debt_to_ebitda > 6
replace debt_to_ebitda_gr6 = 1 if debt_to_ebitda < 0
replace debt_to_ebitda_gr6 = 0 if debt_to_ebitda_gr6 == .

gen ln_amount = ln(facility_amount)
la var ln_amount "Ln(Loan Amount)"

gen term_loan = 1 if strpos(facility_type, "Term Loan") > 0
replace term_loan = 0 if term_loan == .

gen secured_dummy = 1 if strpos(secured, "Yes") > 0
replace secured_dummy = 0 if secured_dummy == .

gen nonbank_pc_inter = nonbank_lender * private_credit_lender
replace nonbank_pc_inter = 0 if nonbank_pc_inter == .
la var nonbank_pc_inter "Nonbank Lender x Private Credit"

gen maturity = maturity_months / 12

local all_borr_cov "assets last_year_revenue prev_ebitda_dummy last_year_ebitda debt_to_ebitda leverage market_to_book last_year_rnd_intensity tangibility current_ratio rolling_12m_return rolling_12m_vol"
local all_deal_vars "facility_amount ln_amount maturity clean_rate multiple_facilities term_loan secured_dummy" 	 

* winsorize at 1%-99%s
winsor2 `all_borr_cov' `all_deal_vars', cuts(1 99) replace

/**************
	Summary Table
	***************/

*** Table 1: Descriptives *** 

la var other_nonbank_lender "Other Nonbank Lender"
la var nonbank_lender "Nonbank Lender"
la var private_credit_lender "Private Credit Lender"
la var monthly_fs "Monthly Financial Statement"
la var projected_fs "Annual Budget/Projection"
la var lender_meeting "Lender Meeting"

la var assets "Total Assets (Million USD)"
la var last_year_revenue "Revenue (Million USD)"
la var debt "Debt (Million USD)"
la var last_year_rnd_intensity "R\&D Intensity"
la var tangibility "Tangibility"
la var leverage "Leverage Ratio"
la var last_year_ebitda "EBITDA (Million USD)"
la var debt_to_ebitda "Debt/EBITDA"
la var rolling_12m_return "Past Return"
la var rolling_12m_vol "Stock Volatility"
la var market_to_book "Market-to-book"
la var current_ratio "Current Ratio"

la var multiple_facilities "Multiple Tranches"
la var term_loan "Term Loan"
la var secured_dummy "Secured"
la var margin_bps "Floating Interest Margin (Basis Points)"
la var facility_amount "Loan Amount (Million USD)"
la var maturity_months "Maturity in Months"

la var clean_rate "Interest Margin"
la var maturity "Maturity"
la var ln_amount "Ln(Amount)"

* Panel A (Number of Deals By Industry)
dtable i.ff_12, by(nonbank_lender) export("$tabdir/tabulation_ff12_all.tex", tableonly replace) note("Panel A: Number of Deals By Industry")
dtable i.ff_12, by(private_credit_lender) export("$tabdir/tabulation_ff12_pc.tex", tableonly replace) note("Panel A: Number of Deals By Industry")
dtable i.ff_12, by(other_nonbank_lender) export("$tabdir/tabulation_ff12_other.tex", tableonly replace) note("Panel A: Number of Deals By Industry")

* Panel B (Summary Statistics by Banks and Nonbanks Respectively)
estpost sum monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars', de
estpost tabstat monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars', stats(n mean sd p5 p50 p95) columns(statistics)
esttab using "$tabdir/summary_table_all.tex", replace fragment ///
    cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) p5(fmt(%9.2f)) p50(fmt(%9.2f)) p95(fmt(%9.2f))") ///
    noobs nonum collabels(Count Mean "Std. Dev." "5th Pct." Median "95th Pct.") ///
    title(": Summary Statistics (Full Sample)") label

estpost sum monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if nonbank_lender == 0, de
estpost tabstat monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if nonbank_lender == 0, stats(n mean sd p5 p50 p95) columns(statistics)
esttab using "$tabdir/summary_table_by_bank.tex", replace fragment ///
    cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) p5(fmt(%9.2f)) p50(fmt(%9.2f)) p95(fmt(%9.2f))") ///
    noobs nonum collabels(Count Mean "Std. Dev." "5th Pct." Median "95th Pct.") ///
    title(": Summary Statistics for Banks") label

estpost sum monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if nonbank_lender == 1, de
estpost tabstat monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if nonbank_lender == 1, stats(n mean sd p5 p50 p95) columns(statistics)
esttab using "$tabdir/summary_table_by_nonbank.tex", replace fragment ///
    cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) p5(fmt(%9.2f)) p50(fmt(%9.2f)) p95(fmt(%9.2f))") ///
    noobs nonum collabels(Count Mean "Std. Dev." "5th Pct." Median "95th Pct.") ///
    title(": Summary Statistics for All Nonbank Lenders") label

estpost sum monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if other_nonbank_lender == 1, de
estpost tabstat monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if other_nonbank_lender == 1, stats(n mean sd p5 p50 p95) columns(statistics)
esttab using "$tabdir/summary_table_by_other_nonbank.tex", replace fragment ///
    cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) p5(fmt(%9.2f)) p50(fmt(%9.2f)) p95(fmt(%9.2f))") ///
    noobs nonum collabels(Count Mean "Std. Dev." "5th Pct." Median "95th Pct.") ///
    title(": Summary Statistics for Other Nonbank Lenders") label

estpost sum monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if private_credit_lender == 1, de
estpost tabstat monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if private_credit_lender == 1, stats(n mean sd p5 p50 p95) columns(statistics)
esttab using "$tabdir/summary_table_by_private_credit.tex", replace fragment ///
    cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) p5(fmt(%9.2f)) p50(fmt(%9.2f)) p95(fmt(%9.2f))") ///
    noobs nonum collabels(Count Mean "Std. Dev." "5th Pct." Median "95th Pct.") ///
    title(": Summary Statistics for Direct Lenders") label

save "$cleandir/final_regression_sample.dta", replace
export delimited "$cleandir/final_regression_sample.csv", replace

	 
/**************
	Figure 1 & 2 (Number of Deals by Bank, Private Credit, and Other Nonbank Direct Lender)
	***************/
	* Done in Python

/**************
	Determinant Tables
	***************/

	use "$cleandir/final_regression_sample.dta", clear

*** Determinant Regressions (Table III)

local borr_vars "ln_assets prev_ebitda_dummy scaled_ebitda debt_to_ebitda leverage market_to_book last_year_rnd_intensity tangibility rolling_12m_return rolling_12m_vol"
local deal_vars "ln_amount maturity clean_rate term_loan secured_dummy"

local y_vars "nonbank_lender private_credit_lender other_nonbank_lender" 
local info_vars "monthly_fs projected_fs lender_meeting info_n all_info"


eststo: reghdfe nonbank_lender `var' `borr_vars', absorb(ff_12 year) vce(cluster gvkey)
eststo: reghdfe private_credit_lender `var' `borr_vars' if other_nonbank_lender == 0, absorb(ff_12 year) vce(cluster gvkey)
eststo: reghdfe other_nonbank_lender `var' `borr_vars' if private_credit_lender == 0, absorb(ff_12 year) vce(cluster gvkey)

esttab using "$tabdir/Table3_treatment.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' `borr_vars' `deal_vars', absorb(ff_12 year) vce(cluster gvkey)
}
esttab using "$tabdir/Table3_infocov.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear

/**************
	Main Regression Table 
	***************/
	
	use "$cleandir/final_regression_sample.dta", clear

local borr_vars "ln_assets prev_ebitda_dummy scaled_ebitda debt_to_ebitda leverage market_to_book last_year_rnd_intensity tangibility rolling_12m_return rolling_12m_vol"
local deal_vars "ln_amount maturity clean_rate term_loan secured_dummy"

local y_vars "nonbank_lender private_credit_lender other_nonbank_lender" 
local info_vars "monthly_fs projected_fs lender_meeting info_n all_info"
	
*** Main (Table IV-All (all nonbank lenders))

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' nonbank_lender `borr_vars' `deal_vars', absorb(ff_12 year) vce(cluster gvkey)
}
esttab using "$tabdir/Table4_main_regression_all.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant

esttab using "$tabdir/Table4_main_regression_all_noctrl.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant ///
keep(nonbank_lender)

* clear storeed est
eststo clear

*** Main (Table IV)

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' private_credit_lender `borr_vars' `deal_vars' if other_nonbank_lender == 0, absorb(ff_12 year) vce(cluster gvkey)
}
esttab using "$tabdir/Table4_main_regression_pc.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant

esttab using "$tabdir/Table4_main_regression_pc_noctrl.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant ///
keep(private_credit_lender)

* clear storeed est
eststo clear

*** Main (Table IV-1 (for other nonbanks))

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' other_nonbank_lender `borr_vars' `deal_vars' if private_credit_lender == 0, absorb(ff_12 year) vce(cluster gvkey)
}
esttab using "$tabdir/Table4_main_regression_other.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant

esttab using "$tabdir/Table4_main_regression_other_noctrl.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant ///
keep(other_nonbank_lender)

* clear storeed est
eststo clear

*** Main regression with nonbank_lender and private_credit_lender in the same table

	use "$cleandir/final_regression_sample.dta", clear

local borr_vars "ln_assets prev_ebitda_dummy scaled_ebitda debt_to_ebitda leverage market_to_book last_year_rnd_intensity tangibility rolling_12m_return rolling_12m_vol"
local deal_vars "ln_amount maturity clean_rate term_loan secured_dummy"

local y_vars "nonbank_lender private_credit_lender other_nonbank_lender" 
local info_vars "monthly_fs projected_fs lender_meeting info_n all_info"	

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' nonbank_lender nonbank_pc_inter `borr_vars' `deal_vars', absorb(ff_12 year) vce(cluster gvkey)
	*test nonbank_lender + nonbank_pc_inter = 0 
}
esttab using "$tabdir/Table4_main_regression.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant

esttab using "$tabdir/Table4_main_regression_noctrl.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant ///
keep(nonbank_lender nonbank_pc_inter)

* clear storeed est
eststo clear

*** Main regression with firm fixed effects
foreach var of varlist `info_vars' {
	eststo: reghdfe `var' private_credit_lender `borr_vars' `deal_vars', absorb(gvkey year) vce(cluster gvkey)
}
esttab using "$tabdir/Table4_main_regression_gvkey_all.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant

esttab using "$tabdir/Table4_main_regression_gvkey_all_noctrl.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant ///
keep(private_credit_lender)

* clear storeed est
eststo clear

/**************
	Robustness Table for Main Effects (monthly_fs lender_meeting all_info)
	***************/
	
use "$cleandir/final_regression_sample.dta", clear

local borr_vars "ln_assets prev_ebitda_dummy scaled_ebitda debt_to_ebitda leverage market_to_book last_year_rnd_intensity tangibility rolling_12m_return rolling_12m_vol"
local deal_vars "ln_amount maturity clean_rate term_loan secured_dummy"
local y_vars "nonbank_lender private_credit_lender other_nonbank_lender" 
local info_vars "monthly_fs projected_fs lender_meeting info_n all_info"

corr `borr_vars' `deal_vars' `y_vars' `info_vars'

eststo: reghdfe monthly_fs nonbank_lender `deal_vars', absorb(gvkey year) vce(cluster gvkey)
eststo: reghdfe monthly_fs nonbank_lender `borr_vars' `deal_vars', absorb(gvkey year) vce(cluster gvkey)
eststo: reghdfe projected_fs nonbank_lender `deal_vars', absorb(gvkey year) vce(cluster gvkey)

eststo: reghdfe projected_fs nonbank_lender `borr_vars' `deal_vars', absorb(gvkey year) vce(cluster gvkey)

eststo: reghdfe lender_meeting nonbank_lender `deal_vars', absorb(gvkey year) vce(cluster gvkey)

eststo: reghdfe lender_meeting nonbank_lender `borr_vars' `deal_vars', absorb(gvkey year) vce(cluster gvkey)

eststo: reghdfe all_info nonbank_lender `deal_vars', absorb(gvkey year) vce(cluster gvkey)

eststo: reghdfe all_info nonbank_lender `borr_vars' `deal_vars', absorb(gvkey year) vce(cluster gvkey)
esttab using "$tabdir/main_regression_robustness_original.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear


eststo: reghdfe monthly_fs nonbank_lender nonbank_pc_inter `deal_vars', absorb(gvkey year) vce(cluster gvkey)
test nonbank_lender + nonbank_pc_inter = 0

eststo: reghdfe monthly_fs nonbank_lender nonbank_pc_inter `borr_vars' `deal_vars', absorb(gvkey year) vce(cluster gvkey)
test nonbank_lender + nonbank_pc_inter = 0

eststo: reghdfe projected_fs nonbank_lender nonbank_pc_inter `deal_vars', absorb(gvkey year) vce(cluster gvkey)
test nonbank_lender + nonbank_pc_inter = 0

eststo: reghdfe projected_fs nonbank_lender nonbank_pc_inter `borr_vars' `deal_vars', absorb(gvkey year) vce(cluster gvkey)
test nonbank_lender + nonbank_pc_inter = 0

eststo: reghdfe lender_meeting nonbank_lender nonbank_pc_inter `deal_vars', absorb(gvkey year) vce(cluster gvkey)
test nonbank_lender + nonbank_pc_inter = 0

eststo: reghdfe lender_meeting nonbank_lender nonbank_pc_inter `borr_vars' `deal_vars', absorb(gvkey year) vce(cluster gvkey)
test nonbank_lender + nonbank_pc_inter = 0

eststo: reghdfe all_info nonbank_lender nonbank_pc_inter `deal_vars', absorb(gvkey year) vce(cluster gvkey)
test nonbank_lender + nonbank_pc_inter = 0

eststo: reghdfe all_info nonbank_lender nonbank_pc_inter `borr_vars' `deal_vars', absorb(gvkey year) vce(cluster gvkey)
test nonbank_lender + nonbank_pc_inter = 0
esttab using "$tabdir/main_regression_robustness.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant

esttab using "$tabdir/main_regression_robustness_noctrl.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant ///
keep(nonbank_lender nonbank_pc_inter)

* clear storeed est
eststo clear

/**************
	Propensity Score Matching
	***************/

	use "$cleandir/final_regression_sample.dta", clear

local borr_vars "ln_assets prev_ebitda_dummy scaled_ebitda debt_to_ebitda leverage market_to_book last_year_rnd_intensity tangibility rolling_12m_return rolling_12m_vol"
local deal_vars "ln_amount maturity clean_rate term_loan secured_dummy"
local y_vars "nonbank_lender private_credit_lender other_nonbank_lender" 
local info_vars "monthly_fs projected_fs lender_meeting info_n all_info"

eststo clear
foreach var of local info_vars {
    psmatch2 nonbank_lender `borr_vars' i.ff_12 `deal_vars', out(`var') logit n(1) ai(3) common caliper(0.01)
    
    // Add ATT estimate and standard error to eststo
    eststo: estadd scalar ATT = r(att)
    estadd scalar SE_ATT = r(seatt)
}

// Export the results to LaTeX
esttab using "$tabdir/Table5_main_psm_all.tex", replace ///
    obslast nodepvars nomti nonum collabels(none) label ///
    b(3) se(3) parentheses star(* 0.10 ** 0.05 *** 0.01) ///
    plain lines fragment noconstant
eststo clear

pstest `borr_vars' `deal_vars', graph both graphregion(color(white)) bgcolor(white) label
graph export "$figdir/Figure5_psm_all.pdf", replace

foreach var of varlist `info_vars' {
	eststo: psmatch2 private_credit_lender `borr_vars' i.ff_12 `deal_vars', out(`var') logit n(1) ai(3) common caliper(0.01)
}
estadd scalar r(att)
estadd scalar r(seatt)
esttab using "$tabdir/Table5_main_psm_pc.tex", replace obslast nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses star(* 0.10 ** 0.05 *** 0.01) plain lines fragment noconstant
eststo clear

pstest `borr_vars' `deal_vars', graph both graphregion(color(white)) bgcolor(white) label
graph export "$figdir/Figure5_psm_pc.pdf", replace

foreach var of varlist `info_vars' {
	eststo: psmatch2 other_nonbank_lender `borr_vars' i.ff_12 `deal_vars', out(`var') logit n(1) ai(3) common caliper(0.01)
}
estadd scalar r(att)
estadd scalar r(seatt)
esttab using "$tabdir/Table5_main_psm_other.tex", replace obslast nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses star(* 0.10 ** 0.05 *** 0.01) plain lines fragment noconstant
eststo clear

pstest `borr_vars' `deal_vars', graph both graphregion(color(white)) bgcolor(white) label
graph export "$figdir/Figure5_psm_other.pdf", replace

/**************
	Regression Discontinuity Design Around prev_ebitda == 0
	***************/
	use "$cleandir/final_regression_sample.dta", clear
	
drop if secured_dummy != 1
drop if prev_ebitda < 0

* check for binds around debt_to_ebitda_gr6
egen debt_to_ebitda_bins = cut(debt_to_ebitda), at(6 8 10 12 14)
preserve
	collapse (mean) nonbank_lender (count) gvkey, by(debt_to_ebitda_bins)
	scatter nonbank_lender debt_to_ebitda_bins
	*scatter gvkey debt_to_ebitda_bins
restore

rddensity debt_to_ebitda, c(6) plot
rdrobust nonbank_lender debt_to_ebitda, c(6)
rdrobust monthly_fs debt_to_ebitda, c(6) fuzzy(nonbank_lender)

	use "$cleandir/final_regression_sample.dta", clear
	drop if secured_dummy != 1

* check for share of nonbank loans for over prev_ebitda
egen prev_ebitda_bins = cut(last_year_ebitda), at(-50 -20 -10 -5 -1 5 10 20 50)
preserve
	collapse (mean) nonbank_lender (count) gvkey, by(prev_ebitda_bins)
	scatter nonbank_lender prev_ebitda_bins
	*scatter gvkey prev_ebitda_bins
restore

histogram last_year_ebitda, bin(50) normal kdensity freq

rddensity last_year_ebitda, c(0) plot

rdrobust nonbank_lender last_year_ebitda, c(0)
rdrobust monthly_fs last_year_ebitda, c(0) fuzzy(nonbank_lender)

/**************
	DiD
	***************/
	
local borr_vars "ln_assets prev_ebitda_dummy scaled_ebitda debt_to_ebitda leverage market_to_book last_year_rnd_intensity tangibility rolling_12m_return rolling_12m_vol"
local deal_vars "ln_amount maturity clean_rate term_loan secured_dummy"
local y_vars "nonbank_lender private_credit_lender other_nonbank_lender" 
local info_vars "monthly_fs projected_fs lender_meeting info_n all_info"
*** in 2018 from SNC increase from $20 to $100 million	
use "$cleandir/final_regression_sample.dta", clear
drop if secured_dummy != 1
*** generate treat and post variables 

gen post = 1 if year > 2013
replace post = 0 if post == .

gen treat_post = debt_to_ebitda_gr6 * post

*TWFE
reghdfe nonbank_lender debt_to_ebitda_gr6 treat_post `deal_vars' ln_assets leverage, absorb(year ff_12)
*/
reghdfe projected_fs debt_to_ebitda_gr6 treat_post `deal_vars' ln_assets leverage, absorb(year ff_12)
*** in 2013 SNC guideline says that firms with <0 ebitda is substandard -> after 2013 firms
*	with < 0 ebitda is less likely to borrow from banks and more likely to borrow from PCs
* 	Treat: < EBITDA, Post: 2014 onward
use "$cleandir/final_regression_sample.dta", clear

*** generate treat and post variables 

gen treat = 1 if prev_ebitda_dummy == 1
replace treat = 0 if treat == .

gen post = 1 if year >= 2014
replace post = 0 if post == .

gen treat_post = treat * post

* Year-Indutry FE with Treat and Treat_Post
reghdfe nonbank_lender treat post treat_post `deal_vars', absorb(year ff_12)
reghdfe monthly_fs treat post treat_post `deal_vars', absorb(year ff_12)

* Year-Indutry FE with Treat and Treat_Post
la var treat_post "Treat*Post"

eststo: reghdfe nonbank_lender treat_post `deal_vars' `borr_vars', absorb(year ff_12)

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' treat treat_post `deal_vars' `borr_vars', absorb(year ff_12)
}
esttab using "$tabdir/Table6_main_did.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear

/**************
	Cross-Sectional Tests
	***************/
	use "$cleandir/final_regression_sample.dta", clear

local borr_vars "ln_assets prev_ebitda_dummy scaled_ebitda debt_to_ebitda leverage market_to_book last_year_rnd_intensity tangibility rolling_12m_return rolling_12m_vol"
local deal_vars "ln_amount maturity clean_rate term_loan secured_dummy"
local y_vars "nonbank_lender private_credit_lender other_nonbank_lender" 
local info_vars "monthly_fs projected_fs lender_meeting info_n all_info"

*** Large Versus Small Direct Lenders Lenders
	use "$cleandir/final_regression_sample.dta", replace
	
* generate large and small Direct Lenders lenders	
bysort clean_lead_arranger: gen deal_count = _N
tab deal_count if nonbank_lender == 1	
egen median_deal_count_nonbank = median(deal_count) if nonbank_lender == 1

gen big_nonbank = 1 if nonbank_lender == 1 & deal_count >= 4
replace big_nonbank = 0 if big_nonbank ==. & nonbank_lender == 1

* generate big_bank == 1 if it's ont of the g-sibs

* get rid of . 
replace clean_lead_arranger = subinstr(clean_lead_arranger, ".", "", .)

gen big_bank = 1 if strpos(clean_lead_arranger, "bank of america") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "bank of america") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "bofa securities") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "citibank") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "citigroup") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "citicorp") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "credit suisse") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "deutsche") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "jp morgan") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "jpmorgan") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "merrill lynch") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "morgan stanley") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "wells fargo") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "goldman sachs") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "ubs") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "pnc") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "us bank") > 0
replace big_bank = 1 if strpos(clean_lead_arranger, "barclays") > 0
replace big_bank = 0 if big_bank == . & nonbank_lender == 0

egen big = rowmax(big_bank big_nonbank)

la var big_bank "Large Bank"
la var big_nonbank "Large Nonbank"
la var big "Large Lender"

gen big_nonbank_inter = big * nonbank_lender
replace big_nonbank_inter = 0 if big_nonbank_inter == .
la var big_nonbank_inter "Nonbank Lender x Large"

gen big_nonbank_pc_inter = big_nonbank_inter * nonbank_pc_inter
replace big_nonbank_pc_inter = 0 if big_nonbank_pc_inter == .
la var big_nonbank_pc_inter "Nonbank Lender x Private Credit x Large"

*** Dec 12 Update: Regression with bank and nonbank + nonbank*big
	foreach var of varlist `info_vars' {
		eststo: reghdfe `var' nonbank_lender nonbank_pc_inter big_nonbank_inter big_nonbank_pc_inter `borr_vars' `deal_vars', absorb(year ff_12) vce(cluster gvkey)
	}
	esttab using "$tabdir/Table7.tex", replace ///
	nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
	star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
	
	esttab using "$tabdir/Table7_noctrl.tex", replace ///
	nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
	star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant ///
	keep(nonbank_lender nonbank_pc_inter big_nonbank_inter big_nonbank_pc_inter)
	* clear storeed est
	eststo clear

*** Same/Different Industry (Specialization)

use "$cleandir/final_regression_sample.dta", clear
recast str100 clean_lead_arranger
save "$cleandir/final_regression_sample.dta", replace

* generate industry specialization from deals in the sample
bysort clean_lead_arranger ff_12: gen dealcounts_ff_12 = _N
keep clean_lead_arranger dealcounts_ff_12 ff_12 
duplicates drop 

bysort clean_lead_arranger (dealcounts_ff_12): egen rank_dealcounts_ff_12 = rank(dealcounts_ff_12), field
order clean_lead_arranger dealcounts_ff_12 rank_dealcounts_ff_12

* merge back to final_regression_sample
merge 1:m clean_lead_arranger ff_12 using "$cleandir/final_regression_sample.dta", nogen

* generate top 3 industry = 1 if industry rank is <= 3 for banks
gen topthree_industry = 1 if rank_dealcounts_ff_12 <= 3 & nonbank_lender == 0
replace topthree_industry = 0 if topthree_industry == .

local borr_vars "ln_assets prev_ebitda_dummy scaled_ebitda debt_to_ebitda leverage market_to_book last_year_rnd_intensity tangibility rolling_12m_return rolling_12m_vol"
local deal_vars "ln_amount maturity clean_rate term_loan secured_dummy"
local y_vars "nonbank_lender private_credit_lender other_nonbank_lender" 
local info_vars "monthly_fs projected_fs lender_meeting info_n all_info"

*** different industry from expertise
* change industry to ff_12
forvalues i = 1/3 {
	gen industry`i'_12 = 6 if industry_`i' == "Business Services" | industry_`i' == "Information Technology"
	replace industry`i'_12 = 1 if industry_`i' == "Consumer Discretionary"
	replace industry`i'_12 = 4 if industry_`i' == "Energy & Utilities"
	replace industry`i'_12 = 11 if industry_`i' == "Financial & Insurance Services"
	replace industry`i'_12 = 10 if industry_`i' == "Healthcare"
	replace industry`i'_12 = 3 if industry_`i' == "Industrials"
	replace industry`i'_12 = 4 if industry_`i' == "Raw Materials & Natural Resources"
}
order industry* ff_12

gen same_industry = 1 if industry1_12 == ff_12 
replace same_industry = 1 if industry2_12 == ff_12
replace same_industry = 1 if industry3_12 == ff_12
replace same_industry = 1 if rank_dealcounts_ff_12 == 1

replace topthree_industry = 1 if topthree_industry == 0 & same_industry == 1

gen inter = nonbank_lender * topthree_industry
gen inter_pc = private_credit_lender * topthree_industry

*la var topthree_industry "Top 3 Industry"
la var topthree_industry "Top 3 Industry"
la var inter "Nonbank Lender x Same Industry"
la var inter_pc "Nonbank Lender x Private Credit x Same Industry"

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' nonbank_lender nonbank_pc_inter inter inter_pc `borr_vars' `deal_vars', absorb(year ff_12) vce(cluster gvkey)
}
esttab using "$tabdir/Table8_industry.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant

esttab using "$tabdir/Table8_industry_noctrl.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant ///
keep(nonbank_lender nonbank_pc_inter inter inter_pc)

* clear storeed est
eststo clear


********************************************************************************
*log close 
