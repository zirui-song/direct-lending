/* This file uses the all_merged_cleaned.csv to analyze and output regression and summary tables
*/

*** Generate a list of globals (path) for future references
global repodir "/Users/zrsong/Dropbox (MIT)/Research Projects/Direct Lending"
global datadir "$repodir/Data"
global rawdir "$datadir/Raw"
global intdir "$datadir/Intermediate"
global cleandir "$datadir/Cleaned"
*global tabdir "$repodir/Results/Tables"
global tabdir "/Users/zrsong/Dropbox (MIT)/Apps/Overleaf/Information Covenants of Direct Lending/Tables"
global figdir "/Users/zrsong/Dropbox (MIT)/Apps/Overleaf/Information Covenants of Direct Lending/Figures"
global logdir "$repodir/Code/LogFiles"

*log using "$logdir/Analyze_Merged_Covenants.log", text replace

/**************
	Data Cleaning
	***************/

import delimited "$cleandir/agreements_mm_dealinfo.csv", clear
keep if year >= 2010
destring atq-prev_ebitda sic deal_amount1 interest_spread1 lender_is_private_credit, replace ignore("NA")
drop if atq >= .
* generate maturity
replace maturity1 = "" if maturity1 == "NA"
gen maturity_date = date(maturity1, "YMD")
gen start_date = date(date, "YMD")
gen maturity_year = (maturity_date - start_date)/365
drop if maturity_year < 0

* use libor3 to convert floating rates to fixed rates
replace interest_spread1 = interest_spread1 + rate if interest_type == "floating"
gen floating = 1 if interest_type == "floating"
replace floating = 0 if floating == .

* generate margin 
gen margin_bps = interest_spread1*100

* winsorize at 1%-99%s
winsor2 atq-leverage prev_ebitda deal_amount1 interest_spread1 maturity_year margin_bps , cuts(1 99) replace
sum lender_is_nonbank lender_is_private_credit_entity monthly_fs-lender_meeting atq-leverage prev_ebitda deal_amount1 maturity_year margin_bps

* generate FF-12 Industry
sicff sic, ind(12)

label define ff_12_lab 1 "Consumer NonDurables" 2 "Consumer Durables" 3 "Manufacturing" 4 "Oil, Gas, and Coal Extraction and Products" 5 "Chemicals and Allied Products" 6 "Business Equipment" 7 "Telephone and Television Transmission" 8 "Utilities" 9 "Wholesale, Retail, and Some Services" 10 "Healthcare, Medical Equipment, and Drugs" 11 "Finance" 12 "Other"
label values ff_12 ff_12_lab

egen hard_info = rowmax(monthly_fs projected_fs)
gen info_n = monthly_fs + projected_fs + lender_meeting
egen all_info = rowmin(monthly_fs projected_fs lender_meeting)

gen lender_is_other_nonbank = 1 if lender_is_nonbank == 1 & lender_is_private_credit != 1
replace lender_is_other_nonbank = 0 if lender_is_nonbank == 0

gen scaled_ebitda = prev_ebitda/atq
la var scaled_ebitda "EBITDA (Scaled)"

gen ln_atq = log(atq)
la var ln_atq "Ln(Total Assets)"
	
gen prev_ebitda_dummy = 1 if prev_ebitda < 0 
replace prev_ebitda_dummy = 0 if prev_ebitda_dummy == .
la var prev_ebitda_dummy "EBITDA < 0"

gen debt_to_ebitda = ltq/prev_ebitda
replace debt_to_ebitda = 0 if debt_to_ebitda < 0 /// according to Chernenko et al 2022

* winsorize at 5 and 95% according to chernenko et al
winsor2 debt_to_ebitda, cuts(5 95) replace

gen debt_to_ebitda_gr6 = 1 if debt_to_ebitda > 6
replace debt_to_ebitda_gr6 = 1 if debt_to_ebitda == 0
replace debt_to_ebitda_gr6 = 0 if debt_to_ebitda_gr6 == .

export delimited "$intdir/full_sample_without_int.csv", replace
save "$intdir/full_sample_without_int.dta", replace


drop if deal_amount1 == .

save "$intdir/intermediate_data_after_main_regression.dta", replace

*** manually fill in missing maturity + interest spread for private credit deals
preserve
	keep if lender_is_private_credit == 1
	keep if maturity_date == . | interest_spread1 == .
	keep accession type_filing type_attachment deal_amount* interest_spread* maturity*
	export delimited "$intdir/manual_check_dl_deals.csv", replace
restore

* merge the private credit manual added observations back 
import delimited "$intdir/manual_checked_dl_deals.csv", clear
drop v10 v11

merge 1:1 accession type_filing type_attachment using "$intdir/intermediate_data_after_main_regression.dta", nogen

drop if maturity_year == .
drop if interest_spread1 == .

*** drop obs with missing ebitda, tangibility
drop if prev_ebitda == . | ppegtq == .

save "$intdir/intermediate_data_after_main_regression.dta", replace

*** New Nov 6th 
keep accession-type_attachment
export delimited "$intdir/final_regression_sample_filings.csv", replace
*sample 1
*export delimited "$intdir/final_regression_sample_filings_sample.csv", replace

use "$intdir/intermediate_data_after_main_regression.dta", clear

*** merge newly cleaned datasets to final data
import delimited "$datadir/LoansFull/loancontracts_with_other_dealinfo.csv", varnames(1) clear
merge 1:1 accession type_filing type_attachment using "$intdir/intermediate_data_after_main_regression.dta", nogen

gen secured1 = 1 if secured != "Not Found"
replace secured1 = 0 if secured1 == .

gen second_lien1 = 1 if second_lien == "Yes" | second_lien == "Second Lien"
replace second_lien1 = 0 if second_lien1 == . 

gen senior1 = 1 if senior != "Not Found"
replace senior1 = 0 if senior1 == .

gen asset_based1 = 1 if asset_based != "Not Found"
replace asset_based1 = 0 if asset_based1 == .

drop secured second_lien senior asset_based

save "$intdir/intermediate_data_after_main_regression.dta", replace

*** merge with abl and second lien classification using textual extraction instead (more conservative)
import delimited "$intdir/abl_secondlien.csv", varnames(1) clear
merge 1:1 accession type_filing type_attachment using "$intdir/intermediate_data_after_main_regression.dta", nogen

save "$intdir/intermediate_data_after_main_regression.dta", replace

/**************
	Summary Table
	***************/

*** Table 1: Descriptives *** 

la var lender_is_other_nonbank "Other Nonbank Lender"
la var lender_is_nonbank "Nonbank Lender"
la var lender_is_private_credit "Private Credit Lender"
la var monthly_fs "Monthly Financial Statement"
la var projected_fs "Annual Budget/Projection"
la var lender_meeting "Lender Meeting"
la var atq "Total Assets (Million USD)"
la var revtq "Revenue (Million USD)"
la var niq "Net Income (Million USD)"
la var ibq "Income Before Extraordinary Items (Million USD)"
la var ltq "Long-Term Debt (Million USD)"
la var xrdq "R\&D Intensity"
la var ppegtq "Tangibility"
la var roa "ROA"
la var leverage "Leverage Ratio"
la var prev_ebitda "EBITDA (Million USD)"
la var maturity_year "Maturity (Years)"
la var margin_bps "Floating Interest Margin (Basis Points)"
la var deal_amount1 "Deal Amount (Million USD)"
la var interest_spread1 "Interest Margin (Percentage)"
la var floating "Floating Rate"
la var asset_based "Asset Based Loan"
la var second_lien "Second Lien"

* Panel A (Number of Deals By Industry)
dtable i.ff_12, by(lender_is_nonbank) export("$tabdir/tabulation_ff12_all.tex", tableonly replace) note("Panel A: Number of Deals By Industry")
dtable i.ff_12, by(lender_is_private_credit) export("$tabdir/tabulation_ff12_pc.tex", tableonly replace) note("Panel A: Number of Deals By Industry")
dtable i.ff_12, by(lender_is_other_nonbank) export("$tabdir/tabulation_ff12_other.tex", tableonly replace) note("Panel A: Number of Deals By Industry")

local all_borr_cov "atq revtq niq roa leverage prev_ebitda xrdq ppegtq"
local all_deal_vars "deal_amount1 maturity_year interest_spread1 floating asset_based second_lien" 	 

* Panel B (Summary Statistics by Banks and Nonbanks Respectively)
estpost sum monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if lender_is_nonbank == 0, de
esttab using "$tabdir/summary_table_by_bank.tex", replace ///
    cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) p50(fmt(%9.2f)) max(fmt(%9.2f))") noobs nonum collabels(Count Mean "Std. Dev." Min Median Max) title(": Summary Statistics for Banks") label
	
estpost sum monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if lender_is_nonbank == 1, de
esttab using "$tabdir/summary_table_by_nonbank.tex", replace ///
    cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) p50(fmt(%9.2f)) max(fmt(%9.2f))") noobs nonum collabels(Count Mean "Std. Dev." Min Median Max) title(": Summary Statistics for All Nonbank Lenders") label
	
estpost sum monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if lender_is_other_nonbank == 1, de
esttab using "$tabdir/summary_table_by_other_nonbank.tex", replace ///
     cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) p50(fmt(%9.2f))  max(fmt(%9.2f))") noobs nonum collabels(Count Mean "Std. Dev." Min Median Max) title(": Summary Statistics for Other Nonbank Lenders") label

estpost sum monthly_fs-lender_meeting `all_borr_cov' `all_deal_vars' if lender_is_private_credit == 1, de
esttab using "$tabdir/summary_table_by_private_credit.tex", replace ///
     cells("count(fmt(%9.0fc)) mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) p50(fmt(%9.2f))  max(fmt(%9.2f))") noobs nonum collabels(Count Mean "Std. Dev." Min Median Max) title(": Summary Statistics for Direct Lenders") label

/**************
	Figure 1 & 2 (Number of Deals by Bank, Private Credit, and Other Nonbank Direct Lender)
	***************/
	
	use "$intdir/intermediate_data_after_main_regression.dta", clear

gen lender_type = lender_is_private_credit + 1
replace lender_type = 3 if lender_type == .
	
collapse (count) deal_amount1 (mean) monthly_fs projected_fs lender_meeting, by(year lender_type)
label define lender_type_lab 1 "Bank" 2 "Private Credit" 3 "Other Nonbank Lender"
label values lender_type lender_type_lab

reshape wide deal_amount1 monthly_fs-lender_meeting, i(year) j(lender_type)

tsset year

* generate shares of deals 
gen count = deal_amount11+deal_amount12+deal_amount13
gen share1 = deal_amount11/count
gen share2 = deal_amount12/count
gen share3 = deal_amount13/count

*** Figure 1: Number of Deals (in sample) by Year
twoway (tsline deal_amount11, lcolor(blue) lpattern(solid)) ///
       (tsline deal_amount12, lcolor(red) lpattern(dash)) ///
       (tsline deal_amount13, lcolor(green) lpattern(longdash)) ///
       , ///
       title("Number of Deals by Lender Types Over Time") ///
       legend(order(1 "Bank" 2 "Private Credit" 3 "Other Nonbank Lender") position(1) ring(0)) ///
       ytitle("Deal Count") xtitle("Year") ///
       xlabel(2010(2)2023) ///
       graphregion(color(white)) plotregion(color(white)) 
	   
graph export "$figdir/Figure1.pdf", replace

*** Figure 1A: Share of Deals (in sample) by year
twoway (tsline share2, lcolor(red) lpattern(dash)) ///
       (tsline share3, lcolor(green) lpattern(longdash)) ///
       , ///
       title("Share of Deals by Lender Types Over Time") ///
       legend(order(1 "Private Credit" 2 "Other Nonbank Lender") position(1) ring(0)) ///
       ytitle("Deal Count") xtitle("Year") ///
       xlabel(2010(2)2023) ///
       graphregion(color(white)) plotregion(color(white)) 
	   
graph export "$figdir/Figure1A.pdf", replace
	   
*** Figure 2: Usage of different information covenants:
* Monthly FS
twoway (tsline monthly_fs1, lcolor(blue) lpattern(solid)) ///
       (tsline monthly_fs2, lcolor(red) lpattern(dash)) ///
       (tsline monthly_fs3, lcolor(green) lpattern(longdash)) ///
       , ///
       title("Use of Monthly Financial Statement Covenant by Lender Types") ///
       legend(order(1 "Bank" 2 "Private Credit" 3 "Other Nonbank Lender") position(1) ring(0)) ///
       ytitle("Frequency") xtitle("Year") ///
       xlabel(2010(2)2023) ///
       graphregion(color(white)) plotregion(color(white))
	   
graph export "$figdir/Figure2_monthlyfs.pdf", replace
	   
* Projected FS
twoway (tsline projected_fs1, lcolor(blue) lpattern(solid)) ///
       (tsline projected_fs2, lcolor(red) lpattern(dash)) ///
       (tsline projected_fs3, lcolor(green) lpattern(longdash)) ///
       , ///
       title("Use of Annual Budget/Project Covenant by Lender Types") ///
       legend(order(1 "Bank" 2 "Private Credit" 3 "Other Nonbank Lender") position(1) ring(0)) ///
       ytitle("Frequency") xtitle("Year") ///
       xlabel(2010(2)2023) ///
       graphregion(color(white)) plotregion(color(white))
	   
graph export "$figdir/Figure2_projectedfs.pdf", replace

* Lender Meeting
twoway (tsline lender_meeting1, lcolor(blue) lpattern(solid)) ///
       (tsline lender_meeting2, lcolor(red) lpattern(dash)) ///
       (tsline lender_meeting3, lcolor(green) lpattern(longdash)) ///
       , ///
       title("Use of Lender Meeting Covenant by Lender Types") ///
       legend(order(1 "Bank" 2 "Private Credit" 3 "Other Nonbank Lender") position(1) ring(0)) ///
       ytitle("Frequency") xtitle("Year") ///
       xlabel(2010(2)2023) ///
       graphregion(color(white)) plotregion(color(white))

graph export "$figdir/Figure2_lendermeeting.pdf", replace

/**************
	Determinant Tables
	***************/
	
use "$intdir/intermediate_data_after_main_regression.dta", clear

la var lender_is_nonbank "Nonbank Lender"
la var lender_is_other_nonbank "Other Nonbank Lender"
la var lender_is_private_credit "Private Credit Lender"
la var monthly_fs "Monthly Financial Statement"
la var projected_fs "Annual Budget/Projection"
la var lender_meeting "Lender Meeting"
la var atq "Total Assets (Million USD)"
la var revtq "Revenue (Million USD)"
la var niq "Net Income (Million USD)"
la var ibq "Income Before Extraordinary Items (Million USD)"
la var ltq "Long-Term Debt (Million USD)"
la var xrdq "R\&D Intensity"
la var ppegtq "Tangibility"
la var roa "ROA"
la var leverage "Leverage Ratio"
la var prev_ebitda "EBITDA (Million USD)"
la var maturity_year "Maturity (Years)"
la var margin_bps "Floating Interest Margin (Basis Points)"
la var deal_amount1 "Deal Amount (Million USD)"
la var interest_spread1 "Interest Margin (Percentage)"
la var floating "Floating Rate"
la var asset_based "Asset Based Loan"

gen ln_deal_amount = ln(deal_amount1)
la var ln_deal_amount "Ln(Deal Amount)"

save "$cleandir/final_regression_sample.dta", replace
export delimited "$cleandir/final_regression_sample.csv", replace

*** Determinant Regressions (Table III)

local borr_vars "scaled_ebitda ln_atq roa leverage"
local deal_vars "ln_deal_amount maturity_year interest_spread1"	
local y_vars "lender_is_nonbank lender_is_private_credit lender_is_other_nonbank" 
local info_vars "monthly_fs projected_fs lender_meeting hard_info info_n all_info"
	
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
	
*** Main (Table IV-All (all nonbank lenders))

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' lender_is_nonbank `borr_vars' `deal_vars', absorb(ff_12 year)
}
esttab using "$tabdir/Table4_main_regression_all.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear

*** Main (Table IV)

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' lender_is_private_credit `borr_vars' `deal_vars', absorb(ff_12 year)
}
esttab using "$tabdir/Table4_main_regression_pc.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear

*** Main (Table IV-1 (for other nonbanks))

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' lender_is_other_nonbank `borr_vars' `deal_vars', absorb(ff_12 year)
}
esttab using "$tabdir/Table4_main_regression_other.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear

*** Main regression with lender_is_nonbank and lender_is_private_credit in the same table

gen nonbank_pc_inter = lender_is_nonbank * lender_is_private_credit
replace nonbank_pc_inter = 0 if nonbank_pc_inter == .
la var nonbank_pc_inter "Nonbank Lender * Private Credit"

local borr_vars "scaled_ebitda ln_atq roa leverage"
local deal_vars "ln_deal_amount maturity_year interest_spread1"	
local y_vars "lender_is_nonbank lender_is_private_credit" 
local info_vars "monthly_fs projected_fs lender_meeting hard_info info_n all_info"

foreach var of varlist `info_vars' {
	eststo: reghdfe `var' lender_is_nonbank nonbank_pc_inter `borr_vars' `deal_vars', absorb(ff_12 year)
}
esttab using "$tabdir/Table4_main_regression.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear

*** Propensity Score Matching

	use "$cleandir/final_regression_sample.dta", clear

foreach var of varlist `info_vars' {
	eststo: psmatch2 lender_is_nonbank `borr_vars' i.ff_12 `deal_vars', out(`var') logit n(1) ai(3) common caliper(0.01)
}
estadd scalar r(att)
estadd scalar r(seatt)
esttab using "$tabdir/Table5_main_psm_all.tex", replace obslast nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses star(* 0.10 ** 0.05 *** 0.01) plain lines fragment noconstant
eststo clear

pstest `borr_vars' `deal_vars', graph both graphregion(color(white)) bgcolor(white)
graph export "$figdir/Figure5_psm_all.pdf", replace

foreach var of varlist `info_vars' {
	eststo: psmatch2 lender_is_private_credit `borr_vars' i.ff_12 `deal_vars', out(`var') logit n(1) ai(3) common caliper(0.01)
}
estadd scalar r(att)
estadd scalar r(seatt)
esttab using "$tabdir/Table5_main_psm_pc.tex", replace obslast nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses star(* 0.10 ** 0.05 *** 0.01) plain lines fragment noconstant
eststo clear

pstest `borr_vars' `deal_vars', graph both graphregion(color(white)) bgcolor(white)
graph export "$figdir/Figure5_psm_pc.pdf", replace

foreach var of varlist `info_vars' {
	eststo: psmatch2 lender_is_other_nonbank `borr_vars' i.ff_12 `deal_vars', out(`var') logit n(1) ai(3) common caliper(0.01)
}
estadd scalar r(att)
estadd scalar r(seatt)
esttab using "$tabdir/Table5_main_psm_other.tex", replace obslast nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses star(* 0.10 ** 0.05 *** 0.01) plain lines fragment noconstant
eststo clear

pstest `borr_vars' `deal_vars', graph both graphregion(color(white)) bgcolor(white)
graph export "$figdir/Figure5_psm_other.pdf", replace

/**************
	Regression Discontinuity Design Around prev_ebitda == 0
	***************/
	use "$cleandir/final_regression_sample.dta", clear
	use "$intdir/full_sample_without_int.dta", clear

	
*drop if asset_based == 1
*drop if debt_to_ebitda_gr6 == 1

* check for binds around debt_to_ebitda_gr6
egen debt_to_ebitda_bins = cut(debt_to_ebitda), at(4 6 8 10 15 20 25 30 35 40)
preserve
	collapse (mean) lender_is_nonbank (count) gvkey, by(debt_to_ebitda_bins)
	scatter lender_is_nonbank debt_to_ebitda_bins
	*scatter gvkey debt_to_ebitda_bins
restore

rddensity debt_to_ebitda, c(4) plot
rdrobust margin_bps debt_to_ebitda, c(4) fuzzy(lender_is_nonbank)

	use "$cleandir/final_regression_sample.dta", clear
	use "$intdir/full_sample_without_int.dta", clear
	drop if debt_to_ebitda <= 4 & debt_to_ebitda > 0
	
* check for share of nonbank loans for over prev_ebitda
egen prev_ebitda_bins = cut(prev_ebitda), at(-50 -20 -10 -5 -1 5 10 20 50)
preserve
	collapse (mean) lender_is_nonbank (count) gvkey, by(prev_ebitda_bins)
	scatter lender_is_nonbank prev_ebitda_bins
	*scatter gvkey prev_ebitda_bins
restore

histogram prev_ebitda, bin(50) normal kdensity freq

rddensity prev_ebitda, c(0) plot

rdrobust lender_is_nonbank prev_ebitda, c(0)
rdrobust monthly_fs prev_ebitda, c(0) fuzzy(lender_is_private_credit)

/**************
	DiD
	***************/
	
*** in 2018 from SNC increase from $20 to $100 million	
use "$cleandir/final_regression_sample.dta", clear
drop if asset_based == 1

*** generate treat and post variables 

gen post = 1 if year > 2013
replace post = 0 if post == .

*keep if debt_to_ebitda > 4 | debt_to_ebitda == 0
gen treat = 1 if debt_to_ebitda > 4 | debt_to_ebitda == 0
replace treat = 0 if treat == .

gen treat_post = treat * post

*TWFE
reghdfe lender_is_nonbank post deal_amount1 maturity_year interest_spread1 ln_atq leverage, absorb(ff_12)
*/
reghdfe monthly_fs post deal_amount1 maturity_year interest_spread1 ln_atq leverage, absorb(ff_12)

ivregress 2sls monthly_fs deal_amount1 maturity_year interest_spread1 ln_atq leverage i.ff_12 (lender_is_nonbank = post)
*** in 2013 SNC guideline says that firms with <0 ebitda is substandard -> after 2013 firms
*	with < 0 ebitda is less likely to borrow from banks and more likely to borrow from PCs
* 	Treat: < EBITDA, Post: 2014 onward
use "$cleandir/final_regression_sample.dta", clear

*** generate treat and post variables 

gen treat = 1 if prev_ebitda < 0
replace treat = 0 if treat == .

gen post = 1 if year >= 2014
replace post = 0 if post == .

gen treat_post = treat * post

* Year-Indutry FE with Treat and Treat_Post
reghdfe lender_is_nonbank treat post treat_post maturity_year interest_spread1, absorb(year ff_12)
reghdfe monthly_fs treat post treat_post maturity_year interest_spread1, absorb(year ff_12)

* Year-Indutry FE with Treat and Treat_Post
la var treat_post "Treat*Post"
local borr_vars "scaled_ebitda ln_atq roa leverage"
local deal_vars "ln_deal_amount maturity_year interest_spread1"	
local info_vars "monthly_fs projected_fs lender_meeting hard_info info_n all_info"
eststo: reghdfe lender_is_private_credit treat_post `deal_vars', absorb(year gvkey)
foreach var of varlist `info_vars' {
	eststo: reghdfe `var' treat_post `deal_vars', absorb(year gvkey)
}
esttab using "$tabdir/Table6_main_did.tex", replace ///
nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
* clear storeed est
eststo clear

* fuzzy DID
*sort treat year
*by treat year: egen mean_D = mean(lender_is_private_credit)
*by treat: g lag_mean_D = mean_D[_n-1] if treat == treat[_n-1] & year-1==year[_n-1]
*g G_T = sign(mean_D - lag_mean_D)
*g G_Tplus1 = G_T[_n+1] if treat==treat[_n+1] & year+1 == year[_n+1]
*replace G_T = 0 if G_T == .
*replace G_Tplus1 = 0 if G_Tplus1 == .
*fuzzydid monthly_fs treat year lender_is_private_credit, did


/**************
	Cross-Sectional Tests
	***************/
	
local borr_vars "scaled_ebitda ln_atq roa leverage"
local deal_vars "ln_deal_amount maturity_year interest_spread1"	
local y_vars "lender_is_nonbank lender_is_private_credit" 
local info_vars "monthly_fs projected_fs lender_meeting hard_info info_n all_info"

*** Large Versus Small Direct Lenders Lenders
	use "$cleandir/final_regression_sample.dta", replace
	
* generate large and small Direct Lenders lenders	
bysort lead_arranger: gen deal_count = _N
tab deal_count if lender_is_nonbank == 1	
gen big_nonbank = 1 if lender_is_nonbank == 1 & deal_count >= 5
replace big_nonbank = 0 if big_nonbank ==. & lender_is_nonbank == 1

* generate big_bank == 1 if it's ont of the g-sibs
* get rid of . 
replace lead_arranger = subinstr(lead_arranger, ".", "", .)

gen big_bank = 1 if strpos(lead_arranger, "bank of america") > 0
replace big_bank = 1 if strpos(lead_arranger, "bank of america") > 0
replace big_bank = 1 if strpos(lead_arranger, "bofa securities") > 0
replace big_bank = 1 if strpos(lead_arranger, "citibank") > 0
replace big_bank = 1 if strpos(lead_arranger, "citigroup") > 0
replace big_bank = 1 if strpos(lead_arranger, "citicorp") > 0
replace big_bank = 1 if strpos(lead_arranger, "credit suisse") > 0
replace big_bank = 1 if strpos(lead_arranger, "deutsche") > 0
replace big_bank = 1 if strpos(lead_arranger, "jp morgan") > 0
replace big_bank = 1 if strpos(lead_arranger, "jpmorgan") > 0
replace big_bank = 1 if strpos(lead_arranger, "merrill lynch") > 0
replace big_bank = 1 if strpos(lead_arranger, "morgan stanley") > 0
replace big_bank = 1 if strpos(lead_arranger, "wells fargo") > 0
replace big_bank = 1 if strpos(lead_arranger, "goldman sachs") > 0
replace big_bank = 0 if big_bank == . & lender_is_nonbank == 0

la var big_bank "Big Bank"
la var big_nonbank "Big Nonbank"
gen big_nonbank_inter = big_nonbank * lender_is_nonbank
replace big_nonbank_inter = 0 if big_nonbank_inter == .
la var big_nonbank_inter "Lender is Nonbank * Big"

*** Nov 12 Update: Regression with bank and nonbank + nonbank*big_nonbank
	foreach var of varlist `info_vars' {
		eststo: reghdfe `var' lender_is_nonbank big_nonbank_inter `borr_vars' `deal_vars', absorb(year ff_12)
	}
	esttab using "$tabdir/Table7.tex", replace ///
	nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
	star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
	* clear storeed est
	eststo clear

preserve 
	keep if lender_is_nonbank == 0
	tab lead_arranger lender_meeting
	tab lead_arranger big_bank
	* regression	
	local borr_vars "scaled_ebitda ln_atq roa leverage"
	local deal_vars "ln_deal_amount maturity_year interest_spread1"	
	local y_vars "lender_is_nonbank lender_is_private_credit" 
	local info_vars "monthly_fs projected_fs lender_meeting hard_info info_n all_info"
	order lead_arranger `info_vars' `borr_vars' `deal_vars'
	gsort lead_arranger
	foreach var of varlist `info_vars' {
		eststo: reghdfe `var' big_bank `borr_vars' `deal_vars', absorb(year ff_12)
	}
	esttab using "$tabdir/Table7_bank.tex", replace ///
	nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
	star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
	* clear storeed est
	eststo clear
restore

*** Same/Different Industry (Specialization)

use "$cleandir/final_regression_sample.dta", clear

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
	replace same_industry = 0 if same_industry == .
	
	gen inter = lender_is_nonbank * same_industry
	
	la var same_industry "Same Industry"
	la var inter "Lender is Nonbank X Same Industry"
	
	foreach var of varlist `info_vars' {
		eststo: reghdfe `var' lender_is_nonbank inter `borr_vars' `deal_vars', absorb(year ff_12)
	}
	esttab using "$tabdir/Table8_industry.tex", replace ///
	nodepvars nomti nonum collabels(none) label b(3) se(3) parentheses ///
	star(* 0.10 ** 0.05 *** 0.01) ar2 plain lines fragment noconstant
	* clear storeed est
	eststo clear
	

********************************************************************************
log close 
