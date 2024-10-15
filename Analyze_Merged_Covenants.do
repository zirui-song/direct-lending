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
* generate maturity
replace maturity1 = "" if maturity1 == "NA"
gen maturity_date = date(maturity1, "YMD")
gen start_date = date(date, "YMD")
gen maturity_year = (maturity_date - start_date)/365
drop if maturity_year < 0
* generate margin 
gen margin_bps = interest_spread1*100

* winsorize at 1%-99%s
winsor2 atq-leverage prev_ebitda deal_amount1 maturity_year margin_bps, cuts(1 99) replace
sum lender_is_nonbank lender_is_private_credit_entity monthly_fs-lender_meeting atq-leverage prev_ebitda deal_amount1 maturity_year margin_bps

* generate FF-12 Industry
sicff sich, ind(12)

label define ff_12_lab 1 "Consumer NonDurables" 2 "Consumer Durables" 3 "Manufacturing" 4 "Oil, Gas, and Coal Extraction and Products" 5 "Chemicals and Allied Products" 6 "Business Equipment" 7 "Telephone and Television Transmission" 8 "Utilities" 9 "Wholesale, Retail, and Some Services" 10 "Healthcare, Medical Equipment, and Drugs" 11 "Finance" 12 "Other"
label values ff_12 ff_12_lab

*** Table 1: Descriptives *** 

* Panel A (Number of Deals Per Year by banks and nonbanks)
tab year lender_is_nonbank
tab year lender_is_private_credit_entity

* Panel B (Number of Deals By Industry)
egen proportion_nonbank = mean(lender_is_nonbank), by(ff_12)
egen proportion_private_credit = mean(lender_is_private_credit_entity), by(ff_12)

tab ff_12 lender_is_nonbank, matcell(freq) matrow(ff_12_lab) matcol(lender_is_nonbank)
esttab matrix(freq) using "$tabdir/tabulation_ff12_nonbank.tex", ///
    replace label nomtitles nonumbers nostar cells(b) varwidth(30)
	
tab ff_12 lender_is_private_credit_entity

* Panel C (Summary Statistics by Banks and Nonbanks Respectively)
estpost sum monthly_fs-lender_meeting atq-leverage prev_ebitda if lender_is_nonbank == 0, de
esttab using "$tabdir/summary_table_by_bank.tex", ///
    cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) p25(fmt(2)) p50(fmt(2)) p75(fmt(2)) max(fmt(2))") label replace title("Summary Statistics for Banks") nonumbers noobs
estpost sum monthly_fs-lender_meeting atq-leverage prev_ebitda if lender_is_nonbank == 1, de
esttab using "$tabdir/summary_table_by_nonbank.tex", ///
    cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) p25(fmt(2)) p50(fmt(2)) p75(fmt(2)) max(fmt(2))") label replace title("Summary Statistics for Nonbanks") nonumbers noobs

estpost sum monthly_fs-lender_meeting atq-leverage prev_ebitda if lender_is_private_credit_entity == 1, de
esttab using "$tabdir/summary_table_by_private_credit.tex", ///
    cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) p25(fmt(2)) p50(fmt(2)) p75(fmt(2)) max(fmt(2))") label replace title("Summary Statistics for Private Credit Lenders") nonumbers noobs


*** Table 2: Borrowers with both ***
gsort gvkey year
order lender_is_nonbank coname gvkey year date lead_arranger atq revtq prev_ebitda ff_12 monthly_fs-lender_meeting

/**************
	Determinant Tables
	***************/
	
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


/**************
	Main Regression Table 
	***************/

local borr_vars "prev_ebitda atq roa leverage"
local deal_vars "deal_amount1 maturity_year margin_bps"	

* keep only those with floating rates
keep if interest_type == "floating"

reg margin_bps lender_is_private_credit_entity `borr_vars' deal_amount1 maturity_year i.ff_12 i.year, vce(cluster year)

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

* keep only those with prev_ebitda <= +- 25
* keep if abs(prev_ebitda) < 50
histogram prev_ebitda if abs(prev_ebitda) < 50 , bin(100) normal kdensity freq

rddensity prev_ebitda, c(0) plot
rdrobust lender_is_nonbank prev_ebitda, c(0) h(25)
rdrobust margin_bps prev_ebitda, c(0) fuzzy(lender_is_nonbank)

/**************
	DiD in 2018 from SNC increase from $20 to $100 million
	***************/
	
hist deal_amount1, freq
hist deal_amount1 if deal_amount1 < 100, freq

*** generate treat and post variables 

gen treat = 1 if inrange(deal_amount1, 20, 100)
replace treat = 0 if treat == .

gen post = 1 if year >= 2018
replace post = 0 if post == .

gen treat_post = treat * post

*** TWFE

reghdfe lender_is_nonbank treat_post, absorb(year gvkey)

	