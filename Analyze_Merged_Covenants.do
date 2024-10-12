/* This file uses the all_merged_cleaned.csv to analyze and output regression and summary tables
*/

*** Generate a list of globals (path) for future references
global repodir "/Users/zrsong/Dropbox (MIT)/Research Projects/Direct Lending"
global datadir "$repodir/Data"
global rawdir "$datadir/Raw"
global intdir "$datadir/Intermediate"
global cleandir "$datadir/Cleaned"
global outdir "$repodir/Results"

/**************
	Summary Table
	***************/

import delimited "$cleandir/agreements_mm.csv", clear
keep if year >= 2010
destring atq-prev_ebitda sich, replace ignore("NA")
*replace margin_bps = margin_bps/100 if margin_bps > 10000
*replace maturity = 0 if maturity < 0 

gen lender_is_nonbank = 1 if lender_is_bank == 0
replace lender_is_nonbank = 0 if lender_is_bank == 1

* winsorize atq-leverage at 1%-99%s
winsor2 atq-leverage prev_ebitda, cuts(1 99) replace

* generate FF-12 Industry
sicff sich, ind(12)

label define ff_12_lab 1 "Consumer NonDurables" 2 "Consumer Durables" 3 "Manufacturing" 4 "Energy" 5 "Chemicals" 6 "Business Equipment" 7 "Telecommunication" 8 "Utilities" 9 "Shops" 10 "Healthcare" 11 "Finance" 12 "Other"
label values ff_12 ff_12_lab

*** Table 1: Descriptives *** 

* Panel A (Number of Deals Per Year by banks and nonbanks)
tab year lender_is_nonbank
tab year lender_is_private_credit_entity

* Panel B (Number of Deals By Industry)
tab ff_12 lender_is_nonbank

* Panel C (Summary Statistics by Banks and Nonbanks Respectively)
by lender_is_nonbank, sort: summarize monthly_fs-lender_meeting
by lender_is_nonbank, sort: summarize atq-leverage prev_ebitda

*** Table 2: Borrowers with both ***
gsort gvkey year
order lender_is_nonbank coname gvkey year date lead_arranger atq revtq prev_ebitda ff_12 monthly_fs-lender_meeting

/**************
	Determinant Tables
	***************/
	
gen prev_ebitda_dummy = 1 if prev_ebitda > 0 
replace prev_ebitda_dummy = 0 if prev_ebitda_dummy == .
	
reg lender_is_nonbank prev_ebitda_dummy prev_ebitda atq roa leverage i.ff_12 i.year

reg monthly_fs prev_ebitda atq roa leverage i.ff_12 i.year
reg projected_fs prev_ebitda atq roa leverage i.ff_12 i.year
reg lender_meeting prev_ebitda atq roa leverage i.ff_12 i.year

egen hard_info = rowmax(monthly_fs projected_fs)
egen back_info = rowmax(monthly_fs lender_meeting)
egen all_info = rowmin(monthly_fs projected_fs lender_meeting)

reg hard_info prev_ebitda atq roa leverage i.ff_12 i.year
reg back_info prev_ebitda atq roa leverage i.ff_12 i.year
reg all_info prev_ebitda atq roa leverage i.ff_12 i.year

/**************
	Main Regression Table 
	***************/

reg monthly_fs lender_is_nonbank prev_ebitda atq roa leverage i.ff_12 i.year, vce(cluster year)
reg projected_fs lender_is_nonbank prev_ebitda atq roa leverage i.ff_12 i.year, vce(cluster year)
reg lender_meeting lender_is_nonbank prev_ebitda atq roa leverage i.ff_12 i.year, vce(cluster year)

reg hard_info lender_is_nonbank prev_ebitda atq roa leverage i.ff_12 i.year, vce(cluster year)
reg back_info lender_is_nonbank prev_ebitda atq roa leverage i.ff_12 i.year, vce(cluster year)
reg all_info lender_is_nonbank prev_ebitda atq roa leverage i.ff_12 i.year, vce(cluster year)

/**************
	Regression Discontinuity Design Around prev_ebitda == 0
	***************/
	
* check for share of nonbank loans for over prev_ebitda
egen prev_ebitda_bins = cut(prev_ebitda), at(-50 -20 -10 -1 10 20 50 100 200)
preserve
	collapse (mean) lender_is_nonbank (count) gvkey, by(prev_ebitda_bins)
	scatter lender_is_nonbank prev_ebitda_bins
	scatter gvkey prev_ebitda_bins
restore

* keep only those with prev_ebitda <= +- 25
keep if abs(prev_ebitda) < 50
histogram prev_ebitda, bin(100) normal kdensity

rddensity prev_ebitda, c(0) plot
rdrobust lender_is_nonbank prev_ebitda, c(0)
