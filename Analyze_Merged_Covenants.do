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

import delimited "$cleandir/all_merged_deals.csv", clear
keep if year >= 2010
destring atq-leverage maturity margin_bps, replace ignore("NA")
replace margin_bps = margin_bps/100 if margin_bps > 10000
replace maturity = 0 if maturity < 0 

* winsorize atq-leverage at 1%-99%s
winsor2 deal_amount margin_bps maturity atq-leverage, cuts(1 99)

* Panel A (Number of Deals Per Year by banks and nonbanks)
tab year lender_is_bank
esttab using "deals_summary.txt", label nodepvar replace

* Panel B (Summary Statistics by Banks and Nonbanks Respectively)
by lender_is_bank, sort: summarize deal_amount margin_bps maturity monthly_fs-lender_meeting
by lender_is_bank, sort: summarize atq-leverage

/**************
	Main Regression Table 
	***************/

reg monthly_fs lender_is_bank deal_amount margin_bps maturity atq roa leverage i.year
reg projected_fs lender_is_bank deal_amount margin_bps maturity atq roa leverage i.year
reg lender_meeting lender_is_bank deal_amount margin_bps maturity atq roa leverage i.year

egen hard_info = rowmax(monthly_fs projected_fs)

reg hard_info lender_is_bank deal_amount margin_bps maturity atq roa leverage i.year


/**************
	Regression Discontinuity Design Around EBITDA == 0
	***************/

	keep if abs(niq) <= 50
	hist niq
