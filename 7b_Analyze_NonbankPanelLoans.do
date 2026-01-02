/*
7b_Analyze_NonbankPanelLoans.do

Analyze the relationship between spread and information covenants for nonbank loans.

This script examines how information covenant requirements relate to loan spreads
in the nonbank lending market, controlling for borrower characteristics and loan terms.

Inputs:
- ../Data/Intermediate/6f_CleanNonbankPanel.csv (cleaned nonbank panel with covenant data)

Output:
- Regression results saved to ../Results/Tables/
- Log file saved to ../Code/LogFiles/

Author: Zirui Song
Date: Oct 2025
*/

*** Set up paths
global repodir "/Users/zrsong/MIT Dropbox/Zirui Song/Research Projects/PSW_Nonbank Direct Lending"
global datadir "$repodir/Data"
global intdir "$datadir/Intermediate"
global cleandir "$datadir/Cleaned"
global tabdir "$repodir/Results/Tables"
global logdir "$repodir/Code/LogFiles"

*** Start log
log using "$logdir/7b_Analyze_NonbankPanelLoans.log", text replace

/**************
	Data Preparation
	***************/

*** Load cleaned nonbank panel with covenant data and firm financials
import delimited "$intdir/6f_CleanNonbankPanel.csv", clear bindquote(strict)

*** Generate spread variable from clean_interest_spread
gen spread = clean_interest_spread

*** Generate FF12 industry classification from SIC codes using sicff command
sicff sic, ind(12) gen(ff12)

*** Generate all_info_covenants dummy (1 if all three covenants are present)
gen all_info_covenants = (monthly_fs == 1 & projected_fs == 1 & lender_meeting == 1)
replace all_info_covenants = 0 if missing(all_info_covenants)

*** Define control variable lists
local borr_vars "log_at_lag1 leverage_lag1 tangibility_lag1 profitability_lag1 ret_buy_and_hold ret_vol"
local loan_vars "maturity_months facility_amount"

*** Summary statistics
summarize spread monthly_fs projected_fs lender_meeting total_info_covenants all_info_covenants
summarize `borr_vars' `loan_vars'

/**************
	Correlation Table
	***************/

*** Correlation matrix
correlate spread monthly_fs projected_fs lender_meeting total_info_covenants all_info_covenants, means
matrix C = r(C)
putexcel set "$tabdir/7b_CorrelationTable.xlsx", replace
putexcel A1 = "Correlation Matrix - Nonbank Loans"
putexcel A2 = matrix(C), names

/**************
	Spread Sensitivity to Information Covenants (Table 1)
	***************/

*** Monthly FS - No controls
reghdfe spread monthly_fs, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", replace excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, NO, Loan Controls, NO) ///
	keep(monthly_fs)

*** Monthly FS - Borrower controls
reghdfe spread monthly_fs `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, NO) ///
	keep(monthly_fs)

*** Monthly FS - Borrower + Loan controls
reghdfe spread monthly_fs `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, YES) ///
	keep(monthly_fs)

*** Projected FS - No controls
reghdfe spread projected_fs, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, NO, Loan Controls, NO) ///
	keep(projected_fs)

*** Projected FS - Borrower controls
reghdfe spread projected_fs `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, NO) ///
	keep(projected_fs)

*** Projected FS - Borrower + Loan controls
reghdfe spread projected_fs `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, YES) ///
	keep(projected_fs)

*** Lender Meeting - No controls
reghdfe spread lender_meeting, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, NO, Loan Controls, NO) ///
	keep(lender_meeting)

*** Lender Meeting - Borrower controls
reghdfe spread lender_meeting `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, NO) ///
	keep(lender_meeting)

*** Lender Meeting - Borrower + Loan controls
reghdfe spread lender_meeting `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, YES) ///
	keep(lender_meeting)

*** Total Info Cov - No controls
reghdfe spread total_info_covenants, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, NO, Loan Controls, NO) ///
	keep(total_info_covenants)

*** Total Info Cov - Borrower controls
reghdfe spread total_info_covenants `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, NO) ///
	keep(total_info_covenants)

*** Total Info Cov - Borrower + Loan controls
reghdfe spread total_info_covenants `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, YES) ///
	keep(total_info_covenants)

*** All Info Cov - No controls
reghdfe spread all_info_covenants, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, NO, Loan Controls, NO) ///
	keep(all_info_covenants)

*** All Info Cov - Borrower controls
reghdfe spread all_info_covenants `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, NO) ///
	keep(all_info_covenants)

*** All Info Cov - Borrower + Loan controls
reghdfe spread all_info_covenants `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7b_Table1_SpreadRegressions.xls", append excel ///
	ctitle("Spread") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, YES) ///
	keep(all_info_covenants)

/**************
	Subsample Analysis by Lender Type (Table 2)
	***************/

*** Check if lender_type_detail exists
capture confirm variable lender_type_detail

if _rc == 0 {
	*** BDC subsample - Monthly FS
	preserve
	keep if lender_type_detail == "BDC"
	
	reghdfe spread monthly_fs, absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", replace excel ///
		ctitle("Spread (BDC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, BDC, Borrower Controls, NO, Loan Controls, NO) ///
		keep(monthly_fs)
	
	reghdfe spread monthly_fs `borr_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (BDC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, BDC, Borrower Controls, YES, Loan Controls, NO) ///
		keep(monthly_fs)
	
	reghdfe spread monthly_fs `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (BDC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, BDC, Borrower Controls, YES, Loan Controls, YES) ///
		keep(monthly_fs)
	
	*** BDC subsample - Projected FS
	reghdfe spread projected_fs, absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (BDC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, BDC, Borrower Controls, NO, Loan Controls, NO) ///
		keep(projected_fs)
	
	reghdfe spread projected_fs `borr_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (BDC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, BDC, Borrower Controls, YES, Loan Controls, NO) ///
		keep(projected_fs)
	
	reghdfe spread projected_fs `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (BDC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, BDC, Borrower Controls, YES, Loan Controls, YES) ///
		keep(projected_fs)
	
	*** BDC subsample - Lender Meeting
	reghdfe spread lender_meeting, absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (BDC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, BDC, Borrower Controls, NO, Loan Controls, NO) ///
		keep(lender_meeting)
	
	reghdfe spread lender_meeting `borr_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (BDC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, BDC, Borrower Controls, YES, Loan Controls, NO) ///
		keep(lender_meeting)
	
	reghdfe spread lender_meeting `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (BDC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, BDC, Borrower Controls, YES, Loan Controls, YES) ///
		keep(lender_meeting)
	
	restore
	
	*** Private Credit subsample - Monthly FS
	preserve
	keep if lender_type_detail == "Private Credit"
	
	reghdfe spread monthly_fs, absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (PC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, Private Credit, Borrower Controls, NO, Loan Controls, NO) ///
		keep(monthly_fs)
	
	reghdfe spread monthly_fs `borr_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (PC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, Private Credit, Borrower Controls, YES, Loan Controls, NO) ///
		keep(monthly_fs)
	
	reghdfe spread monthly_fs `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (PC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, Private Credit, Borrower Controls, YES, Loan Controls, YES) ///
		keep(monthly_fs)
	
	*** Private Credit subsample - Projected FS
	reghdfe spread projected_fs, absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (PC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, Private Credit, Borrower Controls, NO, Loan Controls, NO) ///
		keep(projected_fs)
	
	reghdfe spread projected_fs `borr_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (PC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, Private Credit, Borrower Controls, YES, Loan Controls, NO) ///
		keep(projected_fs)
	
	reghdfe spread projected_fs `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (PC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, Private Credit, Borrower Controls, YES, Loan Controls, YES) ///
		keep(projected_fs)
	
	*** Private Credit subsample - Lender Meeting
	reghdfe spread lender_meeting, absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (PC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, Private Credit, Borrower Controls, NO, Loan Controls, NO) ///
		keep(lender_meeting)
	
	reghdfe spread lender_meeting `borr_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (PC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, Private Credit, Borrower Controls, YES, Loan Controls, NO) ///
		keep(lender_meeting)
	
	reghdfe spread lender_meeting `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
	outreg2 using "$tabdir/7b_Table2_SubsampleAnalysis.xls", append excel ///
		ctitle("Spread (PC)") label dec(3) ///
		addtext(Industry FE, YES, Year FE, YES, Sample, Private Credit, Borrower Controls, YES, Loan Controls, YES) ///
		keep(lender_meeting)
	
	restore
}
else {
	display "Warning: lender_type_detail variable not found, skipping subsample analysis"
}

*** End log
log close

