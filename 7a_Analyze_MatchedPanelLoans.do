/*
7a_Analyze_MatchedPanelLoans.do

Analyze panel loans with regression models following the format of 
Analyze_Merged_Covenants_New.do:

1) Run regressions of information covenants on the LHS and nonbank_lender dummy 
   as the main X variable, with industry and year fixed effects
2) Run clean_interest_rate on the LHS and information covenants on the RHS 
   for subsamples of bank and nonbank loans

Inputs:
- ../Data/Intermediate/6b_PanelWithInfoCovenants.csv (panel with covenant data)

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
log using "$logdir/7a_Analyze_MatchedPanelLoans.log", text replace

/**************
	Data Preparation
	***************/

*** Load matched panel with covenant data
import delimited "$intdir/6e_CleanMatchedPanel.csv", clear

gen nonbank_lender = (lender_type == "nonbank")
replace nonbank_lender = 0 if missing(nonbank_lender)

*** Generate all_info_covenants dummy (1 if all three covenants are present)
gen all_info_covenants = (monthly_fs == 1 & projected_fs == 1 & lender_meeting == 1)
replace all_info_covenants = 0 if missing(all_info_covenants)

local borr_vars "log_at_lag1 leverage_lag1 tangibility_lag1 profitability_lag1 ret_buy_and_hold ret_vol"
local loan_vars "maturity_months facility_amount"

/**************
	Covenant Regressions (Table 1)
	***************/

*** Monthly financial statements - No controls
reghdfe monthly_fs nonbank_lender, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", replace excel ///
	ctitle("Monthly FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, NO, Loan Controls, NO) ///
	keep(nonbank_lender)

*** Monthly financial statements - Borrower controls
reghdfe monthly_fs nonbank_lender `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("Monthly FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, NO) ///
	keep(nonbank_lender `borr_vars')

*** Monthly financial statements - Borrower + Loan controls
reghdfe monthly_fs nonbank_lender `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("Monthly FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, YES) ///
	keep(nonbank_lender `borr_vars' `loan_vars')

*** Projected financial statements - No controls
reghdfe projected_fs nonbank_lender, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("Projected FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, NO, Loan Controls, NO) ///
	keep(nonbank_lender)

*** Projected financial statements - Borrower controls
reghdfe projected_fs nonbank_lender `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("Projected FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, NO) ///
	keep(nonbank_lender `borr_vars')

*** Projected financial statements - Borrower + Loan controls
reghdfe projected_fs nonbank_lender `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("Projected FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, YES) ///
	keep(nonbank_lender `borr_vars' `loan_vars')

*** Lender meetings - No controls
reghdfe lender_meeting nonbank_lender, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("Lender Meeting") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, NO, Loan Controls, NO) ///
	keep(nonbank_lender)

*** Lender meetings - Borrower controls
reghdfe lender_meeting nonbank_lender `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("Lender Meeting") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, NO) ///
	keep(nonbank_lender `borr_vars')

*** Lender meetings - Borrower + Loan controls
reghdfe lender_meeting nonbank_lender `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("Lender Meeting") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, YES) ///
	keep(nonbank_lender `borr_vars' `loan_vars')

*** Total information covenants - No controls
reghdfe total_info_covenants nonbank_lender, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("Total Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, NO, Loan Controls, NO) ///
	keep(nonbank_lender)

*** Total information covenants - Borrower controls
reghdfe total_info_covenants nonbank_lender `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("Total Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, NO) ///
	keep(nonbank_lender `borr_vars')

*** Total information covenants - Borrower + Loan controls
reghdfe total_info_covenants nonbank_lender `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("Total Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, YES) ///
	keep(nonbank_lender `borr_vars' `loan_vars')

*** All information covenants - No controls
reghdfe all_info_covenants nonbank_lender, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("All Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, NO, Loan Controls, NO) ///
	keep(nonbank_lender)

*** All information covenants - Borrower controls
reghdfe all_info_covenants nonbank_lender `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("All Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, NO) ///
	keep(nonbank_lender `borr_vars')

*** All information covenants - Borrower + Loan controls
reghdfe all_info_covenants nonbank_lender `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table1_CovenantRegressions.xls", append excel ///
	ctitle("All Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Borrower Controls, YES, Loan Controls, YES) ///
	keep(nonbank_lender `borr_vars' `loan_vars')

/**************
	Correlation Tables
	***************/

*** Correlation table for nonbank loans
preserve
keep if nonbank_lender == 1
correlate spread monthly_fs projected_fs lender_meeting total_info_covenants all_info_covenants, means
matrix C_nonbank = r(C)
putexcel set "$tabdir/7a_CorrelationTables.xlsx", sheet("Nonbank") replace
putexcel A1 = "Correlation Matrix - Nonbank Loans"
putexcel A2 = matrix(C_nonbank), names
restore

*** Correlation table for bank loans
preserve
keep if nonbank_lender == 0
correlate spread monthly_fs projected_fs lender_meeting total_info_covenants all_info_covenants, means
matrix C_bank = r(C)
putexcel set "$tabdir/7a_CorrelationTables.xlsx", sheet("Bank") modify
putexcel A1 = "Correlation Matrix - Bank Loans"
putexcel A2 = matrix(C_bank), names
restore

/**************
	Spread Sensitivity to Information Covenants (Table 2)
	***************/

*** Monthly FS - Nonbank - No controls
reghdfe spread monthly_fs if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", replace excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, NO, Loan Controls, NO) ///
	keep(monthly_fs)

*** Monthly FS - Nonbank - Borrower controls
reghdfe spread monthly_fs `borr_vars' if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, YES, Loan Controls, NO) ///
	keep(monthly_fs)

*** Monthly FS - Nonbank - Borrower + Loan controls
reghdfe spread monthly_fs `borr_vars' `loan_vars' if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, YES, Loan Controls, YES) ///
	keep(monthly_fs)

*** Monthly FS - Bank - No controls
reghdfe spread monthly_fs if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, NO, Loan Controls, NO) ///
	keep(monthly_fs)

*** Monthly FS - Bank - Borrower controls
reghdfe spread monthly_fs `borr_vars' if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, YES, Loan Controls, NO) ///
	keep(monthly_fs)

*** Monthly FS - Bank - Borrower + Loan controls
reghdfe spread monthly_fs `borr_vars' `loan_vars' if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, YES, Loan Controls, YES) ///
	keep(monthly_fs)

*** Projected FS - Nonbank - No controls
reghdfe spread projected_fs if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, NO, Loan Controls, NO) ///
	keep(projected_fs)

*** Projected FS - Nonbank - Borrower controls
reghdfe spread projected_fs `borr_vars' if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, YES, Loan Controls, NO) ///
	keep(projected_fs)

*** Projected FS - Nonbank - Borrower + Loan controls
reghdfe spread projected_fs `borr_vars' `loan_vars' if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, YES, Loan Controls, YES) ///
	keep(projected_fs)

*** Projected FS - Bank - No controls
reghdfe spread projected_fs if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, NO, Loan Controls, NO) ///
	keep(projected_fs)

*** Projected FS - Bank - Borrower controls
reghdfe spread projected_fs `borr_vars' if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, YES, Loan Controls, NO) ///
	keep(projected_fs)

*** Projected FS - Bank - Borrower + Loan controls
reghdfe spread projected_fs `borr_vars' `loan_vars' if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, YES, Loan Controls, YES) ///
	keep(projected_fs)

*** Lender Meeting - Nonbank - No controls
reghdfe spread lender_meeting if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, NO, Loan Controls, NO) ///
	keep(lender_meeting)

*** Lender Meeting - Nonbank - Borrower controls
reghdfe spread lender_meeting `borr_vars' if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, YES, Loan Controls, NO) ///
	keep(lender_meeting)

*** Lender Meeting - Nonbank - Borrower + Loan controls
reghdfe spread lender_meeting `borr_vars' `loan_vars' if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, YES, Loan Controls, YES) ///
	keep(lender_meeting)

*** Lender Meeting - Bank - No controls
reghdfe spread lender_meeting if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, NO, Loan Controls, NO) ///
	keep(lender_meeting)

*** Lender Meeting - Bank - Borrower controls
reghdfe spread lender_meeting `borr_vars' if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, YES, Loan Controls, NO) ///
	keep(lender_meeting)

*** Lender Meeting - Bank - Borrower + Loan controls
reghdfe spread lender_meeting `borr_vars' `loan_vars' if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, YES, Loan Controls, YES) ///
	keep(lender_meeting)

*** Total Info Cov - Nonbank - No controls
reghdfe spread total_info_covenants if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, NO, Loan Controls, NO) ///
	keep(total_info_covenants)

*** Total Info Cov - Nonbank - Borrower controls
reghdfe spread total_info_covenants `borr_vars' if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, YES, Loan Controls, NO) ///
	keep(total_info_covenants)

*** Total Info Cov - Nonbank - Borrower + Loan controls
reghdfe spread total_info_covenants `borr_vars' `loan_vars' if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, YES, Loan Controls, YES) ///
	keep(total_info_covenants)

*** Total Info Cov - Bank - No controls
reghdfe spread total_info_covenants if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, NO, Loan Controls, NO) ///
	keep(total_info_covenants)

*** Total Info Cov - Bank - Borrower controls
reghdfe spread total_info_covenants `borr_vars' if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, YES, Loan Controls, NO) ///
	keep(total_info_covenants)

*** Total Info Cov - Bank - Borrower + Loan controls
reghdfe spread total_info_covenants `borr_vars' `loan_vars' if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, YES, Loan Controls, YES) ///
	keep(total_info_covenants)

*** All Info Cov - Nonbank - No controls
reghdfe spread all_info_covenants if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, NO, Loan Controls, NO) ///
	keep(all_info_covenants)

*** All Info Cov - Nonbank - Borrower controls
reghdfe spread all_info_covenants `borr_vars' if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, YES, Loan Controls, NO) ///
	keep(all_info_covenants)

*** All Info Cov - Nonbank - Borrower + Loan controls
reghdfe spread all_info_covenants `borr_vars' `loan_vars' if nonbank_lender == 1, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (NB)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Nonbank, Borrower Controls, YES, Loan Controls, YES) ///
	keep(all_info_covenants)

*** All Info Cov - Bank - No controls
reghdfe spread all_info_covenants if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, NO, Loan Controls, NO) ///
	keep(all_info_covenants)

*** All Info Cov - Bank - Borrower controls
reghdfe spread all_info_covenants `borr_vars' if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, YES, Loan Controls, NO) ///
	keep(all_info_covenants)

*** All Info Cov - Bank - Borrower + Loan controls
reghdfe spread all_info_covenants `borr_vars' `loan_vars' if nonbank_lender == 0, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table2_SpreadRegressions.xls", append excel ///
	ctitle("Spread (Bank)") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Sample, Bank, Borrower Controls, YES, Loan Controls, YES) ///
	keep(all_info_covenants)

/**************
	Statistical Tests: Coefficient Differences Between Nonbank and Bank (Table 3)
	***************/

*** Create interaction terms for testing coefficient differences
gen monthly_fs_x_nb = monthly_fs * nonbank_lender
gen projected_fs_x_nb = projected_fs * nonbank_lender
gen lender_meeting_x_nb = lender_meeting * nonbank_lender
gen total_info_cov_x_nb = total_info_covenants * nonbank_lender
gen all_info_cov_x_nb = all_info_covenants * nonbank_lender

*** Test 1: Monthly FS - No controls
reghdfe spread monthly_fs monthly_fs_x_nb, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", replace excel ///
	ctitle("Monthly FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, NO) ///
	keep(monthly_fs monthly_fs_x_nb)

*** Test 2: Monthly FS - Borrower controls
reghdfe spread monthly_fs monthly_fs_x_nb `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("Monthly FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, Borrower) ///
	keep(monthly_fs monthly_fs_x_nb)

*** Test 3: Monthly FS - Borrower + Loan controls
reghdfe spread monthly_fs monthly_fs_x_nb `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("Monthly FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, Borrower+Loan) ///
	keep(monthly_fs monthly_fs_x_nb)

*** Test 4: Projected FS - No controls
reghdfe spread projected_fs projected_fs_x_nb, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("Projected FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, NO) ///
	keep(projected_fs projected_fs_x_nb)

*** Test 5: Projected FS - Borrower controls
reghdfe spread projected_fs projected_fs_x_nb `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("Projected FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, Borrower) ///
	keep(projected_fs projected_fs_x_nb)

*** Test 6: Projected FS - Borrower + Loan controls
reghdfe spread projected_fs projected_fs_x_nb `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("Projected FS") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, Borrower+Loan) ///
	keep(projected_fs projected_fs_x_nb)

*** Test 7: Lender Meeting - No controls
reghdfe spread lender_meeting lender_meeting_x_nb, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("Lender Meeting") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, NO) ///
	keep(lender_meeting lender_meeting_x_nb)

*** Test 8: Lender Meeting - Borrower controls
reghdfe spread lender_meeting lender_meeting_x_nb `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("Lender Meeting") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, Borrower) ///
	keep(lender_meeting lender_meeting_x_nb)

*** Test 9: Lender Meeting - Borrower + Loan controls
reghdfe spread lender_meeting lender_meeting_x_nb `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("Lender Meeting") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, Borrower+Loan) ///
	keep(lender_meeting lender_meeting_x_nb)

*** Test 10: Total Info Cov - No controls
reghdfe spread total_info_covenants total_info_cov_x_nb, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("Total Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, NO) ///
	keep(total_info_covenants total_info_cov_x_nb)

*** Test 11: Total Info Cov - Borrower controls
reghdfe spread total_info_covenants total_info_cov_x_nb `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("Total Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, Borrower) ///
	keep(total_info_covenants total_info_cov_x_nb)

*** Test 12: Total Info Cov - Borrower + Loan controls
reghdfe spread total_info_covenants total_info_cov_x_nb `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("Total Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, Borrower+Loan) ///
	keep(total_info_covenants total_info_cov_x_nb)

*** Test 13: All Info Cov - No controls
reghdfe spread all_info_covenants all_info_cov_x_nb, absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("All Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, NO) ///
	keep(all_info_covenants all_info_cov_x_nb)

*** Test 14: All Info Cov - Borrower controls
reghdfe spread all_info_covenants all_info_cov_x_nb `borr_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("All Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, Borrower) ///
	keep(all_info_covenants all_info_cov_x_nb)

*** Test 15: All Info Cov - Borrower + Loan controls
reghdfe spread all_info_covenants all_info_cov_x_nb `borr_vars' `loan_vars', absorb(ff12 year) vce(cluster gvkey)
outreg2 using "$tabdir/7a_Table3_CoefficientTests.xls", append excel ///
	ctitle("All Info Cov") label dec(3) ///
	addtext(Industry FE, YES, Year FE, YES, Controls, Borrower+Loan) ///
	keep(all_info_covenants all_info_cov_x_nb)

*** End log
log close