/* This file first merges Dealscan and Compustat using Roberts and Sufi (2009) link, Legacy-New Dealscan link from WRDS
    Then this file cleans the merged data (keeping only companies with revenue between $10 Million and $1 Billion)
    Finally, this file exports the cleaned data to a csv file for future analysis
*/

*** Generate a list of globals (path) for future references
global repodir "/Users/zrsong/Dropbox (MIT)/Research Projects/Banking Related/Private Credit"
global datadir "$repodir/Data"
global rawdir "$datadir/Raw"
global intdir "$datadir/Intermediate"
global cleandir "$datadir/Cleaned"

/**************
	Merge Dealscan and Compustat 
	***************/

*** Match dealscan (new to legacy) and to compustat gvkey (both borrowers and lenders)

* import dealscan_data.csv using stata
import delimited "$rawdir/dealscan_data.csv", varnames(1) case(lower) clear
compress
save "$intdir/raw_dealscan.dta", replace

* Use WRDS-dealscan (legacy) link file to obtain facilityid
import excel "$rawdir/WRDS_to_LoanConnector_IDs.xlsx", first clear
rename (LoanConnectorDealID WRDSpackage_id LoanConnectorTrancheID WRDSfacility_id) (lpc_deal_id packageid lpc_tranche_id facilityid)
gduplicates tag lpc_deal_id lpc_tranche_id, gen(dup)
tab dup 
* notice that less than 11% of links have duplicates (one set of deal-tranche ids mapping to multiple facilityids, for those we drop the obs)
drop if dup != 0 
drop dup 
save "$intdir/dealscan_new_legacy_link.dta", replace

* import linking file to generate links to Compustat
import excel "$rawdir/Dealscan-Compustat_Linking_Database012024.xlsx", first sheet("links") clear
save "$intdir/dealscan_compustat_link.dta", replace 

* Merge the dealscan sample with Compustat using facility id (obtained from link file between new and legacy dealscan)

use "$intdir/raw_dealscan.dta", clear
* check if lpc_deal_id lpc_tranche_id uniquely identifies data 
gsort lpc_deal_id lpc_tranche_id
order lpc_deal_id lpc_tranche_id

* obtain facilityid packageid
fmerge m:1 lpc_deal_id lpc_tranche_id using "$intdir/dealscan_new_legacy_link.dta"
keep if _merge == 3 
drop _merge 

fmerge m:1 facilityid using "$intdir/dealscan_compustat_link"
keep if _merge == 3
drop _merge

export delimited "$cleandir/dealscan_compustat_matched.csv", replace
save "$cleandir/dealscan_compustat_matched.dta", replace

/**************
	Merge Dealscan and Compustat 
	***************/

* generate deal_active_date in specific quarters 
	drop if deal_active_date == ""
	gen year = substr(deal_active_date,1,4)
	gen month = substr(deal_active_date,6,2)
	destring year month, replace
	gen quarter = 1 if inrange(month, 1, 3)
	replace quarter = 2 if inrange(month, 4, 6)
	replace quarter = 3 if inrange(month, 7, 9)
	replace quarter = 4 if quarter >= .
	
* collapse down to gvkey-year-quarter level (one package)
	bysort gvkey year quarter: keep if _n == 1
	save "$intdir/dealscan_gvkey_unique.dta", replace
	* generate list of gvkey to merge to compustat 
	keep gvkey 
	gduplicates drop
	tempfile gvkey_ds
	save `gvkey_ds'
	
* merge with quarterly compustat
	import delimited "$rawdir/compustat_quarterly.csv", clear
	rename (fyearq fqtr) (year quarter)
	
	* keep only unique gvkey year quarter observations in case of duplicates
	bysort gvkey year quarter (atq): keep if _n == 1
	
	fmerge m:1 gvkey using "`gvkey_ds'"
	keep if _merge == 3
	drop _merge
	
	fmerge 1:1 gvkey year quarter using "$intdir/dealscan_gvkey_unique.dta"
	drop if _merge == 2 
	order _merge 
	gsort - _merge - year quarter
	
	* keep only middle market firms (those with revenue between 10 million and 1,000 million)
	bysort gvkey: egen max_revtq = max(revtq)
	keep if inrange(max_revtq, 10, 1000)
	keep if country == "United States"
	