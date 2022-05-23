*--------------------------------------------------------------------------------------------------------;
* Purpose: Merge road levy data with Ohio housing sales & census dataset houses_census_outcomes_9518.dta
* Created by: Saani Rawat
* Log: 
* 		1. 19may2022: 				;
*--------------------------------------------------------------------------------------------------------;


*------------------------------------------------------------;
* creating subset of housing dataset to test merge   		 ;
* TENDIGIT_FIPS, year and id together are unique identifiers ;
*------------------------------------------------------------;
use "${data}/houses_census_outcomes_9518.dta" , clear

keep TENDIGIT_FIPS year sale_amount fid_1 id fid_2 objectid

sort TENDIGIT_FIPS year id
quietly by TENDIGIT_FIPS year id: gen dup = cond(_N==1,0,_n) 
tab dup

* changing format of fips from 10 to 12 digit;
format %12.0g TENDIGIT_FIPS

* saving interim df;
save "${root}/data/houses_census_outcomes_subset.dta", replace

*------------------------;
* using tax levies data	 ;
*------------------------;
use "${data}/roads_levies2_9118" , clear

generate pctfor=votes_for/(votes_for+votes_against) 

generate inverse_millage_percent=-1*millage_percent 

* method 1;
// sort TENDIGIT_FIPS year 
//
// *finding duplicates for each fips and year;
// * if dup = 2, then that is the duplicate ;
// quietly by TENDIGIT_FIPS year: gen dup = cond(_N==1,0,_n) 
//
// * checking frequency of duplicates;
// tabulate dup 
//
// * dropping A i.e. keeping renewals only;
// drop if dup>1 & description=="A" 

* method 2;
*dropping if additional tax;
drop if description=="A" 

// now sort by levy size, too: 
sort TENDIGIT_FIPS year inverse_millage_percent 

* creating a column to check if duplicates exist;
* dup == 0 : no duplicates for this obs;
* dup == 1 : duplicates exist for this obs ;
* dup == 2 : first duplicate;
* dup == 3 : second duplicate;
quietly by TENDIGIT_FIPS year: gen dup2 = cond(_N==1,0,_n) 

* checking frequency of duplicates;
tabulate dup2 

* dropping duplicates;
drop if dup2>1 

* removing unnecessary columns; 
drop dup2 inverse_millage_percent 


*------------------------;
* merging two datasets   ;
*------------------------;

merge 1:m TENDIGIT_FIPS year using "${data}/houses_census_outcomes_subset.dta"


