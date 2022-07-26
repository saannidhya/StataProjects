*--------------------------------------------------------------------------------------------------------;
* Purpose: see if Dr. Brasington's merge was correct (and improve it if possible)
* Created by: Saani Rawat
* Log: 
* 		1. 15june2022: Waiting on Dr. Brasington to provide unique identifier code for  UnvCinci_AKZA_5YI482_DeedArmsLength_OH_Delivery_20160330.dta				;
*--------------------------------------------------------------------------------------------------------;

* Defining root location via global macros;
global root "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global code "${root}/code"
global data "${root}/data"


*------------------------------------------------------------;
*	TaxCurrent_OH_Delivery									 ;
*------------------------------------------------------------;
use "${root}/data/UnvCinci_AKZA_5YI482_TaxCurrent_OH_Delivery_20160329.dta"

*convert to string;
tostring situshousenumberprefix situshousenumber1 situszipcode, replace

* create unique address;
generate uniqueaddress = situshousenumberprefix + situshousenumber1 + situshousenumber2 + situshousenumbersuffix + situsdirection + situsstreetname + situsmode + situsunitnumber + situszipcode + unformattedapn
sort uniqueaddress

quietly by uniqueaddress: gen dup = cond(_N==1,0,_n)
tabulate dup

*this command drops the 10 obs
drop if dup > 0

* saving dataset;
save "${root}/data/TaxCurrent_OH_Delivery.dta", replace


*------------------------------------------------------------;
*	DeedArmsLength_OH_Delivery								 ;
*------------------------------------------------------------;
clear all
use "${root}/data/UnvCinci_AKZA_5YI482_DeedArmsLength_OH_Delivery_20160330.dta"

*convert to string;
tostring situshousenumberprefix situszipcode, replace

* create unique address;
// generate uniqueaddress = situshousenumberprefix + situshousenumber + situshousenumbersuffix + situsdirection + situsstreetname + situsmode + situsapartmentunit + situszipcode 
generate uniqueaddress = situshousenumberprefix + situshousenumber + situshousenumbersuffix + situsdirection + situsstreetname + situsmode + situsapartmentunit + situszipcode + apnunformatted
sort uniqueaddress 

quietly by uniqueaddress: gen dup2 = cond(_N==1,0,_n)
tabulate dup2


* saving dataset;
save "${root}/data/TaxCurrent_OH_Delivery.dta", replace

merge 1:m uniqueaddress using e:\oldc\2015data\ohio_taxation\data\UnvCinci_AKZA_5YI482_DeedArmsLength_OH_Delivery_20160330.dta

