*--------------------------------------------------------------------------------------------------------;
* Purpose: 
* 1. create unique identifiers for UnvCinci_AKZA_5YI482_DeedArmsLength_OH_Delivery_20160330.dta and UnvCinci_AKZA_5YI482_TaxCurrent_OH_Delivery_20160329.dta
* 2. merge the aformentioned datasets
* Created by: Saani Rawat
* Log: 
* 		1. 12June2022: 				;
*--------------------------------------------------------------------------------------------------------;


* Defining root location via global macros;
global root "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation"
global code "${root}/code"
global data "${root}/data"

*---------------------------------------------------------------------------------;
*		UnvCinci_AKZA_5YI482_DeedArmsLength_OH_Delivery_20160330.dta processing	  ;
*---------------------------------------------------------------------------------;

* loading in UnvCinci_AKZA_5YI482_DeedArmsLength_OH_Delivery_20160330.dta;
use "${data}/UnvCinci_AKZA_5YI482_DeedArmsLength_OH_Delivery_20160330.dta" , clear

* converting required numeric variables to string;
tostring fips, gen(fipscode)
tostring batchseq, gen(batchseq_str)
tostring recordingdate, gen(recordingdate_str)
tostring batchid, gen(batchid_str)

* unique identifer created using fipscode, apnunformatted, batchseq_str, batchid_str and recordingdate_str;
gen unique_id = fipscode + apnunformatted + recordingdate_str + batchseq_str + batchid_str
// gen unique_id = apnunformatted + recordingdate_str + batchseq_str
sort unique_id
quietly by unique_id: gen dup_of_id = cond(_N==1,0,_n)
tab dup_of_id
//   dup_of_id |      Freq.     Percent        Cum.
// ------------+-----------------------------------
//           0 |  4,495,133      100.00      100.00
// ------------+-----------------------------------
//       Total |  4,495,133      100.00



*---------------------------------------------------------------------------------;
*		UnvCinci_AKZA_5YI482_TaxCurrent_OH_Delivery_20160329.dta processing	  	  ;
*---------------------------------------------------------------------------------;
use "${data}/UnvCinci_AKZA_5YI482_TaxCurrent_OH_Delivery_20160329.dta" , clear

* converting required numeric variables to string;
tostring fipscode, replace

* unique identifer created using fipscode, apnunformatted;
gen unique_id = fipscode + unformattedapn

sort unique_id
quietly by unique_id: gen dup_of_id = cond(_N==1,0,_n)
tab dup_of_id
//   dup_of_id |      Freq.     Percent        Cum.
// ------------+-----------------------------------
//           0 |  4,059,775      100.00      100.00
//           1 |          4        0.00      100.00
//           2 |          4        0.00      100.00
// ------------+-----------------------------------
//       Total |  4,059,783      100.00

*Note: 4 duplicates above are "complete duplicates", meaning the rows were recorded twice. Seems like data entry issue. Otherwise, fipscode and unformattedapn make an obs unique.;

