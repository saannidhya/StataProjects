capture log close
log using "C:\Documents and Settings\Steph Cellini\My Documents\Research\BondTest\Stata\test_makefullpanel.log", replace

# delimit;
version 9.0;
set more off;
clear;

**** This program updates Jesse's "makefullpanel" for use with Test Score data;
**** Steph Cellini 
**** updated July 1, 2008 with new test score data;

/************************************************************
* makefullpanel.do
* Jesse Rothstein
* April 10, 2008
* Prepare data for the "full" analysis, using all district-year observations and with all leads and
* lags of the election variables.
*
* Modified, June 26, 2008:
*    1) Extend measures data back to 1987 (there are a couple in 1986,
*       but it looks as if most are missing).
*    2) Modify "bad observation" processing to deal with a couple of
*       measures where "percent" is missing or "req" is incorrect in
*       early years.
*    3) Change code to select GO Bonds over parcel taxes as a higher
*       priority than selecting on the vote share when multiple measures
*       occur in the same year.
* HISTORY:
* Based on:
* graphs.do
*    By Francisco Perez Arce Novaro
*    July 31, 2007
*
*    Generates scatter plots of vote share and financial variables, as well as enrollment.
*    Uses data created by newleaidsforleafinancepanel.do (in this directory)
*    the dataset used is "newleaidsforleafinancepanel.dta (in this directory)
*    Creates gphs, stored in a subfolder of this directory (raw)
*    to run this program: place this do file in the same directory as "newleaidsforleafinancepanel.dta
*    and open two subfolders in this directory with names: "raw" and "graphs"
* prepdata.do (in spending\march2008 directory)
*    Updated:
*    8/1/2007, by fpan:  added these comments
*    8/2/2007, by fpan: created cumulative variables and cumulative graphs
*    8/8/2007, by fpan: changed censors for some variables;
*    8/13/2007 by fpan: changed censors for some variables;
*    8/20/2007 by fpan: created `v'_to for year==-2;
*

**************************************************************/




set mem 100m;

local projectbase "C:\Documents and Settings\Steph Cellini\My Documents\Research\BondTest\";
cd "C:\Documents and Settings\Steph Cellini\My Documents\Research\BondTest\";



*Get the measure data ready;
 use "`projectbase'\Data\referenda_newleaids_capital.dta";
 rename year yearref;
 *note: we are replacing calendar year with schoolyear;
  replace yearref = yearref - 1 if month<5;
  label var yearref "schoolyear starting yr of elec.";
 *change req threshold and type in one election, look at :\\coauthor\referenda_graphing_RDD.do for details;
  replace req="M" if newleaid=="0615180" & yearref==1992;
  replace election_type="Other" if newleaid=="0615180" & yearref==1992;



 *Select sample;
  *Keep only measures since 1995;
   *keep if yearref>=1995;
  *Keep only measures since 1987;
   keep if yearref>=1987;
  *Keep only GO Bonds and parcel taxes;
   keep if election_type== "GO Bond"  | election_type=="Parcel Tax";
   gen gobond=(election_type=="GO Bond");
   gen parcel_cap=(capitaloutlays=="Y")*(gobond==0);
  *Identify measures where percent, passfail, and req dont all match;
   gen mv=percent-req_num;
   gen badobs=(mv>=0 & mv<. & passfail=="F");
   replace badobs=1 if mv<0 & passfail=="P";
   replace badobs=1 if mv==.;
   count if badobs==1 & percent<.;
   assert r(N)==1;
   count if badobs==1 & percent==.;
   replace percent=. if badobs==1;
   label var mv "voteshare-based MARGIN of VICTORY";
  *If there are multiple measures that meet these criteria, keep winning over losing measures
  *and then keep only the highest vote share.  Break ties by (1) lowest requirement (2) GO over parcel.;
   gen win=(passfail=="P");
   gsort newleaid yearref badobs -gobond -win -percent req_num ;
   by newleaid yearref: gen numelec=_N;
   by newleaid yearref: gen numbad=sum(badobs);
   by newleaid yearref: replace badobs=(numbad[_N]>0);
   by newleaid yearref: keep if _n==1;
   label var numelec "# of GO/parcel elecs in same year";
   gen fakewin=(percent>=66.7) if percent<. & req_num<=56;
   replace fakewin=(percent>=55) if percent<. & req_num>=66 & req_num<.;
   assert req_num<=56 | req_num>=66;
   label var fakewin "Indicator for pass at counterfactual threshold";
 *We only need a few variables;
  keep newleaid yearref percent win req_num numelec gobond parcel_cap fakewin;
  rename req_num req;
 *Form counts of measures per district since 1983;
  sort newleaid yearref;
  by newleaid: gen measnum=_n;
  by newleaid: gen nummeas=_N;
  label var measnum "Measure number (chronological) in district";
  label var nummeas "Number of measures in district in sample";
  tab measnum;
  tab nummeas if measnum==nummeas;
  su measnum, meanonly;
  local maxnmeas=r(max);

tempfile referenda referenda_wide;
 sort newleaid yearref;
 save `referenda';
 reshape wide yearref percent win fakewin req numelec gobond parcel_cap, i(newleaid) j(measnum);
 sort newleaid;
 save `referenda_wide';

*Make counts of referenda to date and victories to date;
use `referenda';
gen meastodate=measnum;
by newleaid (yearref): gen winstodate=sum(win);
keep newleaid year meastodate winstodate;
fillin newleaid year;
sort newleaid year;
by newleaid: replace meastodate=0 if _n==1 & _fillin;
by newleaid: replace winstodate=0 if _n==1 & _fillin;
by newleaid: replace meastodate=meastodate[_n-1] if meastodate==.;
by newleaid: replace winstodate=winstodate[_n-1] if winstodate==.;
drop _fillin;
sort newleaid yearref;
rename yearref year;
tempfile refs2date;
save `refs2date';


********** Merging **********;

*Merge to the test score data, and make leads and lags of the referenda variables;
* First I add on the old api score data (this has parental education data);
 use "Data/tscoresall";

*Merge on the new (better) test score data for reading and math;
 sort newleaid year;

 merge newleaid year using "Data/readmath_new";
 tab _merge;

* API score had obs for special ed schools, but reading and math don't (but they do have offices of education);
* Keeping all of them anyway;
drop _merge;

*** Here I try dropping the early years of test score data, since 1994-96 are missing;
*** I think the full sample works better;
* keep if year>=1997;

 sort newleaid year;
 merge newleaid year using `refs2date', unique;
 tab _merge;
 sort newleaid year;

 by newleaid: replace meastodate=0 if _merge==1 & _n==1;
 by newleaid: replace winstodate=0 if _merge==1 & _n==1;
 by newleaid: replace meastodate=meastodate[_n-1] if _merge==1 & _n>1;
 by newleaid: replace winstodate=winstodate[_n-1] if _merge==1 & _n>1;
 drop _merge;
 sort newleaid year;
 
 merge newleaid using `referenda_wide', uniqusing;
 tab _merge;
 *Drop districts that never had referenda or that dont have test data;
  keep if _merge==3; 
  drop _merge;


 ****** Make leads and lags;

  *Finance data run from 1995 to 2005, and measures from 1987-2006.
  *So 19 lags and 11 leads;

  *For test scores: if using all measures (back to 1987) and test score data from 1997-2006 only;
  *Use 19 lags and 8 leads;

  *If using all measures (back to 1987) and all test score data 1992-2006;
  *Use 19 lags and 14 leads;

   foreach v in ismeas percent win fakewin req numelec gobond {;
    forvalues d=0/19 {;
    	gen `v'_prev`d'=0;
    };
    forvalues d=1/14 {;
    	gen `v'_fut`d'=0;
    };
   };
    
  forvalues m=1/`maxnmeas' {;
  	gen dyear`m'=year-yearref`m';
  	forvalues d=0/19 {;
  		replace ismeas_prev`d'=1 if dyear`m'==`d';
      foreach v in percent win fakewin req numelec gobond {;
  		 replace `v'_prev`d'=`v'`m' if dyear`m'==`d';
  		};
  	};
  	forvalues d=1/14 {;
  		replace ismeas_fut`d'=1 if dyear`m'==-`d';
      foreach v in percent win fakewin req numelec gobond {;
  		 replace `v'_fut`d'=`v'`m' if dyear`m'==-`d';
  		};
  	};
  	drop percent`m' win`m' fakewin`m' req`m' numelec`m' gobond`m' yearref`m' dyear`m' ;
  };


*Make squared, cubed, etc. vars;
 forvalues d=0/19 {;
 	gen percent2_prev`d'=percent_prev`d'^2;
 	gen percent3_prev`d'=percent_prev`d'^3;
 };
 forvalues d=1/14 {;
 	gen percent2_fut`d'=percent_fut`d'^2;
 	gen percent3_fut`d'=percent_fut`d'^3;
 };


*** To get a consistent sample for reading and math (they are off by one obs);
drop if std_catread3==. & std_catmath3!=.;


sort newleaid year;
save "Data/test_fullpanel.dta", replace;


log close;
