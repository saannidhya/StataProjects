capture log close
log using "C:\Documents and Settings\Steph Cellini\My Documents\Research\BondTest\Stata\test_makeshortpanel.log", replace

# delimit;
version 9.0;
set more off;
clear;

**** This program updates Jesse's program to run the short panel analysis on the test score data;
**** Steph Cellini;
**** Updated July 1, 2008;

/************************************************************
* makeshortpanel.do
* Jesse Rothstein
* April 28, 2008
*
* Prepare data for the "short panel" analysis, treating each 
* measure as a separate event and analyzing data for several years
* before and after (ignoring overlap in the windows)
* Modified, June 26, 2008:
*    1) Extend measures data back to 1987 (there are a couple in 1986,
*       but it looks as if most are missing).
*    2) Modify "bad observation" processing to deal with a couple of
*       measures where "percent" is missing or "req" is incorrect in
*       early years.
*    3) Change code to select GO Bonds over parcel taxes as a higher
*       priority than selecting on the vote share when multiple measures
*       occur in the same year.
*
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

**************************************************************/;

set mem 100m;


cd "C:\Documents and Settings\Steph Cellini\My Documents\Research\BondTest\";

*Get the measure data ready;
 *use "t:\projects\caelections\houseprices\fromfernando\referenda_newleaids_capital.dta";
 use "Data\referenda_newleaids_capital.dta";

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
  *and then keep only the highest vote share.  Break ties by (1) GO over parcel (2) lowest requirement.;
   gen win=(passfail=="P") if badobs~=1;
   gen fakewin=(percent>=66.7) if percent<. & req_num<=56;
   replace fakewin=(percent>=55) if percent<. & req_num>=66 & req_num<.;
   assert req_num<=56 | req_num>=66;
   label var fakewin "Indicator for pass at counterfactual threshold";
   gsort newleaid yearref badobs -gobond -win -percent req_num ;
   by newleaid yearref: gen numelec=_N;
   by newleaid yearref: gen numbad=sum(badobs);
   by newleaid yearref: replace badobs=(numbad[_N]>0);
   by newleaid yearref: keep if _n==1;
   label var numelec "# of GO/parcel elecs in same year";
 *We only need a few variables;
  keep newleaid yearref percent win fakewin req_num numelec gobond parcel_cap;
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
 gen refid=_n;
 save `referenda';

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



*********** Merging *****************;
*Merge to the test score data, and make leads and lags of the referenda variables;
* First, I merge on the old api data;
* This has the parental education variables;
 use "Data/tscoresall";

*Merge on the new (better) test score data for reading and math;
 sort newleaid year;
 merge newleaid year using "Data/readmath_new";
 tab _merge;

* API score had obs for special ed schools, but reading and math don't (but they do have offices of education);
* Keeping all of them anyway;
drop _merge;

* Trying dropping data before 1997 to avoid missing years;
* I think all works better;
* keep if year>=1997;

 sort newleaid year;
 merge newleaid year using `refs2date', unique;
 sort newleaid year;
 by newleaid: replace meastodate=0 if _merge==1 & _n==1;
 by newleaid: replace winstodate=0 if _merge==1 & _n==1;
 by newleaid: replace meastodate=meastodate[_n-1] if _merge==1 & _n>1;
 by newleaid: replace winstodate=winstodate[_n-1] if _merge==1 & _n>1;
 drop _merge;
 sort newleaid year;
*Fill in measures/wins for observations pre test score;
  fillin newleaid year;
  sort newleaid year;
  by newleaid: replace meastodate=0 if _n==1 & meastodate==.;
  by newleaid: replace winstodate=0 if _n==1 & winstodate==.;
  by newleaid: replace meastodate=meastodate[_n-1] if _n>1 & meastodate==.;
  by newleaid: replace winstodate=winstodate[_n-1] if _n>1 & winstodate==.;
 tempfile testdat;
 save `testdat';

*Another set of outcome variables is the presence of an initiative on the ballot;
 use `referenda';
 keep newleaid year gobond parcel_cap win;
 rename gobond isgobond;
 rename parcel_cap isparcelcap;
 gen byte ismeasure=1;
 rename win iswin;
 label var isgobond "Is a GO Bond on ballot this yr?";
 label var ismeasure "Is any Bond/parcel tax on ballot this yr?";
 label var isparcelcap "Is a parcel tax w/capital on ballot this yr?";
 label var iswin "Did measure on ballot win?";
 fillin newleaid year;
 foreach v of varlist isgobond isparcelcap ismeasure iswin {;
 	replace `v'=0 if _fillin==1;
 };
 drop _fillin;
 rename yearref year;
 sort newleaid year;
 merge newleaid year using `testdat', unique;
 tab _merge;
 foreach v of varlist isgobond isparcelcap ismeasure iswin {;
 	replace `v'=0 if _merge==2;
 };
 drop _merge;
 sort newleaid year;


 joinby newleaid using `referenda', unmatched(none);
 gen dyear=year-yearref;
 tab dyear, m;


*** Note that Jesse uses [-2, 6], but Fernando may be using [-3, 6];
*** Be sure to create lags for it below as well and change them back for 2;
 keep if dyear>=-2 & dyear<=6;

 *Make squared, cubed, etc. vars;
 gen percent2=percent^2;
 gen percent3=percent^3;


 *Make leads and lags;
  foreach v in percent percent2 percent3 win fakewin req numelec gobond parcel_cap {;
   forvalues dy=1/2 {;
    gen `v'_m`dy'=`v'*(dyear==-`dy');
    };
   forvalues dy=0/6 {;
    gen `v'_`dy'=`v'*(dyear==`dy');
   };
   *drop `v';
  };

*** To get a consistent sample for reading and math (they are off by one obs);
drop if sstd_catread3==. & sstd_catmath3!=.;

 	
save "Data/test_shortpanel.dta", replace;


log close;
