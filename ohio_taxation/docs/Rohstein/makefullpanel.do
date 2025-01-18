capture log close
log using makefullpanel.log, replace

# delimit;
version 9.2;
set more off;
clear;



/************************************************************
* makefullpanel.do
* Jesse Rothstein
* April 10, 2008
*
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

**************************************************************/




set mem 100m;

if c(os)=="MacOSX" {;
  local projectbase "/Volumes/Dept/WWS/Research/Rothstein/projects/caelections";
};
else {;
  local projectbase "t:\projects\caelections";
};

*Get the measure data ready;
* use "`projectbase'\houseprices\fromfernando\referenda_newleaids_capital.dta";
 use "referenda_newleaids_capital.dta";
 rename year yearref;
 *note: we are replacing calendar year with schoolyear;
  clonevar year=yearref;
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
   destring proposed_bond, gen(bondamt);
   replace bondamt=0 if bondamt<. & win~=1;
  keep newleaid yearref percent win req_num numelec gobond parcel_cap fakewin bondamt;
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
 *Convert to real 2000$, using "West" CPI-U (CUUR0400SA0,CUUS0400SA0);
  replace bondamt=bondamt*174.8/102.3 if yearref==1984;
  replace bondamt=bondamt*174.8/106.8 if yearref==1985;
  replace bondamt=bondamt*174.8/109.8 if yearref==1986;
  replace bondamt=bondamt*174.8/113.2 if yearref==1987;
  replace bondamt=bondamt*174.8/117.7 if yearref==1988;
  replace bondamt=bondamt*174.8/123.3 if yearref==1989;
  replace bondamt=bondamt*174.8/129.4 if yearref==1990;
  replace bondamt=bondamt*174.8/136.2 if yearref==1991;
  replace bondamt=bondamt*174.8/141.0 if yearref==1992;
  replace bondamt=bondamt*174.8/145.5 if yearref==1993;
  replace bondamt=bondamt*174.8/148.7 if yearref==1994;
  replace bondamt=bondamt*174.8/152.9 if yearref==1995;
  replace bondamt=bondamt*174.8/156.6 if yearref==1996;
  replace bondamt=bondamt*174.8/160.6 if yearref==1997;
  replace bondamt=bondamt*174.8/163.6 if yearref==1998;
  replace bondamt=bondamt*174.8/167.8 if yearref==1999;
  replace bondamt=bondamt*174.8/173.1 if yearref==2000;
  replace bondamt=bondamt*174.8/180.2 if yearref==2001;
  replace bondamt=bondamt*174.8/184.0 if yearref==2002;
  replace bondamt=bondamt*174.8/188.2 if yearref==2003;
  replace bondamt=bondamt*174.8/191.9 if yearref==2004;
  replace bondamt=bondamt*174.8/197.1 if yearref==2005;
  replace bondamt=bondamt*174.8/204.5 if yearref==2006;
  replace bondamt=bondamt*174.8/210.890 if yearref==2007;
 

tempfile referenda referenda_wide;
 sort newleaid yearref;
 save `referenda';
 *Merge on enrollment to get per-pupil bond amounts;
  /*
  use newleaid year v33 if v33>=50 & v33<. using financepanel;
  rename year yearref;
  sort newleaid yearref;
  merge newleaid yearref using `referenda', unique;
  tab yearref _merge;
  drop if _merge==1;
  */
  *Use average enrollment in district;
   use newleaid year v33 if v33>=50 & v33<. using financepanel;
   collapse (mean) v33, by(newleaid);
   sort newleaid;
   merge newleaid using `referenda', uniqmaster;
   tab yearref _merge;
   drop if _merge==1;
  gen bondamt_pp=bondamt/v33;
  drop v33 bondamt _merge;
  sort newleaid yearref;
  save `referenda', replace;
 reshape wide yearref percent win fakewin req numelec gobond bondamt_pp parcel_cap, 
         i(newleaid) j(measnum);
 sort newleaid;
 save `referenda_wide';

*Make counts of referenda to date and victories to date;
use `referenda';
gen meastodate=measnum;
by newleaid (yearref): gen winstodate=sum(win);
keep newleaid yearref meastodate winstodate;
fillin newleaid yearref;
sort newleaid yearref;
by newleaid: replace meastodate=0 if _n==1 & _fillin;
by newleaid: replace winstodate=0 if _n==1 & _fillin;
by newleaid: replace meastodate=meastodate[_n-1] if meastodate==.;
by newleaid: replace winstodate=winstodate[_n-1] if winstodate==.;
drop _fillin;
sort newleaid yearref;
rename yearref year;
tempfile refs2date;
save `refs2date';

*Merge to the finance data, and make leads and lags of the referenda variables;
 use financepanel;
 *Drop observations with <50 enrollment;
  drop if v33<50;
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
 *Drop districts that never had referenda or that dont have CCD data;
  keep if _merge==3;
  drop _merge;

 *Make leads and lags;
  *Finance data run from 1995 to 2005, and measures from 1987-2006.
  *So 19 lags and 11 leads;
   foreach v in ismeas percent win fakewin req numelec gobond bondamt_pp {;
    forvalues d=0/19 {;
    	gen `v'_prev`d'=0;
    };
    forvalues d=1/11 {;
    	gen `v'_fut`d'=0;
    };
   };

  forvalues m=1/`maxnmeas' {;
  	gen dyear`m'=year-yearref`m';
  	forvalues d=0/19 {;
  		replace ismeas_prev`d'=1 if dyear`m'==`d';
      foreach v in percent win fakewin req numelec gobond bondamt_pp {;
  		 replace `v'_prev`d'=`v'`m' if dyear`m'==`d';
  		};
  	};
  	forvalues d=1/11 {;
  		replace ismeas_fut`d'=1 if dyear`m'==-`d';
      foreach v in percent win fakewin req numelec gobond bondamt_pp {;
  		 replace `v'_fut`d'=`v'`m' if dyear`m'==-`d';
  		};
  	};
  	drop percent`m' win`m' fakewin`m' req`m' numelec`m' gobond`m' yearref`m' dyear`m' bondamt_pp`m';
  };
   	 

*Clean up some variables;
 gen dltdebt=_41f-_19h;
 gen dstdebt=_66v-_61v;
 *note: this generates a column of ceros;
 *note: checking back, since leafinane_panel_ca those variables were only zeros;
  rename _21f ltdebtiss;
  rename _31f ltdebtret;
  rename _41f ltdebt;
  rename i86 interest;
  rename t06 locproptax;
  rename v40 curexp_oper;
  rename v45 curexp_xport;
  rename v60 curexp_entop;
  rename f12 cap_constr;
  rename g15 cap_land;
  gen cap_equip=k09+k10+k11;
  rename z33 instrsal;
  rename _66v stdebt;
  rename w01 assets_sink;
  rename w31 assets_bond;
  rename w61 assets_other;
  rename u22 interestearned;
*Current non-instructional expenditure:
*(Total current elem/sec - instructional) + total non-elem/sec
* Excludes the following that are otherwise included in total expenditures:
* L12: Payments to state governments
* M12: Payments to local governments
* Q11: Payments to other school systems
* I86: Interest on debt;
 gen tcurnoninst=tcurelsc-tcurinst+tnonelse;
 gen igpmts = l12 + m12 + q11;
 gen totassets=assets_sink+assets_bond+assets_other;

gen othlocrev = tlocrev - locproptax - interestearned;
gen othcurelsc = tcurelsc - tcurinst - curexp_oper;
gen netrev = totalrev - totalexp;
gen totdebt = ltdebt + stdebt;
gen netassets = totassets - totdebt;

foreach v of varlist tcapout tcurelsc totalexp tcurinst dltdebt ltdebtiss ltdebtret ltdebt interest
             locproptax curexp_oper curexp_xport curexp_entop cap_constr cap_land cap_equip instrsal
             tstrev tfedrev c11 c12 c01 c13 c35 tcurnoninst
             stdebt assets_sink assets_bond assets_other dstdebt tlocrev interestearned
             totalrev tnonelse igpmts totassets othlocrev othcurelsc netrev totdebt netassets
             {;
 *Convert to real 2000$, using "West" CPI for 1st half of yr;
  replace `v'=`v'*174.8/152.9 if year==1995;
  replace `v'=`v'*174.8/156.6 if year==1996;
  replace `v'=`v'*174.8/160.6 if year==1997;
  replace `v'=`v'*174.8/163.6 if year==1998;
  replace `v'=`v'*174.8/167.8 if year==1999;
  replace `v'=`v'*174.8/173.1 if year==2000;
  replace `v'=`v'*174.8/180.2 if year==2001;
  replace `v'=`v'*174.8/184.0 if year==2002;
  replace `v'=`v'*174.8/188.2 if year==2003;
  replace `v'=`v'*174.8/191.9 if year==2004;
  replace `v'=`v'*174.8/197.1 if year==2005;
  replace `v'=`v'*174.8/204.5 if year==2006;
  replace `v'=`v'*174.8/210.890 if year==2007;
 	gen `v'_pp=`v'/v33;
 };



*replace totalexp_pp = totalexp_pp/1000;
*replace tcurelsc_pp = tcurelsc_pp/1000;
*replace tcapout_pp= tcapout_pp/1000;

*Make squared, cubed, etc. vars;
 forvalues d=0/19 {;
 	gen percent2_prev`d'=percent_prev`d'^2;
 	gen percent3_prev`d'=percent_prev`d'^3;
 };
 forvalues d=1/11 {;
 	gen percent2_fut`d'=percent_fut`d'^2;
 	gen percent3_fut`d'=percent_fut`d'^3;
 };

save "fullpanel.dta", replace;


log close;
