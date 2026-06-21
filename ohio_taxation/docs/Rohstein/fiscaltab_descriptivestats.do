/* capture log close
log using fiscaltab_descriptivestats.log, text replace */

*Program to compute estimates for Table 2:  Summary statistics for
* - All districts
* - Districts that never proposed a measure
* - Districts that ever proposed a measure
* - Districts that passed a measure
* - Districts that failed a measure.
* Items 1-3 will pool all available years.  Items 4-5 use year t-1.

#delimit;
clear;
set mem 100m;
*local projectbase "t:\projects\caelections";
*local projectbase "/Volumes/Dept/WWS/Research/Rothstein/projects/caelections";

*Start by grabbing all districts and merging on the "ever propose" indicator;
 use "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/docs/Rohstein/referenda_newleaids_capital.dta"
 rename year yearref;
 replace yearref = yearref - 1 if month<5;
 label var yearref "schoolyear starting yr of elec.";
  *Keep only measures since 1987;
   keep if yearref>=1987;
  *Keep only GO Bonds;
   keep if election_type== "GO Bond";
 sort newleaid yearref;
 *Make two versions:  One a list of elections, one of districts with elections;
  assert passfail=="P" | passfail=="F";
  gen win=(passfail=="P");
  *For list of elections, need to eliminate duplicates;
    *Identify measures where percent, passfail, and req dont all match;
     gen mv=percent-req_num;
     gen badobs=(mv>=0 & mv<. & passfail=="F");
     replace badobs=1 if mv<0 & passfail=="P";
     replace badobs=1 if mv==.;
     count if badobs==1 & percent<.;
     count if badobs==1 & percent==.;
     replace percent=. if badobs==1;
     label var mv "voteshare-based MARGIN of VICTORY";
    *If there are multiple measures that meet these criteria, keep winning over losing measures
    *and then keep only the highest vote share.  Break ties by (1) lowest requirement (2) GO over parcel.;
     gsort newleaid yearref badobs -win -percent req_num ;
     by newleaid yearref: keep if _n==1;
  keep newleaid yearref win;
  assert win<.;
  sort newleaid yearref;
  tempfile oneref;
  save `oneref';
  by newleaid: keep if _n==1;
  gen hasany=1;
  sort newleaid;
  keep newleaid hasany;
  tempfile everref;
  save `everref';
*Now grab the finance data - only districts with > 50 observations!;
 use if v33>50 using financepanel;
 sort newleaid year;
 merge newleaid using `everref', uniqusing;
 tab _merge;
 list newleaid if _merge==2;
 drop if _merge==2;
 drop _merge;
 replace hasany=0 if hasany==.;
 *Identify election outcomes in following year;
  gen yearref=year+1;
  sort newleaid yearref;
  merge newleaid yearref using `oneref', unique;
  tab _merge;
  drop if _merge==2;
  drop _merge;


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
*Current non-instructional expenditure:
*(Total current elem/sec - instructional) + total non-elem/sec
* Excludes the following that are otherwise included in total expenditures:
* L12: Payments to state governments
* M12: Payments to local governments
* Q11: Payments to other school systems
* I86: Interest on debt;
 gen tcurnoninst=tcurelsc-tcurinst+tnonelse;

foreach v of varlist tcapout tcurelsc totalexp tcurinst dltdebt ltdebtiss ltdebtret ltdebt interest
             locproptax curexp_oper curexp_xport curexp_entop cap_constr cap_land cap_equip instrsal
             tstrev tfedrev c11 c12 c01 c13 c35 tcurnoninst {;
 *Convert to real 2000$, using "West" CPI for 1st half of yr;
  qui replace `v'=`v'*174.8/152.9 if year==1995;
  qui replace `v'=`v'*174.8/156.6 if year==1996;
  qui replace `v'=`v'*174.8/160.6 if year==1997;
  qui replace `v'=`v'*174.8/163.6 if year==1998;
  qui replace `v'=`v'*174.8/167.8 if year==1999;
  qui replace `v'=`v'*174.8/173.1 if year==2000;
  qui replace `v'=`v'*174.8/180.2 if year==2001;
  qui replace `v'=`v'*174.8/184.0 if year==2002;
  qui replace `v'=`v'*174.8/188.2 if year==2003;
  qui replace `v'=`v'*174.8/191.9 if year==2004;
  qui replace `v'=`v'*174.8/197.1 if year==2005;
  qui replace `v'=`v'*174.8/204.5 if year==2006;
  qui replace `v'=`v'*174.8/210.890 if year==2007;
 	qui gen `v'_pp=`v'/v33;
  *Truncate top & bottom 1% of each;
   qui su `v'_pp, d;
   replace `v'_pp=r(p1) if `v'_pp<r(p1);
   replace `v'_pp=r(p99) if `v'_pp>r(p99) & `v'_pp<.;
 };
 gen lnenroll=ln(v33);

*Add together state and federal revenue;
 gen tstfedrev_pp=tstrev_pp+tfedrev_pp;

*Now, can make the table for cols 1-3;
 sort newleaid year;
 by newleaid: gen firstobs=(_n==1);
 gen isobs=1;
 tabstat firstobs isobs ltdebt_pp totalexp_pp tcapout_pp tcurinst_pp tstfedrev_pp lnenroll,
         statistics(mean sd n) by(hasany) columns(statistics) longstub;

*And for cols 4-5;
 drop firstobs;
 keep if win<.;
 sort newleaid year;
 by newleaid: gen firstobs=(_n==1);
 tabstat firstobs isobs ltdebt_pp totalexp_pp tcapout_pp tcurinst_pp tstfedrev_pp lnenroll,
         statistics(mean sd n) by(win) columns(statistics) longstub;
log close;
