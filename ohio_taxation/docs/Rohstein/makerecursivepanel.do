capture log close
log using makerecursivepanel.log, replace

# delimit;
version 9.2;
set more off;
clear;



/************************************************************
* makeshortpanel.do
* Jesse Rothstein
* April 28, 2008
*
* Prepare data for the "short panel" analysis, treating each
* measure as a separate event and analyzing data for several years
* before and after (ignoring overlap in the windows)
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
  local projectbase "/Volumes/dept/wws/research/rothstein/projects/caelections";
};
else {;
  local projectbase "t:\projects\caelections";
  *use z:\ccd\leafinance\cleaned\leafinance_panel_ca.dta;
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

 sort newleaid yearref;
 isid newleaid yearref;
 compress;
 save referenda, replace;

tempfile referenda referenda_wide;
 sort newleaid yearref;
 gen refid=_n;
 *We only need a few variables;
  keep newleaid yearref percent win fakewin req_num numelec gobond parcel_cap
       refid measnum nummeas proposed_bond;
  rename req_num req;
 save `referenda';

*Make counts of referenda to date and victories to date;
use `referenda';
gen meastodate=measnum;
by newleaid (yearref): gen winstodate=sum(win);
keep newleaid yearref meastodate winstodate ;
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
 *Fill in measures/wins for observations pre CCD;
  fillin newleaid year;
  sort newleaid year;
  by newleaid: replace meastodate=0 if _n==1 & meastodate==.;
  by newleaid: replace winstodate=0 if _n==1 & winstodate==.;
  by newleaid: replace meastodate=meastodate[_n-1] if _n>1 & meastodate==.;
  by newleaid: replace winstodate=winstodate[_n-1] if _n>1 & winstodate==.;
 tempfile financedat;
 save `financedat';

*Form average enrollment per year, to use as denominator for bond size measure;
 use if v33<. using `financedat';
 collapse (mean) v33, by(newleaid);
 rename v33 avgenrollment;
 label var avgenrollment "Avg. district enrollment across all yrs";
 sort newleaid;

*Another set of outcome variables is the presence of an initiative on the ballot;
 merge newleaid using `referenda', uniqmaster;
 tab _merge;
 drop if _merge==1;
 drop _merge;
 destring proposed_bond, gen(bondamt_thisyr);
 replace bondamt_thisyr=0 if win~=1 | bondamt_thisyr==.;
 *Convert to real 2000$, using "West" CPI-U (CUUR0400SA0,CUUS0400SA0);
  replace bondamt_thisyr=bondamt_thisyr*174.8/102.3 if yearref==1984;
  replace bondamt_thisyr=bondamt_thisyr*174.8/106.8 if yearref==1985;
  replace bondamt_thisyr=bondamt_thisyr*174.8/109.8 if yearref==1986;
  replace bondamt_thisyr=bondamt_thisyr*174.8/113.2 if yearref==1987;
  replace bondamt_thisyr=bondamt_thisyr*174.8/117.7 if yearref==1988;
  replace bondamt_thisyr=bondamt_thisyr*174.8/123.3 if yearref==1989;
  replace bondamt_thisyr=bondamt_thisyr*174.8/129.4 if yearref==1990;
  replace bondamt_thisyr=bondamt_thisyr*174.8/136.2 if yearref==1991;
  replace bondamt_thisyr=bondamt_thisyr*174.8/141.0 if yearref==1992;
  replace bondamt_thisyr=bondamt_thisyr*174.8/145.5 if yearref==1993;
  replace bondamt_thisyr=bondamt_thisyr*174.8/148.7 if yearref==1994;
  replace bondamt_thisyr=bondamt_thisyr*174.8/152.9 if yearref==1995;
  replace bondamt_thisyr=bondamt_thisyr*174.8/156.6 if yearref==1996;
  replace bondamt_thisyr=bondamt_thisyr*174.8/160.6 if yearref==1997;
  replace bondamt_thisyr=bondamt_thisyr*174.8/163.6 if yearref==1998;
  replace bondamt_thisyr=bondamt_thisyr*174.8/167.8 if yearref==1999;
  replace bondamt_thisyr=bondamt_thisyr*174.8/173.1 if yearref==2000;
  replace bondamt_thisyr=bondamt_thisyr*174.8/180.2 if yearref==2001;
  replace bondamt_thisyr=bondamt_thisyr*174.8/184.0 if yearref==2002;
  replace bondamt_thisyr=bondamt_thisyr*174.8/188.2 if yearref==2003;
  replace bondamt_thisyr=bondamt_thisyr*174.8/191.9 if yearref==2004;
  replace bondamt_thisyr=bondamt_thisyr*174.8/197.1 if yearref==2005;
  replace bondamt_thisyr=bondamt_thisyr*174.8/204.5 if yearref==2006;
  replace bondamt_thisyr=bondamt_thisyr*174.8/210.890 if yearref==2007;
 gen bondamt_thisyr_pp=bondamt_thisyr/avgenrollment;
 keep newleaid yearref gobond parcel_cap win bondamt_thisyr_pp;
 rename gobond isgobond;
 rename parcel_cap isparcelcap;
 gen byte ismeasure=1;
 rename win iswin;
 label var isgobond "Is a GO Bond on ballot this yr?";
 label var ismeasure "Is any Bond/parcel tax on ballot this yr?";
 label var isparcelcap "Is a parcel tax w/capital on ballot this yr?";
 label var iswin "Did measure on ballot win?";
 label var bondamt_thisyr_pp "Size of bond passed this year/avg. enroll";
 fillin newleaid yearref;
 foreach v of varlist isgobond isparcelcap ismeasure iswin bondamt_thisyr_pp {;
 	replace `v'=0 if _fillin==1;
 };
 drop _fillin;
 rename yearref year;
 sort newleaid year;
 merge newleaid year using `financedat', unique;
 tab _merge;
 foreach v of varlist isgobond isparcelcap ismeasure iswin bondamt_thisyr_pp {;
 	replace `v'=0 if _merge==2;
 };
 drop _merge;
 sort newleaid year;

 joinby newleaid using `referenda', unmatched(none);
 gen dyear=year-yearref;
 tab dyear, m;
 *keep if dyear>=-2 & dyear<=6;
  keep if dyear>=-19 & dyear<=19;

*Make squared, cubed, etc. vars;
 gen percent2=percent^2;
 gen percent3=percent^3;
 *Make leads and lags;
  foreach v in percent percent2 percent3 win fakewin req numelec gobond parcel_cap {;
   forvalues dy=1/19 {;
    gen `v'_m`dy'=`v'*(dyear==-`dy');
    };
   forvalues dy=0/19 {;
    gen `v'_`dy'=`v'*(dyear==`dy');
   };
   *drop `v';
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

destring proposed_bond, gen(bondamt);

foreach v of varlist tcapout tcurelsc totalexp tcurinst dltdebt ltdebtiss ltdebtret ltdebt interest
             locproptax curexp_oper curexp_xport curexp_entop cap_constr cap_land cap_equip instrsal
             tstrev tfedrev c11 c12 c01 c13 c35 tcurnoninst
             stdebt assets_sink assets_bond assets_other dstdebt tlocrev interestearned
             totalrev tnonelse igpmts totassets othlocrev othcurelsc netrev totdebt
             bondamt
             {;
 *Convert to real 2000$, using "West" CPI-U (CUUR0400SA0,CUUS0400SA0);
  replace `v'=`v'*174.8/102.3 if year==1984;
  replace `v'=`v'*174.8/106.8 if year==1985;
  replace `v'=`v'*174.8/109.8 if year==1986;
  replace `v'=`v'*174.8/113.2 if year==1987;
  replace `v'=`v'*174.8/117.7 if year==1988;
  replace `v'=`v'*174.8/123.3 if year==1989;
  replace `v'=`v'*174.8/129.4 if year==1990;
  replace `v'=`v'*174.8/136.2 if year==1991;
  replace `v'=`v'*174.8/141.0 if year==1992;
  replace `v'=`v'*174.8/145.5 if year==1993;
  replace `v'=`v'*174.8/148.7 if year==1994;
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
  *Truncate top & bottom 1% of each;
   qui su `v'_pp, d;
   replace `v'_pp=r(p1) if `v'_pp<r(p1);
   replace `v'_pp=r(p99) if `v'_pp>r(p99) & `v'_pp<.;
 };

*replace totalexp_pp = totalexp_pp/1000;
*replace tcurelsc_pp = tcurelsc_pp/1000;
*replace tcapout_pp= tcapout_pp/1000;

compress;
save "recursivepanel.dta", replace;


log close;
