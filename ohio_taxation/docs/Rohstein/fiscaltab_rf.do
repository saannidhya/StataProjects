capture log close
log using fiscaltab_rf.log, replace

# delimit;
version 9.2;
set more off;
clear;



/************************************************************
* fiscaltab_rf.do
* Jesse Rothstein
* October 25, 2008
*
* Run reduced-form regressions for finance outcomes.
* Uses data from recursivepanel.dta, created by "makerecursivepanel.do".
*
**************************************************************/


set mem 100m;

*local projectbase "t:\projects\caelections";
*local projectbase "/Volumes/Dept/WWS/Research/Rothstein/projects/caelections";

use if gobond==1 using recursivepanel;

*Use only years -2 to +6;
 keep if dyear>=-2 & dyear<=6;
 *Drop other variables;
  foreach v in percent percent2 percent3 win fakewin req numelec gobond parcel_cap {;
    forvalues y=7/19 {;
      drop `v'_`y';
      };
    forvalues y=3/19 {;
      drop `v'_m`y';
      };
    };

/*
*Restrict to measures with all dyears -2 to 2;
 sort newleaid measnum dyear;
 isid newleaid measnum dyear;
 gen inwindow=(dyear>=-2 & dyear<=2);
 by newleaid measnum: gen ninwindow=sum(inwindow);
 by newleaid measnum: gen havewindow=(ninwindow[_N]==5);
 tab dyear ninwindow if havewindow~=1;
 keep if havewindow==1;
 drop inwindow ninwindow havewindow;
*/

*Clean up the measure outcomes;
 gen bondonballot=isgobond*(dyear~=0);
 gen winbond=bondonballot*iswin;
 replace winbond=0 if bondonballot==0 & iswin==.;

tab year, gen(yrdums);
tab dyear, gen(dydums);

*Exclude year 0 from all analyses;
foreach v of varlist gobond_0 win_0 percent_0 percent2_0 percent3_0 req_0 {;
	rename `v' tmp_`v';
	};

/*
*Prepare bins;
  foreach v in gobond win percent percent2 percent3 req {;
  	gen bin1_`v'=`v'_m2+`v'_m1+tmp_`v'_0;
  	gen bin2_`v'=`v'_1+`v'_2;
  	gen bin3_`v'=`v'_3+`v'_4+`v'_5+`v'_6;
  };
*/

*Add together state and federal revenue;
 gen tstfedrev_pp=tstrev_pp+tfedrev_pp;

*Start to run the regressions;

*Design:  MEASURE FEs, cubic controls, GO bonds only;;

 *Start with unrestricted effects, leaving out future years;
  foreach v of varlist totalexp_pp tcapout_pp tcurinst_pp tstfedrev_pp {;
  	 if "`v'"=="totalexp_pp" {;
  	 	local apprep "replace";
  	 };
  	 else {;
  	 	local apprep "append";
  	 };
     areg `v' yrdums* dydums* win_? req_?
              percent_? percent2_? percent3_?
          if gobond==1, absorb(refid) cluster(newleaid);
     *Tests:  All past;
      testparm win_?;
      local p_past=r(p);
     outreg using "fiscaltab_rf.txt", bdec(8) tdec(8) adec(8) se noaster noparen nolabel
            addstat("Ppast", `p_past') `apprep';
  };

log close;
