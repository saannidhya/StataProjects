capture log close
log using fiscaltab_onestep.log, replace

# delimit;
version 9.2;
set more off;
clear;



/************************************************************
* fiscaltab_onestep.do
* Jesse Rothstein
* October 25, 2008
*
* Run finance regressions with long panel.
* Uses data from fullpanel.dta, created by "makefullpanel.do".
*
**************************************************************/


set mem 100m;

*local projectbase "t:\projects\caelections";
*local projectbase "/Volumes/Dept/WWS/Research/Rothstein/projects/caelections";

use fullpanel;

tab year, gen(yrdums);

*Redefine win, percent variables to zero if GO bond is zero;
 foreach v in win percent percent2 percent3 req {;
   forvalues y=0/19 {;
 	   replace `v'_prev`y'=`v'_prev`y'*gobond_prev`y';
 	  };
   forvalues y=1/11 {;
 	   replace `v'_fut`y'=`v'_fut`y'*gobond_fut`y';
 	  };
 };

/*
*Make bins;
  foreach v in gobond win percent percent2 percent3 req {;
  	gen bin1_`v'=0;
  	forvalues t=3/11 {;
  		replace bin1_`v'=bin1_`v'+`v'_fut`t';
  	};
  	gen bin2_`v'=`v'_fut2+`v'_fut1+`v'_prev0;
  	gen bin3_`v'=`v'_prev1+`v'_prev2;
  	gen bin4_`v'=`v'_prev3+`v'_prev4+`v'_prev5+`v'_prev6;
  	gen bin5_`v'=0;
  	forvalues t=7/19 {;
  		replace bin5_`v'=bin5_`v'+`v'_prev`t';
  	};
  };
*/

*Get rid of year-0 variables;
 foreach v of varlist *_prev0 {;
   rename `v' tmp_`v';
  };
*Drop year-19 variables:  CCD only goes to 2005, so these are identically 0 in sample;
 foreach v of varlist *_prev19 {;
   assert `v'==0 if ltdebt_pp<.;
   drop `v';
 };

gen tstfedrev_pp=tstrev_pp+tfedrev_pp;

*Start to run the regressions;
*Design:  District FEs, cubic controls, GO bonds only;;

 *Start with unrestricted effects;
  foreach v of varlist ltdebt_pp tcapout_pp tcurinst_pp tstfedrev_pp  {;
  	 if "`v'"=="ltdebt_pp" {;
  	 	local apprep "replace";
  	 };
  	 else {;
  	 	local apprep "append";
  	 };
     areg `v' gobond_prev* req_prev* win_prev* yrdums*
              percent_prev* percent2_prev* percent3_prev* ,
            absorb(newleaid) cluster(newleaid);
     *Tests:  All past;
      testparm win_prev*;
      local p_past=r(p);
     outreg using "fiscaltab_onestep.txt", bdec(8) tdec(8) adec(8) se noaster noparen nolabel
            addstat("Ppast", `p_past') `apprep';
  };
/*
 *Now re-do with bins;
  foreach v of varlist ltdebt_pp totalexp_pp tcapout_pp tcurinst_pp tstrev_pp {;
     areg `v' bin3_* bin4_* bin5_* yrdums*,
            absorb(newleaid) cluster(newleaid);
     *Tests:  All past, all future, all;
      testparm bin3_win bin4_win bin5_win;
      local p_past=r(p);
     outreg using "tab6.txt", bdec(8) tdec(8) adec(8) se noaster noparen nolabel
            addstat("Ppast", `p_past') append;
  };
*/

log close;