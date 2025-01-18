capture log close
log using "C:\Documents and Settings\Steph Cellini\My Documents\Research\BondTest\Stata\tab7-dynamic.log", replace

# delimit;
version 9.0;
set more off;
clear;



/************************************************************
* tab5.do
* Jesse Rothstein
* June 26, 2008
*
* Run finance regressions with long panel.
* Uses data from fullpanel.dta, created by "makefullpanel.do".
*
**************************************************************/


set mem 100m;
set matsize 700;

local projectbase "C:\Documents and Settings\Steph Cellini\My Documents\Research\BondTest\";
cd "C:\Documents and Settings\Steph Cellini\My Documents\Research\BondTest\";

use "Data\test_fullpanel";

tab year, gen(yrdums);

*Redefine win, percent variables to zero if GO bond is zero;
 foreach v in win percent percent2 percent3 req {;
   forvalues y=0/19 {;
 	   replace `v'_prev`y'=`v'_prev`y'*gobond_prev`y';
 	  };
   forvalues y=1/14 {;
 	   replace `v'_fut`y'=`v'_fut`y'*gobond_fut`y';
 	  };
 };


*Make bins;
  foreach v in gobond win percent percent2 percent3 req {;
  	gen bin1_`v'=0;
  	forvalues t=3/14 {;
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


*Get rid of year-0 variables;
 foreach v of varlist *_prev0 {;
   rename `v' tmp_`v';
  };

/*;
*Don't need to do this in test score data, since we have 2006;
*Drop year-19 variables:  CCD only goes to 2005, so these are identically 0 in sample;
 foreach v of varlist *_prev19 {;
   assert `v'==0 if ltdebt_pp<.;
   drop `v';
 };
*/;

*Start to run the regressions;
*Design:  District FEs, cubic controls, GO bonds only;;

*Run basic regressions just to see;
 areg std_catread3 gobond_prev* gobond_fut* req_prev* req_fut* win_prev* win_fut* yrdums*
              percent_prev* percent_fut* percent2_prev* percent2_fut* percent3_prev* percent3_fut*,
            absorb(newleaid) cluster(newleaid);
 outreg using "Stata\tmp.txt", ctitle("t7-dynamic") bdec(3) tdec(3) se coefast 3aster nolabel append;           
 areg std_catread3 bin2_* bin3_* bin4_* bin5_* yrdums*,
            absorb(newleaid) cluster(newleaid);
 outreg using "Stata\tmp.txt", ctitle("t7-dynamic") bdec(3) tdec(3) se coefast 3aster nolabel append;           

 *Start with unrestricted effects;
  foreach v of varlist std_catread3 std_catmath3 sstd_catread3 sstd_catmath3 {;
  	 if "`v'"=="std_catread3" {;
  	 	local apprep "replace";
  	 };
  	 else {;
  	 	local apprep "append";
  	 };

   areg `v' gobond_prev* req_prev* win_prev* yrdums*
              percent_prev* percent2_prev* percent3_prev*,
            absorb(newleaid) cluster(newleaid);
     *Tests:  All past, all future, all;
      testparm win_prev*;
      local p_past=r(p);
     outreg using "Stata\tab7-dynamic.txt", bdec(3) tdec(3) adec(3) se coefast 3aster nolabel
            addstat("Ppast", `p_past') `apprep';
  };

 *Now re-do with bins;
  foreach v of varlist  std_catread3 std_catmath3 sstd_catread3 sstd_catmath3 {;
 
    areg `v' bin3_* bin4_* bin5_* yrdums*,
            absorb(newleaid) cluster(newleaid);
     *Tests:  All past, all future, all;
      testparm bin3_win bin4_win bin5_win;
      local p_past=r(p);
     outreg using "Stata\tab7-dynamic.txt", bdec(3) tdec(3) adec(3) se coefast 3aster nolabel
            addstat("Ppast", `p_past') append;
  };

log close;
