capture log close
log using fiscaltab_preelection.log, text replace

*Program to estimate models for pre-election outcomes for Table 3;

*Static specifications for y_t-1 and (y_t-1 - y_t-2);
*Dynamic specifications with all controls, focusing on y_t-1;

#delimit;
clear;
set mem 100m;
set more off;

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

*Add together state and federal revenue;
 gen tstfedrev_pp=tstrev_pp+tfedrev_pp;

*Compute t-2 to t-1 change;
sort newleaid measnum dyear;
foreach v of varlist totalexp_pp tcapout_pp tcurinst_pp tstfedrev_pp {;
  by newleaid measnum: gen d`v'=`v'-`v'[_n-1] if dyear==dyear[_n-1]+1;
};
*Rename year 0 vars, to make them easy to exclude;
foreach v of varlist gobond_0 win_0 percent_0 percent2_0 percent3_0 req_0 {;
	rename `v' tmp_`v';
	};


tab year, gen(yrdums);
tab dyear, gen(dydums);
tempfile shortpan;
save `shortpan';


*Now prepare long panel for dynamic regs;
 use fullpanel, clear;

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
*Rename year-0 variables;
 foreach v of varlist *_prev0 {;
   rename `v' tmp_`v';
  };
 *Drop year-19 variables:  CCD only goes to 2005, so these are identically 0 in sample;
  foreach v of varlist *_prev19 {;
    assert `v'==0 if ltdebt_pp<.;
    drop `v';
  };
*Add together state and federal revenue;
 gen tstfedrev_pp=tstrev_pp+tfedrev_pp;
 tempfile longpan;
 save `longpan';

foreach v of varlist totalexp_pp tcapout_pp tcurinst_pp /* tstfedrev_pp */ {;
  	 if "`v'"=="totalexp_pp" {;
  	 	local apprep "replace";
  	 };
  	 else {;
  	 	local apprep "append";
  	 };
  use `shortpan';
  reg `v' yrdums* win req if dyear==-1 & gobond==1, cluster(newleaid);
     outreg using "fiscaltab_preelection.txt", bdec(8) tdec(8) se noaster noparen nolabel `apprep';
  reg `v' yrdums* win req percent percent2 percent3 if dyear==-1 & gobond==1, cluster(newleaid);
     outreg using "fiscaltab_preelection.txt", bdec(8) tdec(8) se noaster noparen nolabel append;
  reg `v' yrdums* dydums* win_? win_m? req_? req_m?
          percent_? percent2_? percent3_? percent_m? percent2_m? percent3_m?
          tmp_*
          if gobond==1, cluster(newleaid);
     outreg using "fiscaltab_preelection.txt", bdec(8) tdec(8) se noaster noparen nolabel append;
  areg `v' yrdums* dydums* win_? win_m? req_? req_m?
           percent_? percent2_? percent3_? percent_m? percent2_m? percent3_m?
          if gobond==1, absorb(refid) cluster(newleaid);
     outreg using "fiscaltab_preelection.txt", bdec(8) tdec(8) se noaster noparen nolabel append;
  
  reg d`v' yrdums* win req if dyear==-1 & gobond==1, cluster(newleaid);
     outreg using "fiscaltab_preelection.txt", bdec(8) tdec(8) se noaster noparen nolabel append;
  reg d`v' yrdums* win req percent percent2 percent3 if dyear==-1 & gobond==1, cluster(newleaid);
     outreg using "fiscaltab_preelection.txt", bdec(8) tdec(8) se noaster noparen nolabel append;
  reg d`v' yrdums* dydums* win_? win_m? req_? req_m?
          percent_? percent2_? percent3_? percent_m? percent2_m? percent3_m?
          tmp_*
          if gobond==1, cluster(newleaid);
     outreg using "fiscaltab_preelection.txt", bdec(8) tdec(8) se noaster noparen nolabel append;
/* OLD SPECIFICATIONS - NO LONGER USED FOR TABLE
  areg d`v' yrdums* dydums* win_? win_m? req_? req_m?
           percent_? percent2_? percent3_? percent_m? percent2_m? percent3_m?
          if gobond==1, absorb(refid) cluster(newleaid);
     outreg using "fiscaltab_preelection.txt", bdec(8) tdec(8) se noaster noparen nolabel append;

  use `longpan';
  reg `v' gobond_prev* gobond_fut* req_prev* req_fut* win_prev* win_fut* yrdums*
           , cluster(newleaid);
     outreg using "fiscaltab_preelection.txt", bdec(8) tdec(8) se noaster noparen nolabel append;
  reg `v' gobond_prev* gobond_fut* req_prev* req_fut* win_prev* win_fut* yrdums*
           percent_prev* percent_fut* percent2_prev* percent2_fut* percent3_prev* percent3_fut*
           tmp_*
           , cluster(newleaid);
     outreg using "fiscaltab_preelection.txt", bdec(8) tdec(8) se noaster noparen nolabel append;
  areg `v' gobond_prev* gobond_fut* req_prev* req_fut* win_prev* win_fut* yrdums*
           percent_prev* percent_fut* percent2_prev* percent2_fut* percent3_prev* percent3_fut*
           , absorb(newleaid) cluster(newleaid);
     outreg using "fiscaltab_preelection.txt", bdec(8) tdec(8) se noaster noparen nolabel append;
*/
};


