**** This program makes graphs of the test score coefficients from BOTH the static and dynamic specifications;

#delimit;
clear;
set mem 220m;
set more 1;
set scheme s1mono;
cd "C:\Documents and Settings\Steph Cellini\My Documents\My Dropbox\BondTest\";


**** Recursive ****;
use if gobond==1 using "Data\test_recursivepanel", clear;

  qui tab year, gen(yrdums);
  qui tab dyear, gen(dydums);

tempfile recurs_test;
save `recurs_test';

foreach v of varlist sstd_catread3 sstd_catmath3 {;
  matrix coeffs_`v'=J(18, 3, 0);

*Basic estimate, but include [-19, 19] in the sample.  Effects are estimated for [1, 19].;
runrecursive, quietly fname("`recurs_test'") dvar(sstd_catmath3) start(-19) end(19);

  foreach num of numlist 1/15 {;
    local beta=_b[win_`num'];
    matrix coeffs_`v'[`num'+3, 1]=`beta';
    local se=_se[win_`num'];
    matrix coeffs_`v'[`num'+3, 2]=`se';
    matrix coeffs_`v'[`num'+3, 3]=`num';
    };
  
  matrix colnames coeffs_`v'=b se year;
  matrix coleq coeffs_`v'=`v';
  };
matrix coeffsall=(coeffs_sstd_catread3, coeffs_sstd_catmath3);

drop _all;
set obs 18;
svmat coeffsall, names(eqcol);
rename sstd_catread3year year;
foreach v in  sstd_catread3 sstd_catmath3 {;
  gen cil_`v'=`v'b - 1.96*`v'se;
  gen ciu_`v'=`v'b + 1.96*`v'se;  
  line `v'b cil_`v' ciu_`v' year, clpattern(solid shortdash shortdash)
       xtitle("Year (relative to election)")
       ytitle("`ylab_`v''") ylabel(, nogrid) yline(0, lpattern(dot))
       legend(label(1 "Estimate") label(2 "95% CI") order(1 2) cols(2))   
       saving(Stata\fig10_test_recursive_`v', replace) name(fig, replace);
   *graph export figs_type2_test_`v'.pdf, replace name(fig);
};      

*** Here I put them both on one graph;
drop _all;
set obs 18;
svmat coeffsall, names(eqcol);
rename sstd_catread3year year;

foreach v in  sstd_catread3 sstd_catmath3 {;
  gen cil_`v'=`v'b - 1.96*`v'se;
  gen ciu_`v'=`v'b + 1.96*`v'se;
};

 * line sstd_catread3b cil_sstd_catread3 ciu_sstd_catread3 sstd_catmath3b cil_sstd_catmath3 ciu_sstd_catmath3 year, 
       clpattern(solid shortdash shortdash solid shortdash shortdash)
       clcolor(gs10 gs10 gs10 black black black)
       xtitle("Year (relative to election)")
       ytitle("Mean test score (in school-level standard deviations)") ylabel(, nogrid) yline(0, lpattern(dot))
       legend(label(1 "Reading Estimate") label(2 "Reading 95% CI") label(4 "Math Estimate") label(5 "Math 95% CI") order(1 2 4 5) cols(2))   
       saving(Stata\fig10_test_recurs_both, replace) name(fig, replace);
      
sort year;
tempfile recurs_coeffsall;
save recurs_coeffsall, replace;

***** DYNAMIC ****;

use "Data\test_fullpanel", clear;

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


*Get rid of year-0 variables;
 foreach v of varlist *_prev0 {;
   rename `v' tmp_`v';
  };

** renaming so I can later have recursive and dynamic on same graph;
rename sstd_catread3 dysstd_catread3;
rename sstd_catmath3 dysstd_catmath3;

*Start to run the regressions;
*Design:  District FEs, cubic controls, GO bonds only;;

  foreach v of varlist dysstd_catread3 dysstd_catmath3 {;
  	  matrix dycoeffs_`v'=J(18, 3, 0);
   areg `v' gobond_prev* req_prev* win_prev* yrdums*
              percent_prev* percent2_prev* percent3_prev*,
            absorb(newleaid) cluster(newleaid);
   
  foreach num of numlist 1/15 {;
    local beta=_b[win_prev`num'];
    matrix dycoeffs_`v'[`num'+3, 1]=`beta';
    local se=_se[win_prev`num'];
    matrix dycoeffs_`v'[`num'+3, 2]=`se';
    matrix dycoeffs_`v'[`num'+3, 3]=`num';
    };

  matrix colnames dycoeffs_`v'=b se year;
  matrix coleq dycoeffs_`v'=`v';
  };
matrix dycoeffsall=(dycoeffs_dysstd_catread3, dycoeffs_dysstd_catmath3);

*** Here I put them both on one graph;
drop _all;
set obs 18;

svmat dycoeffsall, names(eqcol);
rename dysstd_catread3year year;
*keep if year>=0 & year<=6;

foreach v in  dysstd_catread3 dysstd_catmath3 {;
  gen cil_`v'=`v'b - 1.96*`v'se;
  gen ciu_`v'=`v'b + 1.96*`v'se;
};

 * line dysstd_catread3b cil_dysstd_catread3 ciu_dysstd_catread3 dysstd_catmath3b cil_dysstd_catmath3 ciu_dysstd_catmath3 year, 
       clpattern(solid shortdash shortdash solid shortdash shortdash)
       clcolor(gs10 gs10 gs10 black black black)
       xtitle("Year (relative to election)")
       ytitle("Mean test score (in school-level standard deviations)") ylabel(, nogrid) yline(0, lpattern(dot))
       legend(label(1 "Reading Estimate") label(2 "Reading 95% CI") label(4 "Math Estimate") label(5 "Math 95% CI") order(1 2 4 5) cols(2))   
       saving(Stata\fig10_test_dynamic_both, replace) name(fig, replace);


*** Now I merge on the recursive estimates to put recursive and dynamic math scores on the same graph;
*** This is the final Figure 7 graph used in the QJE paper (used to be Figure 10);
sort year;
merge year using recurs_coeffsall;
drop _merge;


  scatter sstd_catmath3b dysstd_catmath3b year, c(l l) mcolor(black gs10) lcolor (black gs10) lpattern(solid solid) ||
       line cil_sstd_catmath3 ciu_sstd_catmath3 cil_dysstd_catmath3 ciu_dysstd_catmath3 year, 
       clpattern(shortdash shortdash shortdash shortdash)
       clcolor(black black gs10 gs10) xtitle("Year (relative to election)") xlabel(0(3)15)
	 ytitle("Mean math score (in school-level std. deviations)") ylabel(, nogrid) yline(0, lpattern(dot))
       legend(label(2 "One-step estimate") label(5 "One-step 95% CI") label(1 "Recursive estimate")  label(3 "Recursive 95% CI") order(1 2 3 5) cols(2))   
       saving(Stata\fig7_final, replace) name(fig, replace);
 
graph export Stata\fig7_final.eps, logo(off) replace;
graph export Stata\fig7_final.tif, replace;
xxxx;
