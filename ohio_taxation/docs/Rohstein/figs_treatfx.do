capture log close
log using figs_treatfx.log, text replace

/* figs_treatfx.do
*  Program to make figures RD estimates of bond effect on outcomes by year
*  relative to election.  Uses both static and dynamic specifications, with district/measure FEs.
*/

#delimit;
clear;
set mem 200m;
set scheme s2mono;

**Start with one-step estimates;
*  use ../tables/programs/fullpanel;
  use fullpanel;
  *use c:\temp\fullpanel_v9;

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

  *Get rid of year-0 variables;
   foreach v of varlist *_prev0 {;
     rename `v' tmp_`v';
    };
  *Drop year-19 variables:  CCD only goes to 2005, so these are identically 0 in sample;
   foreach v of varlist *_prev19 {;
     assert `v'==0 if ltdebt_pp<.;
     drop `v';
   };

  *Start to run the regressions;
  *Design:  No district FEs, cubic controls, GO bonds only;;
   *areg ltdebt_pp gobond_prev* req_prev* win_prev* yrdums*
                percent_prev* percent2_prev* percent3_prev* ,
              absorb(newleaid) cluster(newleaid);
    foreach v of varlist totalexp_pp /*tcapout_pp tcurinst_pp*/ {;
       matrix coeffs_`v'=J(19, 3, 0);
       qui areg `v' gobond_prev* req_prev* win_prev* yrdums*
                percent_prev* percent2_prev* percent3_prev* ,
              absorb(newleaid) cluster(newleaid);
       forvalues r=1/18 {;
          matrix coeffs_`v'[`r'+1, 1]=_b[win_prev`r'];
          matrix coeffs_`v'[`r'+1, 2]=_se[win_prev`r'];
          matrix coeffs_`v'[`r'+1, 3]=`r';
          };
       matrix colnames coeffs_`v'=b se year;
       matrix coleq coeffs_`v'=d_`v';
    };
  *matrix coeffsall=(coeffs_ltdebt_pp, coeffs_totalexp_pp, coeffs_tcapout_pp, coeffs_tcurinst_pp);
  *matrix coeffsdyn=(coeffs_tcapout_pp, coeffs_tcurinst_pp);
  matrix coeffsdyn=(coeffs_totalexp_pp);

***Now start over and run the recursive specification;
  use if gobond==1 using ../tables/programs/recursivepanel, clear;

  qui tab year, gen(yrdums);
  qui tab dyear, gen(dydums);

  tempfile basedat;
  save `basedat';


  foreach v of varlist totalexp_pp /*tcapout_pp tcurinst_pp*/ {;
  	matrix coeffs_`v'=J(19, 3, 0);
    runrecursive, fname("`basedat'") start(-11) end(18) dvar(`v');
    forvalues r=1/18 {;
        matrix coeffs_`v'[`r'+1, 1]=_b[win_`r'];
        matrix coeffs_`v'[`r'+1, 2]=_se[win_`r'];
        matrix coeffs_`v'[`r'+1, 3]=`r';
        };
     matrix colnames coeffs_`v'=b se year;
     matrix coleq coeffs_`v'=s_`v';
  };
  *matrix coeffsrecurs=(coeffs_tcapout_pp, coeffs_tcurinst_pp);
  matrix coeffsrecurs=coeffs_totalexp_pp;

drop _all;
set obs 19;
svmat coeffsdyn, names(eqcol);
svmat coeffsrecurs, names(eqcol);
rename d_totalexp_ppyear year;
foreach v in  totalexp_pp /*tcapout_pp tcurinst_pp*/ {;
 foreach s in s d {;
  gen `s'_cil_`v'=`s'_`v'b - 1.96*`s'_`v'se;
  gen `s'_ciu_`v'=`s'_`v'b + 1.96*`s'_`v'se;
  };
};
drop if year>15;

scatter s_totalexp_ppb d_totalexp_ppb year, mcolor(black gs10 ) c(l l) clcolor(black gs10) ||
line s_cil_totalexp_pp s_ciu_totalexp_pp
     d_cil_totalexp_pp d_ciu_totalexp_pp year,
       clpattern(shortdash shortdash shortdash shortdash)
       clcolor(black black gs10 gs10)
       xtitle("Year (relative to election)") xlabel(0 (3) 15)
       ytitle("Mean expenditures PP") ylabel(-2000 (1000) 2000, nogrid) yline(0, lpattern(dot))
       legend(label(2 "One-step estimate") label(5 "One-step 95% CI")
              label(1 "Recursive estimate") label(3 "Recursive 95% CI") order( 1 2 3 5) cols(2))
       title("Recursive and one-step estimates of dynamic treatment effects of bond passage "
             "on capital outlays per pupil, by years since election")
       note(`"Notes:  Graphs show coefficients and 95% confidence intervals for estimates of bond passage"'
            `"effects at each lag from the "recursive" and "one-step" estimators discussed in the text."'
            `"Standard errors are clustered at the district level."')
        saving(fig4, replace);
scatter s_totalexp_ppb d_totalexp_ppb year, mcolor(black gs10 ) c(l l) clcolor(black gs10) ||
line s_cil_totalexp_pp s_ciu_totalexp_pp
     d_cil_totalexp_pp d_ciu_totalexp_pp year,
       clpattern(shortdash shortdash shortdash shortdash)
       clcolor(black black gs10 gs10)
       xtitle("Year (relative to election)") xlabel(0 (3) 15)
       ytitle("Mean expenditures PP") ylabel(-2000 (1000) 2000, nogrid) yline(0, lpattern(dot))
       legend(label(2 "One-step estimate") label(5 "One-step 95% CI")
              label(1 "Recursive estimate") label(3 "Recursive 95% CI") order( 1 2 3 5) cols(2))
        saving(fig4_notitle, replace);

/*
scatter s_tcapout_ppb d_tcapout_ppb year, mcolor(black gs10 ) c(l l) clcolor(black gs10) ||
line s_cil_tcapout_pp s_ciu_tcapout_pp
     d_cil_tcapout_pp d_ciu_tcapout_pp year,
       clpattern(shortdash shortdash shortdash shortdash)
       clcolor(black black gs10 gs10)
       xtitle("Year (relative to election)") xlabel(0 (3) 15)
       ytitle("Mean capital outlays PP") ylabel(-2000 (1000) 2000, nogrid) yline(0, lpattern(dot))
       legend(label(2 "One-step estimate") label(5 "One-step 95% CI")
              label(1 "Recursive estimate") label(3 "Recursive 95% CI") order( 1 2 3 5) cols(2))
       title("Recursive and one-step estimates of dynamic treatment effects of bond passage "
             "on capital outlays per pupil, by years since election")
       note(`"Notes:  Graphs show coefficients and 95% confidence intervals for estimates of bond passage"'
            `"effects at each lag from the "recursive" and "one-step" estimators discussed in the text."'
            `"Standard errors are clustered at the district level."')
        saving(fig7, replace);
scatter s_tcapout_ppb d_tcapout_ppb year, mcolor(black gs10 ) c(l l) clcolor(black gs10) ||
line s_cil_tcapout_pp s_ciu_tcapout_pp
     d_cil_tcapout_pp d_ciu_tcapout_pp year,
       clpattern(shortdash shortdash shortdash shortdash)
       clcolor(black black gs10 gs10)
       xtitle("Year (relative to election)") xlabel(0 (3) 15)
       ytitle("Mean capital outlays PP") ylabel(-2000 (1000) 2000, nogrid) yline(0, lpattern(dot))
       legend(label(2 "One-step estimate") label(5 "One-step 95% CI")
              label(1 "Recursive estimate") label(3 "Recursive 95% CI") order( 1 2 3 5) cols(2))
        saving(fig7_notitle, replace);

scatter s_tcurinst_ppb d_tcurinst_ppb year, mcolor(black gs10 ) c(l l) clcolor(black gs10) ||
line s_cil_tcurinst_pp s_ciu_tcurinst_pp
     d_cil_tcurinst_pp d_ciu_tcurinst_pp year,
       clpattern(shortdash shortdash shortdash shortdash)
       clcolor(black black gs10 gs10)
       xtitle("Year (relative to election)") xlabel(0 (3) 15)
       ytitle("Mean current instruct. expend. PP") ylabel(-2000 (1000) 2000, nogrid) yline(0, lpattern(dot))
       legend(label(2 "One-step estimate") label(5 "One-step 95% CI")
              label(1 "Recursive estimate") label(3 "Recursive 95% CI") order( 1 2 3 5) cols(2))
       title("Recursive and one-step estimates of dynamic treatment effects of bond passage "
             "on current instructional expenditures per pupil, by years since election")
       note(`"Notes:  Graphs show coefficients and 95% confidence intervals for estimates of bond passage"'
            `"effects at each lag from the "recursive" and "one-step" estimators discussed in the text."'
            `"Standard errors are clustered at the district level."')
        saving(fig7b, replace);
scatter s_tcurinst_ppb d_tcurinst_ppb year, mcolor(black gs10 ) c(l l) clcolor(black gs10) ||
line s_cil_tcurinst_pp s_ciu_tcurinst_pp
     d_cil_tcurinst_pp d_ciu_tcurinst_pp year,
       clpattern(shortdash shortdash shortdash shortdash)
       clcolor(black black gs10 gs10)
       xtitle("Year (relative to election)") xlabel(0 (3) 15)
       ytitle("Mean current instruct. expend. PP") ylabel(-2000 (1000) 2000, nogrid) yline(0, lpattern(dot))
       legend(label(2 "One-step estimate") label(5 "One-step 95% CI")
              label(1 "Recursive estimate") label(3 "Recursive 95% CI") order( 1 2 3 5) cols(2))
        saving(fig7b_notitle, replace);
*/
log close;
