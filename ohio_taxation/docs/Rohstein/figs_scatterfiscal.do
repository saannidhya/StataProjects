capture log close
log using figs_scatterfiscal.log, text replace

/* figs_scatterfiscal.do
*  Program to make figures of outcomes by vote share relative to threshold,
*  pooling 66 and 55% thresholds.
*/

#delimit;
clear;
set mem 100m;
set scheme s2mono;

*use ../tables/programs/recursivepanel;
use recursivepanel;
*use c:\temp\shortpanel_v9;
keep if gobond==1;

gen mv=percent-req;
gen mvbin=(ceil(abs(percent-req))-0.5)*sign(percent-req);
replace mvbin=0.5 if percent==req & percent<.;
gen mvbin2=(2*ceil(abs(percent-req)/2) - 1)*sign(percent-req);
replace mvbin2=1 if percent==req & percent<.;

 gen bondonballot=isgobond;
 gen winbond=bondonballot*iswin;
 replace winbond=0 if bondonballot==0 & iswin==.;

keep if v33>50;
tempfile basedat;
save `basedat';

*Regress outcomes on year dummies and bin dummies, separately by relative year;
 tab year, gen(yrdums);
 tab mvbin2, gen(bin2dums);
 forvalues b=1/34 {;
	 su mvbin2 if bin2dums`b'==1, meanonly;
 	 local mv`b'=r(mean);
 	};
 *Drop bin for -1;
  gen binm1=(mvbin2==-1);
  assert bin2dums20==binm1;
  drop bin2dums20;

 foreach v in totalexp tcapout /*ltdebt tcapout tcurinst */ {;
 	 matrix ests_`v'=J(34, 9, 0);
   forvalues col=1/9 {;
 	 	 reg `v'_pp yrdums* bin2dums* if dyear==`col'-3;
 	 	 forvalues b=1/34 {;
 	 	 	 if `b'~=20 {;
 	 	 	 	 local est=_b[bin2dums`b'];
 	 	 	 	 matrix ests_`v'[`b', `col']=`est';
 	 	 	 };
 	 	 };
 	 };
 	 matrix colnames ests_`v'=rym2 rym1 ry0 ry1 ry2 ry3 ry4 ry5 ry6;
 	 matrix coleq ests_`v'=`v';
 };

drop _all;
set obs 34;
svmat ests_totalexp, names(eqcol);
svmat ests_tcapout, names(eqcol);
*svmat ests_tcurinst, names(eqcol);
*svmat ests_ltdebt, names(eqcol);
gen mvbin=0;
forvalues i=1/34 {;
	replace mvbin=`mv`i'' in `i';
};
/*
scatter ltdebtrym1 ltdebtry3 mvbin if mvbin>=-10 & mvbin<=10, c(l l) clpattern(dash solid)
          xline(0)
          legend(label(1 "Year before election") label(2 "Three years after election"))
          xtitle("Vote share relative to threshold (2 pp bins)")
          ytitle("Mean long term debt PP")
        title("Long term debt per pupil, by margin of victory"
              "one year before and three years after election")
        note("Notes:  Graphs show average long term debt per pupil in each bin in the listed year relative to the "
             "election.  Averages are conditional on year fixed effects, and the -1 bin is normalized to zero."
             "Measures that passed by between 0.001% and 2% are assigned to the 1 bin, those that passed by"
             "between 2.001% and 4% are assigned to the 3 bin, and so on; those that failed by similar margins "
             "are assigned to the -1 and -3 bins, respectively.  Districts with fewer than 50 students are excluded.")
          saving(fig4, replace) ;
scatter ltdebtrym1 ltdebtry3 mvbin if mvbin>=-10 & mvbin<=10, c(l l) clpattern(dash solid)
          xline(0)
          legend(label(1 "Year before election") label(2 "Three years after election"))
          xtitle("Vote share relative to threshold (2 pp bins)")
          ytitle("Mean long term debt PP")
          saving(fig4_notitle, replace) nodraw;
*/

*Total expenditures;
scatter totalexprym1 totalexpry3 mvbin if mvbin>=-10 & mvbin<=10, c(l l) clpattern(dash solid)
          xline(0)
          legend(label(1 "Year before election") label(2 "Three years after election"))
          xtitle("Vote share relative to threshold (2 pp bins)")
          ytitle("Mean total expenditures PP") ylabel(-500 (500) 1500)
        title("Total expenditures per pupil, by margin of victory"
              "one year before and three years after election")
        note("Notes:  Graphs show average capital outlays per pupil in each bin in the listed year relative to the "
             "election.  Averages are conditional on year fixed effects, and the -1 bin is normalized to zero."
             "Measures that passed by between 0.001% and 2% are assigned to the 1 bin, those that passed by"
             "between 2.001% and 4% are assigned to the 3 bin, and so on; those that failed by similar margins "
             "are assigned to the -1 and -3 bins, respectively.  Districts with fewer than 50 students are excluded.")
          saving(fig2a, replace) ;
scatter totalexprym1 totalexpry3 mvbin if mvbin>=-10 & mvbin<=10, c(l l) clpattern(dash solid)
          xline(0)
          legend(off)
          xtitle("Vote share relative to threshold (2 pp bins)")
          ytitle("Mean total expenditures per pupil") ylabel(-500 (500) 1500)
          title("Total expenditures")
          saving(fig2a_notitle, replace) nodraw;

*Capital outlays;
scatter tcapoutrym1 tcapoutry3 mvbin if mvbin>=-10 & mvbin<=10, c(l l) clpattern(dash solid)
          xline(0)
          legend(label(1 "Year before election") label(2 "Three years after election"))
          xtitle("Vote share relative to threshold (2 pp bins)")
          ytitle("Mean capital outlays PP") ylabel(-500 (500) 1500)
        title("Capital outlays per pupil, by margin of victory"
              "one year before and three years after election")
        note("Notes:  Graphs show average capital outlays per pupil in each bin in the listed year relative to the "
             "election.  Averages are conditional on year fixed effects, and the -1 bin is normalized to zero."
             "Measures that passed by between 0.001% and 2% are assigned to the 1 bin, those that passed by"
             "between 2.001% and 4% are assigned to the 3 bin, and so on; those that failed by similar margins "
             "are assigned to the -1 and -3 bins, respectively.  Districts with fewer than 50 students are excluded.")
          saving(fig2b, replace) ;
scatter tcapoutrym1 tcapoutry3 mvbin if mvbin>=-10 & mvbin<=10, c(l l) clpattern(dash solid)
          xline(0)
          legend(label(1 "Year before election") label(2 "Three years after election") cols(1) pos(11) ring(0))
          xtitle("Vote share relative to threshold (2 pp bins)")
          ytitle("Mean capital outlays per pupil") ylabel(-500 (500) 1500)
          title("Capital outlays")
          saving(fig2b_notitle, replace) nodraw;

graph combine fig2a_notitle.gph fig2b_notitle.gph, xcommon ycommon xsize(6.5) ysize(4) saving(fig2_notitle, replace);

/*
*Current instructional expenditures;
scatter tcurinstrym1 tcurinstry3 mvbin if mvbin>=-10 & mvbin<=10, c(l l) clpattern(dash solid)
          xline(0)
          legend(label(1 "Year before election") label(2 "Three years after election"))
          xtitle("Vote share relative to threshold (2 pp bins)")
          ytitle("Mean current instr. expenditures PP") ylabel(-500 (500) 1000)
        title("Current instructional expenditures per pupil, by margin of victory"
              "one year before and three years after election")
        note("Notes:  Graphs show average current instructional expenditures per pupil in each bin in the listed year "
             "relative to the election.  Averages are conditional on year fixed effects, and the -1 bin is normalized "
             "to zero.  Measures that passed by between 0.001% and 2% are assigned to the 1 bin, those that passed by"
             "between 2.001% and 4% are assigned to the 3 bin, and so on; those that failed by similar margins "
             "are assigned to the -1 and -3 bins, respectively.  Districts with fewer than 50 students are excluded.")
          saving(fig6, replace) ;
scatter tcurinstrym1 tcurinstry3 mvbin if mvbin>=-10 & mvbin<=10, c(l l) clpattern(dash solid)
          xline(0)
          legend(label(1 "Year before election") label(2 "Three years after election"))
          xtitle("Vote share relative to threshold (2 pp bins)")
          ytitle("Mean current instr. expenditures PP") ylabel(-500 (500) 1000)
          saving(fig6_notitle, replace) nodraw;
*/

log close;
