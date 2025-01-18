capture log close
log using figs_pis.log, text replace

#delimit;
clear;
set mem 100m;
set scheme s2mono;

if c(os)=="MacOSX" {;
  local projectbase "/Volumes/dept/wws/research/rothstein/projects/caelections";
};
else {;
  local projectbase "t:\projects\caelections";
  *use z:\ccd\leafinance\cleaned\leafinance_panel_ca.dta;
};

***Start with figure 1:
*  # of measures considered/passed within 4 years
*  of election, by vote share bin relative to threshold,
*  pooling 66 and 55% thresholds.;

*use ../tables/programs/recursivepanel;
use recursivepanel;
keep if gobond==1;

gen mv=percent-req;
gen mvbin=ceil(abs(percent-req))*sign(percent-req);
replace mvbin=1 if percent==req & percent<.;

 gen bondonballot=isgobond;
 gen winbond=bondonballot*iswin;
 replace winbond=0 if bondonballot==0 & iswin==.;

tab year, gen(yrdums);
tab dyear, gen(dydums);

*Exclude year 0 from all analyses;
foreach v of varlist gobond_0 win_0 percent_0 percent2_0 percent3_0 req_0 {;
	rename `v' tmp_`v';
	};
drop if dyear==0;

     areg iswin yrdums* dydums*
          win_? win_1? req_? req_1? percent_? percent_1? percent2_? percent2_1? percent3_? percent3_1?
          if gobond==1, absorb(refid) cluster(newleaid);
 matrix fullb=e(b);
 matrix fullV=e(V);

 matrix bwin=fullb[1,"win_1".."win_19"]';
 matrix Vwin=fullV["win_1".."win_19", "win_1".."win_19"];
 matrix SEwin=vecdiag(cholesky(diag(vecdiag(Vwin))))';

 matrix ests=bwin, SEwin;
 matrix colnames ests = beta SE;
*Now make data set so can make graph;
 drop _all;
 set obs 20;
 gen year=_n;
 svmat ests, names(col);
 replace year=0 if year==20;
 replace beta=1 if year==0;
 replace SE=0 if year==0;
 gen cil=beta-1.96*SE;
 gen ciu=beta+1.96*SE;
 sort year;
 keep if year<=15;

scatter beta year, mcolor(black) c(l) clcolor(black) ||
line cil ciu year,
       clpattern(shortdash shortdash) clcolor(black black )
       xtitle("Year (relative to election)") xlabel(0 (3) 15)
       ytitle("Effect on pr(pass bond)") yline(0, lpattern(dot))
       legend(label(1 "Reduced-form RD estimate") label(2 "95% CI") order( 1 2) cols(1))
       title("Reduced form RD estimates of the effect of bond passage "
             "on the probability of passing a later bond")
       note(`"Notes:  Graph shows coefficients and 95% confidence intervals from reduced-form RD estimate"'
            `"of the effect of measure passage in year t on the probability of passing a measure in "'
            `"t+tau, using the specification described in equation (#), taking an indicator for measure"'
            `"passage as the dependent variable.  Standard errors are clustered at the district level."')
        saving(fig3, replace);
scatter beta year, mcolor(black) c(l) clcolor(black) ||
line cil ciu year,
       clpattern(shortdash shortdash) clcolor(black black )
       xtitle("Year (relative to election)") xlabel(0 (3) 15) ylabel(-0.5 (0.25) 1)
       ytitle("Effect on pr(pass bond)") yline(0, lpattern(dot))
       legend(label(1 "Reduced-form RD estimate") label(2 "95% CI") order( 1 2) cols(1) ring(0) pos(1))
        saving(fig3_notitle, replace);


log close;
