#delimit;

capture program drop runrecursive;
program define runrecursive, eclass;
  syntax , dvar(name) fname(string)
           [indvars(namelist) start(real -2) end(real 6) usezero quietly
            outfile(string) append replace outrf];

  use if dyear>=`start' & dyear<=`end' using `fname', clear;

  *Start to run the regressions;
  *Design:  MEASURE FEs, cubic controls, GO bonds only;;


  *Stack the data - two copies each;
   isid newleaid refid dyear;
   expand 2;
   sort newleaid refid dyear;
   by newleaid refid dyear: gen obsnum=_n;
   `quietly' gen dvar=`dvar' if obsnum==1;
   `quietly' replace dvar=iswin if obsnum==2;
   gen refidAB=refid*10+obsnum;
   foreach v of varlist yrdums* dydums* win_? win_1? req_? percent_? percent2_? percent3_?
                        req_1? percent_1? percent2_1? percent3_1? {;
    gen A_`v'=`v'*(obsnum==1);
    gen B_`v'=`v'*(obsnum==2);
   };
   *Drop dyear0 observation for iswin;
    `quietly' replace dvar=. if obsnum==2 & dyear==0;
  if `"`usezero'"'~=`"usezero"' {;
    *Exclude year 0 from all analyses;
  	drop A_*_0 B_*_0;
	  };
  else {;
   *Don't estimate year-0 effects for iswin;
    drop B_*_0;
  };
 *And create control variables;
  if `"`indvars'"'~=`""' {;
  	foreach v of varlist `indvars' {;
 			`quietly' gen A_`v'=`v'*(obsnum==1);
  	};
  };

pause;
  `quietly' areg dvar A_* B_* , absorb(refidAB) cluster(newleaid);
  if `"`outfile'"'~=`""' & "`outrf'"=="outrf" {;
     outreg using "`outfile'_rf.txt", bdec(8) tdec(8) se noaster noparen nolabel `append' `replace';
  };
  matrix coeffs=e(b);
  matrix var=e(V);
  tempvar samp;
  `quietly' gen `samp'=e(sample);
  qui count if `samp'==1 & obsnum==1;
  local n=r(N);
  if `"`usezero'"'~=`"usezero"' {;
    matrix b=0, coeffs[1, "A_win_1".."A_win_`end'"];
    matrix p=coeffs[1, "B_win_1".."B_win_`end'"];
    matrix subV1=var["A_win_1".."A_win_`end'", "A_win_1".."A_win_`end'"];
    matrix subV2=var["A_win_1".."A_win_`end'", "B_win_1".."B_win_`end'"];
    matrix subV3=var["B_win_1".."B_win_`end'", "A_win_1".."A_win_`end'"];
    matrix subV4=var["B_win_1".."B_win_`end'", "B_win_1".."B_win_`end'"];
    matrix V1=(0, J(1,`end',0)) \ (J(`end',1,0), subV1);
    matrix V2=J(1,`end',0) \ subV2;
    matrix V3=J(`end',1,0), subV3;
    matrix V4=subV4;
  };
  else  {;
    matrix b=coeffs[1, "A_win_0".."A_win_`end'"];
    matrix p=coeffs[1, "B_win_1".."B_win_`end'"];
    matrix V1=var["A_win_0".."A_win_`end'", "A_win_0".."A_win_`end'"];
    matrix V2=var["A_win_0".."A_win_`end'", "B_win_1".."B_win_`end'"];
    matrix V3=var["B_win_1".."B_win_`end'", "A_win_0".."A_win_`end'"];
    matrix V4=var["B_win_1".."B_win_`end'", "B_win_1".."B_win_`end'"];
  };
  *Pad V out to 19;
   local nextra=19-`end';
   if `nextra'>0 {;
     matrix V1=(V1, J(`end'+1, `nextra', 0)) \ J(`nextra', 20, 0);
     matrix V2=(V2, J(`end'+1, `nextra', 0)) \ J(`nextra', 19, 0);
     matrix V3=(V3, J(`end', `nextra', 0)) \ J(`nextra', 20, 0);
     matrix V4=(V4, J(`end', `nextra', 0)) \ J(`nextra', 19, 0);
   };
   matrix V=(V1, V2) \ (V3, V4);

  *Grab the bs and the ps;
   forvalues i=0/19 {;
   	 local p`i'=0;
   	 local b`i'=0;
   	};
   forvalues i=1/`end' {;
     local p`i'=el("p", 1, `i');
     };
   forvalues i=0/`end' {;
     local b`i'=el("b", 1, `i'+1);
     };
  local beta0=`b0';
  local beta1=`b1' - `p1'*`beta0';
  local beta2=`b2' - `p1'*`beta1' - `p2'*`beta0';
  local beta3=`b3' - `p1'*`beta2' - `p2'*`beta1' - `p3'*`beta0';
  local beta4=`b4' - `p1'*`beta3' - `p2'*`beta2' - `p3'*`beta1' - `p4'*`beta0';
  local beta5=`b5' - `p1'*`beta4' - `p2'*`beta3' - `p3'*`beta2' - `p4'*`beta1'
                   - `p5'*`beta0';
  local beta6=`b6' - `p1'*`beta5' - `p2'*`beta4' - `p3'*`beta3' - `p4'*`beta2'
                   - `p5'*`beta1' - `p6'*`beta0';
  local beta7=`b7' - `p1'*`beta6' - `p2'*`beta5' - `p3'*`beta4' - `p4'*`beta3'
                   - `p5'*`beta2' - `p6'*`beta1' - `p7'*`beta0';
  local beta8=`b8' - `p1'*`beta7' - `p2'*`beta6' - `p3'*`beta5' - `p4'*`beta4'
                   - `p5'*`beta3' - `p6'*`beta2' - `p7'*`beta1' - `p8'*`beta0';
  local beta9=`b9' - `p1'*`beta8' - `p2'*`beta7' - `p3'*`beta6' - `p4'*`beta5'
                   - `p5'*`beta4' - `p6'*`beta3' - `p7'*`beta2' - `p8'*`beta1'
                   - `p9'*`beta0';
  local beta10=`b10' - `p1'*`beta9'  - `p2'*`beta8'  - `p3'*`beta7'  - `p4'*`beta6'
                     - `p5'*`beta5'  - `p6'*`beta4'  - `p7'*`beta3'  - `p8'*`beta2'
                     - `p9'*`beta1'  - `p10'*`beta0';
  local beta11=`b11' - `p1'*`beta10' - `p2'*`beta9' - `p3'*`beta8'  - `p4'*`beta7'
                     - `p5'*`beta6'  - `p6'*`beta5'  - `p7'*`beta4'  - `p8'*`beta3'
                     - `p9'*`beta2'  - `p10'*`beta1' - `p11'*`beta0';
  local beta12=`b12' - `p1'*`beta11' - `p2'*`beta10' - `p3'*`beta9' - `p4'*`beta8'
                     - `p5'*`beta7'  - `p6'*`beta6'  - `p7'*`beta5'  - `p8'*`beta4'
                     - `p9'*`beta3'  - `p10'*`beta2' - `p11'*`beta1' - `p12'*`beta0';
  local beta13=`b13' - `p1'*`beta12' - `p2'*`beta11' - `p3'*`beta10' - `p4'*`beta9'
                     - `p5'*`beta8'  - `p6'*`beta7'  - `p7'*`beta6'  - `p8'*`beta5'
                     - `p9'*`beta4'  - `p10'*`beta3' - `p11'*`beta2' - `p12'*`beta1'
                     - `p13'*`beta0';
  local beta14=`b14' - `p1'*`beta13' - `p2'*`beta12' - `p3'*`beta11' - `p4'*`beta10'
                     - `p5'*`beta9' - `p6'*`beta8'  - `p7'*`beta7'  - `p8'*`beta6'
                     - `p9'*`beta5'  - `p10'*`beta4' - `p11'*`beta3' - `p12'*`beta2'
                     - `p13'*`beta1' - `p14'*`beta0';
  local beta15=`b15' - `p1'*`beta14' - `p2'*`beta13' - `p3'*`beta12' - `p4'*`beta11'
                     - `p5'*`beta10' - `p6'*`beta9'  - `p7'*`beta8'  - `p8'*`beta7'
                     - `p9'*`beta6'  - `p10'*`beta5' - `p11'*`beta4' - `p12'*`beta3'
                     - `p13'*`beta2' - `p14'*`beta1' - `p15'*`beta0';
  local beta16=`b16' - `p1'*`beta15' - `p2'*`beta14' - `p3'*`beta13' - `p4'*`beta12'
                     - `p5'*`beta11' - `p6'*`beta10' - `p7'*`beta9'  - `p8'*`beta8'
                     - `p9'*`beta7'  - `p10'*`beta6' - `p11'*`beta5' - `p12'*`beta4'
                     - `p13'*`beta3' - `p14'*`beta2' - `p15'*`beta1' - `p16'*`beta0';
  local beta17=`b17' - `p1'*`beta16' - `p2'*`beta15' - `p3'*`beta14' - `p4'*`beta13'
                     - `p5'*`beta12' - `p6'*`beta11' - `p7'*`beta10' - `p8'*`beta9'
                     - `p9'*`beta8'  - `p10'*`beta7' - `p11'*`beta6' - `p12'*`beta5'
                     - `p13'*`beta4' - `p14'*`beta3' - `p15'*`beta2' - `p16'*`beta1'
                     - `p17'*`beta0';
  local beta18=`b18' - `p1'*`beta17' - `p2'*`beta16' - `p3'*`beta15' - `p4'*`beta14'
                     - `p5'*`beta13' - `p6'*`beta12' - `p7'*`beta11' - `p8'*`beta10'
                     - `p9'*`beta9'  - `p10'*`beta8' - `p11'*`beta7' - `p12'*`beta6'
                     - `p13'*`beta5' - `p14'*`beta4' - `p15'*`beta3' - `p16'*`beta2'
                     - `p17'*`beta1' - `p18'*`beta0';
  local beta19=`b19' - `p1'*`beta18' - `p2'*`beta17' - `p3'*`beta16' - `p4'*`beta15'
                     - `p5'*`beta14' - `p6'*`beta13' - `p7'*`beta12' - `p8'*`beta11'
                     - `p9'*`beta10' - `p10'*`beta9' - `p11'*`beta8' - `p12'*`beta7'
                     - `p13'*`beta6' - `p14'*`beta5' - `p15'*`beta4' - `p16'*`beta3'
                     - `p17'*`beta2' - `p18'*`beta1' - `p19'*`beta0';

  matrix beta = (`beta0', `beta1', `beta2', `beta3', `beta4', `beta5', `beta6', `beta7', `beta8', `beta9',
                 `beta10',`beta11',`beta12',`beta13',`beta14',`beta15',`beta16',`beta17',`beta18',`beta19');

  * Make the jacobian with respect to the bs;
   matrix j_b=J(20,20,0);
   forvalues r=1/20 {;
     matrix j_b[`r', `r']=1;
     local prevr=`r'-1;
     forvalues c=`prevr'(-1)1 {;
       forvalues pr=1/`prevr' {;
         local d=`r'-`pr';
         matrix j_b[`r', `c']=j_b[`r',`c'] - `p`d''*j_b[`pr',`c'];
       };
     };
   };
  *And the jacobian with respect to the ps;
   matrix j_p=J(20,19,0);
   forvalues r=2/20 {;
     local prevr=`r'-1;
     forvalues c=`prevr'(-1)1 {;
       local d=`r'-`c';
       local dm1=`d'-1;
       matrix j_p[`r', `c']=-1*`beta`dm1'';
       forvalues pr=1/`prevr' {;
         local d=`r'-`pr';
         matrix j_p[`r', `c']=j_p[`r',`c'] - `p`d''*j_p[`pr', `c'];
       };
     };
   };

  matrix jacobian=j_b, j_p;

  matrix fullvcov=jacobian*V*jacobian';

  local ncols=`end'+1;
  matrix colnames beta=win_0 win_1 win_2 win_3 win_4 win_5 win_6 win_7 win_8 win_9 win_10
                           win_11 win_12 win_13 win_14 win_15 win_16 win_17 win_18 win_19;
  matrix colnames fullvcov=win_0 win_1 win_2 win_3 win_4 win_5 win_6 win_7 win_8 win_9 win_10
                           win_11 win_12 win_13 win_14 win_15 win_16 win_17 win_18 win_19;
  matrix rownames fullvcov=win_0 win_1 win_2 win_3 win_4 win_5 win_6 win_7 win_8 win_9 win_10
                           win_11 win_12 win_13 win_14 win_15 win_16 win_17 win_18 win_19;

  if `"`usezero'"'~=`"usezero"' {;
    matrix varbeta=fullvcov[2..`ncols',2..`ncols'];
    matrix ses=vecdiag(cholesky(diag(vecdiag(varbeta))));
    matrix subbeta=beta[1, 2..`ncols'];
  };
  else {;
    matrix varbeta=fullvcov[1..`ncols',1..`ncols'];
    matrix ses=vecdiag(cholesky(diag(vecdiag(varbeta))));
    matrix subbeta=beta[1, 1..`ncols'];
  };

 ereturn post subbeta varbeta, depname("`dvar'") obs(`n') esample(`samp');
 ereturn local depvar `"`dvar'"';
 ereturn local cmd `"recursiveregs"';
 ereturn display;
 if `"`outfile'"'~=`""' {;
    outreg using "`outfile'.txt", bdec(8) tdec(8) se noaster noparen nolabel `append' `replace';
 };
 * matrix list ests;

end;
