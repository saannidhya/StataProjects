capture log close
log using fiscaltab_recursive.log, replace

/************************************************************
* fiscaltab_recursive.do
* Jesse Rothstein
* October 25, 2008
*
* Run reduced-form regressions for finance outcomes then use
* these to compute dynamic treatment effects recursively.
* Uses data from recursivepanel.dta, created by "makerecursivepanel.do".
*
**************************************************************/

# delimit;
version 9.2;
set more off;
clear;
set mem 200m;
pause off;

*local projectbase "t:\projects\caelections";
*local projectbase "/Volumes/Dept/WWS/Research/Rothstein/projects/caelections";

  use if gobond==1 using recursivepanel, clear;

  qui tab year, gen(yrdums);
  qui tab dyear, gen(dydums);

gen tstfedrev_pp=tstrev_pp+tfedrev_pp;

tempfile basedat;
save `basedat';


/*
*Basic estimate, using [-2, 6] window and estimating effects in [1, 6] -- [-2, 0] are constrained to zero.;
 *old order:  tcapout, tcurinst, totalexp;
 *runrecursive, fname("`basedat'") dvar(tcapout_pp) outfile("recursiveregs.v4") replace outrf;
 *runrecursive, fname("`basedat'") dvar(tcurinst_pp) outfile("recursiveregs.v4") append outrf;
 *runrecursive, fname("`basedat'") dvar(totalexp_pp) outfile("recursiveregs.v4") append outrf;
 runrecursive, fname("`basedat'") dvar(ltdebt_pp) outfile("recursiveregs.v4") replace outrf;
 runrecursive, fname("`basedat'") dvar(totalexp_pp) outfile("recursiveregs.v4") append outrf;
 runrecursive, fname("`basedat'") dvar(tcapout_pp) outfile("recursiveregs.v4") append outrf;
 runrecursive, fname("`basedat'") dvar(tcurinst_pp) outfile("recursiveregs.v4") append outrf;
 runrecursive, fname("`basedat'") dvar(tstrev_pp) outfile("recursiveregs.v4") append outrf;
*/

*Use all years with finance data -- -11 to +18.;
 runrecursive, fname("`basedat'") start(-11) end(18) dvar(totalexp_pp) outfile("fiscaltab_recursive") replace outrf;
 runrecursive, fname("`basedat'") start(-11) end(18) dvar(tcapout_pp) outfile("fiscaltab_recursive") append outrf;
 runrecursive, fname("`basedat'") start(-11) end(18) dvar(tcurinst_pp) outfile("fiscaltab_recursive") append outrf;
 runrecursive, fname("`basedat'") start(-11) end(18) dvar(tstfedrev_pp) outfile("fiscaltab_recursive") append outrf;

log close;
