** RD simulation with the Lee DGP **
** CCT bandwidth, bias-corrected/robust estimator **

capture cd C:\Users\sal278_RS
capture cd C:\Users\user\Dropbox\RA_Pei\Suejin_Lee\local_poly\data
capture cd C:\Users\zp53\Dropbox\rdd3\data
capture cd C:\Users\peizhuan\Dropbox\rdd3\data

set more off
set rmsg on

*parallel setclusters 5

local machine = 2
set rngstream `machine'
set seed 6

cap program drop RD_sim_DL_estbw
program define RD_sim_DL_estbw, rclass

syntax [, obs(integer 500)]

drop _all
set obs `obs'	   
gen x = 2*rbeta(2,4)-1
gen epsilon = 0.1295*rnormal()
gen Y = (0.48+1.27*x+7.18*x^2+20.21*x^3+21.54*x^4+7.33*x^5)*(x<0) ///
       +(0.52+0.84*x-3.00*x^2+ 7.99*x^3- 9.01*x^4+3.56*x^5)*(x>=0) + epsilon

forvalues pol = 0/4 {

	local polp1 = `pol' + 1
	
	capture altrdrobust Y x, deriv(0) c(0) p(`pol') q(`polp1') bwselect(CCT) kernel(uniform)
	scalar tau_bc_unif_`pol' = e(tau_bc)
	scalar se_rb_unif_`pol' = e(se_rb)
	scalar h_unif_`pol' = e(h_bw)
	scalar b_unif_`pol' = e(b_bw)
	scalar n_unif_`pol' = e(N)

	altrdcctamse_preasym Y x, deriv(0) c(0) p(`pol') q(`polp1') bwselect(CCT) kernel(uniform)
	scalar amse_rb_unif_`pol' = e(CCT_AMSE)

	capture altrdrobust Y x, deriv(0) c(0) p(`pol') q(`polp1') bwselect(CCT) kernel(triangular)
	scalar tau_bc_tri_`pol' = e(tau_bc)
	scalar se_rb_tri_`pol' = e(se_rb)
	scalar h_tri_`pol' = e(h_bw)
	scalar b_tri_`pol' = e(b_bw)
	scalar n_tri_`pol' = e(N)

	altrdcctamse_preasym Y x, deriv(0) c(0) p(`pol') q(`polp1') bwselect(CCT) kernel(triangular)
	scalar amse_rb_tri_`pol' = e(CCT_AMSE)

	capture altrdrobust Y x, deriv(0) c(0) p(`pol') q(`polp1') bwselect(CCT) kernel(uniform) scaleregul(0)
	scalar tau_bc_unif_noreg_`pol' = e(tau_bc)
	scalar se_rb_unif_noreg_`pol' = e(se_rb)
	scalar h_unif_noreg_`pol' = e(h_bw)
	scalar b_unif_noreg_`pol' = e(b_bw)
	scalar n_unif_noreg_`pol' = e(N)

	altrdcctamse_preasym Y x, deriv(0) c(0) p(`pol') q(`polp1') bwselect(CCT) kernel(uniform) scaleregul(0)
	scalar amse_rb_unif_noreg_`pol' = e(CCT_AMSE)

	capture altrdrobust Y x, deriv(0) c(0) p(`pol') q(`polp1') bwselect(CCT) kernel(triangular) scaleregul(0)
	scalar tau_bc_tri_noreg_`pol' = e(tau_bc)
	scalar se_rb_tri_noreg_`pol' = e(se_rb)
	scalar h_tri_noreg_`pol' = e(h_bw)
	scalar b_tri_noreg_`pol' = e(b_bw)
	scalar n_tri_noreg_`pol' = e(N)

	altrdcctamse_preasym Y x, deriv(0) c(0) p(`pol') q(`polp1') bwselect(CCT) kernel(triangular) scaleregul(0)
	scalar amse_rb_tri_noreg_`pol' = e(CCT_AMSE)

	}

forvalues pol = 0/4 {
	foreach ker in unif tri unif_noreg tri_noreg {

	return scalar tau_bc_`ker'_`pol' = tau_bc_`ker'_`pol'
	return scalar se_rb_`ker'_`pol' =  se_rb_`ker'_`pol'
	return scalar amse_rb_`ker'_`pol' = amse_rb_`ker'_`pol'
	return scalar h_`ker'_`pol' = h_`ker'_`pol'
	return scalar b_`ker'_`pol' = b_`ker'_`pol'
	return scalar n_`ker'_`pol' = n_`ker'_`pol'
	
	}
}
end

foreach n in 500 {
#delimit ;
simulate tau_bc_unif_0=r(tau_bc_unif_0) se_rb_unif_0=r(se_rb_unif_0) amse_rb_unif_0=r(amse_rb_unif_0) 
h_unif_0=r(h_unif_0) b_unif_0=r(b_unif_0) n_unif_0=r(n_unif_0)
tau_bc_unif_1=r(tau_bc_unif_1) se_rb_unif_1=r(se_rb_unif_1) amse_rb_unif_1=r(amse_rb_unif_1) 
h_unif_1=r(h_unif_1) b_unif_1=r(b_unif_1) n_unif_1=r(n_unif_1)
tau_bc_unif_2=r(tau_bc_unif_2) se_rb_unif_2=r(se_rb_unif_2) amse_rb_unif_2=r(amse_rb_unif_2)
h_unif_2=r(h_unif_2) b_unif_2=r(b_unif_2) n_unif_2=r(n_unif_2)
tau_bc_unif_3=r(tau_bc_unif_3) se_rb_unif_3=r(se_rb_unif_3) amse_rb_unif_3=r(amse_rb_unif_3) 
h_unif_3=r(h_unif_3) b_unif_3=r(b_unif_3) n_unif_3=r(n_unif_3)
tau_bc_unif_4=r(tau_bc_unif_4) se_rb_unif_4=r(se_rb_unif_4) amse_rb_unif_4=r(amse_rb_unif_4) 
h_unif_4=r(h_unif_4) b_unif_4=r(b_unif_4) n_unif_4=r(n_unif_4)

tau_bc_tri_0=r(tau_bc_tri_0) se_rb_tri_0=r(se_rb_tri_0) amse_rb_tri_0=r(amse_rb_tri_0) 
h_tri_0=r(h_tri_0) b_tri_0=r(b_tri_0) n_tri_0=r(n_tri_0)
tau_bc_tri_1=r(tau_bc_tri_1) se_rb_tri_1=r(se_rb_tri_1) amse_rb_tri_1=r(amse_rb_tri_1) 
h_tri_1=r(h_tri_1) b_tri_1=r(b_tri_1) n_tri_1=r(n_tri_1)
tau_bc_tri_2=r(tau_bc_tri_2) se_rb_tri_2=r(se_rb_tri_2) amse_rb_tri_2=r(amse_rb_tri_2)
h_tri_2=r(h_tri_2) b_tri_2=r(b_tri_2) n_tri_2=r(n_tri_2)
tau_bc_tri_3=r(tau_bc_tri_3) se_rb_tri_3=r(se_rb_tri_3) amse_rb_tri_3=r(amse_rb_tri_3)
h_tri_3=r(h_tri_3) b_tri_3=r(b_tri_3) n_tri_3=r(n_tri_3)
tau_bc_tri_4=r(tau_bc_tri_4) se_rb_tri_4=r(se_rb_tri_4) amse_rb_tri_4=r(amse_rb_tri_4)
h_tri_4=r(h_tri_4) b_tri_4=r(b_tri_4) n_tri_4=r(n_tri_4)

tau_bc_unif_noreg_0=r(tau_bc_unif_noreg_0) se_rb_unif_noreg_0=r(se_rb_unif_noreg_0) amse_rb_unif_noreg_0=r(amse_rb_unif_noreg_0) 
h_unif_noreg_0=r(h_unif_noreg_0) b_unif_noreg_0=r(b_unif_noreg_0) n_unif_noreg_0=r(n_unif_noreg_0)
tau_bc_unif_noreg_1=r(tau_bc_unif_noreg_1) se_rb_unif_noreg_1=r(se_rb_unif_noreg_1) amse_rb_unif_noreg_1=r(amse_rb_unif_noreg_1) 
h_unif_noreg_1=r(h_unif_noreg_1) b_unif_noreg_1=r(b_unif_noreg_1) n_unif_noreg_1=r(n_unif_noreg_1)
tau_bc_unif_noreg_2=r(tau_bc_unif_noreg_2) se_rb_unif_noreg_2=r(se_rb_unif_noreg_2) amse_rb_unif_noreg_2=r(amse_rb_unif_noreg_2)
h_unif_noreg_2=r(h_unif_noreg_2) b_unif_noreg_2=r(b_unif_noreg_2) n_unif_noreg_2=r(n_unif_noreg_2)
tau_bc_unif_noreg_3=r(tau_bc_unif_noreg_3) se_rb_unif_noreg_3=r(se_rb_unif_noreg_3) amse_rb_unif_noreg_3=r(amse_rb_unif_noreg_3)
h_unif_noreg_3=r(h_unif_noreg_3) b_unif_noreg_3=r(b_unif_noreg_3) n_unif_noreg_3=r(n_unif_noreg_3)
tau_bc_unif_noreg_4=r(tau_bc_unif_noreg_4) se_rb_unif_noreg_4=r(se_rb_unif_noreg_4) amse_rb_unif_noreg_4=r(amse_rb_unif_noreg_4)
h_unif_noreg_4=r(h_unif_noreg_4) b_unif_noreg_4=r(b_unif_noreg_4) n_unif_noreg_4=r(n_unif_noreg_4)

tau_bc_tri_noreg_0=r(tau_bc_tri_noreg_0) se_rb_tri_noreg_0=r(se_rb_tri_noreg_0) amse_rb_tri_noreg_0=r(amse_rb_tri_noreg_0) 
h_tri_noreg_0=r(h_tri_noreg_0) b_tri_noreg_0=r(b_tri_noreg_0) n_tri_noreg_0=r(n_tri_noreg_0)
tau_bc_tri_noreg_1=r(tau_bc_tri_noreg_1) se_rb_tri_noreg_1=r(se_rb_tri_noreg_1) amse_rb_tri_noreg_1=r(amse_rb_tri_noreg_1) 
h_tri_noreg_1=r(h_tri_noreg_1) b_tri_noreg_1=r(b_tri_noreg_1) n_tri_noreg_1=r(n_tri_noreg_1)
tau_bc_tri_noreg_2=r(tau_bc_tri_noreg_2) se_rb_tri_noreg_2=r(se_rb_tri_noreg_2) amse_rb_tri_noreg_2=r(amse_rb_tri_noreg_2)
h_tri_noreg_2=r(h_tri_noreg_2) b_tri_noreg_2=r(b_tri_noreg_2) n_tri_noreg_2=r(n_tri_noreg_2)
tau_bc_tri_noreg_3=r(tau_bc_tri_noreg_3) se_rb_tri_noreg_3=r(se_rb_tri_noreg_3) amse_rb_tri_noreg_3=r(amse_rb_tri_noreg_3) 
h_tri_noreg_3=r(h_tri_noreg_3) b_tri_noreg_3=r(b_tri_noreg_3) n_tri_noreg_3=r(n_tri_noreg_3)
tau_bc_tri_noreg_4=r(tau_bc_tri_noreg_4) se_rb_tri_noreg_4=r(se_rb_tri_noreg_4) amse_rb_tri_noreg_4=r(amse_rb_tri_noreg_4) 
h_tri_noreg_4=r(h_tri_noreg_4) b_tri_noreg_4=r(b_tri_noreg_4) n_tri_noreg_4=r(n_tri_noreg_4), reps(2500): RD_sim_DL_estbw, obs(`n');
#delimit cr

saveold "RD_sim_DL_rb_estbw_`n'_m`machine'_jbes.dta", replace

}