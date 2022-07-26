** RD simulation with the LM DGP **
** Infeasible (optimal) bandwidth, bias-corrected/robust estimator **

capture cd C:\Users\sal278_RS
capture cd C:\Users\user\Dropbox\RA_Pei\Suejin_Lee\local_poly\data
capture cd C:\Users\zp53\Dropbox\rdd3\data
capture cd C:\Users\peizhuan\Dropbox\rdd3\data

set more off
set rmsg on

*parallel setclusters 5

local machine = 1
set rngstream `machine'
set seed 3

cap program drop RD_sim_LM_optbw
program define RD_sim_LM_optbw, rclass

syntax [, obs(integer 500)]

drop _all 
set obs `obs'	   
gen x = 2*rbeta(2,4)-1
gen epsilon = 0.1295*rnormal()
gen Y = (3.71+2.30*x+3.28*x^2+1.45*x^3+0.23*x^4+0.03*x^5)*(x<0) ///
       + (0.26+18.49*x-54.81*x^2+74.30*x^3-45.02*x^4+9.83*x^5)*(x>=0)+epsilon

if (`obs'==500) {

	********************
	** uniform kernel **
	********************	
	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.0648282) b(0.196278) kernel(uniform)
	scalar tau_bc_unif_1 = e(tau_bc)
	scalar se_rb_unif_1 = e(se_rb)
	scalar h_unif_1 = e(h_bw)
	scalar b_unif_1 = e(b_bw)
	scalar n_unif_1 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(1) q(2) h(0.0648282) b(0.196278) q_pilot(0.431046) kernel(uniform)
	scalar amse_rb_unif_1 = e(CCT_AMSE)

	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.196278) b(0.431046) kernel(uniform)
	scalar tau_bc_unif_2 = e(tau_bc)
	scalar se_rb_unif_2 = e(se_rb)
	scalar h_unif_2 = e(h_bw)
	scalar b_unif_2 = e(b_bw)
	scalar n_unif_2 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(2) q(3) h(0.196278) b(0.431046) q_pilot(0.853584) kernel(uniform)
	scalar amse_rb_unif_2 = e(CCT_AMSE)

	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.431046) b(0.853584) kernel(uniform)
	scalar tau_bc_unif_3 = e(tau_bc)
	scalar se_rb_unif_3 = e(se_rb)
	scalar h_unif_3 = e(h_bw)
	scalar b_unif_3 = e(b_bw)
	scalar n_unif_3 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(3) q(4) h(0.431046) b(0.853584) q_pilot(1) kernel(uniform)
	scalar amse_rb_unif_3 = e(CCT_AMSE)

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.853584) b(1) kernel(uniform)
	scalar tau_bc_unif_4 = e(tau_bc)
	scalar se_rb_unif_4 = e(se_rb)
	scalar h_unif_4 = e(h_bw)
	scalar b_unif_4 = e(b_bw)
	scalar n_unif_4 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(4) q(5) h(0.853584) b(1) q_pilot(1) kernel(uniform)
	scalar amse_rb_unif_4 = e(CCT_AMSE)

	***********************
	** Triangular kernel **
	***********************
	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.0824783) b(0.234745) kernel(triangular)
	scalar tau_bc_tri_1 = e(tau_bc)
	scalar se_rb_tri_1 = e(se_rb)
	scalar h_tri_1 = e(h_bw)
	scalar b_tri_1 = e(b_bw)
	scalar n_tri_1 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(1) q(2) h(0.0824783) b(0.234745) q_pilot(0.496975) kernel(triangular)
	scalar amse_rb_tri_1 = e(CCT_AMSE)

	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.234745) b(0.496975) kernel(triangular)
	scalar tau_bc_tri_2 = e(tau_bc)
	scalar se_rb_tri_2 = e(se_rb)
	scalar h_tri_2 = e(h_bw)
	scalar b_tri_2 = e(b_bw)
	scalar n_tri_2 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(2) q(3) h(0.234745) b(0.496975) q_pilot(0.960602) kernel(triangular)
	scalar amse_rb_tri_2 = e(CCT_AMSE)

	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.496975) b(0.960602) kernel(triangular)
	scalar tau_bc_tri_3 = e(tau_bc)
	scalar se_rb_tri_3 = e(se_rb)
	scalar h_tri_3 = e(h_bw)
	scalar b_tri_3 = e(b_bw)
	scalar n_tri_3 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(3) q(4) h(0.496975) b(0.960602) q_pilot(1) kernel(triangular)
	scalar amse_rb_tri_3 = e(CCT_AMSE)

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.960602) b(1) kernel(triangular)
	scalar tau_bc_tri_4 = e(tau_bc)
	scalar se_rb_tri_4 = e(se_rb)
	scalar h_tri_4 = e(h_bw)
	scalar b_tri_4 = e(b_bw)
	scalar n_tri_4 = e(N)
	
	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(4) q(5) h(0.960602) b(1) q_pilot(1) kernel(triangular)
	scalar amse_rb_tri_4 = e(CCT_AMSE)

	}

if (`obs'==3105) {

	********************
	** uniform kernel **
	********************	
	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.0449931) b(0.151208) kernel(uniform)
	scalar tau_bc_unif_1 = e(tau_bc)
	scalar se_rb_unif_1 = e(se_rb)
	scalar h_unif_1 = e(h_bw)
	scalar b_unif_1 = e(b_bw)
	scalar n_unif_1 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(1) q(2) h(0.0449931) b(0.151208) q_pilot(0.351886) kernel(uniform)
	scalar amse_rb_unif_1 = e(CCT_AMSE)

	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.151208) b(0.351886) kernel(uniform)
	scalar tau_bc_unif_2 = e(tau_bc)
	scalar se_rb_unif_2 = e(se_rb)
	scalar amse_rb_unif_2 = e(CCT_AMSE)
	scalar h_unif_2 = e(h_bw)
	scalar b_unif_2 = e(b_bw)
	scalar n_unif_2 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(2) q(3) h(0.151208) b(0.351886) q_pilot(0.723014) kernel(uniform)
	scalar amse_rb_unif_2 = e(CCT_AMSE)

	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.351886) b(0.723014) kernel(uniform)
	scalar tau_bc_unif_3 = e(tau_bc)
	scalar se_rb_unif_3 = e(se_rb)
	scalar amse_rb_unif_3 = e(CCT_AMSE)
	scalar h_unif_3 = e(h_bw)
	scalar b_unif_3 = e(b_bw)
	scalar n_unif_3 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(3) q(4) h(0.351886) b(0.723014) q_pilot(1) kernel(uniform)
	scalar amse_rb_unif_3 = e(CCT_AMSE)

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.723014) b(1) kernel(uniform)
	scalar tau_bc_unif_4 = e(tau_bc)
	scalar se_rb_unif_4 = e(se_rb)
	scalar amse_rb_unif_4 = e(CCT_AMSE)
	scalar h_unif_4 = e(h_bw)
	scalar b_unif_4 = e(b_bw)
	scalar n_unif_4 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(4) q(5) h(0.723014) b(1) q_pilot(1) kernel(uniform)
	scalar amse_rb_unif_4 = e(CCT_AMSE)

	***********************
	** Triangular kernel **
	***********************
	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.0572429) b(0.180841) kernel(triangular)
	scalar tau_bc_tri_1 = e(tau_bc)
	scalar se_rb_tri_1 = e(se_rb)
	scalar amse_rb_tri_1 = e(CCT_AMSE)
	scalar h_tri_1 = e(h_bw)
	scalar b_tri_1 = e(b_bw)
	scalar n_tri_1 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(1) q(2) h(0.0572429) b(0.180841) q_pilot(0.405708) kernel(triangular)
	scalar amse_rb_tri_1 = e(CCT_AMSE)	

	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.180841) b(0.405708) kernel(triangular)
	scalar tau_bc_tri_2 = e(tau_bc)
	scalar se_rb_tri_2 = e(se_rb)
	scalar amse_rb_tri_2 = e(CCT_AMSE)
	scalar h_tri_2 = e(h_bw)
	scalar b_tri_2 = e(b_bw)
	scalar n_tri_2 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(2) q(3) h(0.180841) b(0.405708) q_pilot(0.813662) kernel(triangular)
	scalar amse_rb_tri_2 = e(CCT_AMSE)	
	
	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.405708) b(0.813662) kernel(triangular)
	scalar tau_bc_tri_3 = e(tau_bc)
	scalar se_rb_tri_3 = e(se_rb)
	scalar amse_rb_tri_3 = e(CCT_AMSE)
	scalar h_tri_3 = e(h_bw)
	scalar b_tri_3 = e(b_bw)
	scalar n_tri_3 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(3) q(4) h(0.405708) b(0.813662) q_pilot(1) kernel(triangular)
	scalar amse_rb_tri_3 = e(CCT_AMSE)		

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.813662) b(1) kernel(triangular)
	scalar tau_bc_tri_4 = e(tau_bc)
	scalar se_rb_tri_4 = e(se_rb)
	scalar amse_rb_tri_4 = e(CCT_AMSE)
	scalar h_tri_4 = e(h_bw)
	scalar b_tri_4 = e(b_bw)
	scalar n_tri_4 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(4) q(5) h(0.813662) b(1) q_pilot(1) kernel(triangular)
	scalar amse_rb_tri_4 = e(CCT_AMSE)
	
	}
				
forvalues pol = 1/4 {
	foreach ker in unif tri {

	return scalar tau_bc_`ker'_`pol' = tau_bc_`ker'_`pol'
	return scalar se_rb_`ker'_`pol' =  se_rb_`ker'_`pol'
	return scalar amse_rb_`ker'_`pol' = amse_rb_`ker'_`pol'
	return scalar h_`ker'_`pol' = h_`ker'_`pol'
	return scalar b_`ker'_`pol' = b_`ker'_`pol'
	return scalar n_`ker'_`pol' = n_`ker'_`pol'
	
	}
}

				
end	   


foreach n in 3105 {

#delimit ;
simulate tau_bc_unif_1=r(tau_bc_unif_1) se_rb_unif_1=r(se_rb_unif_1) amse_rb_unif_1=r(amse_rb_unif_1) 
h_unif_1=r(h_unif_1) b_unif_1=r(b_unif_1) n_unif_1=r(n_unif_1)
tau_bc_unif_2=r(tau_bc_unif_2) se_rb_unif_2=r(se_rb_unif_2) amse_rb_unif_2=r(amse_rb_unif_2)
h_unif_2=r(h_unif_2) b_unif_2=r(b_unif_2) n_unif_2=r(n_unif_2)
tau_bc_unif_3=r(tau_bc_unif_3) se_rb_unif_3=r(se_rb_unif_3) amse_rb_unif_3=r(amse_rb_unif_3) 
h_unif_3=r(h_unif_3) b_unif_3=r(b_unif_3) n_unif_3=r(n_unif_3)
tau_bc_unif_4=r(tau_bc_unif_4) se_rb_unif_4=r(se_rb_unif_4) amse_rb_unif_4=r(amse_rb_unif_4) 
h_unif_4=r(h_unif_4) b_unif_4=r(b_unif_4) n_unif_4=r(n_unif_4)

tau_bc_tri_1=r(tau_bc_tri_1) se_rb_tri_1=r(se_rb_tri_1) amse_rb_tri_1=r(amse_rb_tri_1) 
h_tri_1=r(h_tri_1) b_tri_1=r(b_tri_1) n_tri_1=r(n_tri_1)
tau_bc_tri_2=r(tau_bc_tri_2) se_rb_tri_2=r(se_rb_tri_2) amse_rb_tri_2=r(amse_rb_tri_2)
h_tri_2=r(h_tri_2) b_tri_2=r(b_tri_2) n_tri_2=r(n_tri_2)
tau_bc_tri_3=r(tau_bc_tri_3) se_rb_tri_3=r(se_rb_tri_3) amse_rb_tri_3=r(amse_rb_tri_3)
h_tri_3=r(h_tri_3) b_tri_3=r(b_tri_3) n_tri_3=r(n_tri_3)
tau_bc_tri_4=r(tau_bc_tri_4) se_rb_tri_4=r(se_rb_tri_4) amse_rb_tri_4=r(amse_rb_tri_4)
h_tri_4=r(h_tri_4) b_tri_4=r(b_tri_4) n_tri_4=r(n_tri_4), reps(5000): RD_sim_LM_optbw, obs(`n');
#delimit cr

saveold "RD_sim_LM_rb_optbw_`n'_m`machine'_jbes.dta", replace
}
