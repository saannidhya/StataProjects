** RD simulation with the LM DGP **
** Infeasible (optimal) bandwidth, conventional estimator **

capture cd C:\Users\sal278_RS
capture cd C:\Users\user\Dropbox\RA_Pei\Suejin_Lee\local_poly\data
capture cd C:\Users\zp53\Dropbox\rdd3\data
capture cd C:\Users\peizhuan\Dropbox\rdd3\data

set more off
set rmsg on

*parallel setclusters 5

local machine = 2
set rngstream `machine'
set seed 1

cap program drop RD_sim_LM_optbw
program define RD_sim_LM_optbw, rclass

syntax [, obs(integer 500)]

drop _all 
set obs `obs'	   
gen x = 2*rbeta(2,4)-1
gen epsilon = 0.1295*rnormal()
gen Y = (3.71+2.30*x+3.28*x^2+1.45*x^3+0.23*x^4+0.03*x^5)*(x<0) ///
       + (0.26+18.49*x-54.81*x^2+74.30*x^3-45.02*x^4+9.83*x^5)*(x>=0)+epsilon

if (`obs'==30000) {

	********************
	** uniform kernel **
	********************
	** local constant **
	capture altrdrobust Y x, deriv(0) c(0) p(0) q(1) h(0.00202285) b(0.0285847) kernel(uniform)
	scalar tau_cl_unif_0 = e(tau_cl)
	scalar se_cl_unif_0 = e(se_cl)
	scalar amse_cl_unif_0 = e(Bias_tau)^2 + e(se_cl)^2
	scalar h_unif_0 = e(h_bw)
	scalar b_unif_0 = e(b_bw)
	scalar n_unif_0 = e(N)
	
	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.0285847) b(0.109358) kernel(uniform)
	scalar tau_cl_unif_1 = e(tau_cl)
	scalar se_cl_unif_1 = e(se_cl)
	scalar amse_cl_unif_1 = e(Bias_tau)^2 + e(se_cl)^2
	scalar h_unif_1 = e(h_bw)
	scalar b_unif_1 = e(b_bw)
	scalar n_unif_1 = e(N)
	
	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.109358) b(0.273496) kernel(uniform)
	scalar tau_cl_unif_2 = e(tau_cl)
	scalar se_cl_unif_2 = e(se_cl)
	scalar amse_cl_unif_2 = e(Bias_tau)^2 + e(se_cl)^2
	scalar h_unif_2 = e(h_bw)
	scalar b_unif_2 = e(b_bw)
	scalar n_unif_2 = e(N)

	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.273496) b(0.588296) kernel(uniform)
	scalar tau_cl_unif_3 = e(tau_cl)
	scalar se_cl_unif_3 = e(se_cl)
	scalar amse_cl_unif_3 = e(Bias_tau)^2 + e(se_cl)^2
	scalar h_unif_3 = e(h_bw)
	scalar b_unif_3 = e(b_bw)
	scalar n_unif_3 = e(N)

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.588296) b(1) kernel(uniform)
	scalar tau_cl_unif_4 = e(tau_cl)
	scalar se_cl_unif_4 = e(se_cl)
	scalar amse_cl_unif_4 = e(Bias_tau)^2 + e(se_cl)^2
	scalar h_unif_4 = e(h_bw)
	scalar b_unif_4 = e(b_bw)
	scalar n_unif_4 = e(N)

	***********************
	** Triangular kernel **
	***********************
	** local constant **
	capture altrdrobust Y x, deriv(0) c(0) p(0) q(1) h(0.00291745) b(0.0363672) kernel(triangular)
	scalar tau_cl_tri_0 = e(tau_cl)
	scalar se_cl_tri_0 = e(se_cl)
	scalar amse_cl_tri_0 = e(Bias_tau)^2 + e(se_cl)^2
	scalar h_tri_0 = e(h_bw)
	scalar b_tri_0 = e(b_bw)
	scalar n_tri_0 = e(N)
	
	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.0363672) b(0.13079) kernel(triangular)
	scalar tau_cl_tri_1 = e(tau_cl)
	scalar se_cl_tri_1 = e(se_cl)
	scalar amse_cl_tri_1 = e(Bias_tau)^2 + e(se_cl)^2
	scalar h_tri_1 = e(h_bw)
	scalar b_tri_1 = e(b_bw)
	scalar n_tri_1 = e(N)
	
	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.13079) b(0.315328) kernel(triangular)
	scalar tau_cl_tri_2 = e(tau_cl)
	scalar se_cl_tri_2 = e(se_cl)
	scalar amse_cl_tri_2 = e(Bias_tau)^2 + e(se_cl)^2
	scalar h_tri_2 = e(h_bw)
	scalar b_tri_2 = e(b_bw)
	scalar n_tri_2 = e(N)

	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.315328) b(0.662054) kernel(triangular)
	scalar tau_cl_tri_3 = e(tau_cl)
	scalar se_cl_tri_3 = e(se_cl)
	scalar amse_cl_tri_3 = e(Bias_tau)^2 + e(se_cl)^2
	scalar h_tri_3 = e(h_bw)
	scalar b_tri_3 = e(b_bw)
	scalar n_tri_3 = e(N)

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.662054) b(1) kernel(triangular)
	scalar tau_cl_tri_4 = e(tau_cl)
	scalar se_cl_tri_4 = e(se_cl)
	scalar amse_cl_tri_4 = e(Bias_tau)^2 + e(se_cl)^2
	scalar h_tri_4 = e(h_bw)
	scalar b_tri_4 = e(b_bw)
	scalar n_tri_4 = e(N)
	}

				
forvalues pol = 0/4 {
	foreach ker in unif tri {

	return scalar tau_cl_`ker'_`pol' = tau_cl_`ker'_`pol'
	return scalar se_cl_`ker'_`pol' =  se_cl_`ker'_`pol'
	return scalar amse_cl_`ker'_`pol' = amse_cl_`ker'_`pol'
	return scalar h_`ker'_`pol' = h_`ker'_`pol'
	return scalar b_`ker'_`pol' = b_`ker'_`pol'
	return scalar n_`ker'_`pol' = n_`ker'_`pol'
	
	}
}

end	   


foreach n in 30000 {

#delimit ;
simulate tau_cl_unif_0=r(tau_cl_unif_0) se_cl_unif_0=r(se_cl_unif_0) amse_cl_unif_0=r(amse_cl_unif_0) 
h_unif_0=r(h_unif_0) b_unif_0=r(b_unif_0) n_unif_0=r(n_unif_0)
tau_cl_unif_1=r(tau_cl_unif_1) se_cl_unif_1=r(se_cl_unif_1) amse_cl_unif_1=r(amse_cl_unif_1) 
h_unif_1=r(h_unif_1) b_unif_1=r(b_unif_1) n_unif_1=r(n_unif_1)
tau_cl_unif_2=r(tau_cl_unif_2) se_cl_unif_2=r(se_cl_unif_2) amse_cl_unif_2=r(amse_cl_unif_2)
h_unif_2=r(h_unif_2) b_unif_2=r(b_unif_2) n_unif_2=r(n_unif_2)
tau_cl_unif_3=r(tau_cl_unif_3) se_cl_unif_3=r(se_cl_unif_3) amse_cl_unif_3=r(amse_cl_unif_3) 
h_unif_3=r(h_unif_3) b_unif_3=r(b_unif_3) n_unif_3=r(n_unif_3)
tau_cl_unif_4=r(tau_cl_unif_4) se_cl_unif_4=r(se_cl_unif_4) amse_cl_unif_4=r(amse_cl_unif_4) 
h_unif_4=r(h_unif_4) b_unif_4=r(b_unif_4) n_unif_4=r(n_unif_4)

tau_cl_tri_0=r(tau_cl_tri_0) se_cl_tri_0=r(se_cl_tri_0) amse_cl_tri_0=r(amse_cl_tri_0) 
h_tri_0=r(h_tri_0) b_tri_0=r(b_tri_0) n_tri_0=r(n_tri_0)
tau_cl_tri_1=r(tau_cl_tri_1) se_cl_tri_1=r(se_cl_tri_1) amse_cl_tri_1=r(amse_cl_tri_1) 
h_tri_1=r(h_tri_1) b_tri_1=r(b_tri_1) n_tri_1=r(n_tri_1)
tau_cl_tri_2=r(tau_cl_tri_2) se_cl_tri_2=r(se_cl_tri_2) amse_cl_tri_2=r(amse_cl_tri_2)
h_tri_2=r(h_tri_2) b_tri_2=r(b_tri_2) n_tri_2=r(n_tri_2)
tau_cl_tri_3=r(tau_cl_tri_3) se_cl_tri_3=r(se_cl_tri_3) amse_cl_tri_3=r(amse_cl_tri_3)
h_tri_3=r(h_tri_3) b_tri_3=r(b_tri_3) n_tri_3=r(n_tri_3)
tau_cl_tri_4=r(tau_cl_tri_4) se_cl_tri_4=r(se_cl_tri_4) amse_cl_tri_4=r(amse_cl_tri_4)
h_tri_4=r(h_tri_4) b_tri_4=r(b_tri_4) n_tri_4=r(n_tri_4),
reps(5000): RD_sim_LM_optbw, obs(`n');
#delimit cr

save "RD_sim_LM_cl_optbw_`n'_m`machine'_jbes.dta", replace
}