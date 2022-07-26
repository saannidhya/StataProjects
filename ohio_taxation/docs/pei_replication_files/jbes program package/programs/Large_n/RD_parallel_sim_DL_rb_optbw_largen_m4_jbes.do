** RD simulation with the Lee DGP **
** Infeasible (optimal) bandwidth, bias-corrected/robust estimator **

capture cd C:\Users\sal278_RS
capture cd C:\Users\user\Dropbox\RA_Pei\Suejin_Lee\local_poly\data
capture cd C:\Users\zp53\Dropbox\rdd3\data
capture cd C:\Users\peizhuan\Dropbox\rdd3\data

set more off
set rmsg on

*parallel setclusters 5

local machine = 4
set rngstream `machine'
set seed 2

cap program drop RD_sim_DL_optbw
program define RD_sim_DL_optbw, rclass

syntax [, obs(integer 500)]

drop _all 
set obs `obs'	   
gen x = 2*rbeta(2,4)-1
gen epsilon = 0.1295*rnormal()
gen Y = (0.48+1.27*x+7.18*x^2+20.21*x^3+21.54*x^4+7.33*x^5)*(x<0) ///
       +(0.52+0.84*x-3.00*x^2+ 7.99*x^3- 9.01*x^4+3.56*x^5)*(x>=0) + epsilon

if (`obs'==500) {

	********************
	** uniform kernel **
	********************
	** local constant **
	capture altrdrobust Y x, deriv(0) c(0) p(0) q(1) h(0.0363967) b(0.130108) kernel(uniform)
	scalar tau_bc_unif_0 = e(tau_bc)
	scalar se_rb_unif_0 = e(se_rb)
	scalar h_unif_0 = e(h_bw)
	scalar b_unif_0 = e(b_bw)
	scalar n_unif_0 = e(N)
	
	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(0) q(1) h(0.0363967) b(0.130108) q_pilot(0.260305) kernel(uniform) 
	scalar amse_rb_unif_0 = e(CCT_AMSE)	
	
	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.130108) b(0.260305) kernel(uniform)
	scalar tau_bc_unif_1 = e(tau_bc)
	scalar se_rb_unif_1 = e(se_rb)
	scalar h_unif_1 = e(h_bw)
	scalar b_unif_1 = e(b_bw)
	scalar n_unif_1 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(1) q(2) h(0.130108) b(0.260305) q_pilot(0.470366) kernel(uniform)
	scalar amse_rb_unif_1 = e(CCT_AMSE)	

	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.260305) b(0.470366) kernel(uniform)
	scalar tau_bc_unif_2 = e(tau_bc)
	scalar se_rb_unif_2 = e(se_rb)
	scalar h_unif_2 = e(h_bw)
	scalar b_unif_2 = e(b_bw)
	scalar n_unif_2 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(2) q(3) h(0.260305) b(0.470366) q_pilot(0.838302) kernel(uniform)
	scalar amse_rb_unif_2 = e(CCT_AMSE)	

	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.470366) b(0.838302) kernel(uniform)
	scalar tau_bc_unif_3 = e(tau_bc)
	scalar se_rb_unif_3 = e(se_rb)
	scalar h_unif_3 = e(h_bw)
	scalar b_unif_3 = e(b_bw)
	scalar n_unif_3 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(3) q(4) h(0.470366) b(0.838302) q_pilot(1) kernel(uniform)
	scalar amse_rb_unif_3 = e(CCT_AMSE)	

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.838302) b(1) kernel(uniform)
	scalar tau_bc_unif_4 = e(tau_bc)
	scalar se_rb_unif_4 = e(se_rb)
	scalar h_unif_4 = e(h_bw)
	scalar b_unif_4 = e(b_bw)
	scalar n_unif_4 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(4) q(5) h(0.838302) b(1) q_pilot(1) kernel(uniform)
	scalar amse_rb_unif_4 = e(CCT_AMSE)	

	***********************
	** Triangular kernel **
	***********************
	** local constant **
	capture altrdrobust Y x, deriv(0) c(0) p(0) q(1) h(0.0524931) b(0.165532) kernel(triangular)
	scalar tau_bc_tri_0 = e(tau_bc)
	scalar se_rb_tri_0 = e(se_rb)
	scalar h_tri_0 = e(h_bw)
	scalar b_tri_0 = e(b_bw)
	scalar n_tri_0 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(0) q(1) h(0.0524931) b(0.165532) q_pilot(0.311319) kernel(triangular)
	scalar amse_rb_tri_0 = e(CCT_AMSE)	

	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.165532) b(0.311319) kernel(triangular)
	scalar tau_bc_tri_1 = e(tau_bc)
	scalar se_rb_tri_1 = e(se_rb)
	scalar h_tri_1 = e(h_bw)
	scalar b_tri_1 = e(b_bw)
	scalar n_tri_1 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(1) q(2) h(0.165532) b(0.311319) q_pilot(0.54231) kernel(triangular)
	scalar amse_rb_tri_1 = e(CCT_AMSE)	

	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.311319) b(0.54231) kernel(triangular)
	scalar tau_bc_tri_2 = e(tau_bc)
	scalar se_rb_tri_2 = e(se_rb)
	scalar h_tri_2 = e(h_bw)
	scalar b_tri_2 = e(b_bw)
	scalar n_tri_2 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(2) q(3) h(0.311319) b(0.54231) q_pilot(0.943404) kernel(triangular)
	scalar amse_rb_tri_2 = e(CCT_AMSE)	

	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.54231) b(0.943404) kernel(triangular)
	scalar tau_bc_tri_3 = e(tau_bc)
	scalar se_rb_tri_3 = e(se_rb)
	scalar h_tri_3 = e(h_bw)
	scalar b_tri_3 = e(b_bw)
	scalar n_tri_3 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(3) q(4) h(0.54231) b(0.943404) q_pilot(1) kernel(triangular)
	scalar amse_rb_tri_3 = e(CCT_AMSE)	

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.943404) b(1) kernel(triangular)
	scalar tau_bc_tri_4 = e(tau_bc)
	scalar se_rb_tri_4 = e(se_rb)
	scalar h_tri_4 = e(h_bw)
	scalar b_tri_4 = e(b_bw)
	scalar n_tri_4 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(4) q(5) h(0.943404) b(1) q_pilot(1) kernel(triangular)
	scalar amse_rb_tri_4 = e(CCT_AMSE)	

				}
				
				
if (`obs'==6558) {

	********************
	** uniform kernel **
	********************
	** local constant **
	capture altrdrobust Y x, deriv(0) c(0) p(0) q(1) h(0.0154334) b(0.077758) kernel(uniform)
	scalar tau_bc_unif_0 = e(tau_bc)
	scalar se_rb_unif_0 = e(se_rb)
	scalar h_unif_0 = e(h_bw)
	scalar b_unif_0 = e(b_bw)
	scalar n_unif_0 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(0) q(1) h(0.0154334) b(0.077758) q_pilot(0.180217) kernel(uniform)
	scalar amse_rb_unif_0 = e(CCT_AMSE)	

	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.077758) b(0.180217) kernel(uniform)
	scalar tau_bc_unif_1 = e(tau_bc)
	scalar se_rb_unif_1 = e(se_rb)
	scalar h_unif_1 = e(h_bw)
	scalar b_unif_1 = e(b_bw)
	scalar n_unif_1 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(1) q(2) h(0.077758) b(0.180217) q_pilot(0.353375) kernel(uniform)
	scalar amse_rb_unif_1 = e(CCT_AMSE)	

	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.180217) b(0.353375) kernel(uniform)
	scalar tau_bc_unif_2 = e(tau_bc)
	scalar se_rb_unif_2 = e(se_rb)
	scalar h_unif_2 = e(h_bw)
	scalar b_unif_2 = e(b_bw)
	scalar n_unif_2 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(2) q(3) h(0.180217) b(0.353375) q_pilot(0.66341) kernel(uniform)
	scalar amse_rb_unif_2 = e(CCT_AMSE)	

	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.353375) b(0.66341) kernel(uniform)
	scalar tau_bc_unif_3 = e(tau_bc)
	scalar se_rb_unif_3 = e(se_rb)
	scalar h_unif_3 = e(h_bw)
	scalar b_unif_3 = e(b_bw)
	scalar n_unif_3 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(3) q(4) h(0.353375) b(0.66341) q_pilot(1) kernel(uniform)
	scalar amse_rb_unif_3 = e(CCT_AMSE)	

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.66341) b(1) kernel(uniform)
	scalar tau_bc_unif_4 = e(tau_bc)
	scalar se_rb_unif_4 = e(se_rb)
	scalar h_unif_4 = e(h_bw)
	scalar b_unif_4 = e(b_bw)
	scalar n_unif_4 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(4) q(5) h(0.66341) b(1) q_pilot(1) kernel(uniform)
	scalar amse_rb_unif_4 = e(CCT_AMSE)	

	***********************
	** Triangular kernel **
	***********************
	** local constant **
	capture altrdrobust Y x, deriv(0) c(0) p(0) q(1) h(0.0222588) b(0.0989283) kernel(triangular)
	scalar tau_bc_tri_0 = e(tau_bc)
	scalar se_rb_tri_0 = e(se_rb)
	scalar h_tri_0 = e(h_bw)
	scalar b_tri_0 = e(b_bw)
	scalar n_tri_0 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(0) q(1) h(0.0222588) b(0.0989283) q_pilot(0.215536) kernel(triangular)
	scalar amse_rb_tri_0 = e(CCT_AMSE)	

	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.0989283) b(0.215536) kernel(triangular)
	scalar tau_bc_tri_1 = e(tau_bc)
	scalar se_rb_tri_1 = e(se_rb)
	scalar h_tri_1 = e(h_bw)
	scalar b_tri_1 = e(b_bw)
	scalar n_tri_1 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(1) q(2) h(0.0989283) b(0.215536) q_pilot(0.407424) kernel(triangular)
	scalar amse_rb_tri_1 = e(CCT_AMSE)	

	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.215536) b(0.407424) kernel(triangular)
	scalar tau_bc_tri_2 = e(tau_bc)
	scalar se_rb_tri_2 = e(se_rb)
	scalar h_tri_2 = e(h_bw)
	scalar b_tri_2 = e(b_bw)
	scalar n_tri_2 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(2) q(3) h(0.215536) b(0.407424) q_pilot(0.746585) kernel(triangular)
	scalar amse_rb_tri_2 = e(CCT_AMSE)	

	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.407424) b(0.746585) kernel(triangular)
	scalar tau_bc_tri_3 = e(tau_bc)
	scalar se_rb_tri_3 = e(se_rb)
	scalar h_tri_3 = e(h_bw)
	scalar b_tri_3 = e(b_bw)
	scalar n_tri_3 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(3) q(4) h(0.407424) b(0.746585) q_pilot(1) kernel(triangular)
	scalar amse_rb_tri_3 = e(CCT_AMSE)	

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.746585) b(1) kernel(triangular)
	scalar tau_bc_tri_4 = e(tau_bc)
	scalar se_rb_tri_4 = e(se_rb)
	scalar h_tri_4 = e(h_bw)
	scalar b_tri_4 = e(b_bw)
	scalar n_tri_4 = e(N)
	scalar amse_rb_tri_4 = e(CCT_AMSE)	

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(4) q(5) h(0.746585) b(1) q_pilot(1) kernel(triangular)

				}

if (`obs'==60000) {

	********************
	** uniform kernel **
	********************
	** local constant **
	capture altrdrobust Y x, deriv(0) c(0) p(0) q(1) h(0.00737906) b(0.0499424) kernel(uniform)
	scalar tau_bc_unif_0 = e(tau_bc)
	scalar se_rb_unif_0 = e(se_rb)
	scalar h_unif_0 = e(h_bw)
	scalar b_unif_0 = e(b_bw)
	scalar n_unif_0 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(0) q(1) h(0.00737906) b(0.0499424) q_pilot(0.131358) kernel(uniform)
	scalar amse_rb_unif_0 = e(CCT_AMSE)	

	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.0499424) b(0.131358) kernel(uniform)
	scalar tau_bc_unif_1 = e(tau_bc)
	scalar se_rb_unif_1 = e(se_rb)
	scalar h_unif_1 = e(h_bw)
	scalar b_unif_1 = e(b_bw)
	scalar n_unif_1 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(1) q(2) h(0.0499424) b(0.131358) q_pilot(0.276322) kernel(uniform)
	scalar amse_rb_unif_1 = e(CCT_AMSE)	

	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.131358) b(0.276322) kernel(uniform)
	scalar tau_bc_unif_2 = e(tau_bc)
	scalar se_rb_unif_2 = e(se_rb)
	scalar h_unif_2 = e(h_bw)
	scalar b_unif_2 = e(b_bw)
	scalar n_unif_2 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(2) q(3) h(0.131358) b(0.276322) q_pilot(0.54248) kernel(uniform)
	scalar amse_rb_unif_2 = e(CCT_AMSE)	

	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.276322) b(0.54248) kernel(uniform)
	scalar tau_bc_unif_3 = e(tau_bc)
	scalar se_rb_unif_3 = e(se_rb)
	scalar h_unif_3 = e(h_bw)
	scalar b_unif_3 = e(b_bw)
	scalar n_unif_3 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(3) q(4) h(0.276322) b(0.54248) q_pilot(1) kernel(uniform)
	scalar amse_rb_unif_3 = e(CCT_AMSE)	

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.54248) b(1) kernel(uniform)
	scalar tau_bc_unif_4 = e(tau_bc)
	scalar se_rb_unif_4 = e(se_rb)
	scalar h_unif_4 = e(h_bw)
	scalar b_unif_4 = e(b_bw)
	scalar n_unif_4 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(4) q(5) h(0.54248) b(1) q_pilot(1) kernel(uniform)
	scalar amse_rb_unif_4 = e(CCT_AMSE)	

	***********************
	** Triangular kernel **
	***********************
	** local constant **
	capture altrdrobust Y x, deriv(0) c(0) p(0) q(1) h(0.0106424) b(0.0635396) kernel(triangular)
	scalar tau_bc_tri_0 = e(tau_bc)
	scalar se_rb_tri_0 = e(se_rb)
	scalar h_tri_0 = e(h_bw)
	scalar b_tri_0 = e(b_bw)
	scalar n_tri_0 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(0) q(1) h(0.0106424) b(0.0635396) q_pilot(0.157101) kernel(triangular)
	scalar amse_rb_tri_0 = e(CCT_AMSE)	

	** linear **
	capture altrdrobust Y x, deriv(0) c(0) p(1) q(2) h(0.0635396) b(0.157101) kernel(triangular)
	scalar tau_bc_tri_1 = e(tau_bc)
	scalar se_rb_tri_1 = e(se_rb)
	scalar h_tri_1 = e(h_bw)
	scalar b_tri_1 = e(b_bw)
	scalar n_tri_1 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(1) q(2) h(0.0635396) b(0.157101) q_pilot(0.318586) kernel(triangular)
	scalar amse_rb_tri_1 = e(CCT_AMSE)	

	** quadratic **
	capture altrdrobust Y x, deriv(0) c(0) p(2) q(3) h(0.157101) b(0.318586) kernel(triangular)
	scalar tau_bc_tri_2 = e(tau_bc)
	scalar se_rb_tri_2 = e(se_rb)
	scalar h_tri_2 = e(h_bw)
	scalar b_tri_2 = e(b_bw)
	scalar n_tri_2 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(2) q(3) h(0.157101) b(0.318586) q_pilot(0.610494) kernel(triangular)
	scalar amse_rb_tri_2 = e(CCT_AMSE)	

	** cubic **
	capture altrdrobust Y x, deriv(0) c(0) p(3) q(4) h(0.318586) b(0.610494) kernel(triangular)
	scalar tau_bc_tri_3 = e(tau_bc)
	scalar se_rb_tri_3 = e(se_rb)
	scalar h_tri_3 = e(h_bw)
	scalar b_tri_3 = e(b_bw)
	scalar n_tri_3 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(3) q(4) h(0.318586) b(0.610494) q_pilot(1) kernel(triangular)
	scalar amse_rb_tri_3 = e(CCT_AMSE)	

	** quartic **
	capture altrdrobust Y x, deriv(0) c(0) p(4) q(5) h(0.610494) b(1) kernel(triangular)
	scalar tau_bc_tri_4 = e(tau_bc)
	scalar se_rb_tri_4 = e(se_rb)
	scalar h_tri_4 = e(h_bw)
	scalar b_tri_4 = e(b_bw)
	scalar n_tri_4 = e(N)

	capture altrdcctamse_preasym_fixbw Y x, deriv(0) c(0) p(4) q(5) h(0.610494) b(1) q_pilot(1) kernel(triangular)
	scalar amse_rb_tri_4 = e(CCT_AMSE)	

				}

forvalues pol = 0/4 {
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

foreach n in /*500 6558*/ 60000 {

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
h_tri_4=r(h_tri_4) b_tri_4=r(b_tri_4) n_tri_4=r(n_tri_4), reps(2500): RD_sim_DL_optbw, obs(`n');
#delimit cr

saveold "RD_sim_DL_rb_optbw_`n'_m`machine'_jbes.dta", replace
}
