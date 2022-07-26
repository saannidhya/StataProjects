* edited Oct 25, 2020
* Added multicollearity checks 

capture program drop altrdcctamse_preasym_fixbw
program define altrdcctamse_preasym_fixbw, eclass
	syntax anything [if] [in] [, c(real 0) deriv(real 0) fuzzy(string) p(real 1) q(real 0) h(real 0) b(real 0) q_pilot(real 0) kernel(string) bwselect(string) delta(real 0.5) rho(real 0) vce(string) matches(real 3) level(real 95) cvgrid_min(real 0) cvgrid_max(real 0) cvgrid_length(real 0) cvplot all scalepar(real 1) scaleregul(real 1) nowarning]
	
	marksample touse

	local kernel = lower("`kernel'")
	local bwselect = upper("`bwselect'")
	local vce = lower("`vce'")

	preserve
	qui keep if `touse'

	tokenize "`anything'"
	local y `1'
	local x `2'

	qui drop if `y'==. | `x'==.
	
	if ("`fuzzy'"~="") {
		qui drop if `y'==. | `x'==. | `fuzzy'==.
	}


	tempvar x_l x_r y_l y_r uh_l uh_r ub_l ub_r T T_l T_r

	qui gen `x_l' = `x' if `x'<`c'
	qui gen `x_r' = `x' if `x'>=`c'
	qui gen `y_l' = `y' if `x'<`c'
	qui gen `y_r' = `y' if `x'>=`c'
	qui su `x'
	local x_min = r(min)
	local x_max = r(max)
	qui su `x_l'
	local N_l = r(N)
	local range_l = abs(r(max)-r(min))
	qui su `x_r' 
	local N_r = r(N)
	local range_r = abs(r(max)-r(min))
	local N = `N_r' + `N_l'
	local m = `matches' + 1

	if ("`deriv'">"0" & "`p'"=="1" & "`q'"=="0"){
		local p = `deriv'+1
	}

	if ("`q'"=="0") {
		local q = `p'+1
	}

	local p1 = `p' + 1
	local q1 = `q' + 1
	
	**************************** ERRORS
	if (`c'<=`x_min' | `c'>=`x_max'){
	 di "{err}{cmd:c()} should be set within the range of `x'"  
	 exit 125
	}
	
	if (`N_l'<10 | `N_r'<10){
	 di "{err}Not enough observations to perform calculations"  
	 exit 2001
	}
	
	if ("`p'">"8"){
	 di "{err}{cmd:p()} should be less or equal than 8 for this version of the software package"  
	 exit 125
	}
	
	if ("`kernel'"~="uni" & "`kernel'"~="uniform" & "`kernel'"~="tri" & "`kernel'"~="triangular" & "`kernel'"~="epa" & "`kernel'"~="epanechnikov" & "`kernel'"~="" ){
	 di "{err}{cmd:kernel()} incorrectly specified"  
	 exit 7
	}

	if ("`bwselect'"~="CCT" & "`bwselect'"~="IK" & "`bwselect'"~="CV" & "`bwselect'"~=""){
	 di "{err}{cmd:bwselect()} incorrectly specified"  
	 exit 7
	}

	if ("`vce'"~="resid" & "`vce'"~="nn" & "`vce'"~=""){ 
	 di "{err}{cmd:vce()} incorrectly specified"  
	 exit 7
	}

	if ("`p'"<"0" | "`q'"<="0" | "`deriv'"<"0" | "`matches'"<="0" | `scaleregul'<0){
	 di "{err}{cmd:p()}, {cmd:q()}, {cmd:deriv()}, {cmd:matches()} and {cmd:scaleregul()} should be positive"  
	 exit 411
	}
		
	if ("`p'">="`q'" & "`q'">"0"){
	 di "{err}{cmd:q()} should be higher than {cmd:p()}"  
	 exit 125
	}
	
	if ("`deriv'">"`p'" & "`deriv'">"0" ){
	 di "{err}{cmd:deriv()} can not be higher than {cmd:p()}"  
	 exit 125
	}

	if ("`p'">"0" ) {
	local p_round = round(`p')/`p'
	local q_round = round(`q')/`q'
	local d_round = round(`deriv'+1)/(`deriv'+1)
	local m_round = round(`matches')/`matches'

	if (`p_round'!=1 | `q_round'!=1 |`d_round'!=1 |`m_round'!=1 ){
	 di "{err}{cmd:p()}, {cmd:q()}, {cmd:deriv()} and {cmd:matches()} should be integers"  
	 exit 126
	}
	}
	
	if (`delta'>1 | `delta'<=0){
	 di "{err}{cmd:delta()}should be set between 0 and 1"  
	 exit 125
	}

	if (`level'>100 | `level'<=0){
	 di "{err}{cmd:level()}should be set between 0 and 100"  
	 exit 125
	}

	if (`rho'>1 | `rho'<0){
	 di "{err}{cmd:rho()}should be set between 0 and 1"  
	 exit 125
	}

	if (`cvgrid_min'<0 | `cvgrid_max'<0 | `cvgrid_length'<0 ){
	 di "{err}{cmd:cvgrid_min()}, {cmd:cvgrid_max()} and {cmd:cvgrid_length()} should be positive numbers"
	 exit 126
	}

	if (`cvgrid_min'>`cvgrid_max' ){
		di "{err}{cmd:cvgrid_min()} should be lower than {cmd:cvgrid_max()}"
		exit 125
	}

	if (`deriv'>0 & ("`bwselect'"=="IK" | "`bwselect'"=="CV" )) {
		di "{err}{cmd:IK} and {cmd:CV} implementations are not availale for {cmd:deriv}>0; use CCT instead"
		exit 125
	}
	
	if `h' >0 {
		local bwselect = "Manual"
	}
	
	local use_rho = 0
		
	if ("`h'"=="0" & "`b'"=="0" & `rho'>0) {
		local use_rho = 1
	}
	
	if (`h'>0 & `b'==0 & `rho'==0) {
		local b = `h'
	}
	
	if ("`kernel'"=="epanechnikov" | "`kernel'"=="epa") {
		local kernel_type = "Epanechnikov"
	}
	else if ("`kernel'"=="uniform" | "`kernel'"=="uni") {
		local kernel_type = "Uniform"
	}
	else  {
		local kernel_type = "Triangular"
	}

	
	
	disp in yellow "Preparing data." 
	
	
	sort `x', stable

	mata {

	Y   = st_data(.,("`y'"),   0)
	X   = st_data(.,("`x'"),   0)
	Y_l = st_data(.,("`y_l'"), 0)
	Y_r = st_data(.,("`y_r'"), 0)
	X_l = st_data(.,("`x_l'"), 0)
	X_r = st_data(.,("`x_r'"), 0)
	c = `c'
	N_l = length(X_l)
	N_r = length(X_r)
	c_l  = J(N_l, 1, c)
	c_r  = J(N_r, 1, c)
	p1 = `p' + 1
	q1 = `q' + 1
	q2 = `q' + 2	
	X_lq1 =  J(N_l,q2,.)
	X_rq1 =  J(N_r,q2,.)
	for (j=1; j<=q2; j++)  {
		X_lq1[.,j]  = (X_l:-c):^(j-1)
		X_rq1[.,j]  = (X_r:-c):^(j-1)
							}
	w_q_l = kweight(X_l,c,`q_pilot',"`kernel'")
	w_q_r = kweight(X_r,c,`q_pilot',"`kernel'")
	
*	if (rank(quadcross(X_lq1, w_q_l, X_lq1))<cols(quadcross(X_lq1, w_q_l, X_lq1)) | rank(quadcross(X_rq1, w_q_r, X_rq1))<cols(quadcross(X_rq1, w_q_r, X_rq1))){
*		errprintf("multicollinearity encountered\n")
*		exit(499)
*		}	
	
	m3_l = (cholinv(quadcross(X_lq1, w_q_l, X_lq1))*quadcross(X_lq1, w_q_l, Y_l))[q2,1]
	m3_r = (cholinv(quadcross(X_rq1, w_q_r, X_rq1))*quadcross(X_rq1, w_q_r, Y_r))[q2,1]

	
	wh_l = kweight(X_l,`c',`h',"`kernel'");	wh_r = kweight(X_r,`c',`h',"`kernel'")
	wb_l = kweight(X_l,`c',`b',"`kernel'");	wb_r = kweight(X_r,`c',`b',"`kernel'")

	uh_l = (X_l-c_l)/`h';	uh_r = (X_r-c_r)/`h';
	uhh_l=select(uh_l,wh_l:> 0);	uhh_r=select(uh_r,wh_r:> 0)
	
	ub_l = (X_l-c_l)/`b';   ub_r = (X_r-c_r)/`b';
	ubb_l = select(ub_l,wb_l:>0); ubb_r=select(ub_r,wb_r:>0)

	Yh_l  = select(Y_l,  wh_l:> 0);	Yh_r  = select(Y_r,  wh_r:> 0)
	Yb_l  = select(Y_l,  wb_l:> 0);	Yb_r  = select(Y_r,  wb_r:> 0)
	Xh_l  = select(X_l,  wh_l:> 0);	Xh_r  = select(X_r,  wh_r:> 0)
	Xb_l  = select(X_l,  wb_l:> 0);	Xb_r  = select(X_r,  wb_r:> 0)
	whh_l = select(wh_l, wh_l:> 0);	whh_r = select(wh_r, wh_r:> 0)
	wbb_l = select(wb_l, wb_l:> 0);	wbb_r = select(wb_r, wb_r:> 0)


	if ("`fuzzy'"~="") {
		T  = st_data(.,("`fuzzy'"), 0)
		T_l  = select(T,X:<`c');		T_r  = select(T,X:>=`c')
		Th_l = select(T_l,wh_l:>0);		Th_r = select(T_r,wh_r:>0)
		Tb_l = select(T_l,wb_l:>0);		Tb_r = select(T_r,wb_r:>0)
	}

	Nh_l = length(Xh_l);	Nb_l = length(Xb_l)
	Nh_r = length(Xh_r);	Nb_r = length(Xb_r)
	X_lp  = J(N_l,p1,.);	X_rp  = J(N_r,p1,.)
	X_lq =  J(N_l,q1,.);	X_rq =  J(N_r,q1,.)
	Xh_lp = J(Nh_l,p1,.);	Xh_rp = J(Nh_r,p1,.)
	Xb_lq = J(Nb_l,q1,.);	Xb_rq = J(Nb_r,q1,.)

	for (j=1; j<=p1; j++)  {
		X_lp[.,j]  = (X_l:-c):^(j-1);		X_rp[.,j]  = (X_r:-c):^(j-1)
		Xh_lp[.,j] = (Xh_l:-c):^(j-1);		Xh_rp[.,j] = (Xh_r:-c):^(j-1)
	}
	
	for (j=1; j<=q1; j++)  {
		X_lq[.,j]  = (X_l:-c):^(j-1);		X_rq[.,j]  = (X_r:-c):^(j-1)
		Xb_lq[.,j] = (Xb_l:-c):^(j-1);		Xb_rq[.,j] = (Xb_r:-c):^(j-1)
	}
	
	st_numscalar("Nh_l", Nh_l[1,1]);	st_numscalar("Nb_l", Nb_l[1,1])
	st_numscalar("Nh_r", Nh_r[1,1]);	st_numscalar("Nb_r", Nb_r[1,1])

	if (Nh_l<5 | Nh_r<5 | Nb_l<5 | Nb_r<5){
	 display("{err}Not enough observations to perform calculations")
	 exit()
	}
  
	display("Computing Variance-Covariance Matrix.")

	sigmah_l = altrdvce(Xh_l, Yh_l, Yh_l, `p', `h', `matches', "`vce'", "`kernel'")
	sigmah_r = altrdvce(Xh_r, Yh_r, Yh_r, `p', `h', `matches', "`vce'", "`kernel'")
	sigmab_l = altrdvce(Xb_l, Yb_l, Yb_l, `p', `h', `matches', "`vce'", "`kernel'")
	sigmab_r = altrdvce(Xb_r, Yb_r, Yb_r, `p', `h', `matches', "`vce'", "`kernel'")
	
	display("Computing AMSE")

	Gamma_lp  = quadcross(X_lp,wh_l,X_lp);	Gamma_rp  = quadcross(X_rp,wh_r,X_rp)
	Gamma_lq  = quadcross(X_lq,wb_l,X_lq);	Gamma_rq  = quadcross(X_rq,wb_r,X_rq)
	Gammah_lp = quadcross(Xh_lp,whh_l,Xh_lp);	Gammah_rp = quadcross(Xh_rp,whh_r,Xh_rp)
	Gammab_lq = quadcross(Xb_lq,wbb_l,Xb_lq);	Gammab_rq = quadcross(Xb_rq,wbb_r,Xb_rq)
	
*	if (rank(Gamma_lp)<cols(Gamma_lp) | rank(Gamma_rp)<cols(Gamma_rp) | rank(Gamma_lq)<cols(Gamma_lq) | rank(Gamma_rq)<cols(Gamma_rq)) {
*		errprintf("regressors are multicollinear\n")
*		exit(499)
*		}
	
*	if (rank(Gammah_lp)<cols(Gammah_lp) | rank(Gammah_rp)<cols(Gammah_rp) | rank(Gammab_lq)<cols(Gammab_lq) | rank(Gammab_rq)<cols(Gammab_rq)) {
*		errprintf("regressors are multicollinear\n")
*		exit(499)
*		}	
	
	invGamma_lp  = cholinv(Gamma_lp);	invGamma_rp  = cholinv(Gamma_rp)
	invGamma_lq  = cholinv(Gamma_lq);	invGamma_rq  = cholinv(Gamma_rq)
	invGammah_lp = cholinv(Gammah_lp);	invGammah_rp = cholinv(Gammah_rp)
	invGammab_lq = cholinv(Gammab_lq);	invGammab_rq = cholinv(Gammab_rq)
	
	Psih_lp = quadcross(Xh_lp, whh_l:*sigmah_l:*whh_l, Xh_lp);	Psih_rp = quadcross(Xh_rp, whh_r:*sigmah_r:*whh_r, Xh_rp)
	Psib_lq = quadcross(Xb_lq, wbb_l:*sigmab_l:*wbb_l, Xb_lq);	Psib_rq = quadcross(Xb_rq, wbb_r:*sigmab_r:*wbb_r, Xb_rq)

	factor_p = J(p1, 1, .);	factor_q = J(q1, 1, .)
	
	for (j=1; j<=p1; j++) {
		factor_p[j] = factorial(j-1)
	}
	
	for (j=1; j<=q1; j++) {
		factor_q[j] = factorial(j-1)
	}
	
	Hp_vec = J(p1, 1, .)
	for (j=1; j<=p1; j++) {
		Hp_vec[j] = `h'^(-(j-1))
	}
	Hp = diag(Hp_vec)
	
	Hq_vec = J(q1, 1, .)
	for (j=1; j<=q1; j++) {
		Hq_vec[j] = `b'^(-(j-1))
	}
	Hq = diag(Hq_vec)
	
	tau_lp = factor_p:*(invGammah_lp*quadcross(Xh_lp, whh_l, Yh_l));	tau_rp = factor_p:*(invGammah_rp*quadcross(Xh_rp, whh_r, Yh_r))
	tau_lq = factor_q:*(invGammab_lq*quadcross(Xb_lq, wbb_l, Yb_l));	tau_rq = factor_q:*(invGammab_rq*quadcross(Xb_rq, wbb_r, Yb_r))

	V_lp = invGammah_lp*Psih_lp*invGammah_lp;	V_rp = invGammah_rp*Psih_rp*invGammah_rp
	V_lq = invGammab_lq*Psib_lq*invGammab_lq;	V_rq = invGammab_rq*Psib_rq*invGammab_rq
	
	if (`b'>=`h'){
    whb_l = select(wh_l,wb_l:>0);    whb_r = select(wh_r,wb_r:>0)
    Xb_lp = select(X_lp,wb_l:>0);    Xb_rp = select(X_rp,wb_r:>0)
    Psi_lpq = quadcross(Xb_lp,whb_l:*sigmab_l:*wbb_l,Xb_lq);    Psi_rpq = quadcross(Xb_rp,whb_r:*sigmab_r:*wbb_r,Xb_rq)
  }
  else {
    wbh_l = select(wb_l,wh_l:>0);    wbh_r = select(wb_r,wh_r:>0)
    Xh_lq = select(X_lq,wh_l:>0);    Xh_rq = select(X_rq,wh_r:>0)
    Psi_lpq = quadcross(Xh_lp,whh_l:*sigmah_l:*wbh_l,Xh_lq);    Psi_rpq = quadcross(Xh_rp,whh_r:*sigmah_r:*wbh_r,Xh_rq)
  }
	
	Cov_l = invGamma_lp*Psi_lpq*invGamma_lq;	Cov_r = invGamma_rp*Psi_rpq*invGamma_rq

	v_lp = (Xh_lp:*whh_l)'*(uhh_l:^(`p'+1));	v_rp = (Xh_rp:*whh_r)'*(uhh_r:^(`p'+1)) 
	v_lq = (Xb_lq:*wbb_l)'*(ubb_l:^(`q'+1));	v_rq = (Xb_rq:*wbb_r)'*(ubb_r:^(`q'+1))
	v_lp1 = (Xh_lp:*whh_l)'*(uhh_l:^(`p'+2));	v_rp1 = (Xh_rp:*whh_r)'*(uhh_r:^(`p'+2)) 
	

	
	BiasConst_lp = factorial(`deriv')*cholinv(Hp)*invGamma_lp*v_lp 
	BiasConst_rp = factorial(`deriv')*cholinv(Hp)*invGamma_rp*v_rp	
	
	BiasConst_lp1= factorial(`deriv')*cholinv(Hp)*invGamma_lp*v_lp1
	BiasConst_rp1= factorial(`deriv')*cholinv(Hp)*invGamma_rp*v_rp1
	
	BiasConst_lq = cholinv(Hq)*invGamma_lq*v_lq
	BiasConst_rq = cholinv(Hq)*invGamma_rq*v_rq

	Bias_tau_CCT_part1 = (`h')^(`p'+2-`deriv')*(m3_r*BiasConst_rp1[`deriv'+1,1]-m3_l*BiasConst_lp1[`deriv'+1,1])
	Bias_tau_CCT_part2 = (`h')^(`p'+1-`deriv')*(`b'^(`q'-`p'))*(m3_r*BiasConst_rq[`p'+2,1]*BiasConst_rp[`deriv'+1,1] - m3_l*BiasConst_lq[`p'+2,1]*BiasConst_lp[`deriv'+1,1])
	
	Bias_tau_CCT = Bias_tau_CCT_part1-Bias_tau_CCT_part2

	Bias_tau = (tau_rq[`p'+2,1]*BiasConst_rp[`deriv'+1,1] - tau_lq[`p'+2,1]*BiasConst_lp[`deriv'+1,1])*(`h'^(`p'+1-`deriv')/factorial(`p'+1))

	tau_cl = `scalepar'*(tau_rp[`deriv'+1,1] - tau_lp[`deriv'+1,1])
	tau_bc = `scalepar'*(tau_rp[`deriv'+1,1] - tau_lp[`deriv'+1,1] - Bias_tau)

	V_l_cl = factorial(`deriv')^2*V_lp[`deriv'+1,`deriv'+1] 
	V_r_cl = factorial(`deriv')^2*V_rp[`deriv'+1,`deriv'+1]  
	V_l_rb = factorial(`deriv')^2*V_lp[`deriv'+1,`deriv'+1] + factorial(`p'+1)^2*V_lq[`p'+2,`p'+2]*(BiasConst_lp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))^2 - 2*factorial(`deriv')*factorial(`p'+1)*Cov_l[`deriv'+1,`p'+2]*(BiasConst_lp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))
	V_r_rb = factorial(`deriv')^2*V_rp[`deriv'+1,`deriv'+1] + factorial(`p'+1)^2*V_rq[`p'+2,`p'+2]*(BiasConst_rp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))^2 - 2*factorial(`deriv')*factorial(`p'+1)*Cov_r[`deriv'+1,`p'+2]*(BiasConst_rp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))
	V_cl   = `scalepar'^2*(V_l_cl + V_r_cl)
	V_rb   = `scalepar'^2*(V_l_rb + V_r_rb)
	
	st_numscalar("CCT_AMSE", (V_rb + Bias_tau_CCT^2))
	
		display("Estimation Completed.") 
		}
		
	restore
		
	ereturn clear
	ereturn scalar CCT_AMSE = CCT_AMSE
	ereturn scalar h_bw = `h'
	ereturn scalar b_bw = `b'		
	/*
	if "`fuzzy'"~="" {
		ereturn scalar tau_F_cl = tau_F_cl
		ereturn scalar tau_F_bc = tau_F_bc
		** added by Pei **
		ereturn local ci_F_r_rb = ci_F_r_rb
		ereturn local ci_F_l_rb = ci_F_l_rb
		ereturn scalar se_F_cl  = se_F_cl
		ereturn scalar se_F_rb  = se_F_rb
		ereturn scalar pv_F_cl  = pv_F_cl
		ereturn scalar pv_F_bc  = pv_F_bc
		ereturn scalar pv_F_rb  = pv_F_rb
		ereturn scalar tau_T_bc = tau_T_bc
		ereturn scalar ci_T_r_rb = ci_T_r_rb
		ereturn scalar ci_T_l_rb = ci_T_l_rb
		ereturn scalar t_T_rb = t_T_rb
		ereturn scalar pv_T_rb = pv_T_rb
	}
	*/
	mata mata clear
	
end
	





