* Updated Oct 27 2020
* Changed to a more stable way matrix inverse is taken

capture program drop altrdrobust 
program define altrdrobust, eclass
	syntax anything [if] [in] [, c(real 0) deriv(real 0) fuzzy(string) p(real 1) q(real 0) h(real 0) b(real 0) kernel(string) bwselect(string) delta(real 0.5) rho(real 0) vce(string) matches(real 3) level(real 95) cvgrid_min(real 0) cvgrid_max(real 0) cvgrid_length(real 0) cvplot all scalepar(real 1) scaleregul(real 1) nowarning]
	
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
	
	disp in yellow "Preparing data." 
	
	if ("`h'"=="0" & "`bwselect'"=="IK") {
		disp in ye "Computing Bandwidth Selectors."
		qui altrdbwselect `y' `x', c(`c') deriv(`deriv') p(`p') q(`q') bwselect(`bwselect') rho(`rho') kernel(`kernel') vce(`vce') precalc scaleregul(`scaleregul')
		local h = e(h_IK)
		local b = e(b_IK)
		local bwselect = "IK"
	}
	else if "`h'"=="0" & "`bwselect'"=="CV" {
		disp in ye "Computing Bandwidth Selectors."
		qui altrdbwselect `y' `x', c(`c') deriv(`deriv') p(`p') q(`q')  bwselect(`bwselect') rho(`rho') kernel(`kernel') delta(`delta') cvgrid_min(`cvgrid_min') cvgrid_max(`cvgrid_max') cvgrid_length(`cvgrid_length') `cvplot' vce(`vce') precalc scaleregul(`scaleregul')
		local h = e(h_CV)
		local b = e(h_CV)
		local bwselect = "CV"
	}
	else if "`h'"=="0" {
		disp in ye "Computing Bandwidth Selectors."
		local bwselect = "CCT"
		qui altrdbwselect `y' `x', c(`c') deriv(`deriv') p(`p') q(`q') matches(`matches') bwselect(`bwselect') rho(`rho') kernel(`kernel') vce(`vce') precalc scaleregul(`scaleregul')
		local h = e(h_CCT)
		local b = e(b_CCT)
	}
	
	if (`use_rho'==1 & `rho'>0) {
		local b = `h'/`rho'
	}
	
	if (`h'>0 & `rho'>0) {
		local b = `h'/`rho'
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

	** added by Pei **
	sort `x', stable
	
	mata{
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
	
	wh_l = kweight(X_l,`c',`h',"`kernel'");	wh_r = kweight(X_r,`c',`h',"`kernel'")
	wb_l = kweight(X_l,`c',`b',"`kernel'");	wb_r = kweight(X_r,`c',`b',"`kernel'")

	uh_l = (X_l-c_l)/`h';	uh_r = (X_r-c_r)/`h';
	uhh_l=select(uh_l,wh_l:> 0);	uhh_r=select(uh_r,wh_r:> 0)

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
	
	display("Computing RD Estimates.")
	
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
	
	BiasConst_lp = factorial(`deriv')*cholinv(Hp)*invGamma_lp*v_lp 
	BiasConst_rp = factorial(`deriv')*cholinv(Hp)*invGamma_rp*v_rp
	Bias_tau = (tau_rq[`p'+2,1]*BiasConst_rp[`deriv'+1,1] - tau_lq[`p'+2,1]*BiasConst_lp[`deriv'+1,1])*(`h'^(`p'+1-`deriv')/factorial(`p'+1))

	tau_cl = `scalepar'*(tau_rp[`deriv'+1,1] - tau_lp[`deriv'+1,1])
	tau_bc = `scalepar'*(tau_rp[`deriv'+1,1] - tau_lp[`deriv'+1,1] - Bias_tau)

	V_l_cl = factorial(`deriv')^2*V_lp[`deriv'+1,`deriv'+1] 
	V_r_cl = factorial(`deriv')^2*V_rp[`deriv'+1,`deriv'+1]  
	V_l_rb = factorial(`deriv')^2*V_lp[`deriv'+1,`deriv'+1] + factorial(`p'+1)^2*V_lq[`p'+2,`p'+2]*(BiasConst_lp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))^2 - 2*factorial(`deriv')*factorial(`p'+1)*Cov_l[`deriv'+1,`p'+2]*(BiasConst_lp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))
	V_r_rb = factorial(`deriv')^2*V_rp[`deriv'+1,`deriv'+1] + factorial(`p'+1)^2*V_rq[`p'+2,`p'+2]*(BiasConst_rp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))^2 - 2*factorial(`deriv')*factorial(`p'+1)*Cov_r[`deriv'+1,`p'+2]*(BiasConst_rp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))
	V_cl   = `scalepar'^2*(V_l_cl + V_r_cl)
	V_rb   = `scalepar'^2*(V_l_rb + V_r_rb)
	
	V_bias = factorial(`p'+1)^2*V_rq[`p'+2,`p'+2]*(BiasConst_rp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))^2 + factorial(`p'+1)^2*V_lq[`p'+2,`p'+2]*(BiasConst_lp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))^2
	
	
	quant = -invnormal(abs((1-(`level'/100))/2))
	se_cl = sqrt(V_cl)
	se_rb = sqrt(V_rb)
	t_cl =  tau_cl/se_cl;t_bc =  tau_bc/se_cl;t_rb =  tau_bc/se_rb
	st_numscalar("tau_cl", tau_cl)
	st_numscalar("tau_bc", tau_bc)
	st_numscalar("Bias_tau", Bias_tau)
	st_numscalar("se_cl", sqrt(V_cl))
	st_numscalar("se_rb", sqrt(V_rb))
	st_numscalar("se_bias", sqrt(V_bias))
	st_numscalar("t_cl", tau_cl/se_cl)
	st_numscalar("t_bc", tau_bc/se_cl)
	st_numscalar("t_rb", tau_bc/se_rb)
	st_numscalar("quant", -invnormal(abs((1-(`level'/100))/2)))
	st_numscalar("ci_l_cl", tau_cl[1,1] - quant*se_cl)
	st_numscalar("ci_r_cl", tau_cl[1,1] + quant*se_cl)
	st_numscalar("ci_l_bc", tau_bc[1,1] - quant*se_cl)
	st_numscalar("ci_r_bc", tau_bc[1,1] + quant*se_cl)
	st_numscalar("ci_l_rb", tau_bc[1,1] - quant*se_rb)
	st_numscalar("ci_r_rb", tau_bc[1,1] + quant*se_rb)
*	st_numscalar("t_cl",  tau_cl[1,1]/se_cl)
*	st_numscalar("t_bc",  tau_bc[1,1]/se_cl)
*	st_numscalar("t_rb",  tau_bc[1,1]/se_rb)
	st_numscalar("pv_cl", 2*normal(-abs(t_cl)))
	st_numscalar("pv_bc", 2*normal(-abs(t_bc)))
	st_numscalar("pv_rb", 2*normal(-abs(t_rb)))
	st_matrix("b", (tau_cl))
	st_matrix("V", (V_cl))
	
	if ("`all'"~="") {
		st_matrix("b", (tau_cl,tau_bc,tau_bc))
		st_matrix("V", (V_cl,0,0 \ 0,V_cl,0 \0,0,V_rb))
	}
	
	if ("`fuzzy'"~="") {
		*********** First Stage ***************
		tau_T_lp = factor_p:*(invGammah_lp*quadcross(Xh_lp, whh_l, Th_l))
		tau_T_rp = factor_p:*(invGammah_rp*quadcross(Xh_rp, whh_r, Th_r))
		tau_T_lq = factor_q:*(invGammab_lq*quadcross(Xb_lq, wbb_l, Tb_l))
		tau_T_rq = factor_q:*(invGammab_rq*quadcross(Xb_rq, wbb_r, Tb_r))
	
		sigmah_T_l = altrdvce(Xh_l, Th_l, Th_l, `p', `h',`matches',"`vce'","`kernel'")
		sigmah_T_r = altrdvce(Xh_r, Th_r, Th_r, `p', `h',`matches',"`vce'","`kernel'")
		sigmab_T_l = altrdvce(Xb_l, Tb_l, Tb_l, `p', `h',`matches',"`vce'","`kernel'")
		sigmab_T_r = altrdvce(Xb_r, Tb_r, Tb_r, `p', `h',`matches',"`vce'","`kernel'")
	
		Psih_T_lp = quadcross(Xh_lp, whh_l:*sigmah_T_l:*whh_l, Xh_lp)
		Psih_T_rp = quadcross(Xh_rp, whh_r:*sigmah_T_r:*whh_r, Xh_rp)
		Psib_T_lq = quadcross(Xb_lq, wbb_l:*sigmab_T_l:*wbb_l, Xb_lq)
		Psib_T_rq = quadcross(Xb_rq, wbb_r:*sigmab_T_r:*wbb_r, Xb_rq)
		
		if (`b'>=`h'){
			Psi_T_lpq = quadcross(Xb_lp,whb_l:*sigmab_T_l:*wbb_l,Xb_lq);    
			Psi_T_rpq = quadcross(Xb_rp,whb_r:*sigmab_T_r:*wbb_r,Xb_rq)
		}
		else {
			Psi_T_lpq = quadcross(Xh_lp,whh_l:*sigmah_T_l:*wbh_l,Xh_lq);    
			Psi_T_rpq = quadcross(Xh_rp,whh_r:*sigmah_T_r:*wbh_r,Xh_rq)
		} 
	
		V_T_lp = invGammah_lp*Psih_T_lp*invGammah_lp
		V_T_rp = invGammah_rp*Psih_T_rp*invGammah_rp
		V_T_lq = invGammab_lq*Psib_T_lq*invGammab_lq
		V_T_rq = invGammab_rq*Psib_T_rq*invGammab_rq

		Cov_T_l = invGamma_lp*Psi_T_lpq*invGamma_lq
		Cov_T_r = invGamma_rp*Psi_T_rpq*invGamma_rq

		Bias_T_tau = (tau_T_rq[`p'+2,1]*BiasConst_rp[`deriv'+1,1] - tau_T_lq[`p'+2,1]*BiasConst_lp[`deriv'+1,1])*(`h'^(`p'+1-`deriv')/factorial(`p'+1))
		tau_T_cl = tau_T_rp[`deriv'+1,1] - tau_T_lp[`deriv'+1,1]
		tau_T_bc = tau_T_rp[`deriv'+1,1] - tau_T_lp[`deriv'+1,1] - Bias_T_tau

		V_T_l_cl = factorial(`deriv')^2*V_T_lp[`deriv'+1,`deriv'+1]
		V_T_r_cl = factorial(`deriv')^2*V_T_rp[`deriv'+1,`deriv'+1]  
		V_T_cl   = V_T_l_cl + V_T_r_cl 
		V_T_l_rb = V_T_l_cl + factorial(`p'+1)^2*V_T_lq[`p'+2,`p'+2]*(BiasConst_lp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))^2 - 2*factorial(`deriv')*factorial(`p'+1)*Cov_T_l[`deriv'+1,`p'+2]*(BiasConst_lp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))
		V_T_r_rb = V_T_r_cl + factorial(`p'+1)^2*V_T_rq[`p'+2,`p'+2]*(BiasConst_rp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))^2 - 2*factorial(`deriv')*factorial(`p'+1)*Cov_T_r[`deriv'+1,`p'+2]*(BiasConst_rp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))
		V_T_rb   = V_T_l_rb + V_T_r_rb
		
		V_T_bias = factorial(`p'+1)^2*V_T_lq[`p'+2,`p'+2]*(BiasConst_lp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))^2 + factorial(`p'+1)^2*V_T_rq[`p'+2,`p'+2]*(BiasConst_rp[`deriv'+1]*`h'^(`p'+1-`deriv')/factorial(`p'+1))^2

		st_numscalar("tau_T_cl", tau_T_cl)
		se_T_cl = sqrt(V_T_cl);se_T_rb = sqrt(V_T_rb)
		t_T_cl =  tau_T_cl/se_T_cl;t_T_bc =  tau_T_bc/se_T_cl;t_T_rb =  tau_T_bc/se_T_rb
		st_numscalar("tau_T_bc", tau_T_bc)
		st_numscalar("Bias_T_tau", Bias_T_tau)
		st_numscalar("se_T_cl", se_T_cl)
		st_numscalar("se_T_cl", se_T_cl);st_numscalar("se_T_rb", se_T_rb)
		st_numscalar("se_T_bias", sqrt(V_T_bias))
		st_numscalar("t_T_cl", tau_T_cl/se_T_cl);
		st_numscalar("t_T_bc", tau_T_bc/se_T_cl);st_numscalar("t_T_rb", tau_T_bc/se_T_rb);
		st_numscalar("ci_T_l_cl", tau_T_cl - quant*se_T_cl)
		st_numscalar("ci_T_r_cl", tau_T_cl + quant*se_T_cl);st_numscalar("ci_T_l_bc", tau_T_bc - quant*se_T_cl)
		st_numscalar("ci_T_r_bc", tau_T_bc + quant*se_T_cl);st_numscalar("ci_T_l_rb", tau_T_bc - quant*se_T_rb)
		st_numscalar("ci_T_r_rb", tau_T_bc + quant*se_T_rb);
		st_numscalar("t_T_cl",  tau_T_cl/se_T_cl);st_numscalar("t_T_bc",  tau_T_bc/se_T_cl);st_numscalar("t_T_rb",  tau_T_bc/se_T_rb)
		st_numscalar("pv_T_cl", 2*normal(-abs(t_T_cl)));st_numscalar("pv_T_bc", 2*normal(-abs(t_T_bc)));st_numscalar("pv_T_rb", 2*normal(-abs(t_T_rb)))

		****************** Second Stage
		Bias_F_tau = (1/tau_T_cl)*Bias_tau - (tau_cl/tau_T_cl^2)*Bias_T_tau 
		tau_F_cl = tau_cl/tau_T_cl
		tau_F_bc = tau_F_cl - Bias_F_tau

		sigmah_TY_l = altrdvce(Xh_l, Yh_l, Th_l, `p', `h',`matches',"`vce'","`kernel'")
		sigmah_TY_r = altrdvce(Xh_r, Yh_r, Th_r, `p', `h',`matches',"`vce'","`kernel'")
		sigmab_TY_l = altrdvce(Xb_l, Yb_l, Tb_l, `p', `h',`matches',"`vce'","`kernel'")
		sigmab_TY_r = altrdvce(Xb_r, Yb_r, Tb_r, `p', `h',`matches',"`vce'","`kernel'")
		
		Psih_TY_lp = quadcross(Xh_lp, whh_l:*sigmah_TY_l:*whh_l, Xh_lp)
		Psih_TY_rp = quadcross(Xh_rp, whh_r:*sigmah_TY_r:*whh_r, Xh_rp)
		Psib_TY_lq = quadcross(Xb_lq, wbb_l:*sigmab_TY_l:*wbb_l, Xb_lq)
		Psib_TY_rq = quadcross(Xb_rq, wbb_r:*sigmab_TY_r:*wbb_r, Xb_rq)
		
		if (`b'>=`h'){
			Psi_TY_lpq = quadcross(Xb_lp,whb_l:*sigmab_TY_l:*wbb_l,Xb_lq);    
			Psi_TY_rpq = quadcross(Xb_rp,whb_r:*sigmab_TY_r:*wbb_r,Xb_rq)
		}
		else {
			Psi_TY_lpq = quadcross(Xh_lp,whh_l:*sigmah_TY_l:*wbh_l,Xh_lq);    
			Psi_TY_rpq = quadcross(Xh_rp,whh_r:*sigmah_TY_r:*wbh_r,Xh_rq)
		} 
		
		Cov_TY_l = invGamma_lp*Psi_TY_lpq*invGamma_lq
		Cov_TY_r = invGamma_rp*Psi_TY_rpq*invGamma_rq
				
		V_TY_lp = invGammah_lp*Psih_TY_lp*invGammah_lp
		V_TY_rp = invGammah_rp*Psih_TY_rp*invGammah_rp
		V_TY_lq = invGammab_lq*Psib_TY_lq*invGammab_lq
		V_TY_rq = invGammab_rq*Psib_TY_rq*invGammab_rq
		V_TY_cl   = factorial(`deriv')^2*(V_TY_lp[`deriv'+1,`deriv'+1] + V_TY_rp[`deriv'+1,`deriv'+1])
		V_F_cl = (1/tau_T_cl^2)*V_cl + (tau_cl^2/tau_T_cl^4)*V_T_cl -(2*tau_cl/tau_T_cl^3)*V_TY_cl

		C_F_l_pq = factorial(`deriv')*factorial(`p'+1)*((1/tau_T_cl^2)*Cov_l - (2*tau_cl/tau_T_cl^3)*Cov_TY_l + (tau_cl^2/tau_T_cl^4)*Cov_T_l)
		C_F_r_pq = factorial(`deriv')*factorial(`p'+1)*((1/tau_T_cl^2)*Cov_r - (2*tau_cl/tau_T_cl^3)*Cov_TY_r + (tau_cl^2/tau_T_cl^4)*Cov_T_r)
		V_F_rb_t2 = -2*(C_F_l_pq[`deriv'+1,`p'+2]*BiasConst_lp[`deriv'+1] + C_F_r_pq[`deriv'+1,`p'+2]*BiasConst_rp[`deriv'+1])*`h'^(`p'+1-`deriv')/factorial(`p'+1)
		V_cl_lq    = factorial(`p'+1)^2*V_lq[`p'+2,`p'+2]
		V_T_cl_lq  = factorial(`p'+1)^2*V_T_lq[`p'+2,`p'+2]
		V_TY_cl_lq = factorial(`p'+1)^2*V_TY_lq[`p'+2,`p'+2]
		V_cl_rq    = factorial(`p'+1)^2*V_rq[`p'+2,`p'+2]
		V_T_cl_rq  = factorial(`p'+1)^2*V_T_rq[`p'+2,`p'+2]
		V_TY_cl_rq = factorial(`p'+1)^2*V_TY_rq[`p'+2,`p'+2]
		V_F_cl_lq = (1/tau_T_cl^2)*V_cl_lq + (tau_cl^2/tau_T_cl^4)*V_T_cl_lq -(2*tau_cl/tau_T_cl^3)*V_TY_cl_lq
		V_F_cl_rq = (1/tau_T_cl^2)*V_cl_rq + (tau_cl^2/tau_T_cl^4)*V_T_cl_rq -(2*tau_cl/tau_T_cl^3)*V_TY_cl_rq
		V_F_rb_t3 = (V_F_cl_lq*BiasConst_lp[`deriv'+1]^2 + V_F_cl_rq*BiasConst_rp[`deriv'+1]^2)*(`h'^(`p'+1-`deriv')/factorial(`p'+1))^2
		V_F_rb = V_F_cl + V_F_rb_t2 + V_F_rb_t3

		se_F_cl = sqrt(V_F_cl)
		se_F_rb = sqrt(V_F_rb)
		t_F_cl =  tau_F_cl/se_F_cl
		t_F_bc =  tau_F_bc/se_F_cl
		t_F_rb =  tau_F_bc/se_F_rb
		st_numscalar("tau_F_cl", tau_F_cl[1,1])
		st_numscalar("tau_F_bc", tau_F_bc[1,1])
		st_numscalar("Bias_F_tau", Bias_F_tau)
		st_numscalar("se_F_cl", se_F_cl)
		st_numscalar("se_F_rb", se_F_rb)
		st_numscalar("se_F_bias", sqrt(V_F_rb_t3))
		st_numscalar("t_F_cl", tau_F_cl/se_F_cl)
		st_numscalar("t_F_bc", tau_F_bc/se_F_cl)
		st_numscalar("t_F_rb", tau_F_bc/se_F_rb)
		st_numscalar("ci_F_l_cl", tau_F_cl[1,1] - quant*se_F_cl)
		st_numscalar("ci_F_r_cl", tau_F_cl[1,1] + quant*se_F_cl)
		st_numscalar("ci_F_l_bc", tau_F_bc[1,1] - quant*se_F_cl)
		st_numscalar("ci_F_r_bc", tau_F_bc[1,1] + quant*se_F_cl)
		st_numscalar("ci_F_l_rb", tau_F_bc[1,1] - quant*se_F_rb)
		st_numscalar("ci_F_r_rb", tau_F_bc[1,1] + quant*se_F_rb)
		st_numscalar("pv_F_cl", 2*normal(-abs(t_F_cl)))
		st_numscalar("pv_F_bc", 2*normal(-abs(t_F_bc)))
		st_numscalar("pv_F_rb", 2*normal(-abs(t_F_rb)))
		
		st_matrix("b", (tau_F_cl))
		st_matrix("V", (V_F_cl))
	
		if ("`all'"~="") {
			st_matrix("b", (tau_F_cl,tau_F_bc,tau_F_bc))
			st_matrix("V", (V_F_cl,0,0 \ 0,V_F_cl,0 \0,0,V_F_rb))
		}
		}
		display("Estimation Completed.") 
		}

	************************************************
	********* OUTPUT TABLE *************************
	************************************************
	local alpha = `h'/`b'

	disp ""
	if ("`deriv'"=="0") {
		disp "Sharp RD Estimates using Local Polynomial Regression." 
	}
	if ("`deriv'">"0") {
		disp "Sharp RD Estimates using Local Polynomial Regression. Derivative of order " `deriv' "."
	}
	disp ""
	disp in smcl in gr "{ralign 21: Cutoff c = `c'}"      _col(22) " {c |} " _col(23) in gr "Left of " in yellow "c"  _col(36) in gr "Right of " in yellow "c" _col(61) in gr "Number of obs = "  in yellow %10.0f `N_l'+`N_r'
	disp in smcl in gr "{hline 22}{c +}{hline 22}"                                                                                                             _col(61) in gr "NN Matches    = "  in yellow %10.0f `matches'
	disp in smcl in gr "{ralign 21:Number of obs}"        _col(22) " {c |} " _col(23) as result %9.0f Nh_l    _col(37) %9.0f  Nh_r                             _col(61) in gr "BW Type       = "  in yellow "{ralign 10:`bwselect'}" 
	disp in smcl in gr "{ralign 21:Order Loc. Poly. (p)}" _col(22) " {c |} " _col(23) as result %9.0f `p'     _col(37) %9.0f  `p'                              _col(61) in gr "Kernel Type   = "  in yellow "{ralign 10:`kernel_type'}" 
	disp in smcl in gr "{ralign 21:Order Bias (q)}"       _col(22) " {c |} " _col(23) as result %9.0f `q'     _col(37) %9.0f  `q'                               
	disp in smcl in gr "{ralign 21:BW Loc. Poly. (h)}"    _col(22) " {c |} " _col(23) as result %9.3f `h'     _col(37) %9.3f  `h' 
	disp in smcl in gr "{ralign 21:BW Bias (b)}"          _col(22) " {c |} " _col(23) as result %9.3f `b'     _col(37) %9.3f  `b'
	disp in smcl in gr "{ralign 21:rho (h/b)}"            _col(22) " {c |} " _col(23) as result %9.3f `alpha' _col(37) %9.3f  `alpha'

	if "`fuzzy'"=="" {
		disp ""
		disp "Outcome: `y'. Running Variable: `x'."
		disp in smcl in gr "{hline 22}{c TT}{hline 63}"
		disp in smcl in gr "{ralign 21:Method}"            _col(22) " {c |} " _col(28) "Coef."      _col(36) `"Std. Err."'  _col(50) "z" _col(57) "P>|z|" _col(67) `"[`level'% Conf. Interval]"' _n  "{hline 22}{c +}{hline 63}"
		disp in smcl in gr "{ralign 21:Conventional}"      _col(22) " {c |} " _col(26) in ye %7.0g tau_cl  _col(36) %7.0g se_cl   _col(47) %5.4f t_cl _col(57) %5.3f  pv_cl  _col(68) %8.0g ci_l_cl _col(79) %8.0g ci_r_cl 
		disp in smcl in gr "{ralign 21:Robust}"            _col(22) " {c |} " _col(26) in ye %7.0g "    -" _col(36) %7.0g "    -" _col(47) %5.4f t_rb _col(57) %5.3f  pv_rb  _col(68) %8.0g ci_l_rb _col(79) %8.0g ci_r_rb  
	}

	if "`fuzzy'"~="" {
		disp ""
		disp "Structural Estimates. Outcome: `y'. Running Variable: `x'. Instrument: `fuzzy'."
		disp in smcl in gr "{hline 22}{c TT}{hline 63}"
		disp in smcl in gr "{ralign 21:Method}"            _col(22) " {c |} " _col(28) "Coef."      _col(36) `"Std. Err."'  _col(50) "z" _col(57) "P>|z|" _col(67) `"[`level'% Conf. Interval]"' _n  "{hline 22}{c +}{hline 63}"
		disp in smcl in gr "{ralign 21:Conventional}"      _col(22) " {c |} " _col(26) in ye %7.0g tau_F_cl _col(36) %7.0g se_F_cl  _col(47) %5.4f t_F_cl _col(57) %5.3f  pv_F_cl  _col(68) %8.0g  ci_F_l_cl _col(79) %8.0g ci_F_r_cl 
		disp in smcl in gr "{ralign 21:Robust}"            _col(22) " {c |} " _col(26) in ye %7.0g  "    -" _col(36) %7.0g "    -"  _col(47) %5.4f t_F_rb _col(57) %5.3f  pv_F_rb  _col(68) %8.0g  ci_F_l_rb _col(79) %8.0g ci_F_r_rb   
		disp in smcl in gr "{hline 22}{c BT}{hline 63}"

		disp ""
		disp in yellow "First-Stage Estimates. Outcome: `fuzzy'. Running Variable: `x'."
		disp in smcl in gr "{hline 22}{c TT}{hline 63}"
		disp in smcl in gr "{ralign 21:Method}"            _col(22) " {c |} " _col(28) "Coef."              _col(36) `"Std. Err."' _col(50) "z"          _col(57) "P>|z|"         _col(67) `"[`level'% Conf. Interval]"' _n  "{hline 22}{c +}{hline 63}"
		disp in smcl in gr "{ralign 21:Conventional}"      _col(22) " {c |} " _col(26) in ye %7.0g tau_T_cl _col(36) %7.0g se_T_cl _col(47) %5.4f t_T_cl _col(57) %5.3f  pv_T_cl  _col(68) %8.0g  ci_T_l_cl _col(79) %8.0g ci_T_r_cl 
		disp in smcl in gr "{ralign 21:Robust}"            _col(22) " {c |} " _col(26) in ye %7.0g "    -"  _col(36) %7.0g "    -" _col(47) %5.4f t_T_rb _col(57) %5.3f  pv_T_rb  _col(68) %8.0g  ci_T_l_rb _col(79) %8.0g ci_T_r_rb  
	}

	disp in smcl in gr "{hline 22}{c BT}{hline 63}"
	disp ""
	if ("`all'"~="" & "`fuzzy'"=="") {
		disp in yellow "All Estimates."
		disp in smcl in gr "{hline 22}{c TT}{hline 63}"
		disp in smcl in gr "{ralign 21:Method}"         _col(22) " {c |} " _col(28) "Coef." _col(36) `"Std. Err."' _col(50) "z" _col(57) "P>|z|" _col(67) `"[`level'% Conf. Interval]"' _n  "{hline 22}{c +}{hline 63}"
		disp in smcl in gr "{ralign 21:Conventional}"   _col(22) " {c |} " _col(26) in ye %7.0g tau_cl _col(36) %7.0g se_cl _col(47) %5.4f t_cl _col(57) %5.3f  pv_cl _col(68) %8.0g  ci_l_cl _col(79) %8.0g ci_r_cl  
		disp in smcl in gr "{ralign 21:Bias-Corrected}" _col(22) " {c |} " _col(26) in ye %7.0g tau_bc _col(36) %7.0g se_cl _col(47) %5.4f t_bc _col(57) %5.3f  pv_bc _col(68) %8.0g  ci_l_bc _col(79) %8.0g ci_r_bc  
		disp in smcl in gr "{ralign 21:Robust}"         _col(22) " {c |} " _col(26) in ye %7.0g tau_bc _col(36) %7.0g se_rb _col(47) %5.4f t_rb _col(57) %5.3f  pv_rb _col(68) %8.0g  ci_l_rb _col(79) %8.0g ci_r_rb  
		disp in smcl in gr "{hline 22}{c BT}{hline 63}"
	}

	if ("`all'"~="" & "`fuzzy'"~="") {
		disp in yellow "All Structural Estimates."
		disp in smcl in gr "{hline 22}{c TT}{hline 63}"
		disp in smcl in gr "{ralign 21:Method}"         _col(22) " {c |} " _col(28) "Coef." _col(36) `"Std. Err."' _col(50) "z" _col(57) "P>|z|" _col(67) `"[`level'% Conf. Interval]"' _n  "{hline 22}{c +}{hline 63}"
		disp in smcl in gr "{ralign 21:Conventional}"   _col(22) " {c |} " _col(26) in ye %7.0g tau_F_cl _col(36) %7.0g se_F_cl _col(47) %5.4f t_F_cl _col(57) %5.3f  pv_F_cl _col(68) %8.0g  ci_F_l_cl _col(79) %8.0g ci_F_r_cl  
		disp in smcl in gr "{ralign 21:Bias-Corrected}" _col(22) " {c |} " _col(26) in ye %7.0g tau_F_bc _col(36) %7.0g se_F_cl _col(47) %5.4f t_F_bc _col(57) %5.3f  pv_F_bc _col(68) %8.0g  ci_F_l_bc _col(79) %8.0g ci_F_r_bc  
		disp in smcl in gr "{ralign 21:Robust}"         _col(22) " {c |} " _col(26) in ye %7.0g tau_F_bc _col(36) %7.0g se_F_rb _col(47) %5.4f t_F_rb _col(57) %5.3f  pv_F_rb _col(68) %8.0g  ci_F_l_rb _col(79) %8.0g ci_F_r_rb  
		disp in smcl in gr "{hline 22}{c BT}{hline 63}"
	}

	if ("`scalepar'"!="1") {
		disp "Scale Parameter: " `scalepar' 
	}
	if ("`scaleregul'"!="1") {
		disp "Scale Regularization: " `scaleregul'
	}
	
	if ("`nowarning'"!="") {
	
	if (`h'>=`range_l' | `h'>=`range_r') {
		disp in red "WARNING: bandwidth {it:h} greater than the range of the data."
	}

	if (`b'>=`range_l' | `b'>=`range_r') {
		disp in red "WARNING: Note that the bandwidth {it:b} is greater than the range of the data."
	}

	if (Nh_l<20 | Nh_r<20) {
		disp in red "WARNING: bandwidth {it:h} too low."
	}

	if (Nb_l<20 | Nb_r<20) {
		disp in red "WARNING: bandwidth {it:b} too low."
	}

	if ("`fuzzy'"~="" & "`bwselect'"~="Manual") {
		disp in ye "WARNING: Note that the estimated bandwidths correspond to the Sharp RD estimation of `y' on `x'."
	}
	}
	
	matrix rownames V = RD_Estimate
	matrix colnames V = RD_Estimate
	matrix colnames b = RD_Estimate
	
	local tempo: colfullnames V
	matrix rownames V = `tempo'
	
	if ("`all'"~="") {
	matrix rownames V = Conventional Bias-Corrected Robust
	matrix colnames V = Conventional Bias-Corrected Robust
	matrix colnames b = Conventional Bias-Corrected Robust
	}
	
	
	local ci_l_rb = round(ci_l_rb,0.01)
	local ci_r_rb = round(ci_r_rb,0.01)
	
	restore

	ereturn clear
	ereturn post b V
	ereturn scalar N_l = Nh_l
	ereturn scalar N_r = Nh_r
	ereturn scalar N = Nh_l + Nh_r
	ereturn scalar c = `c'
	ereturn scalar p = `p'
	ereturn scalar q = `q'
	ereturn scalar h_bw = `h'
	ereturn scalar b_bw = `b'
	ereturn scalar tau_cl = tau_cl
	ereturn scalar tau_bc = tau_bc
	** added by Pei **
	ereturn scalar Bias_tau = Bias_tau
	ereturn scalar se_cl  = se_cl
	ereturn scalar se_rb  = se_rb
	ereturn scalar se_bias = se_bias
	ereturn scalar pv_cl = pv_cl 
	ereturn scalar pv_bc = pv_bc
	ereturn scalar pv_rb = pv_rb
	ereturn scalar ci_l_rb = ci_l_rb
	ereturn scalar ci_r_rb = ci_r_rb
	ereturn scalar alpha  = `alpha'
	ereturn scalar level = `level'
	ereturn local ci_l_cl = ci_l_cl
	ereturn local ci_r_cl = ci_r_cl
	ereturn local ci_l_rb = ci_l_rb
	ereturn local ci_r_rb = ci_r_rb
	ereturn local ci_rb  [`ci_l_rb' ; `ci_r_rb']
	ereturn local kernel = "`kernel_type'"
	ereturn local bwsel = "`bwselect'"
	*ereturn local selvce = "`vce'"
	ereturn local outcomevar "`y'"
	ereturn local runningvar "`x'"
	ereturn local depvar "`y'"
	
	if "`fuzzy'"~="" {
	
		ereturn scalar Bias_T_tau = Bias_T_tau
		ereturn scalar se_T_bias = se_T_bias
		ereturn scalar Bias_F_tau = Bias_F_tau
		ereturn scalar se_F_bias = se_F_bias
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

	mata mata clear
	
end
	





