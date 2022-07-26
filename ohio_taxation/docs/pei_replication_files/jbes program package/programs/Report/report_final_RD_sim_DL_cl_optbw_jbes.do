** this file reports the simulation results based on various 
** polynomial orders for the DL DGP

capture cd C:\Users\user\Dropbox\RA_Pei\Suejin_Lee\local_poly\data
capture cd C:\Users\zp53\Dropbox\rdd3\data
capture cd C:\Users\peizhuan\Dropbox\rdd3\data

foreach n in 500 6558 60000 {
	
	clear all
	use "RD_sim_DL_cl_optbw_`n'_jbes.dta", clear

	scalar true_disc = 0.04
	
	forvalues pol=0/4 {
		foreach ker in unif tri {

		* Confidence intervals
		qui gen ci_upper_cl_`ker'_`pol' = tau_cl_`ker'_`pol' + 1.96*se_cl_`ker'_`pol'
		qui gen ci_lower_cl_`ker'_`pol' = tau_cl_`ker'_`pol' - 1.96*se_cl_`ker'_`pol'
		
		* Absolute value of t-stat
		qui gen t_cl_`ker'_`pol' = abs((tau_cl_`ker'_`pol' - true_disc)/se_cl_`ker'_`pol')
		
		* Critical value at 95th percentile
		qui sum t_cl_`ker'_`pol', d
		scalar cvalue_cl_`ker'_`pol' = r(p95)

		* Size-adjusted CI
		qui gen ciadj_upper_cl_`ker'_`pol' = tau_cl_`ker'_`pol' + cvalue_cl_`ker'_`pol'*se_cl_`ker'_`pol'
		qui gen ciadj_lower_cl_`ker'_`pol' = tau_cl_`ker'_`pol' - cvalue_cl_`ker'_`pol'*se_cl_`ker'_`pol'
	
		* Indicator for coverage
		qui gen cvr_cl_`ker'_`pol' = (ci_upper_cl_`ker'_`pol' > true_disc) & (ci_lower_cl_`ker'_`pol' < true_disc)
		
		* Indicator for coverage using size-adjusted CI
		qui gen cvradj_cl_`ker'_`pol' = (ciadj_upper_cl_`ker'_`pol' > true_disc) & (ciadj_lower_cl_`ker'_`pol' < true_disc)
		
		* Length of CI
		qui gen lng_cl_`ker'_`pol' = ci_upper_cl_`ker'_`pol' - ci_lower_cl_`ker'_`pol'
		
		* Size-adjusted CI length
		qui gen lngadj_cl_`ker'_`pol' = ciadj_upper_cl_`ker'_`pol' - ciadj_lower_cl_`ker'_`pol'
		
		* Squared deviation
		qui gen devsq_cl_`ker'_`pol' = (tau_cl_`ker'_`pol'- true_disc)^2

		* AMSE-chosen p
		qui gen pfer_cl_`ker'_`pol' = min(amse_cl_`ker'_0, amse_cl_`ker'_1, amse_cl_`ker'_2, amse_cl_`ker'_3, amse_cl_`ker'_4) == amse_cl_`ker'_`pol'
		}
	}	
	
	foreach stat in n h tau_cl devsq_cl cvr_cl lng_cl cvradj_cl lngadj_cl {
		foreach ker in unif tri {
		
		* Estimates of AMSE-chosen p
		qui gen `stat'_`ker'_pf =.
		qui replace `stat'_`ker'_pf = `stat'_`ker'_0 if pfer_cl_`ker'_0 == 1
		qui replace `stat'_`ker'_pf = `stat'_`ker'_1 if pfer_cl_`ker'_1 == 1
		qui replace `stat'_`ker'_pf = `stat'_`ker'_2 if pfer_cl_`ker'_2 == 1
		qui replace `stat'_`ker'_pf = `stat'_`ker'_3 if pfer_cl_`ker'_3 == 1
		qui replace `stat'_`ker'_pf = `stat'_`ker'_4 if pfer_cl_`ker'_4 == 1

		}
	}
	
	foreach p in 0 2 3 4 pf {
		foreach ker in unif tri {

		* Indicator variable for smaller squared deviation
		qui gen smler_devsq_cl_`ker'_`p' = devsq_cl_`ker'_`p' - devsq_cl_`ker'_1 < 0
		
		* Indicator variable for smaller CI length
		qui gen smler_lng_cl_`ker'_`p' = lng_cl_`ker'_`p' - lng_cl_`ker'_1 < 0
		}
	}

	foreach p in 0 1 2 3 4 pf {
		foreach ker in unif tri {

		* MSE
		qui sum devsq_cl_`ker'_`p'
		scalar mse_cl_`ker'_`p' = r(mean)*1000
		scalar mse_cl_`ker'_`p'_r = round(r(mean)*1000, 0.001)
		
		* Coverage rate
		qui sum cvr_cl_`ker'_`p'
		scalar ave_cvr_cl_`ker'_`p' = r(mean)
		scalar ave_cvr_cl_`ker'_`p'_r = round(r(mean), 0.001)
		
		* Coverage rate (SIZE-ADJUSTED)
		qui sum cvradj_cl_`ker'_`p'
		scalar ave_cvradj_cl_`ker'_`p' = r(mean)
		scalar ave_cvradj_cl_`ker'_`p'_r = round(r(mean), 0.001)
		
		* Average CI length
		qui sum lng_cl_`ker'_`p'
		scalar ave_lng_cl_`ker'_`p' = r(mean)
		scalar ave_lng_cl_`ker'_`p'_r = round(r(mean), 0.001)
		
		* Average CI length (SIZE-ADJUSTED)
		qui sum lngadj_cl_`ker'_`p'
		scalar ave_lngadj_cl_`ker'_`p' = r(mean)
		scalar ave_lngadj_cl_`ker'_`p'_r = round(r(mean), 0.001)
		
		}
	}

	
	foreach p in 0 1 2 3 4 pf {
		foreach ker in unif tri {

		* Average n
		qui sum n_`ker'_`p'
		scalar ave_n_`ker'_`p' = round(r(mean))
		
		* Average bandwidth (h)
		qui sum h_`ker'_`p'
		scalar ave_h_`ker'_`p' = round(r(mean), 0.001)
		
		}
	}	

	foreach p in 0 2 3 4 pf {
		foreach ker in unif tri {
	
		* Delta of MSE
		scalar d_mse_cl_`ker'_`p' = mse_cl_`ker'_`p'/mse_cl_`ker'_1
		
		* Delta coverage
		scalar d_ave_cvr_cl_`ker'_`p' = ave_cvr_cl_`ker'_`p'/ave_cvr_cl_`ker'_1
		
		* Delta average CI length 
		scalar d_ave_lng_cl_`ker'_`p' = ave_lng_cl_`ker'_`p'/ave_lng_cl_`ker'_1
		
		* Delta average CI length (SIZE-ADJUSTED)
		scalar d_ave_lngadj_cl_`ker'_`p' = ave_lngadj_cl_`ker'_`p'/ave_lngadj_cl_`ker'_1
		}
	}	

	forvalues p=0/4 {
		foreach ker in unif tri {
		
		*Prob of AMSE-chosen p 
		qui sum pfer_cl_`ker'_`p'
		scalar pr_pfer_cl_`ker'_`p' = round(r(mean), 0.001)
		
		}		
	}
		
		
		
****************************************************************************************************************************	
	di "Obs is `n'"
	di "Uniform kernel"
	
	di "Theo. Optimal;" 1 ";" ave_h_unif_1 ";" ave_n_unif_1 ";" mse_cl_unif_1 ";" ///
		ave_cvr_cl_unif_1 ";" ave_lng_cl_unif_1 ";" ave_lngadj_cl_unif_1		
		
	di ""	
	
	di "Theo. Optimal;" 0 ";" ave_h_unif_0 ";" ave_n_unif_0 ";" d_mse_cl_unif_0 ";" ///
		ave_cvr_cl_unif_0 ";" d_ave_lng_cl_unif_0 ";" d_ave_lngadj_cl_unif_0		
	di ";" 2 ";" ave_h_unif_2 ";" ave_n_unif_2 ";" d_mse_cl_unif_2 ";" ///
		ave_cvr_cl_unif_2 ";" d_ave_lng_cl_unif_2 ";" d_ave_lngadj_cl_unif_2		
	di ";" 3 ";" ave_h_unif_3 ";" ave_n_unif_3 ";" d_mse_cl_unif_3 ";" ///
		ave_cvr_cl_unif_3 ";" d_ave_lng_cl_unif_3 ";" d_ave_lngadj_cl_unif_3		
	di ";" 4 ";" ave_h_unif_4 ";" ave_n_unif_4 ";" d_mse_cl_unif_4 ";" ///
		ave_cvr_cl_unif_4 ";" d_ave_lng_cl_unif_4 ";" d_ave_lngadj_cl_unif_4		
	di ";phat;" ave_h_unif_pf ";" ave_n_unif_pf ";" d_mse_cl_unif_pf ";" ///
		ave_cvr_cl_unif_pf ";" d_ave_lng_cl_unif_pf ";" d_ave_lngadj_cl_unif_pf		
	di ";Fraction of time phat=(0,1,2,3,4): (" pr_pfer_cl_unif_0 ", " pr_pfer_cl_unif_1 ", " ///
		pr_pfer_cl_unif_2 ", " pr_pfer_cl_unif_3 ", " pr_pfer_cl_unif_4 ")"
		
	
	**	
	di ""
	di "Triangular kernel"
	
	di "Theo. Optimal;" 1 ";" ave_h_tri_1 ";" ave_n_tri_1 ";" mse_cl_tri_1 ";" ///
		ave_cvr_cl_tri_1 ";" ave_lng_cl_tri_1 ";" ave_lngadj_cl_tri_1		
		
	di ""	
	
	di "Theo. Optimal;" 0 ";" ave_h_tri_0 ";" ave_n_tri_0 ";" d_mse_cl_tri_0 ";" ///
		ave_cvr_cl_tri_0 ";" d_ave_lng_cl_tri_0 ";" d_ave_lngadj_cl_tri_0		
	di ";" 2 ";" ave_h_tri_2 ";" ave_n_tri_2 ";" d_mse_cl_tri_2 ";" ///
		ave_cvr_cl_tri_2 ";" d_ave_lng_cl_tri_2 ";" d_ave_lngadj_cl_tri_2		
	di ";" 3 ";" ave_h_tri_3 ";" ave_n_tri_3 ";" d_mse_cl_tri_3 ";" ///
		ave_cvr_cl_tri_3 ";" d_ave_lng_cl_tri_3 ";" d_ave_lngadj_cl_tri_3		
	di ";" 4 ";" ave_h_tri_4 ";" ave_n_tri_4 ";" d_mse_cl_tri_4 ";" ///
		ave_cvr_cl_tri_4 ";" d_ave_lng_cl_tri_4 ";" d_ave_lngadj_cl_tri_4		
	di ";phat;" ave_h_tri_pf ";" ave_n_tri_pf ";" d_mse_cl_tri_pf ";" ///
		ave_cvr_cl_tri_pf ";" d_ave_lng_cl_tri_pf ";" d_ave_lngadj_cl_tri_pf		
	di ";Fraction of time phat=(0,1,2,3,4): (" pr_pfer_cl_tri_0 ", " pr_pfer_cl_tri_1 ", " ///
		pr_pfer_cl_tri_2 ", " pr_pfer_cl_tri_3 ", " pr_pfer_cl_tri_4 ")"
				
	}

