cd "C:\Users\zp53\Dropbox\rdd3\data"

* Small n

use RD_sim_DL_cl_optbw_500_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_cl_optbw_500_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_cl_optbw_500_jbes.dta, replace

use RD_sim_DL_rb_optbw_500_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_rb_optbw_500_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_rb_optbw_500_jbes.dta, replace

use RD_sim_DL_cl_estbw_500_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_cl_estbw_500_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_cl_estbw_500_jbes.dta, replace	

use RD_sim_DL_rb_estbw_500_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_rb_estbw_500_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_rb_estbw_500_jbes.dta, replace	

use RD_sim_LM_cl_optbw_500_m1_jbes.dta, clear
append using RD_sim_LM_cl_optbw_500_m2_jbes.dta

saveold RD_sim_LM_cl_optbw_500_jbes.dta, replace
	
use RD_sim_LM_rb_optbw_500_m1_jbes.dta, clear
append using RD_sim_LM_rb_optbw_500_m2_jbes.dta

saveold RD_sim_LM_rb_optbw_500_jbes.dta, replace

use RD_sim_LM_cl_estbw_500_m1_jbes.dta, clear
append using RD_sim_LM_cl_estbw_500_m2_jbes.dta

saveold RD_sim_LM_cl_estbw_500_jbes.dta, replace
	
use RD_sim_LM_rb_estbw_500_m1_jbes.dta, clear
append using RD_sim_LM_rb_estbw_500_m2_jbes.dta

saveold RD_sim_LM_rb_estbw_500_jbes.dta, replace



* Actual n
use RD_sim_DL_cl_optbw_6558_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_cl_optbw_6558_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_cl_optbw_6558_jbes.dta, replace


use RD_sim_DL_rb_optbw_6558_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_rb_optbw_6558_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_rb_optbw_6558_jbes.dta, replace

use RD_sim_DL_cl_estbw_6558_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_cl_estbw_6558_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_cl_estbw_6558_jbes.dta, replace		

use RD_sim_DL_rb_estbw_6558_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_rb_estbw_6558_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_rb_estbw_6558_jbes.dta, replace		

use RD_sim_LM_cl_optbw_3105_m1_jbes.dta, clear
append using RD_sim_LM_cl_optbw_3105_m2_jbes.dta

saveold RD_sim_LM_cl_optbw_3105_jbes.dta, replace

use RD_sim_LM_rb_optbw_3105_m1_jbes.dta, clear
append using RD_sim_LM_rb_optbw_3105_m2_jbes.dta

saveold RD_sim_LM_rb_optbw_3105_jbes.dta, replace

use RD_sim_LM_cl_estbw_3105_m1_jbes.dta, clear
append using RD_sim_LM_cl_estbw_3105_m2_jbes.dta

saveold RD_sim_LM_cl_estbw_3105_jbes.dta, replace

use RD_sim_LM_rb_estbw_3105_m1_jbes.dta, clear
append using RD_sim_LM_rb_estbw_3105_m2_jbes.dta

saveold RD_sim_LM_rb_estbw_3105_jbes.dta, replace



* Large n
use RD_sim_DL_cl_optbw_60000_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_cl_optbw_60000_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_cl_optbw_60000_jbes.dta, replace


use RD_sim_DL_cl_estbw_60000_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_cl_estbw_60000_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_cl_estbw_60000_jbes.dta, replace


use RD_sim_DL_rb_optbw_60000_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_rb_optbw_60000_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_rb_optbw_60000_jbes.dta, replace


use RD_sim_DL_rb_estbw_60000_m1_jbes.dta, clear
forvalues k=2/4 {

	append using RD_sim_DL_rb_estbw_60000_m`k'_jbes.dta
	
				}
saveold RD_sim_DL_rb_estbw_60000_jbes.dta, replace		


use RD_sim_LM_cl_optbw_30000_m1_jbes.dta, clear
append using RD_sim_LM_cl_optbw_30000_m2_jbes.dta

saveold RD_sim_LM_cl_optbw_30000_jbes.dta, replace


use RD_sim_LM_cl_estbw_30000_m1_jbes.dta, clear
append using RD_sim_LM_cl_estbw_30000_m2_jbes.dta

saveold RD_sim_LM_cl_estbw_30000_jbes.dta, replace


use RD_sim_LM_rb_optbw_30000_m1_jbes.dta, clear
append using RD_sim_LM_rb_optbw_30000_m2_jbes.dta

saveold RD_sim_LM_rb_optbw_30000_jbes.dta, replace


use RD_sim_LM_rb_estbw_30000_m1_jbes.dta, clear
append using RD_sim_LM_rb_estbw_30000_m2_jbes.dta

saveold RD_sim_LM_rb_estbw_30000_jbes.dta, replace
