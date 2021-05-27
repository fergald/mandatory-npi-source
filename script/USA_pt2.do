// USA SWE | adm1

// load data
import delimited using "$path/output/regression/SWE_USA_reg_data_10.csv", clear 

destring d_l_cum_confirmed_cases, replace force

egen adm1_id_c = group(adm1_id)
egen dow_c = group(dow)

// main regression model

reghdfe d_l_cum_confirmed_cases p_swe_* p_usa_* testing_change_usa_*, absorb(i.adm1_id_c i.dow_c, savefe) cluster(t) resid

tempfile results_file
postfile results str18 adm0 str50 policy beta se using `results_file', replace

lincom(p_usa_1 + p_usa_2 + p_usa_3 + p_usa_4 + p_usa_5 + p_usa_6 + p_usa_7 + p_usa_8 + p_usa_9 + p_usa_10 + p_usa_11 - (p_swe_1 + p_swe_2 + p_swe_3 + p_swe_4 + p_swe_5 + p_swe_6))

post results ("USA") ("SWE comparator") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

// USA KOR | adm1

import delimited using "$path/output/regression/KOR_USA_reg_data_10.csv", clear 

destring d_l_cum_confirmed_cases, replace force


egen adm1_id_c = group(adm1_id)
egen dow_c = group(dow)

// main regression model

reghdfe d_l_cum_confirmed_cases p_kor_* p_usa_* testing_change_usa_* testing_change_kor_*, absorb(i.adm1_id_c i.dow_c, savefe) cluster(t) resid

lincom(p_usa_1 + p_usa_2 + p_usa_3 + p_usa_4 + p_usa_5 + p_usa_6 + p_usa_7 + p_usa_8 + p_usa_9 + p_usa_10 + p_usa_11 - (p_kor_1 + p_kor_2 + p_kor_3 + p_kor_4))

post results ("USA") ("KOR comparator") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

postclose results
preserve
	use `results_file', clear
	outsheet * using "$path/output/regression/USA_forest_coefs_2.csv", comma replace
restore
