// KOR | ADM1

clear all
set scheme s1color
//-----------------------setup

// import end of sample cut-off 
import delim using "$path/data/processing/cutoff_dates.csv", clear 
keep if tag == "default"
local end_sample = end_date[1]

// load data
insheet using "$path/data/processing/KOR_processed.csv", clear 

// set up time variables
gen t = date(date, "YMD")
lab var t "date"
gen dow = dow(t)
gen month = month(t)
gen year = year(t)
gen day = day(t)

// clean up
capture: drop adm2_id
encode adm1_name, gen(adm1_id)
duplicates report adm1_id t

// set up panel
tsset adm1_id t, daily

// quality control
replace active_cases = . if cum_confirmed_cases < 10 
replace cum_confirmed_cases = . if cum_confirmed_cases < 10 

keep if t >= mdy(2,17,2020) // start date
keep if t <= date("`end_sample'","YMD") // to match other country end dates

// flag which admin unit has longest series
tab adm1_name if active_cases!=., sort 
bysort adm1_name: egen adm1_obs_ct = count(active_cases)

// if multiple admin units have max number of days w/ confirmed cases, 
// choose the admin unit with the max number of confirmed cases 
bysort adm1_name: egen adm1_max_cases = max(active_cases)
egen max_obs_ct = max(adm1_obs_ct)
bysort adm1_obs_ct: egen max_obs_ct_max_cases = max(adm1_max_cases) 

gen longest_series = adm1_obs_ct==max_obs_ct & adm1_max_cases==max_obs_ct_max_cases
drop adm1_obs_ct adm1_max_cases max_obs_ct max_obs_ct_max_cases

sort adm1_id t
tab adm1_name if longest_series==1 & active_cases!=.


// construct dep vars
lab var active_cases "active cases"

gen l_active_cases = log(active_cases)
lab var l_active_cases "log(active_cases)"

g l_cum_confirmed_cases = log(cum_confirmed_cases)
lab var l_active_cases "log(cum_confirmed_cases)"

gen D_l_active_cases = D.l_active_cases 
*lab var D_l_active_cases "change in log(active_cases)"
lab var D_l_active_cases "Growth rate of active cases (\u0916?log per day)"


//------------------------------------------------------------------------ ACTIVE CASES ADJUSTMENT

// this causes a smooth transition to avoid having negative transmissions, corrects for recoveries and deaths when the log approximation is not very good
gen transmissionrate = D.cum_confirmed_cases/L.active_cases 
gen D_l_active_cases_raw = D_l_active_cases 
lab var D_l_active_cases_raw "change in log active cases (no recovery adjustment)"
replace D_l_active_cases = transmissionrate if D_l_active_cases_raw < 0.04

//------------------------------------------------------------------------ ACTIVE CASES ADJUSTMENT: END


// quality control
replace D_l_active_cases = . if D_l_active_cases < 0 // trying to not model recoveries


//------------------testing regime changes

// grab each date of any testing regime change
preserve
	collapse (min) t, by(testing_regime)
	sort t //should already be sorted but just in case
	drop if _n==1 //dropping 1st testing regime of sample (no change to control for)
	levelsof t, local(testing_change_dates)
restore

// create a dummy for each testing regime change date
foreach t_chg of local testing_change_dates{
	local t_str = string(`t_chg', "%td")
	gen testing_change_kor_`t_str' = t==`t_chg'
	
	local t_lbl = string(`t_chg', "%tdMon_DD,_YYYY")
	lab var testing_change_kor_`t_str' "Testing regime change on `t_lbl'"
}

//------------------diagnostic

// diagnostic plot of trends with sample avg as line
reg D_l_active_cases
gen sample_avg = _b[_cons]
replace sample_avg = . if longest_series==0 & e(sample) == 1

reg D_l_active_cases i.t
predict day_avg if longest_series==1 & e(sample) == 1
lab var day_avg "Observed avg. change in log cases"

*tw (sc D_l_active_cases t, msize(tiny))(line sample_avg t)(sc day_avg t)


//------------------grouping treatments (based on timing and similarity)

gen p_kor_1 = (business_closure_opt + work_from_home_opt + social_distance_opt + no_gathering_opt) / 4
gen p_kor_2 = (no_demonstration + religious_closure + welfare_services_closure) / 3
gen p_kor_3 = emergency_declaration
gen p_kor_4 = pos_cases_quarantine

lab var p_kor_1 "Social distance (optional) - original"
lab var p_kor_2 "Social distance (mandatory) - original"
lab var p_kor_3 "Emergency declaration - original"
lab var p_kor_4 "Quarantine positive cases - original"

outsheet using "$path/output/regression/KOR_reg_data.csv", comma replace

reghdfe D_l_active_cases p_kor_* testing_change_kor_*, absorb(i.adm1_id i.dow, savefe) cluster(t) resid

keep if e(sample) == 1

tempfile results_file
postfile results str18 adm0 str50 policy beta se using `results_file', replace

foreach var in "p_kor_1" "p_kor_2" "p_kor_3" "p_kor_4" {
	local varlabel : variable label `var'
	post results ("KOR") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_kor_1 + p_kor_2 + p_kor_3 + p_kor_4

post results ("KOR") ("combined policy - original") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

replace p_kor_1 = ((business_closure_opt + work_from_home_opt + social_distance_opt + no_gathering_opt) / 4 > 0)
replace p_kor_2 = ((no_demonstration + religious_closure + welfare_services_closure) / 3 > 0)
replace p_kor_3 = (emergency_declaration > 0)
replace p_kor_4 = (pos_cases_quarantine > 0)

lab var p_kor_1 "Social distance (optional)"
lab var p_kor_2 "Social distance (mandatory)"
lab var p_kor_3 "Emergency declaration"
lab var p_kor_4 "Quarantine positive cases"
 
outsheet using "$path/output/regression/KOR_reg_data_10.csv", comma replace
// main regression model
reghdfe D_l_active_cases p_kor_* testing_change_kor_*, absorb(i.adm1_id i.dow, savefe) cluster(t) resid

foreach var in "p_kor_1" "p_kor_2" "p_kor_3" "p_kor_4" {
	local varlabel : variable label `var'
	post results ("KOR") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_kor_1 + p_kor_2 + p_kor_3 + p_kor_4

post results ("KOR") ("combined policy") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

// predicting counterfactual growth for each obs
predictnl y_counter = _b[_cons] + __hdfe1__ + __hdfe2__ if e(sample), ci(lb_counter ub_counter)
	
// the mean here is the avg "biological" rate of initial spread
sum y_counter
post results ("KOR") ("no_policy_rate") (round(r(mean), 0.001)) (round(r(sd), 0.001)) 

postclose results
preserve
	use `results_file', clear
	outsheet * using "$path/output/regression/KOR_forest_coefs.csv", comma replace
	outsheet * using "$path/output/figures/KOR_forest_coefs.csv", comma replace
restore
