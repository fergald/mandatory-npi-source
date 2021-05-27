// ITA | adm2

clear all
set scheme s1color
//-----------------------setup

// import end of sample cut-off 
import delim using "$path/data/processing/cutoff_dates.csv", clear 
keep if tag == "default"
local end_sample = end_date[1]

// load data
insheet using "$path/data/processing/ITA_processed.csv", clear

// set up time variables
gen t = date(date, "YMD")
lab var t "date"
gen dow = dow(t)
gen month = month(t)
gen year = year(t)
gen day = day(t)


// clean up
drop if adm2_name == "Unknown"
drop adm1_id adm2_id
encode adm1_name, gen(adm1_id)
encode adm2_name, gen(adm2_id)
duplicates report adm2_id t

// set up panel
tsset adm2_id t, daily

// quality control
replace cum_confirmed_cases = . if cum_confirmed_cases < 10 
keep if t >= mdy(2,26,2020) // start date
keep if t <= date("`end_sample'","YMD") // to match other country end dates

// flag which admin unit has longest series
tab adm2_name if cum_confirmed_cases!=., sort 
bysort adm1_name adm2_name: egen adm2_obs_ct = count(cum_confirmed_cases)

// if multiple admin units have max number of days w/ confirmed cases, 
// choose the admin unit with the max number of confirmed cases 
bysort adm1_name adm2_name: egen adm2_max_cases = max(cum_confirmed_cases)
egen max_obs_ct = max(adm2_obs_ct)
bysort adm2_obs_ct: egen max_obs_ct_max_cases = max(adm2_max_cases) 

gen longest_series = adm2_obs_ct==max_obs_ct & adm2_max_cases==max_obs_ct_max_cases
drop adm2_obs_ct adm2_max_cases max_obs_ct max_obs_ct_max_cases

sort adm2_id t
tab adm2_name if longest_series==1 & cum_confirmed_cases!=.

// construct dep vars
lab var cum_confirmed_cases "cumulative confirmed cases"

gen l_cum_confirmed_cases = log(cum_confirmed_cases)
lab var l_cum_confirmed_cases "log(cum_confirmed_cases)"

gen D_l_cum_confirmed_cases = D.l_cum_confirmed_cases 
lab var D_l_cum_confirmed_cases "change in log(cum_confirmed_cases)"

// quality control: cannot have negative changes in cumulative values
replace D_l_cum_confirmed_cases = . if D_l_cum_confirmed_cases < 0 


//--------------testing regime changes

// testing regime change on Feb 26, which is the start of sample so no changes to account for
// // grab each date of any testing regime change
// preserve
// 	collapse (min) t, by(testing_regime)
// 	sort t //should already be sorted but just in case
// 	drop if _n==1 //dropping 1st testing regime of sample (no change to control for)
// 	levelsof t, local(testing_change_dates)
// restore
//
// // create a dummy for each testing regime change date
// foreach t_chg of local testing_change_dates{
// 	local t_str = string(`t_chg', "%td")
// 	gen testing_regime_change_`t_str' = t==`t_chg'
// }

//------------------diagnostic

// diagnostic plot of trends with sample avg as line
reg D_l_cum_confirmed_cases
gen sample_avg = _b[_cons] if longest_series==1

reg D_l_cum_confirmed_cases i.t
predict day_avg if longest_series==1
lab var day_avg "Observed avg. change in log cases"

*tw (sc D_l_cum_confirmed_cases t, msize(tiny))(line sample_avg t)(sc day_avg t)


//------------------grouping treatments (based on timing and similarity)

// popwt vars = policy intensity * population weight of respective admin 2 unit or sub admin 2 unit

// combine optional policies with respective mandatory policies
// weighing optional policies by 1/2
gen social_distance_comb_popwt = social_distance_popwt + social_distance_opt_popwt * 0.5 
gen work_from_home_comb_popwt = work_from_home_popwt + work_from_home_opt_popwt * 0.5

// originally p_1 & p_5 & p_6were following:
// gen p_ita_1 = (no_gathering_popwt + social_distance_comb_popwt + work_from_home_comb_popwt)/3 + business
gen p_ita_1 = ((no_gathering_popwt + social_distance_comb_popwt + work_from_home_comb_popwt)/3 + business_closure_popwt) / 2
gen p_ita_2 = school_closure_popwt  
gen p_ita_3 = (travel_ban_local_popwt + transit_suspension_popwt)/2 //transit_suspensions all happen on 2/23 with travel_ban_local in respective admin units
gen p_ita_4 = pos_cases_quarantine_popwt 
// gen p_ita_5 = business_closure_popwt
gen p_ita_5 = home_isolation_popwt  
//gen p_ita_6 = home_isolation_popwt  

//lab var p_ita_1 "Social distance - original"
lab var p_ita_1 "Social distance, business closure - original"
lab var p_ita_2 "School closure - original"
lab var p_ita_3 "Travel ban - original"
lab var p_ita_4 "Quarantine positive cases - original"
//lab var p_ita_5 "Business closure - original"
lab var p_ita_5 "Home isolation - original"
//lab var p_ita_6 "Home isolation - original"

// gen p_ita_1 = ((no_gathering_popwt + social_distance_comb_popwt + work_from_home_comb_popwt)/3 + business_closure_popwt)/2

// gen p_ita_5 = home_isolation_popwt  

// lab var p_ita_1 "Social distance and business closure - original"

// lab var p_ita_5 "Home isolation - original"


outsheet using "$path/output/regression/ITA_reg_data.csv", comma replace

reghdfe D_l_cum_confirmed_cases p_ita_*, absorb(i.adm2_id i.dow, savefe) cluster(t) resid

keep if e(sample) == 1

tempfile results_file
postfile results str18 adm0 str50 policy beta se using `results_file', replace

foreach var in "p_ita_1" "p_ita_2" "p_ita_3" "p_ita_4" "p_ita_5" {
	local varlabel : variable label `var'
	post results ("ITA") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_ita_1 + p_ita_2 + p_ita_3 + p_ita_4 + p_ita_5

post results ("ITA") ("combined policy - original") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 


// ------------- generating predicted values and counterfactual predictions based on treatment

// // predicted "actual" outcomes with real policies
// predictnl y_actual = xb() + __hdfe1__ + __hdfe2__ if e(sample), ci(lb_y_actual ub_y_actual) replace
// lab var y_actual "predicted growth with actual policy"

// // estimating magnitude of treatment effects for each obs
// gen treatment = ///
// p_ita_1 * _b[p_ita_1] + ///
// p_ita_2 * _b[p_ita_2] + ///
// p_ita_3 * _b[p_ita_3] + /// 
// p_ita_4 * _b[p_ita_4] + /// 
// p_ita_5 * _b[p_ita_5] + /// 
// p_ita_6 * _b[p_ita_6] /// 
// if e(sample)

replace p_ita_1 = (((no_gathering_popwt + social_distance_comb_popwt + work_from_home_comb_popwt)/3 + business_closure_popwt) / 2 > 0)
replace p_ita_2 = (school_closure_popwt > 0) 
replace p_ita_3 = ((travel_ban_local_popwt + transit_suspension_popwt)/2 > 0) //transit_suspensions all happen on 2/23 with travel_ban_local in respective admin units
replace p_ita_4 = (pos_cases_quarantine_popwt > 0)
//replace p_ita_5 = (business_closure_popwt > 0)
replace p_ita_5 = (home_isolation_popwt > 0)

lab var p_ita_1 "Social distance, business closure"
lab var p_ita_2 "School closure"
lab var p_ita_3 "Travel ban"
lab var p_ita_4 "Quarantine positive cases"
lab var p_ita_5 "Home isolation"
 
outsheet using "$path/output/regression/ITA_reg_data_10.csv", comma replace

// main regression model
reghdfe D_l_cum_confirmed_cases p_ita_*, absorb(i.adm2_id i.dow, savefe) cluster(t) resid


foreach var in "p_ita_1" "p_ita_2" "p_ita_3" "p_ita_4" "p_ita_5" {
	local varlabel : variable label `var'
	post results ("ITA") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_ita_1 + p_ita_2 + p_ita_3 + p_ita_4 + p_ita_5

post results ("ITA") ("combined policy") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

// predicting counterfactual growth for each obs
predictnl y_counter = _b[_cons] + __hdfe1__ + __hdfe2__ if e(sample), ci(lb_counter ub_counter)
	
// the mean here is the avg "biological" rate of initial spread
sum y_counter
post results ("ITA") ("no_policy_rate") (round(r(mean), 0.001)) (round(r(sd), 0.001)) 

postclose results
preserve
	use `results_file', clear
	outsheet * using "$path/output/regression/ITA_forest_coefs_1.csv", comma replace
restore
