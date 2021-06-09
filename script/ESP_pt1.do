// ESP | adm1

clear all
set scheme s1color
//-----------------------setup

// import end of sample cut-off 
import delim using "$path/data/processing/cutoff_dates.csv", clear 
keep if tag == "default"
local end_sample = end_date[1]

// load data
insheet using "$path/output/processed/ESP_processed.csv", clear 

// set up time variables
gen t = date(date, "YMD")
lab var t "date"
gen dow = dow(t)
gen month = month(t)
gen year = year(t)
gen day = day(t)

// clean up
encode adm1_name, gen(adm1_id)

// set up panel
tsset adm1_id t, daily

// quality control
replace cum_confirmed_cases = . if cum_confirmed_cases < 10 
keep if t >= mdy(3,3,2020) // start date
keep if t <= date("`end_sample'","YMD") // to match other country end dates

// flag which admin unit has longest series
tab adm1_name if cum_confirmed_cases!=., sort 
bysort adm1_name: egen adm1_obs_ct = count(cum_confirmed_cases)

// if multiple admin units have max number of days w/ confirmed cases, 
// choose the admin unit with the max number of confirmed cases 
bysort adm1_name: egen adm1_max_cases = max(cum_confirmed_cases)
egen max_obs_ct = max(adm1_obs_ct)
bysort adm1_obs_ct: egen max_obs_ct_max_cases = max(adm1_max_cases) 

gen longest_series = adm1_obs_ct==max_obs_ct & adm1_max_cases==max_obs_ct_max_cases
drop adm1_obs_ct adm1_max_cases max_obs_ct max_obs_ct_max_cases

sort adm1_id t
tab adm1_name if longest_series==1 & cum_confirmed_cases!=.

//construct dep vars
lab var cum_confirmed_cases "cumulative confirmed cases"

gen daily_confirmed_cases = D.cum_confirmed_cases
lab var daily_confirmed_cases "daily_confirmed_cases"

gen l_cum_confirmed_cases = log(daily_confirmed_cases)
lab var l_cum_confirmed_cases "log(cum_confirmed_cases)"

gen D_l_cum_confirmed_cases = D.l_cum_confirmed_cases 
lab var D_l_cum_confirmed_cases "change in log(cum_confirmed_cases)"


// quality control: cannot have negative changes in cumulative values
replace D_l_cum_confirmed_cases = . if D_l_cum_confirmed_cases <= 0

//------------------diagnostic

// diagnostic plot of trends with sample avg as line
reg D_l_cum_confirmed_cases
gen sample_avg = _b[_cons] if longest_series==1

reg D_l_cum_confirmed_cases i.t
predict day_avg if longest_series==1
lab var day_avg "Observed avg. change in log cases"

*tw (sc D_l_cum_confirmed_cases t, msize(tiny))(line sample_avg t)(sc day_avg t)

destring school_closure work_from_home no_public_gathering home_isolation no_private_gathering transit_suspension travel_ban_local travel_ban_intl, replace

//------------------grouping treatments (based on timing and similarity)
gen p_esp_1 = ((school_closure + work_from_home + travel_ban_local) >  0)
gen p_esp_2 = ((no_public_gathering + no_private_gathering + travel_ban_intl) > 0)
gen p_esp_3 = ((transit_suspension + home_isolation) > 0)
 

//gen p_swe_1 = ((no_gathering_popwt + social_distance_comb_popwt + work_from_home_comb_popwt)/3 > 0)
//gen p_swe_2 = (school_closure_popwt > 0) 
//gen p_swe_3 = ((travel_ban_local_popwt + transit_suspension_popwt)/2 > 0) //transit_suspensions all happen on 2/23 with travel_ban_local in respective admin units
//gen p_swe_4 = (pos_cases_quarantine_popwt > 0)
//gen p_swe_5 = (business_closure_popwt > 0)
//gen p_swe_6 = (home_isolation_popwt > 0)
 
lab var p_esp_1 "School clos, work home, trvl ban local"
lab var p_esp_2 "No public / private gathering, trvl ban intl"
lab var p_esp_3 "Transit suspension, home isolation"

// destring p_uk_1 p_uk_2 p_uk_3 p_uk_4 p_uk_5, replace

  
//------------------main estimates

// output data used for reg
outsheet using "$path/output/regression/ESP_reg_data_10.csv", comma replace

// main regression model
reghdfe D_l_cum_confirmed_cases p_esp_*, absorb(i.adm1_id i.dow, savefe) cluster(t) resid


tempfile results_file
postfile results str18 adm0 str50 policy beta se using `results_file', replace

foreach var in "p_esp_1" "p_esp_2" "p_esp_3" {
	local varlabel : variable label `var'
	post results ("ESP") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_esp_1 + p_esp_2 + p_esp_3

post results ("ESP") ("combined policy") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

// predicting counterfactual growth for each obs
predictnl y_counter = _b[_cons] + __hdfe1__ + __hdfe2__ if e(sample), ci(lb_counter ub_counter)
	
// the mean here is the avg "biological" rate of initial spread
sum y_counter
post results ("ESP") ("no_policy_rate") (round(r(mean), 0.001)) (round(r(sd), 0.001)) 

postclose results
preserve
	use `results_file', clear
	outsheet * using "$path/output/regression/ESP_forest_coefs_1.csv", comma replace
restore
