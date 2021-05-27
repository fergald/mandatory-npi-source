// USA | adm1
set matsize 5000
clear all
set scheme s1color
//-----------------------setup

// import end of sample cut-off 
import delim using "$path/data/processing/cutoff_dates.csv", clear 
keep if tag == "default"
local end_sample = end_date[1]

// import state name to abbreviations crosswalk
insheet using "$path/data/processing/state_name_abbrev_xwalk.csv", names clear
tempfile state_abb
save `state_abb'

// load data
insheet using "$path/data/processing/USA_processed.csv", clear 

// set up time variables
gen t = date(date, "YMD",2020)
lab var t "date"
gen dow = dow(t)
gen month = month(t)
gen year = year(t)
gen day = day(t)

// clean up
keep if t >= mdy(3,3,2020) // start date
keep if t <= date("`end_sample'","YMD") // to match other country end dates

encode adm1, gen(adm1_id)
duplicates report adm1_id t

// set up panel
tsset adm1_id t, daily

// add state abbreviations
merge m:1 adm1_name using `state_abb', nogen

// quality control
drop if cum_confirmed_cases < 10 

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

// construct dep vars
lab var cum_confirmed_cases "cumulative confirmed cases"

gen l_cum_confirmed_cases = log(cum_confirmed_cases)
lab var l_cum_confirmed_cases "log(cum_confirmed_cases)"

gen D_l_cum_confirmed_cases = D.l_cum_confirmed_cases 
lab var D_l_cum_confirmed_cases "change in log(cum_confirmed_cases)"

// quality control
replace D_l_cum_confirmed_cases = . if D_l_cum_confirmed_cases < 0 // cannot have negative changes in cumulative values


//--------------testing regime changes

// testing regime changes at the state-level
*tab testing_regime, mi
*tab adm1_name t if testing_regime>0

// grab each date of any testing regime change by state
preserve
	collapse (min) t, by(testing_regime adm1_name adm1_abb)
	sort adm1_name t //should already be sorted but just in case
	by adm1_name: drop if _n==1 //dropping 1st testing regime of state sample (no change to control for)
	
	// create label for testing_regime_change vars
	// that notes the date and states for changes	
	gen var_lbl = "Testing regime change on " + string(t, "%tdMon_DD,_YYYY") + " in " + adm1_abb
	levelsof var_lbl, local(test_var_lbl)
restore

// create a dummy for each testing regime change date w/in state
foreach lbl of local test_var_lbl{
	local t_lbl = substr("`lbl'", 26, 12)
	local t_chg = date("`t_lbl'", "MDY")
	local t_str = string(`t_chg', "%td")
	local adm1 = substr("`lbl'", -2, .)
	
	gen testing_change_usa_`t_str'_`adm1' = t==`t_chg' * D.testing_regime & adm1_abb=="`adm1'"
	lab var testing_change_usa_`t_str'_`adm1' "`lbl'"
}
*order testing_regime_*mar*, before(testing_regime_*apr*)


//------------------diagnostic

// diagnostic plot of trends with sample avg as line
reg D_l_cum_confirmed_cases
gen sample_avg = _b[_cons]
replace sample_avg = . if longest_series==0 & e(sample) == 1

reg D_l_cum_confirmed_cases i.t
predict day_avg if longest_series==1 & e(sample) == 1

lab var day_avg "Observed avg. change in log cases"

*tw (sc D_l_cum_confirmed_cases t, msize(tiny))(line sample_avg t)(sc day_avg t)


//------------------grouping treatments (based on timing and similarity)

// popwt vars = policy intensity * population weight of respective admin 1 unit or sub admin 1 unit

gen p_usa_1 = no_gathering_popwt
gen p_usa_2 = social_distance_popwt 
gen p_usa_3 = pos_cases_quarantine_popwt 
gen p_usa_4 = paid_sick_leave_popwt
gen p_usa_5 = work_from_home_popwt
gen p_usa_6 = school_closure_popwt
gen p_usa_7 = (travel_ban_local_popwt + transit_suspension_popwt) / 2 
gen p_usa_8 = business_closure_popwt
gen p_usa_9 = religious_closure_popwt
gen p_usa_10 = home_isolation_popwt
gen p_usa_11 = federal_guidelines

lab var p_usa_1 "No gathering - original"
lab var p_usa_2 "Social distance - original"
lab var p_usa_3 "Quarantine positive cases - original" 
lab var p_usa_4 "Paid sick leave - original"
lab var p_usa_5 "Work from home - original"
lab var p_usa_6 "School closure - original"
lab var p_usa_7 "Travel ban - original"
lab var p_usa_8 "Business closure - original"
lab var p_usa_9 "Religious closure - original" 
lab var p_usa_10 "Home isolation - original" 
lab var p_usa_11 "Slow the Spread Guidelines - original" 


outsheet using "$path/output/regression/USA_reg_data.csv", comma replace

reghdfe D_l_cum_confirmed_cases p_usa_* testing_change_usa*, absorb(i.adm1_id i.dow, savefe) cluster(t) resid

keep if e(sample) == 1

tempfile results_file
postfile results str18 adm0 str50 policy beta se using `results_file', replace

foreach var in "p_usa_1" "p_usa_2" "p_usa_3" "p_usa_4" "p_usa_5" "p_usa_6" "p_usa_7" "p_usa_8" "p_usa_9" "p_usa_10" "p_usa_11" {
	local varlabel : variable label `var'
	post results ("USA") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_usa_1 + p_usa_2 + p_usa_3 + p_usa_4 + p_usa_5 + p_usa_6 + p_usa_7 + p_usa_8 + p_usa_9 + p_usa_10 + p_usa_11

post results ("USA") ("combined policy - original") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

replace p_usa_1 = (no_gathering_popwt > 0)
replace p_usa_2 = (social_distance_popwt > 0)
replace p_usa_3 = (pos_cases_quarantine_popwt > 0)
replace p_usa_4 = (paid_sick_leave_popwt > 0)
replace p_usa_5 = (work_from_home_popwt > 0)
replace p_usa_6 = (school_closure_popwt > 0)
replace p_usa_7 = ((travel_ban_local_popwt + transit_suspension_popwt) / 2 > 0)
replace p_usa_8 = (business_closure_popwt > 0)
replace p_usa_9 = (religious_closure_popwt > 0)
replace p_usa_10 = (home_isolation_popwt > 0)
replace p_usa_11 = (federal_guidelines > 0)

lab var p_usa_1 "No gathering"
lab var p_usa_2 "Social distance"
lab var p_usa_3 "Quarantine positive cases" 
lab var p_usa_4 "Paid sick leave"
lab var p_usa_5 "Work from home"
lab var p_usa_6 "School closure"
lab var p_usa_7 "Travel ban"
lab var p_usa_8 "Business closure"
lab var p_usa_9 "Religious closure" 
lab var p_usa_10 "Home isolation" 
lab var p_usa_11 "Slow the Spread Guidelines" 

outsheet using "$path/output/regression/USA_reg_data_10.csv", comma replace
// main regression model
reghdfe D_l_cum_confirmed_cases p_usa_* testing_change_usa_*, absorb(i.adm1_id i.dow, savefe) cluster(t) resid

foreach var in "p_usa_1" "p_usa_2" "p_usa_3" "p_usa_4" "p_usa_5" "p_usa_6" "p_usa_7" "p_usa_8" "p_usa_9" "p_usa_10" "p_usa_11" {
	local varlabel : variable label `var'
	post results ("USA") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_usa_1 + p_usa_2 + p_usa_3 + p_usa_4 + p_usa_5 + p_usa_6 + p_usa_7 + p_usa_8 + p_usa_9 + p_usa_10 + p_usa_11

post results ("USA") ("combined policy") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

// predicting counterfactual growth for each obs
predictnl y_counter = _b[_cons] + __hdfe1__ + __hdfe2__ if e(sample), ci(lb_counter ub_counter)
	
// the mean here is the avg "biological" rate of initial spread
sum y_counter
post results ("USA") ("no_policy_rate") (round(r(mean), 0.001)) (round(r(sd), 0.001)) 

postclose results
preserve
	use `results_file', clear
	outsheet * using "$path/output/regression/USA_forest_coefs_1.csv", comma replace
restore
