clear all
set scheme s1color
//-----------------------setup

// IRN | adm1

// import end of sample cut-off 
import delim using "$path/data/processing/cutoff_dates.csv", clear 
keep if tag == "default"
local end_sample = end_date[1]

// load data
insheet using "$path/data/processing/IRN_processed.csv", clear 

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
keep if t >= mdy(2,27,2020) // start date
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


//quality control
replace D_l_cum_confirmed_cases = . if D_l_cum_confirmed_cases < 0 // cannot have negative changes in cumulative values

// note: missing case data on 3/2/2020 and 3/3/2020
replace D_l_cum_confirmed_cases = . if t == 21976 | t == 21977 // dropping obs when no obs were reported
replace l_cum_confirmed_cases = . if t == 21976 | t == 21977 
replace cum_confirmed_cases = . if t == 21976 | t == 21977 


//------------------testing regime changes

// high_screening_regime in Qom/Gilan/Isfahan, which transitioned on Mar 6
// assume rollout completed on Mar 13 w rest of nation
gen testing_change_irn_13mar2020 = t==mdy(3,13,2020)
lab var testing_change_irn_13mar2020 "Testing regime change on Mar 13, 2020"


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

// NOTE: no_gathering has no variation

// create national opt travel ban var for all provinces except for Qom
// since Qom institutes opt travel ban on 2/20 before sample period
// and national opt travel ban enacted on 3/1
gen travel_ban_local_opt_natl = travel_ban_local_opt
	replace travel_ban_local_opt_natl = 0 if adm1_name=="Qom"

// create national school_closure var for provinces that close schools on 3/5
by adm1_id: egen school_closure_natl0 = min(school_closure) 
gen school_closure_natl = school_closure if school_closure_natl0==0
	replace school_closure_natl = 0 if school_closure_natl==.
drop school_closure_natl0
	
// Merging March 1-5 policies, since they all happened at the same time during 
// break in the health data (missing cases for 3/2-3/3)
// so p_irn_1 = 1/3 on 3/1 when opt travel ban enacted, then p_irn_1 = 1 starting 3/5
gen p_irn_1 = (travel_ban_local_opt_natl + work_from_home + school_closure_natl)/3
lab var p_irn_1 "Trvl ban opt, work home, school clos (natl) - original"

// home isolation started March 13
gen p_irn_2 = home_isolation
lab var p_irn_2 "Home isolation - original"

// shrines in Qom closed March 17
// gen p_irn_3 = religious_closure
// lab var p_irn_3 "Religious closure"
// will not include, insufficient data after enactment


//------------------main estimates

// output data used for reg
outsheet using "$path/output/regression/IRN_reg_data.csv", comma replace

// main regression model
reghdfe D_l_cum_confirmed_cases p_irn_* testing_change_irn_*, absorb(i.adm1_id i.dow, savefe) cluster(date) resid
keep if e(sample) == 1

// save regression coef and se
tempfile results_file
postfile results str18 adm0 str50 policy beta se using `results_file', replace

foreach var in "p_irn_1" "p_irn_2" {
	local varlabel : variable label `var'
	post results ("IRN") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_irn_1 + p_irn_2

post results ("IRN") ("combined policy - original") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 


replace p_irn_1 = ((travel_ban_local_opt_natl + work_from_home + school_closure_natl)/3 > 0)
replace p_irn_2 = (home_isolation > 0)
 
lab var p_irn_1 "Trvl ban opt, work home, school clos (natl)"
lab var p_irn_2 "Home isolation" 
 
outsheet using "$path/output/regression/IRN_reg_data_10.csv", comma replace
// main regression model
reghdfe D_l_cum_confirmed_cases p_irn_* testing_change_irn_*, absorb(i.adm1_id i.dow, savefe) cluster(date) resid

foreach var in "p_irn_1" "p_irn_2" {
	local varlabel : variable label `var'
	post results ("IRN") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_irn_1 + p_irn_2

post results ("IRN") ("combined policy") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

// predicting counterfactual growth for each obs
predictnl y_counter = _b[_cons] + __hdfe1__ + __hdfe2__ if e(sample), ci(lb_counter ub_counter)
	
// the mean here is the avg "biological" rate of initial spread
sum y_counter
post results ("IRN") ("no_policy_rate") (round(r(mean), 0.001)) (round(r(sd), 0.001)) 

postclose results
preserve
	use `results_file', clear
	outsheet * using "$path/output/regression/IRN_forest_coefs_1.csv", comma replace
restore
