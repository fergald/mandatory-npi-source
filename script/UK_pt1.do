// UK | adm1

clear all
set scheme s1color
//-----------------------setup

// import end of sample cut-off 
import delim using "$path/data/processing/cutoff_dates.csv", clear 
keep if tag == "default"
local end_sample = end_date[1]

// load data
insheet using "$path/output/processed/UK_processed.csv", clear 

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
keep if t >= mdy(3,4,2020) // start date
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

gen l_cum_confirmed_cases = log(cum_confirmed_cases)
lab var l_cum_confirmed_cases "log(cum_confirmed_cases)"

gen D_l_cum_confirmed_cases = D.l_cum_confirmed_cases 
lab var D_l_cum_confirmed_cases "change in log(cum_confirmed_cases)"


// quality control: cannot have negative changes in cumulative values
replace D_l_cum_confirmed_cases = . if D_l_cum_confirmed_cases < 0 

//------------------diagnostic

// diagnostic plot of trends with sample avg as line
reg D_l_cum_confirmed_cases
gen sample_avg = _b[_cons] if longest_series==1

reg D_l_cum_confirmed_cases i.t
predict day_avg if longest_series==1
lab var day_avg "Observed avg. change in log cases"

*tw (sc D_l_cum_confirmed_cases t, msize(tiny))(line sample_avg t)(sc day_avg t)

destring school_closure work_from_home no_public_gathering home_isolation no_private_gathering transit_suspension travel_ban_local, replace

//------------------grouping treatments (based on timing and similarity)
gen p_uk_1 = school_closure
gen p_uk_2 = ((work_from_home + no_public_gathering + home_isolation) > 0)
gen p_uk_3 = no_private_gathering
gen p_uk_4 = transit_suspension
gen p_uk_5 = travel_ban_local
  
lab var p_uk_1 "School closure"
lab var p_uk_2 "Work home, no public gathering, home iso"
lab var p_uk_3 "No private gathering"
lab var p_uk_4 "Transit suspension"
lab var p_uk_5 "Travel ban domestic"
 
//------------------main estimates

// output data used for reg
outsheet using "$path/output/regression/UK_reg_data_10.csv", comma replace

// main regression model
reghdfe D_l_cum_confirmed_cases p_uk_*, absorb(i.adm1_id i.dow, savefe) cluster(t) resid


tempfile results_file
postfile results str18 adm0 str50 policy beta se using `results_file', replace

foreach var in "p_uk_1" "p_uk_2" "p_uk_3" "p_uk_4" "p_uk_5" {
	local varlabel : variable label `var'
	post results ("UK") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_uk_1 + p_uk_2 + p_uk_3 + p_uk_4 + p_uk_5

post results ("UK") ("combined policy") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

// predicting counterfactual growth for each obs
predictnl y_counter = _b[_cons] + __hdfe1__ + __hdfe2__ if e(sample), ci(lb_counter ub_counter)
	
// the mean here is the avg "biological" rate of initial spread
sum y_counter
post results ("UK") ("no_policy_rate") (round(r(mean), 0.001)) (round(r(sd), 0.001)) 

postclose results
preserve
	use `results_file', clear
	outsheet * using "$path/output/regression/UK_forest_coefs_1.csv", comma replace
restore
