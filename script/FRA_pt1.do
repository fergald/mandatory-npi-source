// FRA | ADM1 

clear all
set scheme s1color
//-----------------------setup

// import end of sample cut-off 
import delim using "$path/data/processing/cutoff_dates.csv", clear 
keep if tag == "default"
local end_sample = end_date[1]

// load data
insheet using "$path/data/processing/FRA_processed.csv", clear 
 
// set up time variables
gen t = date(date, "YMD")
lab var t "date"
gen dow = dow(t)
gen month = month(t)
gen year = year(t)
gen day = day(t)

// clean up
drop adm1_id
encode adm1_name, gen(adm1_id)
duplicates report adm1_id t

// set up panel
tsset adm1_id t, daily

// // set up panel
// xtset adm1_id t

// quality control
drop if cum_confirmed_cases < 10  
keep if t >= mdy(2,29,2020) // Non stable growth before that point & missing data, only one region with +10 but no growth
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


// construct dep vars
lab var cum_confirmed_cases "cumulative confirmed cases"

gen daily_confirmed_cases = D.cum_confirmed_cases
lab var daily_confirmed_cases "daily_confirmed_cases"

gen l_cum_confirmed_cases = log(daily_confirmed_cases)
lab var l_cum_confirmed_cases "log(cum_confirmed_cases)"

gen D_l_cum_confirmed_cases = D.l_cum_confirmed_cases 
lab var D_l_cum_confirmed_cases "change in log(cum. confirmed cases)"

gen l_cum_hospitalized = log(cum_hospitalized)
lab var l_cum_hospitalized "log(cum_hospitalized)"

gen D_l_cum_hospitalized = D.l_cum_hospitalized
lab var D_l_cum_hospitalized "change in log(cum_hospitalized)"


// quality control: cannot have negative changes in cumulative values
replace D_l_cum_confirmed_cases = . if D_l_cum_confirmed_cases <= 0 //0 negative changes for France


//------------------diagnostic

// diagnostic plot of trends with sample avg as line
reg D_l_cum_confirmed_cases
gen sample_avg = _b[_cons] if e(sample)
replace sample_avg = . if longest_series==1

reg D_l_cum_confirmed_cases i.t
predict day_avg if longest_series==1 & e(sample)
lab var day_avg "Observed avg. change in log cases"

*tw (sc D_l_cum_confirmed_cases t, msize(tiny))(line sample_avg t)(sc day_avg t)


//------------------testing regime changes

g testing_change_fra_15mar2020 = t == mdy(3,15,2020) // start of stade 3, none systematic testing
lab var testing_change_fra_15mar2020 "Testing regime change on Mar 15, 2020"


//------------------generate policy packages
gen p_fra_1 = (business_closure + home_isolation_popw) / 2 // big national lockdown policy
lab var p_fra_1 "National lockdown - original"

gen no_gathering_5000 = no_gathering_size <= 5000 
gen no_gathering_1000 = no_gathering_size <= 1000 
gen no_gathering_100 = no_gathering_size <= 100

gen p_fra_2 = (no_gathering_1000 + no_gathering_100 + event_cancel_popw + no_gathering_inside_popw + social_distance_popw) / 5
lab var p_fra_2 "Social distance - original"

gen p_fra_3 = school_closure_popwt
lab var p_fra_3 "School closure - original"

//------------------main estimates

// output data used for reg
outsheet using "$path/output/regression/FRA_reg_data.csv", comma replace

// main regression model
reghdfe D_l_cum_confirmed_cases p_fra_* testing_change_fra_*, absorb(i.adm1_id i.dow, savefe) cluster(t) resid  

keep if e(sample) == 1

tempfile results_file
postfile results str18 adm0 str50 policy beta se using `results_file', replace

foreach var in "p_fra_1" "p_fra_2" "p_fra_3" {
	local varlabel : variable label `var'
	post results ("FRA") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_fra_1 + p_fra_2 + p_fra_3

post results ("FRA") ("combined policy - original") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

replace p_fra_1 = ((business_closure + home_isolation_popw) / 2 > 0)
replace p_fra_2 = ((no_gathering_1000 + no_gathering_100 + event_cancel_popw + no_gathering_inside_popw + social_distance_popw) / 5 > 0) 
replace p_fra_3 = (school_closure_popwt > 0)

lab var p_fra_1 "National lockdown"
lab var p_fra_2 "Social distance"
lab var p_fra_3 "School closure"
 
outsheet using "$path/output/regression/FRA_reg_data_10.csv", comma replace

// main regression model
reghdfe D_l_cum_confirmed_cases p_fra_* testing_change_fra_*, absorb(i.adm1_id i.dow, savefe) cluster(t) resid  

foreach var in "p_fra_1" "p_fra_2" "p_fra_3" {
	local varlabel : variable label `var'
	post results ("FRA") ("`varlabel'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
}

lincom p_fra_1 + p_fra_2 + p_fra_3
post results ("FRA") ("combined policy") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 


// predicting counterfactual growth for each obs
predictnl y_counter = _b[_cons] + __hdfe1__ + __hdfe2__ if e(sample), ci(lb_counter ub_counter)
	
// the mean here is the avg "biological" rate of initial spread
sum y_counter
post results ("FRA") ("no_policy_rate") (round(r(mean), 0.001)) (round(r(sd), 0.001)) 

postclose results
preserve
	use `results_file', clear
	outsheet * using "$path/output/regression/FRA_forest_coefs_1.csv", comma replace
restore
