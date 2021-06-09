clear all

* local wd  "<<name of directory containing folder covid_npi_analysis>>"
global path "."

// Run comparison regressions for each comparison country

do "$path/script/ESP_pt2.do"
do "$path/script/GER_pt2.do"
do "$path/script/NL_pt2.do"
do "$path/script/UK_pt2.do"

do "$path/script/FRA_pt2.do"
do "$path/script/IRN_pt2.do"
do "$path/script/ITA_pt2.do"
do "$path/script/USA_pt2.do"
