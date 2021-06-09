clear all

* local wd  "<<name of directory containing folder covid_npi_analysis>>"
global path "."

// Run regressions for each country

do "$path/script/ESP_pt1.do"
do "$path/script/GER_pt1.do"
do "$path/script/NL_pt1.do"
do "$path/script/UK_pt1.do"

do "$path/script/FRA_pt1.do"
do "$path/script/IRN_pt1.do"
do "$path/script/ITA_pt1.do"
do "$path/script/USA_pt1.do"

do "$path/script/KOR_adm1.do"
do "$path/script/SWE_adm1.do"
