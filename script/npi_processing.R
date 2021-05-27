# Libraries used
library(tidyverse)
library(readxl)
library(lubridate)

# paths
setwd(str_c(getwd(), "/data"))


# Case count files by country

path_sweden <- "./case_counts/sweden_daily_cases_region.xlsx"
uk_region_files <- 
  list.files(
    path = "./case_counts/england/",
    pattern = 'data_2020-Sep-01.*\\.csv$',
    recursive = TRUE,
    full.names = TRUE
  )
path_germany <- "./case_counts/RKI_COVID19.csv"
path_spain <- "./case_counts/ccaa_covid19_casos_long.csv"
path_netherlands <- "./case_counts/rivm_NL_covid19_province.csv"

# Oxford COVID policies datasets
path_policies <- "./policies/"
path_policy_files <- c(
  "c1_schoolclosing",
  "c2_workplaceclosing",
  "c3_cancelpublicevents",
  "c4_restrictionsongatherings",
  "c5_closepublictransport",
  "c6_stayathomerequirements",
  "c7_domestictravel",
  "c8_internationaltravel",
  "e1_incomesupport",
  "e2_debtcontractrelief",
  "h1_publicinfocampaign",
  "h2_testingpolicy",
  "h3_contacttracing"
)
# Output file path
path_output <- "/Users/seungwonho/Dropbox/covid_npi_analysis/output/"

# Temp file path
path_temp <- str_c(path_output, "temp/")
# Procesed output file path
path_processed <- str_c(path_output, "processed/")

# Get case counts
sweden <- 
  read_xlsx(path_sweden) %>% 
  pivot_longer(
    cols = Blekinge:Östergötland,
    names_to = "adm1_name",
    values_to = "case_count"
  ) %>% 
  mutate(
    date = Statistikdatum %>% as_date()
  ) %>% 
  select(-Statistikdatum) %>% 
  group_by(adm1_name) %>% 
  arrange(date) %>% 
  mutate(cum_confirmed_cases = cumsum(case_count))

uk <-
  map(uk_region_files, read_csv) %>% 
  bind_rows() %>% 
  mutate(
    adm1_name = areaName,
    cum_confirmed_cases = cumCasesBySpecimenDate
  ) 

germany <- 
  read_csv(path_germany) %>% 
  group_by(Bundesland, Refdatum) %>% 
  summarize(case_count = sum(AnzahlFall, na.rm = TRUE)) %>% 
  mutate(
    adm1_name = Bundesland,
    date = Refdatum %>% str_replace("\\s.*", "") %>% ymd()
  ) %>% 
  ungroup() %>% 
  group_by(adm1_name) %>% 
  mutate(cum_confirmed_cases = cumsum(case_count))

spain <- 
  read_csv(path_spain) %>% 
  mutate(
    date = fecha,
    cum_confirmed_cases = total,
    adm1_name = CCAA
  )

netherlands <-
  read_csv(path_netherlands) %>% 
  mutate(
    date = Datum,
    adm1_name = Provincienaam
  ) %>% 
  arrange(adm1_name, date) %>% 
  group_by(adm1_name) %>% 
  mutate(cum_confirmed_cases = cumsum(Aantal))

# Policies (from Oxford dataset)

# Extract policies by country
get_country_policies <- function(file_name, country) {
  path_policies <- path_policies
  str_c(path_policies, file_name, ".csv") %>% 
    read_csv() %>% 
    filter(X1 %in% country) %>% 
    pivot_longer(
      cols = -c(X1, X2), 
      names_to = "date", 
      values_to = file_name
    ) %>% 
    mutate(
      date = dmy(date)
    ) %>% 
    arrange(date) %>% 
    select(-X1, -X2) %>%
    {if(file_name != path_policy_files[1]) select(., -date) else .} %>% 
    write_csv(str_c(path_temp, file_name, "_", country,".csv"))
}

walk2(path_policy_files, "Sweden", get_country_policies)
walk2(path_policy_files, "United Kingdom", get_country_policies)
walk2(path_policy_files, "Germany", get_country_policies)
walk2(path_policy_files, "Spain", get_country_policies)
walk2(path_policy_files, "Netherlands", get_country_policies)

# Combine policies
combine_policies <- function(country, adm0) {
  map(str_c(path_temp, path_policy_files, "_", country, ".csv"), read_csv) %>% 
    bind_cols() %>% 
    as_tibble() %>% 
    write_csv(str_c(path_processed, "policies_", adm0, ".csv"))
  }
country_list_add <- c("Sweden", "United Kingdom", "Germany", "Spain", "Netherlands")
adm0_list_add <- c("SWE", "UK", "GER", "ESP", "NL")

walk2(country_list_add, adm0_list_add, combine_policies)

# Remove intermediary policy files
# map(path_policy_files, ~dir(path = path_temp, pattern = .)) %>% 
#   flatten_chr() %>% 
#   file.remove()

# Add policies to case counts
sweden %>% 
  left_join(read_csv(str_c(path_processed, "policies_SWE.csv")), by = "date") %>% 
  mutate(
    school_closure = (as.numeric(c1_schoolclosing) > 0) %>% as.numeric(),
    work_from_home = (as.numeric(c2_workplaceclosing) > 0) %>% as.numeric() ,
    no_public_gathering = (as.numeric(c3_cancelpublicevents) > 0) %>% as.numeric(),
    no_private_gathering = (as.numeric(c4_restrictionsongatherings) > 0) %>% as.numeric(),
    travel_ban_local = (as.numeric(c7_domestictravel) > 0) %>% as.numeric(),
    travel_ban_intl_in = (as.numeric(c8_internationaltravel) > 0) %>% as.numeric()
   ) %>%
  write_csv(str_c(path_processed, "SWE_processed.csv"))

uk %>%
  left_join(read_csv(str_c(path_processed, "policies_UK.csv")), by = "date") %>% 
  mutate(
    school_closure = (as.numeric(c1_schoolclosing) > 0) %>% as.numeric(),
    work_from_home = (as.numeric(c2_workplaceclosing) > 0) %>% as.numeric() ,
    no_public_gathering = (as.numeric(c3_cancelpublicevents) > 0) %>% as.numeric(),
    no_private_gathering = (as.numeric(c4_restrictionsongatherings) > 0) %>% as.numeric(),
    transit_suspension = (as.numeric(c5_closepublictransport) > 0) %>% as.numeric(),
    home_isolation = (as.numeric(c6_stayathomerequirements) > 0) %>% as.numeric(),
    travel_ban_local = (as.numeric(c7_domestictravel) > 0) %>% as.numeric(),
  ) %>% 
  write_csv(str_c(path_processed, "UK_processed.csv"))

germany %>% 
  left_join(read_csv(str_c(path_processed, "policies_GER.csv")), by = "date") %>% 
  mutate(
    school_closure = (as.numeric(c1_schoolclosing) > 0) %>% as.numeric(),
    work_from_home = (as.numeric(c2_workplaceclosing) > 0) %>% as.numeric() ,
    no_public_gathering = (as.numeric(c3_cancelpublicevents) > 0) %>% as.numeric(),
    no_private_gathering = (as.numeric(c4_restrictionsongatherings) > 0) %>% as.numeric(),
    home_isolation = (as.numeric(c6_stayathomerequirements) > 0) %>% as.numeric(),
    travel_ban_local = (as.numeric(c7_domestictravel) > 0) %>% as.numeric(),
    travel_ban_intl = (as.numeric(c8_internationaltravel) > 0) %>% as.numeric(),
  ) %>% 
  write_csv(str_c(path_processed, "GER_processed.csv"))

spain %>% 
  left_join(read_csv(str_c(path_processed, "policies_ESP.csv")), by = "date") %>% 
  mutate(
    school_closure = (as.numeric(c1_schoolclosing) > 0) %>% as.numeric(),
    work_from_home = (as.numeric(c2_workplaceclosing) > 0) %>% as.numeric() ,
    no_public_gathering = (as.numeric(c3_cancelpublicevents) > 0) %>% as.numeric(),
    no_private_gathering = (as.numeric(c4_restrictionsongatherings) > 0) %>% as.numeric(),
    transit_suspension = (as.numeric(c5_closepublictransport) > 0) %>% as.numeric(),
    home_isolation = (as.numeric(c6_stayathomerequirements) > 0) %>% as.numeric(),
    travel_ban_local = (as.numeric(c7_domestictravel) > 0) %>% as.numeric(),
    travel_ban_intl = (as.numeric(c8_internationaltravel) > 0) %>% as.numeric(),
  ) %>% 
  write_csv(str_c(path_processed, "ESP_processed.csv"))

netherlands %>% 
  left_join(read_csv(str_c(path_processed, "policies_NL.csv")), by = "date") %>% 
  mutate(
    school_closure = (as.numeric(c1_schoolclosing) > 0) %>% as.numeric(),
    work_from_home = (as.numeric(c2_workplaceclosing) > 0) %>% as.numeric() ,
    no_public_gathering = (as.numeric(c3_cancelpublicevents) > 0) %>% as.numeric(),
    no_private_gathering = (as.numeric(c4_restrictionsongatherings) > 0) %>% as.numeric(),
    home_isolation = (as.numeric(c6_stayathomerequirements) > 0) %>% as.numeric(),
    travel_ban_local = (as.numeric(c7_domestictravel) > 0) %>% as.numeric(),
    travel_ban_intl = (as.numeric(c8_internationaltravel) > 0) %>% as.numeric(),
  ) %>% 
  write_csv(str_c(path_processed, "NL_processed.csv"))
