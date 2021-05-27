# Library used
library(tidyverse)

setwd(str_c(dirname(getwd()), "/output/regression/"))

# Combine a reference country (SWE, KOR) with another country 
combine_pairs <- function(ref_country, country_2) {
  read_csv(str_c(ref_country, "_reg_data_10.csv")) %>% 
    mutate(country = ref_country) %>% 
    {if(ref_country == "KOR") mutate(., D_l_cum_confirmed_cases = D_l_active_cases) else .} %>% 
    bind_rows(
      read_csv(str_c(country_2, "_reg_data_10.csv")) %>% 
        mutate(country = country_2) %>% 
        {if(country_2 == "ITA") mutate(., adm1_id = adm2_id) else .} 
      #!!str_to_lower(ref_country)
    ) %>% 
    mutate(across(starts_with("p_"), ~ifelse(is.na(.x), 0, .x))) %>% 
    mutate(across(starts_with("testing_change"), ~ifelse(is.na(.x), 0, .x))) %>% 
    write_csv(str_c(ref_country, "_", country_2, "_reg_data_10.csv"))
}

# With South Korea
combine_pairs("KOR", "ITA")
combine_pairs("KOR", "IRN")
combine_pairs("KOR", "USA")
combine_pairs("KOR", "FRA")
combine_pairs("KOR", "UK")
combine_pairs("KOR", "GER")
combine_pairs("KOR", "ESP")
combine_pairs("KOR", "NL")
# With Sweden
combine_pairs("SWE", "ITA")
combine_pairs("SWE", "IRN")
combine_pairs("SWE", "USA")
combine_pairs("SWE", "FRA")
combine_pairs("SWE", "UK")
combine_pairs("SWE", "GER")
combine_pairs("SWE", "ESP")
combine_pairs("SWE", "NL")
