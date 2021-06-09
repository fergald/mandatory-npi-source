# Libraries used
library(tidyverse)
library(lubridate)
library(ggrepel)

getwd() %>% 
  dirname() %>% 
  dirname() %>% 
  setwd()

# List of countries
country_list <- c("FRA", "GER", "IRN", "ITA", "NL", "ESP", "UK", "USA", "KOR", "SWE")

# Collect coefficients datasets for each country
collect_coefs <- function(country) {
  read_csv(str_c("./output/regression/", country, "_forest_coefs_1.csv")) %>% 
    bind_rows(
      read_csv(str_c("./output/regression/", country, "_forest_coefs_2.csv"))
    ) %>% 
    write_csv(str_c("./output/figures/", country, "_forest_coefs.csv"))
}

country_list[!country_list %in% c("KOR", "SWE")] %>% 
  walk(collect_coefs)

# Collect all the regression coefficients
all_coefs <-
  map(str_c("./output/figures/", country_list, "_forest_coefs.csv"), read_csv) %>%
  bind_rows() %>% 
  as_tibble() %>% 
  mutate(
    ub = beta + 1.96 * se,
    lb = beta - 1.96 * se,
    effect_size = str_c(round(beta, 2), " (", round(lb, 2), ", ", round(ub, 2),")"),
    growth = round((exp(beta) - 1) * 100, 2),
    country = 
      recode(
        adm0, 
        "KOR" = "South Korea",
        "SWE" = "Sweden", 
        "FRA" = "France", 
        "IRN" = "Iran", 
        "ITA" = "Italy",
        "GER" = "Germany",
        "UK" = "England",
        "NL" = "Netherlands",
        "ESP" = "Spain"
      ),
    country = as.factor(country) %>% fct_relevel("South Korea", "Sweden", after = Inf)
  ) 


# No policies figure
no_policy_plot <-
  all_coefs %>% 
  filter(str_detect(policy, "no_policy_rate")) %>% 
  mutate(country = country %>% fct_rev()) %>% 
  ggplot(
    aes(
      x = beta,
      xmin = lb,
      xmax = ub,
      y = country
    )
  ) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  geom_vline(
    aes(xintercept = mean(beta) %>% round(2)), 
    color = "black", 
    linetype = "dotted"
  ) +
  # geom_segment(
  #   data = one_zero_coded %>% filter(adm0 == "SWE"), 
  #   aes(y = 4.5, yend = 4.5, x = -.5, xend = .8),
  #   size = .5,
  #   linetype = "dotted",
  #   inherit.aes = FALSE
  # ) +
  geom_text(
    aes(y = country, x = .83, label = effect_size), 
    hjust = 1, vjust = 0.5, size = 2,
    inherit.aes = FALSE
  ) +
  geom_text_repel(
    data = 
      all_coefs %>% 
      filter(str_detect(policy, "no_policy_rate")) %>% 
      mutate(country = country %>% fct_rev()) %>% 
      summarize(
        avg = mean(beta) %>% round(2)
      ),
    aes(
      x = avg, 
      y = 10.5,
      label = str_c("Average (10 countries): ", avg)
    ),
    nudge_x = .15,
    segment.size = .3,
    min.segment.length = 0,
    arrow = arrow(length = unit(0.07, "inches"), type = "closed"),
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(
    #title = "Case Gowth Rate in Absence of Policy",
    x = "Estimated daily growth rate",
    y = ""
  ) +
  coord_cartesian(
    xlim = c(0, .9),
    clip = "off"
  ) +
  scale_x_continuous(breaks = c(0, .2, .4, .6)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 6, hjust = .35),
    axis.text.x = element_text(size = 6)
  )

# Get daily growth rates by country
get_d_l_cases <- function(country) {
  read_csv(str_c("./output/regression/", country, "_reg_data_10.csv")) %>% 
    #{if(country == "KOR") mutate(., D_l_cum_confirmed_cases = D_l_active_cases) else .} %>% 
    #{if(country %in% c("SWE", "GER", "UK", "NL", "ESP")) filter(., cum_confirmed_cases > 10) else .} %>% 
    group_by(date) %>% 
    summarize(cum_confirmed_cases = sum(cum_confirmed_cases, na.rm = TRUE)) %>% 
    filter(cum_confirmed_cases > 0) %>% 
    mutate(
      daily_cases = cum_confirmed_cases - lag(cum_confirmed_cases),
      l_cum_confirmed_cases = log(daily_cases),
      lag_1 = lag(l_cum_confirmed_cases),
      #d_l_cum_confirmed_cases = diff(l_cum_confirmed_cases)
      !!str_c(country, "_d_l_cum_cases") := l_cum_confirmed_cases - lag_1
    ) %>%  
    drop_na(lag_1) %>% 
    mutate(!!str_c(country, "_date_index") := row_number()) %>%
    select(date, !!str_c(country, "_date_index"), !!str_c(country, "_d_l_cum_cases")) %>% 
    write_csv(str_c("./output/figures/", country, "_confirmed_cases.csv"))
}

country_list <- c("KOR", "GER", "ESP", "NL", "UK", "SWE", "FRA", "IRN", "ITA", "USA")

country_list %>% 
  walk(get_d_l_cases)

c(map(str_c("./output/figures/", country_list, "_confirmed_cases.csv"), read_csv)) %>%
  reduce(left_join, by = "date") %>% 
  write_csv("./output/figures/combined_daily_growth.csv")

combined_daily_growth <- read_csv("./output/figures/combined_daily_growth.csv")

growth_plot <-
  combined_daily_growth %>% 
  pivot_longer(
    ends_with("cum_cases"),
    names_to = "adm0",
    names_pattern = "(.{3})",
    values_to = "d_l_cum_cases"
  ) %>% 
  mutate(
    country = 
      recode(
        adm0, 
        "KOR" = "South Korea",
        "SWE" = "Sweden", 
        "FRA" = "France", 
        "IRN" = "Iran", 
        "ITA" = "Italy",
        "GER" = "Germany",
        "NL_" = "Nethrlands",
        "ESP" = "Spain",
        "UK_" = "England"
      ),
    country = as.factor(country) %>% fct_relevel("South Korea", "Sweden", after = Inf)
  ) %>% 
  ggplot(
    aes(x = date, y = d_l_cum_cases)
  ) +
  geom_point(size = .8) +
  geom_smooth(method = "loess", formula = "y ~ x", se = FALSE, size = .7) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(position = "right") +
  facet_grid(country ~ .) +
  coord_cartesian(ylim = c(0, 1.0)) +
  labs(
    y = expression(paste("Growth rate of active cases (", Delta, "log per day)"))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 6, hjust = .41),
    strip.text.y = element_blank(),
    axis.title.y = element_text(size = 6)
  )

# Side by side
figure_1 <- gridExtra::grid.arrange(no_policy_plot, growth_plot, ncol = 2)
ggsave("./output/figures/figure_1.png", figure_1)

# Individual NPIs
main_plot_npi <-
  all_coefs %>% 
  filter(
    !str_detect(policy, "no_policy_rate"),
    !policy %>% str_detect("orig") | policy %>% str_detect("comparator") | adm0 %in% c("SWE")
  ) %>% 
  mutate(
    policy = str_replace(policy, "combined policy", "All policies combined"),
    policy = str_replace(policy, "SWE comparator", "Sweden comparator"),
    policy = str_replace(policy, "KOR comparator", "South Korea comparator")
  ) 

individual_npi <- 
  main_plot_npi %>% 
  filter(!policy %in% c("Sweden comparator", "South Korea comparator", "All policies combined")) %>% 
  mutate(
    index = as.factor(-row_number())
  )

figure_2 <-
  individual_npi %>% 
  ggplot(
    aes(
      x = beta,
      xmin = lb,
      xmax = ub,
      y = index,
      color = ((ub < 0 & lb < 0) | (lb > 0 & ub > 0)) 
    )
  ) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  geom_vline(xintercept = 0, color = "black") +
  geom_text(
    aes(y = index, x = .6, label = effect_size), 
    hjust = 1, vjust = 0.5, size = 1.5,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = individual_npi %>% filter(adm0 == "USA"), 
    aes(y = -.5, yend = -.5, x = -1.3, xend = 1.3),
    size = .3,
    linetype = "dotted",
    inherit.aes = FALSE
  ) +
  facet_grid(country ~ ., scales = "free", space = "free", drop = TRUE) +
  scale_color_discrete(name = "", labels = c("not significant", "significant")) +
  scale_y_discrete(breaks = individual_npi$index, labels = individual_npi$policy) +
  labs(
    x = "Estimated effect on daily growth rate",
    y = ""
  ) +
  coord_cartesian(xlim = c(-.4, .7), clip = "off")+
  scale_x_continuous(breaks = c(-.4, -.2, 0, .2, .4)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 9),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 6, hjust = .33),
    strip.text.y = element_text(angle = 0, size = 6, hjust = 0),
    legend.direction = "horizontal",
    legend.text = element_text(size = 6),
    legend.position = c(.35, -.08),
    plot.margin = unit(c(.5, .5, 1, 0), "cm")
  )

ggsave(file = "./output/figures/figure_2.png", figure_2)

# All policies combined
figure_3 <-
  main_plot_npi %>% 
  filter(
    policy %>% str_detect("All policies")
  ) %>% 
  mutate(country = country %>% fct_rev()) %>% 
  ggplot(
    aes(
      x = beta,
      xmin = lb,
      xmax = ub,
      y = country
    )
  ) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  geom_vline(
    aes(xintercept = 0),
    color = "black"
  ) +
  geom_text(
    aes(y = country, x = .4, label = effect_size), 
    hjust = 1, vjust = 0.5, size = 2,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Estimated effect on daily growth rate",
    y = ""
  ) +
  scale_x_continuous(breaks = c(-.6, -.4, -.2, 0, .2)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 6, hjust = .35),
    axis.text.x = element_text(size = 6)
  )

ggsave("./output/figures/figure_3.png", figure_3)

# Comparisons with South Korea and Sweden
figure_4 <-
  main_plot_npi %>% 
  filter(
    policy %>% str_detect("comparator")
  ) %>% 
  mutate(
    country = country %>% fct_rev(),
    policy = policy %>% str_replace(" comparator", "")
  ) %>% 
  ggplot(
    aes(
      x = beta,
      xmin = lb,
      xmax = ub,
      y = country,
      color = policy
    )
  ) +
  geom_point(position = position_dodge(width = 0.3)) + 
  geom_errorbarh(
    height = .1,
    position = position_dodge(width = 0.3)
  ) +
  geom_vline(
    aes(xintercept = 0),
    color = "black"
  ) +
  geom_text(
      aes(
      x = .72,
      y = country,
      group = policy,
      label = effect_size
    ), 
    hjust = 1, vjust = 0.5, size = 2,
    position = position_dodge(width = .3),
    inherit.aes = FALSE
  ) +
  labs(
    x = "Estimated effect on daily growth rate",
    y = "",
    color = "Comparison country"
  ) +
  scale_color_brewer(type = "qual") +
  scale_x_continuous(breaks = c(-.6, -.4, -.2, 0, .2, .4, .6)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 9),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 6, hjust = .38),
    legend.direction = "horizontal",
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.position = c(.4, -.08),
    plot.margin = unit(c(.5, .5, 1, 0), "cm")
  )

ggsave("./output/figures/figure_4.png", figure_4)


# Comparisons between different codings for the 6 countries
comparison <-
  all_coefs %>% 
  filter(
    !str_detect(policy, "no_policy"),
    !adm0 %in% c("SWE", "GER", "ESP", "UK", "NL"),
    !str_detect(policy, "comparator")
  ) %>% 
  separate(policy, into = c("policy", "encoding"), sep = " - ", extra = "merge", fill = "right") %>%
  replace_na(list(encoding = "1-0 coded")) %>% 
  mutate(
    policy = str_replace(policy, "combined policy", "All policies combined"),
    encoding = 
      recode(
        encoding, 
        "orig" = "original"
      ) %>% 
      fct_rev(),
    index = -row_number(),
    policy = fct_reorder(policy, index) %>% relevel("All policies combined")
  )

figure_5 <-
  comparison %>% 
  ggplot(
    aes(
      x = beta,
      xmin = lb,
      xmax = ub,
      y = policy,
      color = (ub < 0 & lb< 0) | (ub > 0 & lb > 0)
    )
  ) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  geom_vline(xintercept = 0, color = "black") +
  facet_grid(country ~ encoding, scales = "free", space = "free", drop = TRUE) +
  scale_y_discrete(
    breaks = levels(comparison$policy),
    labels = c(expression(bold("All policies combined")),
               levels(comparison$policy)[2:18])
  ) +
  scale_color_discrete(name = "", labels = c("not significant", "significant")) +
  labs(
    x = "Estimated effect on daily growth rate",
    y = ""
  ) +
  coord_cartesian(xlim = c(-.62, 1))+
  scale_x_continuous(breaks = c(-.4, -.2, 0, .2, .40)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 6, hjust = .41),
    strip.text.x = element_text(angle = 0, size = 6, hjust = 0.37),
    strip.text.y = element_text(angle = 0, size = 6, hjust = 0),
    legend.direction = "horizontal",
    legend.text = element_text(size = 6),
    legend.position = c(.425, -.08),
    plot.margin = unit(c(.5, .5 , 1, 0), "cm")
  )
ggsave("./output/figures/figure_5.png", figure_5)
