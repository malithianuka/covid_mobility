# Exploring Google Mobility Data
# By: Malithi 
# Date started: 2021-02-02

# load packages
library(tidyverse)
library(janitor)

#### read data ####

mob_data = read_csv("./data/global_mobility_report.csv",col_types = "cccccccDdddddd" ) %>% 
  rename(alpha_2_code = country_region_code, country = country_region)
names(mob_data) = names(mob_data) %>% gsub("_percent_change_from_baseline", "", .)
# units = change from baseline

weo_data = read_csv("./data/WEO_Data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(value=as.numeric(gsub(",", "", x2018))) %>% 
  select(-starts_with("x"), -starts_with("estimate")) %>% 
  filter(complete.cases(.)) %>%  #missing data for Somalia and Syria
  select(iso, country, subject_descriptor, value) %>% 
  pivot_wider(names_from = subject_descriptor,values_from = value ) %>% 
  rename(GDP2018 = `Gross domestic product per capita, constant prices`)
# units = GDP per capita (2017 constant dollars, PPP); Population (millions)

country_codes = read_csv("./data/country_codes.csv") %>% clean_names() %>% select(-country)

#### merge data ####

test = left_join(mob_data, country_codes, by = c("alpha_2_code")) 

test2 = test %>% select(starts_with("alpha"), country) %>% unique()

weo_data %>% filter(!complete.cases(.))

weo_data%>% 
  mutate_at(select(starts_with("x")),as.numeric(gsub(",", "", y)))
names(mob_data)
mob_data %>% filter(!is.na(metro_area))
