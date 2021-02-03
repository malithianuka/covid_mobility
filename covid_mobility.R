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
  rename(GDP2018 = `Gross domestic product per capita, constant prices`) %>% 
  select(-country)
# units = GDP per capita (2017 constant dollars, PPP); Population (millions)

country_codes = read_csv("./data/country_codes.csv") %>% clean_names() %>% select(-country)

#### merge data ####

full_set = left_join(mob_data, country_codes, by = c("alpha_2_code")) %>% 
  left_join(weo_data, by = c("alpha_3_code" = "iso"))
# # checks
# test %>% filter(is.na(date))
# test3 = test %>% group_by(alpha_2_code) %>% 
#   summarise(unique_country = n_distinct(country)) %>% 
#   filter(unique_country != 1)

full_set %>% filter(is.na(metro_area)) %>% select(country) %>% unique()

test1 = full_set %>% filter(!is.na(sub_region_1)) %>% select(sub_region_1, country) %>% unique()
test2 =full_set %>% filter(!is.na(sub_region_2)) %>% select(sub_region_2, country) %>% unique()
test3 = full_set %>% filter(!is.na(metro_area)) %>% select(metro_area, country) %>% unique()
test4 =full_set %>% filter(!is.na(census_fips_code )) %>% select(census_fips_code , country) %>% unique()

metro_set = full_set %>% select(metro_area, date:GDP2018) %>% 
  filter(!is.na(metro_area)) %>% 
  mutate(metro_area = gsub(" Metropolitan Area", "", metro_area)) %>% 
  pivot_longer(retail_and_recreation:residential, names_to = "destination", values_to = "percent_change")


country_set = full_set %>% 
  filter(is.na(sub_region_1) & is.na(sub_region_2)& is.na(metro_area)& is.na(iso_3166_2_code)& is.na(census_fips_code)) %>% 
  select(country, date:GDP2018) %>% 
  # mutate(metro_area = gsub(" Metropolitan Area", "", metro_area)) %>% 
  pivot_longer(retail_and_recreation:residential, names_to = "destination", values_to = "percent_change")

country_set$country %>% unique() %>% length()

full_set %>% select(iso_3166_2_code) %>% unique()
# look at metro area by GDP to choose subset
metro_set %>% select(metro_area, GDP2018) %>% 
  unique() %>% 
  arrange(desc(GDP2018))

#### visualse ####

ggplot(data = metro_set %>% filter(metro_area=="Doha Metropolitan Area"), 
       mapping = aes(x = date, y = percent_change, colour = destination )) + 
  geom_line() +
  facet_wrap(vars(destination))

ggplot(data = metro_set, 
       mapping = aes(x = date, y = percent_change, colour = destination )) + 
  geom_line() +
  facet_wrap(vars(metro_area))

ggplot(data = country_set, 
       mapping = aes(x = date, y = percent_change, colour = destination )) + 
  geom_line() +
  facet_wrap(vars(country))

names(full_set)
compare_df_cols(full_set$country.x, full_set$country.y)
test2 = test %>% select(starts_with("alpha"), country) %>% unique()
test2$country %>% unique() %>% length()
weo_data %>% filter(!complete.cases(.))

weo_data%>% 
  mutate_at(select(starts_with("x")),as.numeric(gsub(",", "", y)))
names(mob_data)
mob_data %>% filter(!is.na(metro_area))
