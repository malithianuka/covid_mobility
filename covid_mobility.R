# Exploring Google Mobility Data
# By: Malithi 
# Date started: 2021-02-02

# load packages
library(tidyverse)
library(janitor)
library(COVID19)


#### read data ####

# # ended up not using Google Mobilty Reports from here because had already cleaned a country dataset
# gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
# wb = c("gdp"= "NY.GDP.MKTP.PP.CD")
# all = covid19((wb = wb, gmr = gmr))
# # based on the following test, I can see that this data set includes double entries for GMR that have country and metro area numbers separately. 
# test = all %>% filter(date=="2020-04-15", iso_alpha_3=="AFG")

all = covid19() # covid stats and policy responses


#### read data ####

gmr_data = read_csv("./data/global_mobility_report.csv",col_types = "cccccccDdddddd" ) %>% 
  rename(alpha_2_code = country_region_code, country = country_region)
names(gmr_data) = names(gmr_data) %>% gsub("_percent_change_from_baseline", "", .)
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

#### clean data ####

full_gmr = left_join(gmr_data, country_codes, by = c("alpha_2_code")) %>% 
  left_join(weo_data, by = c("alpha_3_code" = "iso"))
# # checks
# test %>% filter(is.na(date))
# test3 = test %>% group_by(alpha_2_code) %>% 
#   summarise(unique_country = n_distinct(country)) %>% 
#   filter(unique_country != 1)

full_gmr %>% filter(is.na(metro_area)) %>% select(country) %>% unique()

test1 = full_gmr %>% filter(!is.na(sub_region_1)) %>% select(sub_region_1, country) %>% unique()
test2 =full_gmr %>% filter(!is.na(sub_region_2)) %>% select(sub_region_2, country) %>% unique()
test3 = full_gmr %>% filter(!is.na(metro_area)) %>% select(metro_area, country) %>% unique()
test4 =full_gmr %>% filter(!is.na(census_fips_code )) %>% select(census_fips_code , country) %>% unique()

# metro areas are only in a handful of countries. 
metro_gmr = full_gmr %>% select(metro_area, date:GDP2018) %>% 
  filter(!is.na(metro_area)) %>% 
  mutate(metro_area = gsub(" Metropolitan Area", "", metro_area)) %>% 
  pivot_longer(retail_and_recreation:residential, names_to = "destination", values_to = "percent_change")

# opt for data by country
country_gmr = full_gmr %>% 
  filter(is.na(sub_region_1) & is.na(sub_region_2)& is.na(metro_area)& is.na(iso_3166_2_code)& is.na(census_fips_code)) %>% #country wide data has NAs for other regions
  select(country, date:GDP2018) %>% 
  # mutate(metro_area = gsub(" Metropolitan Area", "", metro_area)) %>% 
  pivot_longer(retail_and_recreation:residential, names_to = "destination", values_to = "percent_change") %>% 
  select(iso_alpha_3 = alpha_3_code, country, lat = latitude_average, long = longitude_average, GDP2018, date, destination, perc_change = percent_change)


#### combine with covid and policy data ####

# interested in policy measures
# Covid stats
# and how they relate to changes in mobility

# limit observations fo Feb 15 2020 to Jan 29 2021 (where mobility data is available)
gmr_start = country_gmr$date %>% min()
gmr_end = country_gmr$date %>% max()

all_covid = all %>% ungroup() %>% 
  select(iso_alpha_3, country = administrative_area_level_1, date:stringency_index) %>% 
  filter(date >= gmr_start & date <= gmr_end)

covid_stats = all_covid %>% 
  select(iso_alpha_3:population) %>% 
  pivot_longer(vaccines:icu, names_to = "covid_stats", values_to = "status") 

policy_data = all_covid %>% 
  select(iso_alpha_3:date, school_closing:stringency_index) %>% 
  pivot_longer(school_closing:stringency_index, names_to = "policy", values_to = "implementation") %>% 
  filter(policy != "stringency_index")

string_index = policy_data %>% filter(policy == "stringency_index") %>% 
  select(-policy, string_index = implementation)

#### visualse ####


ggplot() +
  geom_line(data = country_gmr %>% filter(country == "Canada"),
            mapping = aes(x=date, y=perc_change, colour = destination)) +
  # scale_color_viridis_d() +
  geom_bar(data = string_index %>% filter(country == "Canada"),
            mapping = aes(x = date, y = string_index), stat = "identity") +
  geom_line(policy_data %>% filter(country =="Canada"), 
            mapping = aes(x=date, y = implementation*100, linetype = policy))+
  scale_y_continuous("Percent Change", sec.axis = sec_axis(~./100, name = "policy") )
  # scale_colour_discrete("spectral")

ggplot(data = metro_set %>% filter(metro_area=="Doha Metropolitan Area"), 
       mapping = aes(x = date, y = percent_change, colour = destination,  )) + 
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

ggplot(data = all %>% filter(iso_alpha_3="AFG"), 
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
