# Exploring Google Mobility Data
# By: Malithi 
# Date started: 2021-02-02

# load packages
library(tidyverse)

# read data

mob_data = read_csv("./data/global_mobility_report.csv",col_types = "cccccccDdddddd" )
weo_data = read_csv("./data/WEO_Data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(x2018=as.numeric(gsub(",", "", x2018)))
weo_data%>% 
  mutate_at(select(starts_with("x")),as.numeric(gsub(",", "", y)))
names(mob_data)
mob_data %>% filter(!is.na(metro_area))
