#### Democracy Promotion - Final Paper #####

# Creator: Finn Hagemann
# First version: 2022-05-20
# This version: 2022-05-27

# This script conducts an exploratory analysis of the key data sets that are
# proposed to be used in a study that examines the impact of arms trade on
# democracy promotion. 

# Data sets used are the Varieties of Democracy data set and the arms trade
# register by the Swedish Institute for Peace Research (SIPRI)

#### Packages & setup ####

#Prevent scientific notations
options(scipen=999)

#Load packages
library(tidyverse)
library(knitr)
library(kableExtra)
library(readxl)
library(countrycode)

#Working directory DELTE BEFORE PUBLISH
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/02 I Studium/4. Semester/DemocracyPromotion/Final Paper")

#Surpress dplyr info on summarize grouping
options(dplyr.summarise.inform = FALSE)

### Load data and match datasets ###

#Load vdem data
vdem <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/02 I Studium/4. Semester/DemocracyPromotion/Final Paper/V-Dem-CY-Core-v12.rds") %>% 
  select(country_name, country_text_id, year, v2x_polyarchy)

#Load SIPRI data, make it a long dataset, and wrangle variables for matching
sipri <- read_excel("TIV_SIPRI_Import.xlsx", 
                               na = " ") %>% 
  pivot_longer(cols = c("1950", "1951", "1952", "1953", "1954", "1955", "1956", "1957", "1958", "1959",
                        "1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969",
                        "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979",
                        "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989",
                        "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999",
                        "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                        "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                        "2020", "2021"),
                            names_to = "year",
                            values_to = "tiv") %>% 
  rename("country_name" = "...1") %>% 
  mutate(year = as.double(year)) %>% 
  select(-`...74`)

#Match datasets and add regions as categorized by World Bank
armsdem <- vdem %>%
  inner_join(sipri,
            by = c("year" = "year", "country_name" = "country_name")) %>% 
  mutate(region = countrycode(country_name, "country.name", "region"))
  
#### Descriptive Analysis ####

#Overview with key indicators
armsdem %>%
  group_by(region) %>% 
  mutate(mean_tiv = mean(tiv, na.rm = T),
         mean_polyarchy = mean(v2x_polyarchy, na.rm = T), 
         n_region = n()) %>% 
  ungroup() %>% 
  distinct(region, .keep_all = T) %>% 
  add_row(country_name = NA, 
          region = "Global", 
          mean_tiv = mean(.$mean_tiv, na.rm = T), 
          mean_polyarchy = mean(.$mean_polyarchy, na.rm = T), 
          n_region = sum(.$n_region)) %>% 
  select(-country_name, -country_text_id, -year, -v2x_polyarchy, -tiv) %>% 
  mutate(across(where(is.numeric), round, 2)) %>%
  kable(col.names = c("Region", "Mean TIV", "Mean elec. dem. score", "Observations")) %>%
  kable_styling() %>% 
  save_kable(file = "figures_tables/table_overview.html",
             bs_theme = "readable")

#Distinct countries in dataset
armsdem %>% 
  distinct(country_name, .keep_all = T) %>% 
  summarize(countries = n())

#Scores over time
armsdem %>% 
  group_by(year) %>% 
  summarize(mean_tiv = mean(tiv, na.rm = T),
            mean_poly = mean(v2x_polyarchy, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c("mean_tiv", "mean_poly"),
               names_to = "names",
               values_to = "values") %>% 
  mutate(names = str_replace(names, "mean_tiv","TIV score"),
         names = str_replace(names, "mean_poly","Electoral democracy score")) %>% 
  ggplot(aes(x = year, y = values, color = "region")) +
  geom_line() +
  scale_color_manual(name="", values=c("#ba0020")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_wrap(~names, scales = "free") +
  labs(x = "Years", 
       y = "") +
  guides(color = "none") +
  theme_bw()

ggsave("figures_tables/plot_scores.png",
       width = 2000,
       height = 1250,
       units = "px")

#Create scatter plot for changes in democracy scores with a lag of 5 years
armsdem %>% 
  drop_na(tiv, v2x_polyarchy) %>% 
  filter(region != "North America") %>% 
  mutate(v2x_polyarchy_change = lag(as.double(v2x_polyarchy), 5, order_by = year)) %>% 
  ggplot(aes(x = tiv, y = v2x_polyarchy_change)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(x = "TIV score",
       y = "Electoral democracy score") +
  facet_wrap(~region) +
  guides(fill = "none") + 
  theme_bw()

ggsave("figures_tables/plot_scatter.png",
       width = 2000,
       height = 1250,
       units = "px")
