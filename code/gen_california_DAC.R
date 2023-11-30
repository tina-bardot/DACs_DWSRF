################################################################################
# Generating DACs using California data 
# National Center for Environmental Economics
# Last edited: 11/27/2023
################################################################################

################################################################################
## Load packages: 
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  cdTools, # for retreiving FIPS codes
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  modelsummary, # regression table generation
  stringr, # string manipulation
  MASS , #For regressions and modeling
  dplyr, #data manipulation
  readxl, #read excel files
  janitor #cleaning names
)

################################################################################
##Set directories
################################################################################

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

getwd()
setwd(paste0(my_path))
getwd()

################################################################################
## Defining DACs in California
################################################################################

# Income
# Population served
# Number of service connections

## Need to get CBG level information for California PWSs
## Need to calculate statewide MHI to compare to CBG MHI

## Load PWSID dataset : epic boundaries data

pwsid_cbg <- read.csv("Data/epic_boundaries/sb_dems_area_v3.csv") %>%
                  clean_names() %>%
                  dplyr::select(-contains("p_"), -starts_with("pred")) %>%
  dplyr::select(2:4, 38:45) %>%
  filter(st_abbrev == "CA")


## Load census data to get MHI

# Census Access Options and API code 
library(tidycensus)
options(tigris_use_cache = TRUE)
census_api_key("b198dc46b551b388307baa5e9a47e32aac0c7842",  overwrite = TRUE, install = TRUE)
# First time, reload your environment so you can use the key without restarting R.
# readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("CENSUS_API_KEY")

# You can check variables to ensure that they match 

#varlist <- load_variables(2021, "acs5", cache = TRUE) %>%
#filter(geography == "block group") #limited to block group level since that will be the unit of analysis
#View(varlist)


census_vars <- c(
  "pop" = "B01003_001",
  "white" ="B02008_001", #WHITE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES
  "black" ="B02009_001", #BLACK OR AFRICAN AMERICAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES
  "native" ="B02010_001", #AMERICAN INDIAN AND ALASKA NATIVE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES
  "asian" ="B02011_001", #ASIAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES
  "pi" = "B02012_001",  #NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES
  "hispanic" ="B03002_012", #Total HISPANIC OR LATINO ORIGIN BY RACE
  "median_income" = "B19113_001", #MEDIAN FAMILY INCOME IN THE PAST 12 MONTHS (IN 2021 INFLATION-ADJUSTED DOLLARS)
  "home_value" ="B25075_001") #Estimate!!Total: VALUE

# Grab 2017-2021 5-year American Community Service data for CA

census <- get_acs(
  geography = "block group",
  variables = census_vars,
  state = "CA", # California
  year = 2021,
  output = "wide",
  geometry = TRUE, # Set geometry to be able to map
  survey = "acs5") %>%
  dplyr::select(-NAME, -ends_with("M")) %>%
  rename_with(., ~gsub("E", "", .), ends_with("E")) %>%
  mutate(GEOID = as.double(GEOID))

## Combine our PWSID info with census data info

ca_pws_cbg <- left_join(pwsid_cbg, census, by = join_by("id" == "GEOID"), relationship = "many-to-many")

# Save to save compute time in the future

saveRDS(ca_pws_cbg, file = "DACs/Data/ca_pws_cbg.rds")

ca_pws_cbg <- readRDS("DACs/Data/ca_pws_cbg.rds")

## Calculate state median income

ca_DAC_cbg <- ca_pws_cbg %>%
  mutate(service_connections_count = as.numeric(service_connections_count)) %>%
  mutate(population_served_count = as.numeric(population_served_count)) %>%
  mutate(state_mhi = mean(median_income, na.rm = TRUE)) %>% # mean of the CBG-level median income 
  mutate(st_80_pct_mhi = quantile(median_income, probs = .8, na.rm = TRUE)) %>% # 80th percentile of state mhi
  mutate(st_60_pct_mhi = quantile(median_income, probs = .6, na.rm = TRUE)) %>% # 60th pctl of state mhi
  mutate(income_DAC = if_else(median_income < st_80_pct_mhi, TRUE, FALSE)) %>% # standard DAC definition 
  mutate(exp_sm_DAC = case_when( # Calculate expanded small DAC
    service_connections_count > 3300 & service_connections_count <= 6600 & income_DAC == TRUE ~ TRUE, 
      population_served_count > 10000 & population_served_count <= 20000 & income_DAC == TRUE ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  mutate(med_DAC = case_when( # calculate Medium DAC
    service_connections_count > 6600 & service_connections_count <= 30000 & income_DAC == TRUE ~ TRUE, 
    population_served_count > 20000 & population_served_count <= 100000 & income_DAC == TRUE ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(small_DAC = case_when( # Calculate Small DAC
    service_connections_count <= 3300 & income_DAC == TRUE ~ TRUE, 
    population_served_count <= 10000 & income_DAC == TRUE ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(small_non_DAC = case_when( # Identify small non-DAC communities
    service_connections_count <= 3300 & income_DAC == FALSE ~ TRUE, 
    population_served_count <= 10000 & income_DAC == FALSE ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(small_DAC_sev = case_when( # Identify small severely DAC
    service_connections_count <= 3300 & median_income < st_60_pct_mhi ~ TRUE, 
    population_served_count <= 10000 & median_income < st_60_pct_mhi ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(DAC_type = case_when(
    exp_sm_DAC == TRUE ~ "exp_sm_DAC",
    small_DAC == TRUE ~ "small_DAC",
    med_DAC == TRUE ~ "med_DAC",
    small_DAC_sev == TRUE ~ "small_DAC_sev",
    small_non_DAC == TRUE ~ "small_non_DAC",
    TRUE ~ "non_DAC"
  ))

saveRDS(ca_DAC_cbg, file = "DACs/Data/ca_DAC_cbg.rds")

ca_DAC_cbg <- readRDS("DACs/Data/ca_DAC_cbg.rds")
