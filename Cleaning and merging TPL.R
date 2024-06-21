library(dplyr)
library(tidyverse)
library(stringr)
library(readr)

setwd("/Users/etiennericardez/Downloads/TPL project")

data_main <- read.csv("Raw data/conservation_funding_votes.csv")

# Filtering for counties only
data_counties <- data_main %>% filter(jurisdiction_type == "County")

data_counties <- data_counties %>% mutate(Index = row_number())

###
### 1 and 2.- Merging population and income data
###

# Read the CSV file with readr::read_csv
data_income <- read_csv("Raw data/2.- CAINC1 - Income/CAINC1__ALL_AREAS_1969_2022.csv")

data_income <- data_income %>%
  mutate(across(everything(), ~ iconv(.x, from = "UTF-8", to = "UTF-8", sub = "byte")))

# Split the GeoName column
data_income <- data_income %>%
  mutate(GeoName = as.character(GeoName)) %>%
  separate(GeoName, into = c("juris_name", "State"), sep = ", ", fill = "right", extra = "merge", remove = FALSE)

data_income <- data_income %>%
  mutate(State = str_replace_all(State, "\\*", ""))

data_income <- data_income %>%
  mutate(State = if_else(juris_name == "Fairfax", "VA", State),
         juris_name = if_else(juris_name == "Clarke", "Athens-Clarke", juris_name),
         juris_name = if_else(juris_name == "Maui + Kalawao", "Maui", juris_name),
         juris_name = if_else(juris_name == "James City + Williamsburg", "James", juris_name),
         juris_name = if_else(juris_name == "Montgomery + Radford", "Montgomery", juris_name),
         State = if_else(juris_name == "Prince William", "VA", State))

data_income <- data_income %>% select(-TableName, -LineCode, -IndustryClassification, -Description)

# Selecting only data from 1988 onwards
data_income <- data_income[, -c(7:25)]

data_pop <- data_income %>% filter(Unit == "Number of persons")
data_income <- data_income %>% filter(Unit == "Thousands of dollars")

data_pop <- data_pop %>% select(-Unit)
data_income <- data_income %>% select(-Unit)

data_pop <- data_pop %>%
  pivot_longer(
    cols = starts_with(c("1", "2")),
    names_to = "Year",
    names_prefix = "X",
    values_to = "Pop"
  )

data_income <- data_income %>%
  pivot_longer(
    cols = starts_with(c("1", "2")),
    names_to = "Year",
    names_prefix = "X",
    values_to = "Income"
  )

# Convert 'year_of_vote' to integer in data_counties
data_counties <- data_counties %>%
  mutate(year_of_vote = as.integer(year_of_vote))

# Convert 'Year' to integer in data_income
data_income <- data_income %>%
  mutate(Year = as.integer(Year))

data_pop <- data_pop %>%
  mutate(Year = as.integer(Year))

data_counties <- data_counties %>%
  mutate(jurisdiction_name = str_replace(jurisdiction_name, "City and County of ", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " County", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Agricultural Preservation and Open Space District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Regional Park and Open Space Districts", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Regional Open Space District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Valley Open Space Authority", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Open Space Authority", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Open Space District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Peninsula Regional Park District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Regional Park and Open Space District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Regional Park and", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Valley Water District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Forest Preserve District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " City and", ""),
         jurisdiction_name = str_replace(jurisdiction_name, "Forest Preserve District of ", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Conservation District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Park and Recreation District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " and Franklin Metro Parks", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Historical Parks District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Preservation Park District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Park District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, "Great Parks of ", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Metroparks", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " MetroParks", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Metropolitan Parks District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, "MetroParks of ", ""),
         jurisdiction_name = str_replace(jurisdiction_name, "Park District of ", ""),
         jurisdiction_name = str_replace(jurisdiction_name, "Preservation Parks of ", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Metro Parks", ""),
         jurisdiction_name = str_replace(jurisdiction_name, "Metropolitan of ", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Flood Control District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " City", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Parish", ""),
         jurisdiction_name = str_replace(jurisdiction_name, "Astabula", "Ashtabula"),
         jurisdiction_name = str_replace(jurisdiction_name, " Columbus", "Columbiana"),
         jurisdiction_name = str_replace(jurisdiction_name, "Columbus", "Columbiana"),
         jurisdiction_name = str_replace(jurisdiction_name, "Five Rivers", "Montgomery"),
         jurisdiction_name = str_replace(jurisdiction_name, "Johnny Appleseed", "Noble"),
         jurisdiction_name = str_replace(jurisdiction_name, "the Toledo Area", "Lucas"),
         jurisdiction_name = str_replace(jurisdiction_name, "Toledo Area", "Lucas"))

# Ensure data_pop has unique combinations of mun_ID and year
data_income_unique <- data_income %>%
  distinct(juris_name, Year, State, .keep_all = TRUE)

# Perform the join
data_counties <- data_counties %>%
  left_join(data_income_unique, by = c("jurisdiction_name" = "juris_name",
                                       "year_of_vote" = "Year",
                                       "state_two_letter_code" = "State"))

data_counties$Region <- as.numeric(data_counties$Region)
data_counties$Income <- as.numeric(data_counties$Income)

# Manually change these records as the join was not working correctly
data_counties <- data_counties %>%
  mutate(
    GeoFIPS = if_else(Index == 437, "\"39095\"", GeoFIPS),
    GeoName = if_else(Index == 437, "Lucas, OH", GeoName),
    Region = if_else(Index == 437, 3, Region),
    Income = if_else(Index == 437, 16561187, Income),
    GeoFIPS = if_else(Index == 539, "\"51942\"", GeoFIPS),
    GeoName = if_else(Index == 539, "Prince William, Manassas + Manassas Park, VA*", GeoName),
    Region = if_else(Index == 539, 5, Region),
    Income = if_else(Index == 539, 29767221, Income)
  )

# Ensure data_pop has unique combinations of mun_ID and year
data_pop_unique <- data_pop %>%
  distinct(juris_name, Year, State, .keep_all = TRUE)

# Perform the join
data_counties <- data_counties %>%
  left_join(data_pop_unique, by = c("GeoFIPS" = "GeoFIPS",
                                    "year_of_vote" = "Year"))

data_counties <- data_counties %>%
  mutate(GeoFIPS = str_replace_all(GeoFIPS, "\"", ""))

###
### 3.- Merging weather data
###

### Getting the FIPS codes of every county
data_FIPS <- read_csv("Raw data/state_and_county_fips_master.csv")

data_FIPS <- data_FIPS %>%
  mutate(
    State_code = str_sub(fips, 1, nchar(fips) - 3),
    County_Code = str_sub(fips, -3, -1)
  )

state_data <- data.frame(
  State_Number = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                   11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                   21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
                   31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
                   41, 42, 43, 44, 45, 46, 47, 48, 50),
  State_Name = c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Idaho",
                 "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
                 "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
                 "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                 "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "Alaska"),
  State_Abbreviation = c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID",
                         "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI",
                         "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
                         "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                         "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "AK")
)

data_FIPS <- left_join(data_FIPS, state_data, by = c("state" = "State_Abbreviation"))

# Read precipitation data
data_precipitation <- read.table("Raw data/3.- Weather/climdiv-pcpncy-v1.0.0-20240606.txt", header = FALSE, sep = "", stringsAsFactors = FALSE)

# This is just precipitation data. For average temperature data download:
# https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmpccy-v1.0.0-20240606

data_precipitation <- data_precipitation %>%
  mutate(across(everything(), ~ na_if(., -9.99)))

# Assign column names
colnames(data_precipitation) <- c("ID", "Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6", "Month_7", "Month_8", "Month_9", "Month_10", "Month_11", "Month_12")

data_precipitation <- data_precipitation %>%
  mutate(
    Year = str_sub(ID, -4, -1),
    Element_Code = str_sub(ID, -6, -5),
    Division_Number = str_sub(ID, -9, -7),
    State_Code = str_sub(ID, 1, nchar(ID) - 9)
  )

data_precipitation <- data_precipitation %>%
  filter(Year > 1987)

data_precipitation <- data_precipitation %>%
  mutate(
    prep_avg = rowMeans(select(., starts_with("Month_")))
  )

# Merge FIPS codes
data_FIPS <- data_FIPS %>%
  distinct(State_code, County_Code, .keep_all = TRUE)

# Perform the join
data_precipitation <- data_precipitation %>%
  mutate(State_Code = as.integer(State_Code))

data_FIPS <- data_FIPS %>%
  mutate(State_Number = as.integer(State_Number))

data_precipitation <- data_precipitation %>%
  left_join(data_FIPS, by = c("State_Code" = "State_Number",
                              "Division_Number" = "County_Code"))

data_precipitation <- data_precipitation %>%
  mutate(Year = as.integer(Year))

data_precipitation <- data_precipitation %>%
  select(Year, fips, prep_avg)

data_precipitation <- data_precipitation %>%
  distinct(Year, fips, .keep_all = TRUE)

data_counties <- data_counties %>%
  mutate(GeoFIPS_0 = str_remove(GeoFIPS, "^0+"))

data_counties <- data_counties %>%
  mutate(GeoFIPS_0 = as.integer(GeoFIPS_0))

# Perform the join
data_counties <- data_counties %>%
  left_join(data_precipitation, by = c("GeoFIPS_0" = "fips",
                                       "year_of_vote" = "Year"))

###
### 4.- Merging local results data
###

data_local_results <- read_csv("Raw data/4.- Local election results/ledb_candidatelevel.csv")

# This dataset contains more than one record per county. 
# Should we still merge it even if we multiply by 5 or 10 the observations we have in the original dataset?

###
### 5.- Merging president results data
###

data_president_results <- read_csv("Raw data/5.- County Presidential Election Returns 2000-2020 - dataverse_files/countypres_2000-2020.csv")

# Perform the join
data_counties <- data_counties %>%
  left_join(data_president_results, by = c("GeoFIPS_0" = "county_fips",
                                           "year_of_vote" = "year"))

###
### 6.- Merging land values 
### Unable to download the data
###

###
### 7.- Merging housing data
###

data_housing <- read_csv("Raw data/7.- Housing/FMR_All_1983_2024_revised.csv")

columns_to_select <- c("fips", "msa", "areaname", grep("^fmr", names(data_housing), value = TRUE))

# Select only necessary columns
data_housing_selected <- data_housing %>% select(all_of(columns_to_select))

# Convert columns to long format
data_housing_long <- data_housing_selected %>%
  pivot_longer(cols = starts_with("fmr"), 
               names_to = c("temp_year", "type"), 
               names_pattern = "fmr(\\d{2})_(\\d)") %>%
  mutate(
    temp_year = as.numeric(temp_year),
    year = if_else(temp_year > 83, 1900 + temp_year, 2000 + temp_year)
  ) %>%
  select(-temp_year) %>%
  group_by(fips, msa, areaname, year, type) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = type, values_from = value, names_prefix = "fmr_")

data_housing_long <- data_housing_long %>%
  mutate(across(everything(), ~ iconv(.x, from = "UTF-8", to = "UTF-8", sub = "byte")))

data_housing_long <- data_housing_long %>%
  mutate(
    fips_county = str_sub(fips, 1, 5)
  )

data_housing_long <- data_housing_long %>%
  select(
    fips_county, year, fmr_0, fmr_1, fmr_3, fmr_4
  )

data_housing_long <- data_housing_long %>%
  mutate(fips_county = as.integer(fips_county))

data_housing_long <- data_housing_long %>%
  mutate(year = as.integer(year))

data_housing_long <- data_housing_long %>%
  distinct(fips_county, year, .keep_all = TRUE)

data_counties <- data_counties %>%
  left_join(data_housing_long, by = c("GeoFIPS_0" = "fips_county",
                                      "year_of_vote" = "year"))

data_counties <- data_counties %>%
  select(-Index, -mode, -GeoName.y, -GeoName.x, -juris_name, -State, -state, -state_po, -county_name, -Region.y)

data_counties <- data_counties %>%
  rename(
    region_number = Region.x,
    income = Income,
    precipitation_avg = prep_avg,
    fair_market_r_0rooms = fmr_0,
    fair_market_r_1rooms = fmr_1,
    fair_market_r_3rooms = fmr_3,
    fair_market_r_4rooms = fmr_4
  )

write_csv(data_counties, "Cleaned data/data_main_final.csv")

