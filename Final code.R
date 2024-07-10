library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
setwd("C:/Users/Guill/OneDrive/Documents/School/RA Stuff/UBC/Guille project/Guille project")

data_main <- read.csv("Raw data/conservation_funding_votes.csv")

# filtering for just counties

data_counties <- data_main %>% filter(jurisdiction_type == "County")

data_counties <- data_counties %>% mutate(Index = row_number())

# ATTENTION: Im removing rows with year_of_vote 2023 as we dont have covariate data for them, please remove when data becomes avalable. 
data_counties <- data_counties %>% filter(year_of_vote != 2023)

###
### 1 and 2.- Merging population and income
###

# Read the CSV file with readr::read_csv
data_income <- read_csv("Raw data/2.- CAINC1 - Income/CAINC1__ALL_AREAS_1969_2022.csv")

data_income <- data_income %>%
  mutate(across(everything(), ~ iconv(.x, from = "UTF-8", to = "UTF-8", sub = "byte")))

# data_income$GeoName <- iconv(data_income$GeoName, from = "UTF-8", to = "UTF-8")

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

data_income <- data_income %>% select(-TableName,
                                      -LineCode,
                                      -IndustryClassification,
                                      -Description)

# Selecting only data that goes from 1988 onwards
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
         jurisdiction_name = str_replace(jurisdiction_name, " Conservation District", ""), #
         jurisdiction_name = str_replace(jurisdiction_name, " Park and Recreation District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " and Franklin Metro Parks", ""), 
         jurisdiction_name = str_replace(jurisdiction_name, " Historical Parks District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Preservation Park District", ""), #
         jurisdiction_name = str_replace(jurisdiction_name, " Park District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, "Great Parks of ", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Metroparks", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " MetroParks", ""), #
         jurisdiction_name = str_replace(jurisdiction_name, " Metropolitan Parks District", ""),
         jurisdiction_name = str_replace(jurisdiction_name, "MetroParks of ", ""), #
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
         jurisdiction_name = str_replace(jurisdiction_name, "Toledo Area", "Lucas"),
         jurisdiction_name = str_replace(jurisdiction_name, "Forest Preserves of ", ""),
         jurisdiction_name = str_replace(jurisdiction_name, "Preservation of ", ""),
         jurisdiction_name = str_replace(jurisdiction_name, " Metropolitan", ""),
         jurisdiction_name = str_replace(jurisdiction_name, "Jacksonville", "Duval"),
         jurisdiction_name = str_replace(jurisdiction_name, " Community Facilities No. 2022-1", ""),
         # Conditional replacement for Cleveland, OH
         jurisdiction_name = ifelse(jurisdiction_name == "Cleveland" & state_two_letter_code == "OH", "Cuyahoga", jurisdiction_name))





# Ensure that data_pop has unique combinations of mun_ID and year
data_income_unique <- data_income %>%
  distinct(juris_name, Year, State, .keep_all = TRUE)

# Perform the join
data_counties <- data_counties %>%
  left_join(data_income_unique, by = c("jurisdiction_name" = "juris_name", 
                                       "year_of_vote" = "Year",
                                       "state_two_letter_code" = "State"))

data_counties$Region <- as.numeric(data_counties$Region)
data_counties$Income <- as.numeric(data_counties$Income)

# I had to manually change these records because, despite they had everything 
# to correctly do the left join, the join was not working. I don't know why :(

data_counties <- data_counties %>%
  mutate(
    GeoFIPS = if_else(Index == 457, "\"39095\"", GeoFIPS),
    GeoName = if_else(Index == 457, "Lucas, OH", GeoName),
    Region = if_else(Index == 457, 3, Region),
    Income = if_else(Index == 457, 16561187, Income),
    GeoFIPS = if_else(Index == 569, "\"51942\"", GeoFIPS),
    GeoName = if_else(Index == 569, "Prince William, Manassas + Manassas Park, VA*", GeoName),
    Region = if_else(Index == 569, 5, Region),
    Income = if_else(Index == 569, 29767221, Income)
  )

# Ensure that data_pop has unique combinations of mun_ID and year
data_pop_unique <- data_pop %>%
  distinct(juris_name, Year, State, .keep_all = TRUE)

# Perform the join
data_counties <- data_counties %>%
  left_join(data_pop_unique, by = c("GeoFIPS" = "GeoFIPS", 
                                    "year_of_vote" = "Year"))

data_counties <- data_counties %>%
  mutate(GeoFIPS = str_replace_all(GeoFIPS, "\"", ""))



#3 weather data

# Read precipitation data
data_precipitation <- read.table("Raw data/3.- Weather/climdiv-pcpncy-v1.0.0-20240606.txt", header = FALSE, sep = "", stringsAsFactors = FALSE)

data_precipitation <- data_precipitation %>%
  mutate(across(everything(), ~ na_if(., -9.99)))

# Assign column names
colnames(data_precipitation) <- c("ID", "Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6", "Month_7", "Month_8", "Month_9", "Month_10", "Month_11", "Month_12")

data_precipitation <- data_precipitation %>%
  mutate(
    Year = str_sub(ID, -4, -1),                          # Last 4 digits for the year
    Element_Code = str_sub(ID, -6, -5),                  # 5th and 6th digits from the end for the element code
    Division_Number = str_sub(ID, -9, -7),               # 7th to 9th digits from the end for the division number
    State_Code = str_sub(ID, 1, nchar(ID) - 9)           # Remaining digits for the state code
  )

data_precipitation <- data_precipitation %>%
  filter(Year > 1987)

data_precipitation <- data_precipitation %>%
  mutate(
    annual_precipitation = rowSums(select(., starts_with("Month_")), na.rm = TRUE)
  )

# Read average temperature data
data_temperature <- read.table("Raw data/3.- Weather/climdiv-tmpccy-v1.0.0-20240606.txt", header = FALSE, sep = "", stringsAsFactors = FALSE)

data_temperature <- data_temperature %>%
  mutate(across(everything(), ~ na_if(., -9.99)))

# Assign column names
colnames(data_temperature) <- c("ID", "Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6", "Month_7", "Month_8", "Month_9", "Month_10", "Month_11", "Month_12")

data_temperature <- data_temperature %>%
  mutate(
    Year = str_sub(ID, -4, -1),                          # Last 4 digits for the year
    Element_Code = str_sub(ID, -6, -5),                  # 5th and 6th digits from the end for the element code
    Division_Number = str_sub(ID, -9, -7),               # 7th to 9th digits from the end for the division number
    State_Code = str_sub(ID, 1, nchar(ID) - 9)           # Remaining digits for the state code
  )

data_temperature <- data_temperature %>%
  filter(Year > 1987)

data_temperature <- data_temperature %>%
  mutate(
    avg_temp = rowMeans(select(., starts_with("Month_")), na.rm = TRUE)
  )

# Read maximum temperature data
data_max_temperature <- read.table("Raw data/3.- Weather/climdiv-tmaxcy-v1.0.0-20240606.txt", header = FALSE, sep = "", stringsAsFactors = FALSE)

data_max_temperature <- data_max_temperature %>%
  mutate(across(everything(), ~ na_if(., -9.99)))

# Assign column names
colnames(data_max_temperature) <- c("ID", "Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6", "Month_7", "Month_8", "Month_9", "Month_10", "Month_11", "Month_12")

data_max_temperature <- data_max_temperature %>%
  mutate(
    Year = str_sub(ID, -4, -1),
    State_Code = str_sub(ID, 1, nchar(ID) - 9),
    Division_Number = str_sub(ID, -9, -7),
    max_temp = apply(select(., starts_with("Month_")), 1, max, na.rm = TRUE)
  )

# Read minimum temperature data
data_min_temperature <- read.table("Raw data/3.- Weather/climdiv-tmincy-v1.0.0-20240606.txt", header = FALSE, sep = "", stringsAsFactors = FALSE)

data_min_temperature <- data_min_temperature %>%
  mutate(across(everything(), ~ na_if(., -9.99)))

# Assign column names
colnames(data_min_temperature) <- c("ID", "Month_1", "Month_2", "Month_3", "Month_4", "Month_5", "Month_6", "Month_7", "Month_8", "Month_9", "Month_10", "Month_11", "Month_12")

data_min_temperature <- data_min_temperature %>%
  mutate(
    Year = str_sub(ID, -4, -1),
    State_Code = str_sub(ID, 1, nchar(ID) - 9),
    Division_Number = str_sub(ID, -9, -7),
    min_temp = apply(select(., starts_with("Month_")), 1, min, na.rm = TRUE)
  )

# Merge FIPS data
data_FIPS <- read_csv("Raw data/state_and_county_fips_master.csv")

data_FIPS <- data_FIPS %>%
  mutate(
    State_code = str_sub(fips, 1, nchar(fips) - 3),                          # First digits for the state code
    County_Code = str_sub(fips, -3, -1)  # Last 3 digits for the county code
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

data_FIPS <- data_FIPS %>%
  distinct(State_code, County_Code, .keep_all = TRUE)

# Ensure that data_precipitation has unique combinations of relevant columns
data_precipitation <- data_precipitation %>%
  distinct(State_Code, Division_Number, Year, .keep_all = TRUE)

# Ensure that data_temperature has unique combinations of relevant columns
data_temperature <- data_temperature %>%
  distinct(State_Code, Division_Number, Year, .keep_all = TRUE)

# Ensure that data_max_temperature has unique combinations of relevant columns
data_max_temperature <- data_max_temperature %>%
  distinct(State_Code, Division_Number, Year, .keep_all = TRUE)

# Ensure that data_min_temperature has unique combinations of relevant columns
data_min_temperature <- data_min_temperature %>%
  distinct(State_Code, Division_Number, Year, .keep_all = TRUE)

# Perform the join for precipitation data
data_precipitation <- data_precipitation %>%
  mutate(
    State_Code = as.integer(State_Code),
    annual_precipitation = rowSums(select(data_precipitation, starts_with("Month_")), na.rm = TRUE),
    avg_precipitation = rowMeans(select(data_precipitation, starts_with("Month_")), na.rm = TRUE)
  ) %>%
  left_join(data_FIPS, by = c("State_Code" = "State_Number", "Division_Number" = "County_Code")) %>%
  mutate(Year = as.integer(Year)) %>%
  select(Year, fips, avg_precipitation, annual_precipitation) %>%
  distinct(Year, fips, .keep_all = TRUE)

# Perform the join for temperature data
data_temperature <- data_temperature %>%
  mutate(State_Code = as.integer(State_Code)) %>%
  left_join(data_FIPS, by = c("State_Code" = "State_Number", "Division_Number" = "County_Code")) %>%
  mutate(Year = as.integer(Year)) %>%
  select(Year, fips, avg_temp) %>%
  distinct(Year, fips, .keep_all = TRUE)

# Perform the join for maximum temperature data
data_max_temperature <- data_max_temperature %>%
  mutate(State_Code = as.integer(State_Code)) %>%
  left_join(data_FIPS, by = c("State_Code" = "State_Number", "Division_Number" = "County_Code")) %>%
  mutate(Year = as.integer(Year)) %>%
  select(Year, fips, max_temp) %>%
  distinct(Year, fips, .keep_all = TRUE)

# Perform the join for minimum temperature data
data_min_temperature <- data_min_temperature %>%
  mutate(State_Code = as.integer(State_Code)) %>%
  left_join(data_FIPS, by = c("State_Code" = "State_Number", "Division_Number" = "County_Code")) %>%
  mutate(Year = as.integer(Year)) %>%
  select(Year, fips, min_temp) %>%
  distinct(Year, fips, .keep_all = TRUE)

# Merge precipitation and temperature data with counties data
data_counties <- data_counties %>%
  mutate(GeoFIPS_0 = str_remove(GeoFIPS, "^0+")) %>%
  mutate(GeoFIPS_0 = as.integer(GeoFIPS_0))

# Perform the joins
data_counties <- data_counties %>%
  left_join(data_precipitation, by = c("GeoFIPS_0" = "fips", "year_of_vote" = "Year")) %>%
  left_join(data_temperature, by = c("GeoFIPS_0" = "fips", "year_of_vote" = "Year")) %>%
  left_join(data_max_temperature, by = c("GeoFIPS_0" = "fips", "year_of_vote" = "Year")) %>%
  left_join(data_min_temperature, by = c("GeoFIPS_0" = "fips", "year_of_vote" = "Year"))

# Ensure all data types are consistent
data_counties <- data_counties %>%
  mutate(
    GeoFIPS = as.numeric(GeoFIPS),
    Income = as.numeric(Income),
    Pop = as.numeric(Pop),
    avg_precipitation = as.numeric(avg_precipitation),
    annual_precipitation = as.numeric(annual_precipitation),
    avg_temp = as.numeric(avg_temp),
    max_temp = as.numeric(max_temp),
    min_temp = as.numeric(min_temp)
  )

# 4.- Merging local results data

# Read the local election results data CSV file
data_local_results <- read_csv("Raw data/4.- Local election results/ledb_candidatelevel.csv")
# Seeing that im not sure how this is useful. 
# 5.- Merging president results data

# Read the presidential election results data CSV file
data_president_results <- read_csv("Raw data/5.- County Presidential Election Returns 2000-2020 - dataverse_files/countypres_2000-2020.csv")

# Combine party and candidate into one column for votes
data_president_results <- data_president_results %>%
  mutate(candidate_party = paste0("candidate_", party),
         candidatevotes_party = paste0("candidatevotes_", party))

# Summarize the total votes for each candidate per county per year
data_president_results_summarized <- data_president_results %>%
  group_by(year, state, state_po, county_name, county_fips, office, candidate_party, candidatevotes_party) %>%
  summarise(candidate = paste(unique(candidate), collapse = ", "),
            candidatevotes = sum(candidatevotes, na.rm = TRUE), 
            totalvotes = max(totalvotes, na.rm = TRUE), 
            version = max(version, na.rm = TRUE),
            mode = "TOTAL", .groups = "drop")

# Pivot the data to have candidates as columns
data_candidates_pivoted <- data_president_results_summarized %>%
  select(year, state, state_po, county_name, county_fips, office, totalvotes, version, mode, candidate_party, candidate) %>%
  pivot_wider(names_from = candidate_party, values_from = candidate, values_fill = list(candidate = NA))

# Pivot the data to have candidate votes as columns
data_votes_pivoted <- data_president_results_summarized %>%
  select(year, state, state_po, county_name, county_fips, office, totalvotes, version, mode, candidatevotes_party, candidatevotes) %>%
  pivot_wider(names_from = candidatevotes_party, values_from = candidatevotes, values_fill = list(candidatevotes = 0))

# Combine the two pivoted dataframes
data_president_results_pivoted <- data_candidates_pivoted %>%
  left_join(data_votes_pivoted, by = c("year", "state", "state_po", "county_name", "county_fips", "office", "totalvotes", "version", "mode"))

# Remove any NAs and trailing commas
data_president_results_pivoted <- data_president_results_pivoted %>%
  mutate(across(where(is.character), ~ str_replace_all(.x, ", NA|NA,|, $", "")))

# Function to expand the years correctly
expand_years <- function(df) {
  df %>%
    rowwise() %>%
    mutate(year_range = list(seq(year, year + 3))) %>%
    unnest(year_range) %>%
    rename(original_year = year, year = year_range)
}

# Expand the years for the dataframe
data_president_results_expanded <- expand_years(data_president_results_pivoted)

# Display the first few rows of the expanded dataframe
head(data_president_results_expanded)

# Check for many-to-many relationships before merging
merged_check <- data_counties %>%
  left_join(data_president_results_expanded, by = c("GeoFIPS_0" = "county_fips", "year_of_vote" = "year"))

# If there are duplicates, stop and notify
if(nrow(merged_check) > nrow(data_counties)) {
  stop("There is a many-to-many relationship causing duplicated rows in the merged dataframe. Please check the format of the input files.")
}

# Merge the expanded presidential results data with the counties data
data_counties <- data_counties %>%
  left_join(data_president_results_expanded, by = c("GeoFIPS_0" = "county_fips", "year_of_vote" = "year"))

# Display the first few rows of the merged dataframe
head(data_counties)



# 6.- Merging land values 
# Note: The data could not be downloaded

# 7.- Merging housing data

# Read the housing data CSV file
data_housing <- read_csv("Raw data/7.- Housing/FMR_All_1983_2024_revised.csv")

# Select necessary columns from housing data
columns_to_select <- c("fips", "msa", "areaname", grep("^fmr", names(data_housing), value = TRUE))

data_housing_selected <- data_housing %>% select(all_of(columns_to_select))

# Convert housing data to long format
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

# Convert all columns to UTF-8 encoding
data_housing_long <- data_housing_long %>%
  mutate(across(everything(), ~ iconv(.x, from = "UTF-8", to = "UTF-8", sub = "byte")))

# Extract county FIPS code from FIPS code
data_housing_long <- data_housing_long %>%
  mutate(
    fips_county = str_sub(fips, 1, 5)
  )

# Select relevant columns from housing data
data_housing_long <- data_housing_long %>%
  select(
    fips_county, year, fmr_0, fmr_1, fmr_3, fmr_4
  )

# Convert county FIPS code to integer in housing data
data_housing_long <- data_housing_long %>%
  mutate(fips_county = as.integer(fips_county))

# Convert year to integer in housing data
data_housing_long <- data_housing_long %>%
  mutate(year = as.integer(year))

# Ensure unique combinations of county FIPS code and year in housing data
data_housing_long <- data_housing_long %>%
  distinct(fips_county, year, .keep_all = TRUE)

# Perform a left join to merge housing data with counties data
data_counties <- data_counties %>%
  left_join(data_housing_long, by = c("GeoFIPS_0" = "fips_county", 
                                      "year_of_vote" = "year"))

# Drop unnecessary columns from counties data
data_counties <- data_counties %>%
  select(-Index, -mode, -GeoName.y, -GeoName.x, -juris_name, -State,
         -state, -state_po, -county_name, -Region.y)

# Rename columns in counties data
data_counties <- data_counties %>%
  rename(
    region_number = Region.x,
    precipitation_avg = avg_precipitation,
    fair_market_r_0rooms = fmr_0,
    fair_market_r_1rooms = fmr_1,
    fair_market_r_3rooms = fmr_3,
    fair_market_r_4rooms = fmr_4,
  )

print(colnames(data_counties))


# Rename columns in counties data for more explicit names
data_counties <- data_counties %>%
  rename(
    jurisdiction_GeoFIPS = GeoFIPS,
    total_income_dollars = Income,
    population_count = Pop,
    jurisdiction_GeoFIPS_numeric = GeoFIPS_0,
    avg_annual_precipitation = precipitation_avg,
    avg_annual_temperature = avg_temp,
    election_year = original_year,
    election_office = office,
    election_candidate_name_DEMOCRAT = candidate_DEMOCRAT,
    election_candidate_name_REPUBLICAN = candidate_REPUBLICAN,
    election_candidate_name_GREEN = candidate_GREEN,
    election_candidate_name_OTHER = candidate_OTHER,
    election_candidate_votes_DEMOCRAT = candidatevotes_DEMOCRAT,
    election_candidate_votes_REPUBLICAN = candidatevotes_REPUBLICAN,
    election_candidate_votes_GREEN = candidatevotes_GREEN,
    election_candidate_votes_OTHER = candidatevotes_OTHER,
    election_total_votes = totalvotes,
    dataset_version = version,
    fair_market_rent_0_bedrooms = fair_market_r_0rooms,
    fair_market_rent_1_bedroom = fair_market_r_1rooms,
    fair_market_rent_3_bedrooms = fair_market_r_3rooms,
    fair_market_rent_4_bedrooms = fair_market_r_4rooms
  )

print(colnames(data_counties))

# Write the cleaned data to a CSV file
write_csv(data_counties, "Cleaned data/data_main.csv")

# Chosen covariates

# Data from CAINC1__ALL_AREAS_1969_2022.csv (US Bureau of Economic Analysis)
# Selected variables:
# - total_income_dollars: Personal income data
# - population_count: Population data
# - jurisdiction_GeoFIPS: Geographic FIPS code

# Data from co-est2020-alldata.csv (US Census)
# Selected variables:
# - region_number: Region number

# Data from county-to-climdivs.txt (NCEI)
# Selected variables:
# - avg_annual_precipitation: Average annual precipitation
# - annual_precipitation: Total annual precipitation
# - avg_annual_temperature: Average annual temperature
# - max_temp: Annual maximum temperature
# - min_temp: Annual minimum temperature

# Data from countypres_2000-2020.csv (MIT Election Data Lab)
# Selected variables:
# - election_year: Election year
# - state: State name
# - county_name: County name
# - election_candidate_name_DEMOCRAT: Candidate name (Democrat)
# - election_candidate_name_REPUBLICAN: Candidate name (Republican)
# - election_candidate_name_GREEN: Candidate name (Green)
# - election_candidate_name_OTHER: Candidate name (Other)
# - election_candidate_votes_DEMOCRAT: Number of votes for the Democrat candidate
# - election_candidate_votes_REPUBLICAN: Number of votes for the Republican candidate
# - election_candidate_votes_GREEN: Number of votes for the Green candidate
# - election_candidate_votes_OTHER: Number of votes for the Other candidate
# - election_total_votes: Total number of votes

# Data from FMR_All_1983_2024_revised.csv (US Department of Housing)
# Selected variables:
# - fair_market_rent_0_bedrooms: Fair Market Rent for 0 bedrooms
# - fair_market_rent_1_bedroom: Fair Market Rent for 1 bedroom
# - fair_market_rent_3_bedrooms: Fair Market Rent for 3 bedrooms
# - fair_market_rent_4_bedrooms: Fair Market Rent for 4 bedrooms

# Data from ledb_candidatelevel.csv (Justin de Benedictis-Kessner, MIT)
# Selected variables:
# - geoid: Geographic identifier
# - year: Year
# - election: Election type
# - candidate_name: Candidate name
# - candidate_party: Candidate party
# - votes: Number of votes
# - pct: Percentage of votes

# Data from data_main.csv (Conservation Funding Votes)
# Selected variables:
# - UniqueID: Unique identifier for the dataset
# - Year: Year of the data
# - FIPS: FIPS code
# - Variable: Name of the variable
# - Value: Value of the variable



# Methodology for Each Dataset:

# 1. County-level population estimates 1980-2023 from the US Census:
# Data on county-level population estimates were obtained from the US Census Bureau. The dataset provides annual population estimates for each county in the United States, covering the period from 1980 to 2023. (This was not used)

# 2. County-level personal income 1969-2020 from the US Bureau of Economic Analysis (CAINC1 dataset):
# This dataset includes county-level personal income data from the US Bureau of Economic Analysis. It covers the years 1969 to 2020 and includes information on total personal income and population for each county.(This was used for both income and pop)

# 3. NCEI county-level weather data (monthly min/max/avg temp, hdd, cdd, precip), 1895-2024:
# Weather data at the county level, including monthly averages for minimum and maximum temperatures, as well as precipitation, were sourced from the National Centers for Environmental Information (NCEI). The dataset spans from 1895 to 2024.

# 4. Local election results from Justin de Benedictis-Kessner (MIT) & co:
# This dataset contains local election results, including candidate names, parties, and vote counts. The spatial and temporal coverage varies, and it was sourced from research data provided by Justin de Benedictis-Kessner and colleagues at MIT. (this was not used)

# 5. County presidential election returns 2000-2020 from the MIT Election Data Lab:
# Data on county-level presidential election returns from 2000 to 2020 were obtained from the MIT Election Data Lab. This dataset includes information on candidates, parties, and vote counts for presidential elections. (This was used)

# 6. County-level fair market rates 1983-2024 from the US Department of Housing:
# Fair Market Rent (FMR) data for each county, covering the period from 1983 to 2024, were sourced from the US Department of Housing and Urban Development (HUD). The dataset includes FMRs for various bedroom sizes. (Could not find so not used)

# 7. US land values 2023 from the PLACES lab:
# County-level land values for the year 2023 were sourced from the PLACES lab. This dataset provides estimated land values for each county in the United States.

