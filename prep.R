library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(httr)
library(datasets)
library(readr)
library(broom)
library(gt)
library(leaflet)
options(scipen=999)

census_api_key("7501b863e3d3c96200d101891c3df2191d2a98d0", install = TRUE, overwrite=TRUE)

# # loading in the dataset from NREL, they recommended using GET from httr package, 
# and I then figured out to use content to get the raw data into a usable format

ev_data_raw <- GET("https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?api_key=cL01al61HtSmjbeVd83mIteBv5EXivGyen42Dj59&fuel_type=ELEC")
ev_raw <- content(ev_data_raw)

# Since it was such a large dataset, I pared it down by selected only the info on
# each station that I need. I can always go back and get more of the 67 attributes 
# I wanted to include them as labels on the maps, for example. I also filtered out data
# with a longitude below 0 because there were some incorrect data points.

ev_data <- ev_raw %>% 
  select("Latitude", "Longitude", "State") %>% 
  filter(Longitude < 0) 

# I created a tibble with state names and abbreviations

state_names <- tibble(state.abb, state.name) %>% 
  rename(State = state.abb)

# I joined the ev station data to my state names because the original dataset only
# has abbreviations for the state names

ev_all_states <- right_join(ev_data, state_names, by = "State")

# # converted the tibble to sf format for use in mapping. I matched the crs to the crs
# that is used for the geographic data on the census since I knew I wanted to combine the two

ev_data_sf <- ev_all_states %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4269, remove = FALSE) 

write_rds(ev_data_sf, "EV-charging-stations/clean-data/ev_data_sf.rds")


# function to pull the median household income data from the tidycensus

census_incomes <- function(state){
  incomes <- get_acs(geography = "county",
                     variables = c("B19013_001"),
                     state = state,
                     year = 2018,
                     output = "wide",
                     geometry = TRUE) %>% 
    rename(county = NAME) %>%
    rename(median_household_income = B19013_001E) %>%
    
    # because the census prints counties in the format "County, State" but I wanted
    # a separate column for state, the mutate below adds a column with the state  
    
    mutate(State = state) %>% 
    select(county,State, median_household_income, geometry)
  return(incomes)
}

# running that function for each state in the states dataset

for(i in state.name){
  census_incomes(i)
}

census_incomes_list <- lapply(state.name,census_incomes)
census_incomes_data <- do.call(rbind, census_incomes_list)

write_rds(census_incomes_data, "EV-charging-stations/clean-data/census_incomes_data.rds")


# function to pull the percent rural data from the tidycensus

census_rural <- function(state){
  rural <- get_decennial(geography = "county",
                         variables = c("P001001", "P002005"),
                         state = state,
                         year = 2010,
                         output = "wide",
                         geometry = TRUE) %>%
    rename(county = NAME) %>%
    
    # mutate to create the prop rural data by taking number of rural residents divided
    # by total residents of a county
    
    mutate(prop_rural = P002005/P001001,
           county = reorder(county, prop_rural)) %>%
    
    # because the census prints counties in the format "County, State" but I wanted
    # a separate column for state, the mutate below adds a column with the state  
    
    mutate(State = state) %>% 
    select(county, State, prop_rural, geometry)
  return(rural)
}

for(i in state.name){
  census_rural(i)
}

census_rural_list <- lapply(state.name, census_rural)
census_rural_data <- do.call(rbind, census_rural_list)

write_rds(census_rural_data, "EV-charging-stations/clean-data/census_rural_data.rds")




# function to pull the cars per capita data from the tidycensus

census_cars <- function(state){
  cars <-  get_acs(geography = "county",
                   variables = c("B25046_001", "B01003_001"),
                   state = state,
                   year = 2018,
                   output = "wide",
                   geometry = TRUE) %>% 
    rename(county = NAME) %>%
 
    # mutate to create the cars per capita data by taking number of cars divided
    # by total residents of a county
    
    mutate(cars_per_capita = B25046_001E/B01003_001E) %>% 
    
    # because the census prints counties in the format "County, State" but I wanted
    # a separate column for state, the mutate below adds a column with the state  
    
    mutate(State = state) %>% 
    select(county, State, cars_per_capita, geometry)
  return(cars)
}

for(i in state.name){
  census_cars(i)
}

census_cars_list <- lapply(state.name, census_cars)
census_cars_data <- do.call(rbind, census_cars_list)

write_rds(census_cars_data, "EV-charging-stations/clean-data/census_cars_data.rds")




# function to pull the total population data from the tidycensus

census_pop <- function(state){
  total_pop <- get_acs(geography = "county",
                       variables = c("B01003_001"),
                       state = state,
                       year = 2018,
                       output = "wide",
                       geometry = TRUE) %>% 
    rename(county = NAME, total_pop = B01003_001E) %>% 
    
    # because the census prints counties in the format "County, State" but I wanted
    # a separate column for state, the mutate below adds a column with the state 
    
    mutate(State = state) %>% 
    select(county, State, total_pop, geometry)
  return(total_pop)
}

for(i in state.name){
  census_pop(i)
}

census_pop_list <- lapply(state.name, census_pop)
census_pop_data <- do.call(rbind, census_pop_list)

write_rds(census_pop_data, "EV-charging-stations/clean-data/census_pop_data.rds")



# function to pull the education data from the tidycensus

census_education <- function(state){
  education <- get_acs(geography = "county",
                       variables = c("B16010_041", "B15002_001"),
                       state = state,
                       year = 2018,
                       output = "wide",
                       geometry = TRUE) %>% 
    rename(county = NAME) %>%
    
    # mutate to create the percent of Bachelor's data by taking number of people
    # with Bachelor's or higher divided by total number of adults over age 25 of a county
    
    mutate(percent_bachelors = B16010_041E/B15002_001E) %>% 
    
    # because the census prints counties in the format "County, State" but I wanted
    # a separate column for state, the mutate below adds a column with the state 
    
    mutate(State = state) %>% 
    select(county, State, percent_bachelors, geometry)
  return(education)
}

for(i in state.name){
  census_education(i)
}

census_education_list <- lapply(state.name, census_education)
census_education_data <- do.call(rbind, census_education_list)

write_rds(census_education_data, "EV-charging-stations/clean-data/census_education_data.rds")


# combining the incomes data with the ev stations data to run stat analysis

combined <- st_join(census_incomes_data, (ev_data_sf %>% select(!State)), left = TRUE)

combined_incomes_count <- as.data.frame(combined) %>% 
  
# creates an indicator variable for whether or not the observation is a station because
# there are some counties with zero stations that would otherwise show up as having
# one station
  
  mutate(station = ifelse(is.na(Latitude),0,1)) %>% 
  select(-geometry) %>% 
  group_by(county, State) %>% 
  summarize(number_stations = sum(station)) %>% 
  select(county,State, number_stations)

census <- as.data.frame(census_incomes_data) %>% 
  select(county, median_household_income)

# combining the count of stations by county to the census data on incomes

reg_data <- left_join(census, combined_incomes_count, by = "county") 

write_rds(reg_data, "EV-charging-stations/clean-data/regression.rds")

# making the static graph for the home page with all stations

ev_data_sf %>%
  ggplot() + 
  geom_point(aes(Longitude, Latitude), size = 0.3, color = "forestgreen") +
  labs(title = "Locations of all E.V. Charging Stations in the United States",
       subtitle = "Including Alaska and Hawaii",
       caption = "Source: National Renewable Energy Laboratory database"
  ) +
  theme_void() 

ggsave(filename = "EV-charging-stations/all_stations.png") 
