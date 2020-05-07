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

ev_data_raw <- GET("https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?api_key=cL01al61HtSmjbeVd83mIteBv5EXivGyen42Dj59&fuel_type=ELEC")
ev_raw <- content(ev_data_raw)

ev_data <- ev_raw %>% 
  select("Latitude", "Longitude", "State") %>% 
  filter(Longitude < 0) 

state_data <- tibble(state.abb, state.name) %>% 
  rename(State = state.abb)

ev_all_states <- right_join(ev_data, state_data, by = "State")

ev_data_sf <- ev_all_states %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4269, remove = FALSE) 

write_rds(ev_data_sf, "EV-charging-stations/clean-data/ev_data_sf.rds")


# getting the median household income data

census_incomes <- function(state){
  incomes <- get_acs(geography = "county",
                     variables = c("B19013_001"),
                     state = state,
                     year = 2018,
                     output = "wide",
                     geometry = TRUE) %>% 
    rename(county = NAME) %>%
    rename(median_household_income = B19013_001E) %>%
    mutate(State = state) %>% 
    select(county,State, median_household_income, geometry)
  return(incomes)
}

for(i in state.name){
  census_incomes(i)
}

census_incomes_list <- lapply(state.name,census_incomes)
census_incomes_data <- do.call(rbind, census_incomes_list)

write_rds(census_incomes_data, "EV-charging-stations/clean-data/census_incomes_data.rds")


# getting the percent rural data

census_rural <- function(state){
  rural <- get_decennial(geography = "county",
                         variables = c("P001001", "P002005"),
                         state = state,
                         year = 2010,
                         output = "wide",
                         geometry = TRUE) %>%
    rename(county = NAME) %>%
    mutate(prop_rural = P002005/P001001,
           county = reorder(county, prop_rural)) %>%
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




# getting the cars data 

census_cars <- function(state){
  cars <-  get_acs(geography = "county",
                   variables = c("B25046_001", "B01003_001"),
                   state = state,
                   year = 2018,
                   output = "wide",
                   geometry = TRUE) %>% 
    rename(county = NAME) %>%
    mutate(cars_per_capita = B25046_001E/B01003_001E) %>% 
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






# getting the total pop data

census_pop <- function(state){
  total_pop <- get_acs(geography = "county",
                       variables = c("B01003_001"),
                       state = state,
                       year = 2018,
                       output = "wide",
                       geometry = TRUE) %>% 
    rename(county = NAME, total_pop = B01003_001E) %>% 
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



# getting the education data

census_education <- function(state){
  education <- get_acs(geography = "county",
                       variables = c("B16010_041", "B15002_001"),
                       state = state,
                       year = 2018,
                       output = "wide",
                       geometry = TRUE) %>% 
    rename(county = NAME) %>%
    mutate(percent_bachelors = B16010_041E/B15002_001E) %>% 
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




combined <- st_join(census_incomes_data, (ev_data_sf %>% select(!State)), left = TRUE)

write_rds(combined, "EV-charging-stations/clean-data/combined.rds")

combined_incomes_count <- as.data.frame(combined) %>% 
  # create an indicator variable for whether or not the observation is a station 
  mutate(station = ifelse(is.na(Latitude),0,1)) %>% 
  select(-geometry) %>% 
  group_by(county, State) %>% 
  summarize(number_stations = sum(station)) %>% 
  select(county,State, number_stations)

census <- as.data.frame(census_incomes_data) %>% 
  select(county, median_household_income)

reg_data <- left_join(census, combined_incomes_count, by = "county") 

write_rds(reg_data, "EV-charging-stations/clean-data/regression.rds")

ev_data_sf %>%
  ggplot() + 
  geom_point(aes(Longitude, Latitude), size = 0.3, color = "forestgreen") +
  labs(title = "Locations of all E.V. Charging Stations \n in the United States",
       subtitle = "Including Alaska and Hawaii",
       caption = "American Communities Survey 2018 and \n National Renewable Energy Laboratory database"
  ) +
  theme_void() 

ggsave(filename = "EV-charging-stations/all_stations.png") 
