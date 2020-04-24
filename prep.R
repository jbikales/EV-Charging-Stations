library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(httr)
library(datasets)
library(readr)
library(broom)
library(gt)
options(scipen=999)

census_api_key("7501b863e3d3c96200d101891c3df2191d2a98d0")

ev_data_raw <- GET("https://developer.nrel.gov/api/alt-fuel-stations/v1.csv?api_key=cL01al61HtSmjbeVd83mIteBv5EXivGyen42Dj59&fuel_type=ELEC")
ev_raw <- content(ev_data_raw)

ev_data <- ev_raw %>% 
  select("Latitude", "Longitude", "State") %>% 
  filter(Longitude < 0)

state_data <- tibble(state.abb, state.region) %>% 
  rename(State = state.abb)

ev_all_states <- right_join(ev_data, state_data, by = "State")

ev_data_sf <- ev_all_states %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4269, remove = FALSE)

census_incomes <- function(state){
  incomes <- get_acs(geography = "county",
                     variables = c("B19013_001"),
                     state = state,
                     year = 2018,
                     output = "wide",
                     geometry = TRUE) %>% 
    rename(county = NAME) %>%
    rename(median_household_income = B19013_001E) %>% 
    select(county, median_household_income, geometry)
}

for(i in state.abb){
  census_incomes(i)
}

census_incomes_data <- census_incomes(state.abb)

# problem here is that when I join the datasets together, the counties that have no stations are also joined to the tbl. Then when I go to count, it counts those as if they have 1 charging station.

combined <- st_join(census_incomes_data, ev_data_sf, left = TRUE)

write_rds(combined, "EV-charging-stations/clean-data/combined.rds")

combined_incomes_count <- as.data.frame(combined) %>% 
  select(-geometry) %>% 
  group_by(county) %>% 
  count() %>% 
  mutate(number_stations = n) %>% 
  select(county, number_stations)

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
