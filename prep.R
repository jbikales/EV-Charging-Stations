library(tidyverse)
library(tidycensus)
library(sf)
library(ggplot2)
library(httr)
library(datasets)
library(readr)

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

Get_Data <- function(state){
  out <- get_decennial(geography = "county",
                       variables = c("P001001", "P002005"),
                       state = state,
                       year = 2010,
                       output = "wide",
                       geometry = TRUE) %>%
    rename(county = NAME) %>%
    mutate(prop_rural = P002005/P001001,
           county = reorder(county, prop_rural))
  return(out)
}

for(i in state.abb){
  Get_Data(i)
}

census_data <- Get_Data(state.abb) 

combined <- st_join(census_data, ev_data_sf, left = TRUE) %>% 
  group_by(county)

write_rds(combined, "clean-data/combined.rds")




