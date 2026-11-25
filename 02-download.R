# confirm setup
library(tidyverse)
library(tidycensus)

# populate variables and time
vars <- c(rent     = "B25058_001",   # Median contract rent ($)   
          gross    = "B25064_001",   # Median gross rent ($)
          pop      = "B01003_001",   # Total population
          hh       = "B25003_001",   # Total occupied housing units
          renters  = "B25003_003",   # Renter-occupied housing units
          mob_rent = "B07013_003",   # Householder lived in renter-occupied housing 
          mob_stay = "B07013_006"   # Renter remained in same house as 1 year ago
  )

years <- c(2010:2019, 2021:2025)

# pull data, filtered by CA places from each survey each year
pull_year <- function(y) {                                       
  message("Downloading ", y) # track progress
  
  tryCatch(
    {
      get_acs(
        geography = "place",
             state = "CA",
             survey = "acs1",
             year = y,
             variables = vars,
             output = "wide"
        ) |> mutate(year = y)
  },
    error = function(e) {
      message("Skipped ", y, ": ", conditionMessage(e))
      NULL 
    }
  )
}

# write data into raw-data (stacked by year)
acs_raw <- map_dfr(years, pull_year)
write_csv(acs_raw, "raw-data/acs1_places_ca.csv") 

# check place count, then test (anywhere you want)
count(acs_raw, year)                                               # 7
acs_raw |> filter(str_starts(NAME, "Mountain View city")) |>
  select(year, rentE, rentM, mob_stayE)  

