library(tidyverse)
# additional packages for filtering cities and spatial data
# install.packages(c("sf", "tigris"))
library(sf); library(tigris)
options(tigris_use_cache = TRUE)

# import raw data
acs_raw <- read_csv("raw-data/acs1_places_ca.csv", show_col_types = FALSE)
udp_source <- read_csv("raw-data/policymap_0526.csv", show_col_types = FALSE)
dates <- read_csv("raw-data/rent_control_dates.csv", show_col_types = FALSE)

# filter out non-cities from ACS (counties, census tracts, uninc)
last_year <- 2025
city_dsgn  <- " (city|town), California$"
acs_cities <- acs_raw |> filter(str_detect(NAME, city_dsgn)) |>
  distinct(city = str_remove(NAME, city_dsgn))

# filter out cities without rent stabilization/rent control from UDP
udp_treated <- udp_source |> 
  filter(
    `Policy Type` == "Rent Stabilization or Rent Control",
    str_trim(`Exists? (Y/N)`) == "Y",
    !is.na(City) # unincorporated LA county
    ) |>
  distinct(City, .keep_all = TRUE) |> # remove duplicates
  transmute( # lowercase variables
    city = City,
    county = County,
    udp_source = Source,
    in_acs = City %in% acs_cities$city)  
write_csv(udp_treated, "clean-data/udp_treated.csv")

# apply criteria for timing and if the policy happened
treated_cities <- udp_treated |>
  filter(in_acs) |> # filter out cities under 65k pop
  left_join(dates, by = "city") |> # merge cities to adoption dates
  mutate(
    effective  = ymd(first_cap_effective),
    treat_year = if_else(month(effective) <= 6, 
                 year(effective), year(effective) + 1), # match policy to effective year
    status = case_when(
           never_in_effect          ~ "no: not enacted", # happened?
           pre_2016                 ~ "no: too early (before 2016)", # in last 10y?
           treat_year > last_year   ~ "no: too late (after 2024)", # enough data?
           treat_year >= 2016 & treat_year <= last_year - 1 ~ "treated",
           TRUE   ~ "no: fewer than 2 post-years") # otherwise
    )
write_csv(treated_cities, "clean-data/treated_cities.csv")

# separate cities (mostly for checking)
final_cities <- treated_cities |> 
  filter(status == "treated") |> 
  select(city, treat_year, cap = cap_strength)
exclude <- treated_cities |>
  filter(str_starts(status, "no")) |> 
  pull(city)  

# Bay Area market sucks, so adding this as a check later
bay_counties <- c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco",
                  "San Mateo", "Santa Clara", "Solano", "Sonoma")
counties <- counties("CA", year = 2020) |> filter(NAME %in% bay_counties)
bay_geoids <- places("CA", year = 2020) |>
  st_point_on_surface() |>
  st_join(counties, join = st_within, left = FALSE) |>
  pull(GEOID.x)         

# finally build the panel 
panel <- acs_raw |>
  filter(str_detect(NAME, city_dsgn) # create cities from ACS
         ) |>
  transmute( # create variables
    geoid = GEOID, 
    city = str_remove(NAME, city_dsgn), 
    year,
    rent = rentE, rent_err = rentM, 
    gross = grossE, pop = popE,
    renter_share = rentersE / hhE,
    stay_share   = mob_stayE / mob_rentE
    ) |>
  filter(!city %in% exclude, !is.na(rent) # filter excl/missing data
         ) |> 
  left_join(final_cities, by = "city") |> # merge our adopting cities
  mutate(treat_year = replace_na(treat_year, 0), # missing years
         treated    = treat_year > 0,
         log_rent   = log(rent),
         log_gross  = log(gross),
         in_bay   = geoid %in% bay_geoids)

n_years <- n_distinct(panel$year)
panel <- panel |> 
  group_by(city) |> 
  filter(n() == n_years) |> # kill composition bias
  ungroup() 

# track filter
sample_log <- tibble(
  step = c("CA cities and towns in ACS",
           "UDP-flagged cities",
           "timing issues",
           "Kept after balanced-panel filter",
           "  treated", "  controls"),
  cities = c(nrow(acs_cities), nrow(treated_cities), length(exclude),
             n_distinct(panel$city),
             n_distinct(panel$city[panel$treated]),
             n_distinct(panel$city[!panel$treated])))

write_csv(panel, "clean-data/panel.csv")
write_csv(sample_log, "clean-data/sample_tracker.csv")

# final checks
count(treated_cities, status) # why filter?
sample_log
setdiff(final_cities$city, panel$city)
panel |> filter(in_bay) |> distinct(city) |> pull(city) # bay cities
summary(panel$stay_share) # making sure ACS wasn't tweaking
