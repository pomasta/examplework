# --------------
# Data Wrangling
# --------------

# Packages:
# --------

library(tidyverse)
library(janitor)
library(here)
library(naniar)
library(countrycode)

# Tidy Formatting:
# --------

# 1. Read in data:
energy_raw <- read_csv(here::here("raw-data", "world_bank_raw_data.csv")) %>% 
  dplyr::select(-"Country Code", -"Series Code")

# 2. Pivot longer:
energy_longer <- energy_raw %>% 
  pivot_longer(cols = 3:42,
               names_to = "year",
               values_to = "value")

# 3. A little tidying:
energy_tidy <- energy_longer %>% 
  replace_with_na(replace = list(value = "..")) 

energy_tidy <- energy_tidy %>% 
  mutate(year = str_sub(string = energy_tidy$year,
                        end = 4))
  
# 4. Pivot wider:
energy_wider <- energy_tidy %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = "Series Name",
              values_from = "value") %>% 
  dplyr::select(-row) 

# 5. Clean column names:
energy_clean <- energy_wider %>% 
  janitor::clean_names()
  
# 6. Merge data rows / columns to remove NAs:
f <- function(x) {
  x <- x[!is.na(x$value),]
  if (nrow(x) > 0) {
    y <- unique(x[colnames(x) != 'row.ID'])
    y$row.ID <- 1:nrow(y)
    return(y)
  } else {
    return(data.frame())
  }
}

merged_energy <- gather(energy_clean, variable, value, -country_name, -year) %>% 
  group_by(country_name, year, variable) %>% 
  mutate(row.ID = 1:n()) %>% 
  do(f(.)) %>% 
  spread(variable, value, convert = T) %>% 
  ungroup()

# 7 Rename Columns:
tidy_energy <- merged_energy %>% 
  dplyr::select(-row.ID) %>% 
  rename(
    country = country_name,
    electric_access_pct = access_to_electricity_percent_of_population,
    co2_tons_pc = co2_emissions_metric_tons_per_capita,
    cdd_change = cooling_degree_days_projected_change_in_number_of_degree_celsius,
    extreme_affected_pct = droughts_floods_extreme_temperatures_percent_of_population_average_1990_2009,
    coal_electric_pct = electricity_production_from_coal_sources_percent_of_total,
    oil_equivalent_pc = energy_use_kg_of_oil_equivalent_per_capita,
    fossil_fuels_pct = fossil_fuel_energy_consumption_percent_of_total,
    ghg_lucf = ghg_net_emissions_removals_by_lucf_mt_of_co2_equivalent,
    poverty_pct = poverty_headcount_ratio_at_national_poverty_lines_percent_of_population,
    renewable_pct = renewable_energy_consumption_percent_of_total_final_energy_consumption)

# 8 Save the file as a .csv for analysis in the app 
write.csv(tidy_energy, here("modified-data","tidy_energy.csv"), row.names = FALSE)

#---
#Wrangling for getting continents on the file
#--- 

# 9 Add in the continents data frame

continent_raw <-  tidy_energy %>% 
  mutate(continent = countrycode(sourcevar = tidy_energy$country,
                                 origin = "country.name",
                                 destination = "continent"))
# 10 Any NAs?

unique(continent_raw$continent)

# 11 Yes. Remove NAs:

continent_na <- continent_raw %>% 
  filter(is.na(continent_raw$continent))

continent_nona <- anti_join(continent_raw,
                            continent_na,
                            by = "continent")

write.csv(continent_nona, here("modified-data","continent_data.csv"), row.names = FALSE)
