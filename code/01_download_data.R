# Data are from https://data.police.uk/data/ but can only be accessed via a web 
# form. Data are downloaded manually as a zip file. That file contains a nested 
# file structure of CSV files, one for each force each month.

# load packages
library(httr)
library(lubridate)
library(osmdata)
library(tidyverse)



# GET SEARCH DATA --------------------------------------------------------------

data_dir <- tempdir()
data_file <- str_glue("{data_dir}/stop_data.zip")
GET(
  "https://data.police.uk/data/archive/2021-12.zip", 
  write_disk(data_file), 
  progress()
)
data_files <- data_file %>% 
  unzip(list = TRUE) %>% 
  as_tibble() %>% 
  filter(str_detect(Name, "stop-and-search.csv$")) %>% 
  pull("Name")
unzip(data_file, files = data_files, exdir = str_glue("{data_dir}"))
data_dir %>% 
  dir(pattern = "stop-and-search.csv$", full.names = TRUE, recursive = TRUE) %>% 
  set_names() %>% 
  map_dfr(
    read_csv, 
    col_types = cols(
      .default = col_character(),
      Date = col_datetime(format = ""),
      Latitude = col_double(),
      Longitude = col_double()
    ), 
    .id = "force"
  ) %>% 
  janitor::clean_names() %>% 
  mutate(
    across(.cols = c(gender, object_of_search, outcome), .fns = str_to_lower),
    force = case_when(
      str_detect(force, "btp") ~ "British Transport Police",
      str_detect(force, "city-of-london") ~ "City of London Police",
      str_detect(force, "metropolitan") ~ "Metropolitan Police Service",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(force)) %>% 
  select(
    -part_of_a_policing_operation, 
    -policing_operation, 
    -outcome_linked_to_object_of_search,
    -removal_of_more_than_just_outer_clothing
  ) %>% 
  write_rds(here::here("analysis-data/stops.rds"))



# GET POPULATION DATA ----------------------------------------------------------

# data is from https://data.london.gov.uk/dataset/ethnic-group-population-projections
download.file(
  url = "https://data.london.gov.uk/download/ethnic-group-population-projections/a9598ef0-808c-4f96-9eac-8bb314bd92cd/Ethnic%20group%20projections%20%282016-based%20central%20trend%29.xlsx",
  destfile = here::here("original-data/population_data.xlsx")
)

map_dfr(
  c("Population - Females", "Population - Males"), 
  ~ readxl::read_excel(path = here::here("original-data/population_data.xlsx"), 
                       sheet = .)
) %>% 
  filter(
    age != "All ages", 
    borough == "Greater London", 
    !ethnic_group %in% c("All persons", "BAME")
  ) %>% 
  mutate(
    age = as.numeric(age),
    age_range = case_when(
      between(age, 0, 9) ~ "under 10",
      between(age, 10, 17) ~ "10-17",
      between(age, 18, 24) ~ "18-24",
      between(age, 25, 34) ~ "25-34",
      age > 34 ~ "over 34",
      TRUE ~ NA_character_
    ),
    self_defined_ethnicity = recode(
      ethnic_group,
      "White British" = "White - English/Welsh/Scottish/Northern Irish/British",
      "White Irish" = "White - Irish",
      "Other White" = "White - Any other White background",
      "Black African" = "Black/African/Caribbean/Black British - African",
      "Black Caribbean" = "Black/African/Caribbean/Black British - Caribbean",
      "Other Black" = "Black/African/Caribbean/Black British - Any other Black/African/Caribbean background",
      "White & Black Caribbean" = "Mixed/Multiple ethnic groups - White and Black Caribbean",
      "White & Black African" = "Mixed/Multiple ethnic groups - White and Black African",
      "White & Asian" = "Mixed/Multiple ethnic groups - White and Asian",
      "Other Mixed" = "Mixed/Multiple ethnic groups - Any other Mixed/Multiple ethnic background",
      "Indian" = "Asian/Asian British - Indian",
      "Pakistani" = "Asian/Asian British - Pakistani",
      "Bangladeshi" = "Asian/Asian British - Bangladeshi",
      "Other Asian" = "Asian/Asian British - Any other Asian background",
      "Chinese" = "Asian/Asian British - Chinese",
      "Arab" = "Other ethnic group - Any other ethnic group",
      "Other Ethnic Group" = "Other ethnic group - Any other ethnic group"
    ),
    sex = str_to_lower(sex)
  ) %>% 
  rename(people = `2020`) %>% 
  count(sex, age_range, self_defined_ethnicity, wt = people, name = "people") %>% 
  write_rds(here::here("analysis-data/people.rds"))


## Borough-level population data ----

map_dfr(
  c("Population - Females", "Population - Males"), 
  ~ readxl::read_excel(path = here::here("original-data/population_data.xlsx"), 
                       sheet = .)
) %>% 
  filter(
    age != "All ages", 
    !borough %in% c("Inner London", "Outer London"), 
    !ethnic_group %in% c("All persons", "BAME")
  ) %>% 
  mutate(
    age = as.numeric(age),
    age_range = case_when(
      between(age, 0, 9) ~ "under 10",
      between(age, 10, 17) ~ "10-17",
      between(age, 18, 24) ~ "18-24",
      between(age, 25, 34) ~ "25-34",
      age > 34 ~ "over 34",
      TRUE ~ NA_character_
    ),
    borough = recode(borough, "Greater London" = "London"),
    self_defined_ethnicity = recode(
      ethnic_group,
      "White British" = "White - English/Welsh/Scottish/Northern Irish/British",
      "White Irish" = "White - Irish",
      "Other White" = "White - Any other White background",
      "Black African" = "Black/African/Caribbean/Black British - African",
      "Black Caribbean" = "Black/African/Caribbean/Black British - Caribbean",
      "Other Black" = "Black/African/Caribbean/Black British - Any other Black/African/Caribbean background",
      "White & Black Caribbean" = "Mixed/Multiple ethnic groups - White and Black Caribbean",
      "White & Black African" = "Mixed/Multiple ethnic groups - White and Black African",
      "White & Asian" = "Mixed/Multiple ethnic groups - White and Asian",
      "Other Mixed" = "Mixed/Multiple ethnic groups - Any other Mixed/Multiple ethnic background",
      "Indian" = "Asian/Asian British - Indian",
      "Pakistani" = "Asian/Asian British - Pakistani",
      "Bangladeshi" = "Asian/Asian British - Bangladeshi",
      "Other Asian" = "Asian/Asian British - Any other Asian background",
      "Chinese" = "Asian/Asian British - Chinese",
      "Arab" = "Other ethnic group - Any other ethnic group",
      "Other Ethnic Group" = "Other ethnic group - Any other ethnic group"
    ),
    sex = str_to_lower(sex)
  ) %>% 
  rename(people = `2020`) %>% 
  count(borough, sex, age_range, self_defined_ethnicity, wt = people, name = "people") %>% 
  write_rds(here::here("analysis-data/people-by-borough.rds"))



# GET HISTORICAL STOPS DATA ----------------------------------------------------

download.file(
  url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/841261/stop-search-open-data-tables-ppp.ods",
  destfile = here::here("original-data/historical_stops_data.ods")
)

hist_data <- here::here("original-data/historical_stops_data.ods") %>% 
  readODS::read_ods(sheet = "S&S_OD") %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  filter(
    force_name %in% 
      c("London, City of", "Metropolitan Police", "British Transport Police")
  ) %>% 
  mutate(
    searches = as.integer(searches)
  ) %>% 
  replace_na(list(searches = NA_integer_)) %>% 
  count(financial_year, force_name, reason_for_search_arrest, wt = searches) %>% 
  mutate(
    financial_year = 
      lubridate::ymd(str_glue("{str_sub(financial_year, end = 4)}-04-01"))
  ) %>% 
  filter(financial_year >= lubridate::ymd("2009-04-01")) %>% 
  write_rds(here::here("analysis-data/historical-stop-data.rds"))



# GET DEPRIVATION --------------------------------------------------------------

download.file(
  url = "https://data.london.gov.uk/download/indices-of-deprivation/9ee0cf66-e6f9-4e38-8eec-79c1d897e248/ID%202019%20for%20London.xlsx",
  destfile = here::here("original-data/deprivation_data.xlsx")
)

here::here("original-data/deprivation_data.xlsx") %>% 
  readxl::read_excel(
    sheet = "IMD 2019", 
    range = cellranger::cell_cols("A:AB")
  ) %>% 
  janitor::clean_names() %>% 
  select(
    lsoa_code = lsoa_code_2011, 
    imd_score = index_of_multiple_deprivation_imd_score
  ) %>% 
  mutate(imd_perc = percent_rank(desc(imd_score))) %>% 
  write_rds(here::here("analysis-data/imd.rds"))



# GET STATIONS DATA ------------------------------------------------------------

stations_data <- getbb("London, UK") %>% 
  opq() %>% 
  add_osm_feature(key = "railway", value = "station") %>% 
  osmdata_sf() %>% 
  pluck("osm_points") %>% 
  select(old_name = name, geometry) %>% 
  filter(!str_detect(old_name, "^Gates ")) %>% 
  mutate(
    name = old_name %>% 
      str_remove(" Thameslink$") %>% 
      str_remove("^London ") %>% 
      str_remove(" (Central|East|North|South|West)$") %>% 
      str_remove(" Terminal .+?$") %>% 
      str_remove(" DLR$") %>% 
      str_remove(" Station$") %>% 
      str_remove(" for .+?$") %>% 
      str_remove("\\b\\(+?\\)\\b") %>% 
      str_squish() %>% 
      recode(
        "Abbey Road" = "West Ham",
        "Acton Main Line" = "Acton",
        "Bridge" = "London Bridge",
        "Caledonian Road & Barnsbury" = "Caledonian Road",
        "East Croydon" = "Croydon Town Centre",
        "Edgware Road (Bakerloo line)" = "Edgware Road",
        "Edgware Road (Circle, District and Hammersmith & City lines)" = "Edgware Road",
        "Finchley Road & Frognal" = "Finchley Road",
        "Green Park" = "Piccadilly",
        "King's Cross St Pancras" = "King's Cross",
        "Maryland" = "Stratford",
        "Piccadilly Circus" = "Piccadilly",
        "Russell Square" = "Bloomsbury",
        "St. Pancras International" = "King's Cross",
        "Shoreditch High Street" = "Shoreditch",
        "Stratford International" = "Stratford",
        "Tower Gateway" = "Tower Hill",
        "West Croydon" = "Croydon Town Centre",
        "Willesden Junction" = "Harlesden"
      ),
    name = case_when(
      old_name %in% c("Clapham Common", "Clapham High Street", "Clapham North") ~ 
        "Clapham High St",
      old_name %in% c("Peckham Rye", "Queens Road Peckham") ~ "Peckham",
      TRUE ~ name
    )
  ) %>% 
  select(name) %>%
  st_write(here::here("analysis-data/stations.gpkg"))
