# This script produces stop-and-search reports for Metropolitan Police basic
# command units

library(lubridate)
library(tidyverse)

# Get date of most-recent data
stops_max_date <- here::here("analysis-data/stops.rds") %>% 
  read_rds() %>% 
  pluck("date") %>% 
  max() %>% 
  as_date() %>% 
  ceiling_date(unit = "months") - days(1)

# Produce report for each BCU
walk(
  c("Central South"), 
  function (x) {
    message(str_glue("\n\n\n\nGENERATING REPORT FOR {str_to_upper(x)} BCU\n"))
    rmarkdown::render(
      input = here::here(str_glue(
        "output/{year(stops_max_date)}-Q{quarter(stops_max_date)}-bcu.Rmd"
      )),
      output_format = "pdf_document",
      output_file = here::here(str_glue(
        "stop-and-search-report-", str_replace_all(str_to_lower(x), " ", "-"), 
        "-{year(stops_max_date)}-q{quarter(stops_max_date)}.pdf"
      )),
      output_dir = here::here(
        str_glue("output/{year(stops_max_date)}_q{quarter(stops_max_date)}/")
      ),
      clean = FALSE,
      params = list(bcu = x)
    )
  }
)
