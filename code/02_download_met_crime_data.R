library("lubridate")
library("tidyverse")

ward_violence <- read_csv("https://data.london.gov.uk/download/recorded_crime_summary/866c05de-c5cd-454b-8fe5-9e7c77ea2313/MPS%20Ward%20Level%20Crime%20%28most%20recent%2024%20months%29.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(starts_with("x"), names_to = "month", names_prefix = "x", values_to = "crimes") %>% 
  mutate(month = ymd(str_glue("{month}01")))

ward_violence %>% 
  filter(
    minor_text == "Violence with Injury", 
    between(month, max(.$month) - months(2), max(.$month))
  ) %>% 
  count(look_up_borough_name, ward_name, ward_code, wt = crimes)
