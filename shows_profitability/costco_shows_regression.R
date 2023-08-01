library(tidyverse)
library(lubridate)
library(tsibble)
library(broom)

### SHOWS DATA PREP

shows <- read.csv("shows_profitability/shows.csv")

# convert to date
shows$start_date <- ymd(shows$start_date)
shows$end_date <- ymd(shows$end_date)
shows$venue_number <- sub("^0+", "", shows$venue_number)

# transpose data and add date records in between
shows_transposed <- shows %>%
  mutate(date = map2(start_date, end_date, seq, by = "day")) %>%
  unnest(date)

# selects shows columns and create flag
shows_fin <- shows_transposed %>%
  select(venue_number, date) %>%
  mutate(event_flag = 1) %>%
  rename("warehouse_code" = "venue_number") %>%
  arrange(date)



### SALES DATA PREP

sales <- read.csv("shows_profitability/sales.csv")

sales <- sales %>%
  select(-party_name)

sales$date <- ymd(sales$date)
sales$warehouse_code <- as.character(sales$warehouse_code)


   



# join data and create final df
final_tbl <- sales %>%
  left_join(shows_fin) %>%
  mutate(event_flag = ifelse(is.na(event_flag), 0, event_flag)) %>%
  mutate(weekday = weekdays(date))



# stores with event

event_stores <- final_tbl %>%
  filter(event_flag == 1) %>%
  select(warehouse_code) %>%
  distinct()
  

final_tbl_2 <- final_tbl %>%
  inner_join(event_stores)



# REGRESSION #

# without interaction - regress sales on event and day of week
summary(lm(sales ~ event_flag + weekday, final_tbl_2))

# reference group = Friday

# intercept = on average, Friday with no event we would expect 534 dollars in sales

# event flag
  # on average, sales are higher by 1867 when we have an event





##### IDEA #####
# run for each warehouse
# grab event flag coefficient and put in table
# sort based on lift


fitted_models <- final_tbl_2 %>%
  group_by(warehouse_code) %>%
  do(model = lm(sales ~ event_flag, data = .))

extract_coefficients <- function(fitted_models) {
  coefficients <- sapply(fitted_models, `[`, 1)  # Modify this line to extract the desired coefficient(s)
  return(coefficients)
}

coefficients_by_group <- tapply(fitted_models$model, fitted_models$warehouse_code, extract_coefficients)


coefficents_df <- data.frame(t(data.frame(coefficients_by_group)))


coefficents_df <- tibble::rownames_to_column(coefficents_df, "warehouse")

coefficents_df <- coefficents_df %>% 
  mutate(across(c('warehouse'), substr, 2, nchar(warehouse)))
  


