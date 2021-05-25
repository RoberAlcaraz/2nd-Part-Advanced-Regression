# DATA CLEANING ----------------------------------------------------------------
spain <- covid19(country = "spain", start = "2020-01-22", end = "2021-05-02")
spain_new_data <- covid19(country = "spain", start = "2021-05-02", end = "2021-05-23")

# Selecting variables
spain <- spain %>%
  dplyr::select(date, deaths, confirmed, vaccines, hosp, icu, stay_home_restrictions,
                school_closing, workplace_closing, transport_closing, gatherings_restrictions,
                internal_movement_restrictions)
spain <- spain[, -1]

spain_new_data <- spain_new_data %>%
  dplyr::select(date, deaths, confirmed, vaccines, hosp, icu, stay_home_restrictions,
                school_closing, workplace_closing, transport_closing, gatherings_restrictions,
                internal_movement_restrictions)
spain_new_data <- spain_new_data[, -1]

# Dealing with missing values
spain <- spain %>%
  mutate(
    deaths = replace(deaths, is.na(deaths), 0),
    confirmed = replace(confirmed, is.na(confirmed), 0)
  )
spain$vaccines[1:348] <- 0

# After that, we should modify our categorical variables, since they are encoded
# as integers
spain <- spain %>%
  mutate(
    stay_home = factor(stay_home_restrictions, 
                       levels = c(0, 1, 2),
                       labels = c("no", "recommended", "mandatory")),
    
    school_closing = factor(school_closing, 
                            levels = c(0, 1, 2, 3),
                            labels = c("no", "recommended", "require", "mandatory")),
    
    workplace_closing = factor(workplace_closing, 
                               levels = c(0, 1, 2, 3),
                               labels = c("no", "recommended", "require", "mandatory")),
    
    transport_closing = factor(transport_closing, 
                               levels = c(0, 1),
                               labels = c("no", "recommended")),
    
    gatherings_restrictions = factor(gatherings_restrictions, 
                                     levels = c(0, 1, 2, 3, 4),
                                     labels = c("no", "no", "no", "10-100", "<10")),
    
    internal_movement_restrictions = factor(internal_movement_restrictions, 
                                            levels = c(0, 1, 2),
                                            labels = c("no", "recommended", "mandatory")),
  ) %>%
  dplyr::select(-stay_home_restrictions)

spain_new_data <- spain_new_data %>%
  mutate(
    stay_home = factor(stay_home_restrictions, 
                       levels = c(0, 1, 2),
                       labels = c("no", "recommended", "mandatory")),
    
    school_closing = factor(school_closing, 
                            levels = c(0, 1, 2, 3),
                            labels = c("no", "recommended", "require", "mandatory")),
    
    workplace_closing = factor(workplace_closing, 
                               levels = c(0, 1, 2, 3),
                               labels = c("no", "recommended", "require", "mandatory")),
    
    transport_closing = factor(transport_closing, 
                               levels = c(0, 1),
                               labels = c("no", "recommended")),
    
    gatherings_restrictions = factor(gatherings_restrictions, 
                                     levels = c(0, 1, 2, 3, 4),
                                     labels = c("no", "no", "no", "10-100", "<10")),
    
    internal_movement_restrictions = factor(internal_movement_restrictions, 
                                            levels = c(0, 1, 2),
                                            labels = c("no", "recommended", "mandatory")),
  ) %>%
  dplyr::select(-stay_home_restrictions)

# For all days that has a lower value than their previous day, we will assign the
# value of their previous day to that day.
for (i in 1:(nrow(spain)-1)){
  if (spain$deaths[i+1] < spain$deaths[i]) spain$deaths[i+1] <- spain$deaths[i]
  if (spain$confirmed[i+1] < spain$confirmed[i]) spain$confirmed[i+1] <- spain$confirmed[i]
}

# Now with the vaccines
for (i in 1:nrow(spain)-1){
  if (is.na(spain$vaccines[i+1])) spain$vaccines[i+1] <- spain$vaccines[i]
}
spain <- spain %>%
  dplyr::select(-hosp, -icu)

spain_new_data$vaccines[1] <- tail(spain$vaccines)[1] # we take the data from the previous day
for (i in 1:nrow(spain_new_data)-1){
  if (is.na(spain_new_data$vaccines[i+1])) spain_new_data$vaccines[i+1] <- spain_new_data$vaccines[i]
}

spain_new_data <- spain_new_data %>%
  dplyr::select(-hosp, -icu)

spain_new_data$vaccines[1] <- 10784997 # we take the data from the previous day
for (i in 1:nrow(spain_new_data)-1){
  if (is.na(spain_new_data$vaccines[i+1])) spain_new_data$vaccines[i+1] <- spain_new_data$vaccines[i]
}

spain_new_data <- spain_new_data %>%
  dplyr::select(-hosp, -icu)

spain$stay_home[278:nrow(spain)] <- "recommended"
spain_new_data$stay_home <- "recommended"


# Feature engineering
calculate_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

spain <- spain %>%
  mutate(
    new_date = as.Date(cut(date, "week")),
    deaths_day = spain$deaths - lag(spain$deaths),
    confirmed_day = spain$confirmed - lag(spain$confirmed),
    vaccines_day = spain$vaccines - lag(spain$vaccines)
  ) %>%
  group_by(date) %>%
  summarise(
    new_date = new_date,
    date = date,
    deaths_week = sum(deaths_day),
    confirmed_week = sum(confirmed_day),
    vaccines_week = sum(vaccines_day),
    school_closing = calculate_mode(school_closing),
    workplace_closing = calculate_mode(workplace_closing),
    gatherings_restrictions = calculate_mode(gatherings_restrictions),
    stay_home = calculate_mode(stay_home),
    internal_movement_restrictions = calculate_mode(internal_movement_restrictions)
  ) %>%
  ungroup() %>%
  filter(date %in% new_date) %>%
  dplyr::select(-new_date)

spain_new_data <- spain_new_data %>%
  mutate(
    new_date = as.Date(cut(date, "week")),
    deaths_day = spain_new_data$deaths - lag(spain_new_data$deaths),
    confirmed_day = spain_new_data$confirmed - lag(spain_new_data$confirmed),
    vaccines_day = spain_new_data$vaccines - lag(spain_new_data$vaccines)
  ) %>%
  group_by(date) %>%
  summarise(
    new_date = new_date,
    date = date,
    deaths_week = sum(deaths_day),
    confirmed_week = sum(confirmed_day),
    vaccines_week = sum(vaccines_day),
    school_closing = calculate_mode(school_closing),
    workplace_closing = calculate_mode(workplace_closing),
    gatherings_restrictions = calculate_mode(gatherings_restrictions),
    stay_home = calculate_mode(stay_home),
    internal_movement_restrictions = calculate_mode(internal_movement_restrictions)
  ) %>%
  ungroup() %>%
  filter(date %in% new_date) %>%
  dplyr::select(-new_date)

spain <- spain %>%
  mutate(
    month = factor(month(date)),
    year = factor(year(date))
  )

spain_new_data <- spain_new_data %>%
  mutate(
    month = factor(month(date)),
    year = factor(year(date))
  )

spain <- spain %>%
  mutate(
    lag_3_deaths_week = lag(deaths_week, n = 3),
    lag_3_confirmed_week = lag(confirmed_week, n = 3)
  ) %>%
  drop_na()

spain_new_data <- spain_new_data %>%
  mutate(
    lag_3_deaths_week = tail(spain$deaths_week, 3),
    lag_3_confirmed_week = tail(spain$confirmed_week, 3)
  )

# saveRDS(spain, "../2nd-Part-Advanced-Regression/00_data/spain.RDS")
# saveRDS(spain_new_data, "../2nd-Part-Advanced-Regression/00_data/spain_new_data.RDS")
