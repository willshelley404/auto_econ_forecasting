library(data.table)
library(tidyverse)
library(lubridate)
library(timetk)
library(fredr)
library(tidymodels)

# library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

 # Helper packages
# library(readr)       # for importing data
# library(broom.mixed) # for converting bayesian models to tidy tibbles
# library(dotwhisker)  # for visualizing regression results


# Set FRED API key
fredr_set_key("8a87821141de832dac33ecf6350d5e71")



# #  BRENT Crude Oil Price ----
value_reg <- fredr(series_id = "MCOILBRENTEU") %>% # MCOILBRENTEU
  dplyr::select(-2) %>% #filter(date > '1999-12-01') %>% 
  select(-realtime_start, -realtime_end) %>% 
  rename(value = value)

value_reg %>% tail()


value_reg <- value_reg %>%
  mutate(value = as.numeric(as.character(value))) %>%
  tk_augment_leads(value, .lags = 1:3) %>%
  na.omit()


# value_reg %>% 
#   plot_acf_diagnostics(
#     .date_var = date, .value = value,
#   )
# 
# value_reg %>% 
#   plot_time_series(date, value, .smooth = F)
# 
# value_reg %>% 
#   plot_acf_diagnostics(date, value)

forecast_data <- value_reg %>% 
  mutate(year = year(date),
         value_inv1 = 1/value_lag1)


# forecast_data_clean <- forecast_data %>%
#   time_decompose(sum_units, method = "stl") %>%
#   anomalize(remainder, method = "iqr") %>%
#   clean_anomalies() %>% 
#   select(date, observed_cleaned) %>% 
#   rename(sum_units = observed_cleaned)

forecast_data %>% tail()
# library(MASS)
# Fit the full model 
full.model <- lm(log(value) ~ . -date,  forecast_data)
# Stepwise regression model
step.model <- MASS::stepAIC(full.model, direction = "backward", 
                            trace = FALSE)
summary(step.model)
#summary(full.model)

# detach("package:MASS")

car::vif(step.model)


# Getting the exogenous variable names of interest
df <- sweep::sw_tidy(step.model) %>% 
  select(term) #%>% 
# rownames_to_column() %>% 
# mutate(rowname = as.integer(rowname))

exogenous_vars <- df %>% 
  # filter(!str_detect(term, "Month")) %>% 
  # filter(!str_detect(term, "Year")) %>% 
  filter(!str_detect(term, "(Intercept)"))  %>% 
  rownames_to_column() %>% 
  spread(term, value = rowname)


names(exogenous_vars)

# Joining back to xreg exogenous vars 
xreg_vars <-  forecast_data %>% 
  select(date, value, value_lag1, value_lag2, value_lag3, value_inv1)

xreg_vars %>% tail()



# Start ----

xreg_vars %>% tail()

last_date <- xreg_vars %>% 
  data.table::last(1) %>% 
  select(date)


next_date <- last_date$date %+time% "1 month"



lm_mod <- linear_reg() %>% 
  set_engine("lm")


lm_fit <- lm_mod %>% fit(value ~ value_lag1 + value_lag2  + value_inv1, data = xreg_vars)

# tidy(lm_fit)
# 
# 
# tidy(lm_fit) %>% 
#   dwplot(dot_args = list(size = 2, color = "black"),
#          whisker_args = list(color = "black"),
#          vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))



new_points <- xreg_vars %>% 
  select(value) %>%
  data.table::last(3) %>% 
  rowid_to_column() %>% 
  pivot_wider(values_from = value, names_from = rowid) %>% 
  rename(value_lag1 = `3`,
         value_lag2 = `2`,
         value_lag3 = `1`) %>% 
  add_column(year = year(max(xreg_vars$date) %+time% "1 month")) %>% 
  mutate(value_inv1 = 1/value_lag1) %>%
  select(year, value_lag1, value_lag2, value_lag3, value_inv1)




mean_pred <- predict(lm_fit, new_data = new_points)
new_preds <- mean_pred %>% 
  add_column(date = next_date) %>% 
  rename(value = .pred) %>% 
  select(date, value)



output <- value_reg %>% 
  select(date, value) %>% 
  rbind(new_preds) 


# next iteration -----
for (i in 1:60) {
  
  new_points <-  output %>% 
    select(value) %>%
    data.table::last(3) %>% 
    rowid_to_column() %>% 
    pivot_wider(values_from = value, names_from = rowid) %>% 
    rename(value_lag1 = `3`,
           value_lag2 = `2`,
           value_lag3 = `1`) %>% 
    add_column(year = year(max(xreg_vars$date) %+time% "1 month")) %>%
    mutate(value_inv1 = 1/value_lag1) %>%
    select(year, value_lag1, value_lag2, value_lag3, value_inv1)
  
  
  # create lags from new data
  
  data <- output %>% 
    tk_augment_leads(value, .lags = 1:3) %>%
    na.omit() %>% 
    mutate(year = year(date),
           value_inv1 = 1/value_lag1)  # ADD NEGATIVE IF NEEDED----
  
  # fit new mod
  lm_fit <- lm_mod %>% fit(value ~ value_lag1 + value_lag2  + value_inv1 , data = data)
  
  
  last_date <- data %>% 
    data.table::last(1) %>% 
    select(date)
  
  
  next_date <- last_date$date %+time% "1 month"
  
  
  
  
  mean_pred <- predict(lm_fit, new_data = new_points)
  new_preds <- mean_pred %>% 
    add_column(date = next_date) %>% 
    rename(value = .pred) %>% 
    select(date, value)
  
  
  
  output <- output %>% 
    select(date, value) %>% 
    rbind(new_preds) 
  
  output %>% tail()
}

output %>% 
  mutate(
    horizon = if_else(date >= today(),  "forecast", "actual")
  ) %>% 
  plot_time_series(date, value, .color_var = horizon, .smooth = F)

#view(output)
# end ----

brent <- output %>% 
  rename(brent = value)
brent



