library(fredr)
library(dplyr)
library(ggplot2)
library(BVAR)
library(vars)
library(forecast)
library(tseries)
library(lubridate)
library(xgboost)
library(rstan)
library(prophet)
library(tidyverse)
library(zoo)

#Set Seed and FRED API key
fredr_set_key("4798dcaec8ee6e1a4747d112dd3adfaf")
set.seed(114)

#Create Necessary Functions
fit_arima_forecast <- function(data, variable_name, max_p = 1, max_q = 1, forecast_horizon = 12) {
  arma_model <- auto.arima(data[[variable_name]], max.p = max_p, max.q = max_q)
  forecast_result <- forecast(arma_model, h = forecast_horizon)
  forecast_df <- as.data.frame(forecast_result)
  forecast_df <- forecast_df %>%
    mutate(
      Variable = variable_name,
      Date = seq(
        from = max(data$date) + months(3),
        by = "quarter",
        length.out = forecast_horizon
      )
    )
  return(forecast_df)
}

#Gather Macroeconomic Data 
cpi_data <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1999-01-01"),
  observation_end = as.Date("2024-01-01"),
  frequency = "q"
) %>%
  arrange(date) %>%
  mutate(
    inflation_qoq = 100 * (value / lag(value) - 1),  
    inflation_yoy = 100 * (value / lag(value, 4) - 1) 
  )
unemp_data <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2024-01-01"),
  frequency = "q"
)
gdp_data <- fredr(
  series_id = "GDPC1",
  observation_start = as.Date("1999-01-01"),
  observation_end = as.Date("2024-01-01"),
  frequency = "q"
)
breakeven_inflation_data <- fredr(
  series_id = "T10YIE",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2024-01-01"),
  frequency = "q"
)

individual_cpi <- read.csv("C:/Users/Jeeshan Huq/Downloads/2024-Capstone-main/Edited_Individual_CPI.csv")
individual_cpi$date <- as.Date(individual_cpi$date, format = "%Y-%m-%d")
individual_cpi <- individual_cpi %>%
  filter(date >= as.Date("2000-01-01") & date <= as.Date("2024-01-01"))
individual_cpi <- individual_cpi %>%
  mutate(across(starts_with("CPI"), ~ as.numeric(gsub("[^0-9.]", "", as.character(.)))))
cpi_long <- individual_cpi %>%
  pivot_longer(cols = starts_with("CPI"), names_to = "CPI_Type", values_to = "CPI_Value")
individual_cpi_agg <- cpi_long %>%
  group_by(date) %>%
  summarize(
    cpi_exp_mean = mean(CPI_Value, na.rm = TRUE),
    cpi_exp_median = median(CPI_Value, na.rm = TRUE),
    cpi_exp_sd = sd(CPI_Value, na.rm = TRUE)
  )

individual_unemp <- read.csv("C:/Users/Jeeshan Huq/Downloads/2024-Capstone-main/Edited_Individual_UNEMP.csv")
individual_unemp$date <- as.Date(individual_unemp$date, format = "%Y-%m-%d")
individual_unemp <- individual_unemp %>%
  filter(date >= as.Date("2000-01-01") & date <= as.Date("2024-01-01"))
individual_unemp <- individual_unemp %>%
  mutate(across(starts_with("UNEMP"), ~ as.numeric(gsub("[^0-9.]","", as.character(.)))))
unemp_long <- individual_unemp %>%
  pivot_longer(cols = starts_with("UNEMP"), names_to = "UNEMP_Type", values_to = "UNEMP_Value")
individual_unemp_agg <- unemp_long %>%
  group_by(date) %>%
  summarize(
    unemp_exp_mean = mean(UNEMP_Value, na.rm = TRUE),
    unemp_exp_median = median(UNEMP_Value, na.rm = TRUE),
    unemp_exp_sd = sd(UNEMP_Value, na.rm = TRUE)
  )

individual_gdp <- read.csv("C:/Users/Jeeshan Huq/Downloads/2024-Capstone-main/Edited_Individual_RGDP.csv")
individual_gdp$date <- as.Date(individual_gdp$date, format = "%Y-%m-%d")
individual_gdp <- individual_gdp %>%
  filter(date >= as.Date("2000-01-01") & date <= as.Date("2024-01-01"))
individual_gdp <- individual_gdp %>%
  mutate(across(starts_with("RGDP"), ~ as.numeric(gsub("[^0-9.]","", as.character(.)))))
gdp_long <- individual_gdp %>%
  pivot_longer(cols = starts_with("RGDP"), names_to = "RDGP_Type", values_to = "RGDP_Value")
individual_gdp_agg <- gdp_long %>%
  group_by(date) %>%
  summarize(
    rgdp_exp_mean = mean(RGDP_Value, na.rm = TRUE),
    rgdp_exp_median = median(RGDP_Value, na.rm = TRUE),
    rgdp_exp_sd = sd(RGDP_Value, na.rm = TRUE)
  )

individual_pce <- read.csv("C:/Users/Jeeshan Huq/Downloads/2024-Capstone-main/Edited_Individual_PCE10.csv")
individual_pce$date <- as.Date(individual_pce$date, format = "%Y-%m-%d")
individual_pce <- individual_pce %>%
  filter(date >= as.Date("2000-01-01") & date <= as.Date("2024-01-01"))
individual_pce <- individual_pce %>%
  mutate(across(starts_with("PCE"), ~ as.numeric(gsub("[^0-9.]", "", as.character(.)))))
pce_long <- individual_pce %>%
  pivot_longer(cols = starts_with("PCE"), names_to = "PCE_Type", values_to = "PCE_Value")
individual_pce_agg <- pce_long %>%
  group_by(date) %>%
  summarize(
    pce_exp_mean = mean(PCE_Value, na.rm = TRUE),
    pce_exp_median = median(PCE_Value, na.rm = TRUE),
    pce_exp_sd = sd(PCE_Value, na.rm = TRUE)
  )

macro_data <- cpi_data %>%
  rename(cpi = value, date = date) %>%
  left_join(unemp_data %>% rename(unemployment = value), by = "date") %>%
  left_join(gdp_data %>% rename(gdp = value), by = "date") %>%
  left_join(breakeven_inflation_data %>% rename(breakeven_inflation = value), by = "date") %>%
  na.omit()
macro_data$date <- as.Date(macro_data$date)
comparison_data <- macro_data %>%
  left_join(individual_cpi_agg, by = "date") %>%
  left_join(individual_unemp_agg, by = "date") %>%
  left_join(individual_gdp_agg, by = "date") %>%
  left_join(individual_pce_agg, by = "date") %>%
  na.omit()

#plot comparison between the real data and predictions.
ggplot(comparison_data, aes(x = date)) +
  geom_line(aes(y = inflation_qoq, color = "Actual CPI")) +
  geom_line(aes(y = cpi_exp_median, color = "Forecasted CPI Median")) +
  labs(title = "Actual CPI vs Forecasted CPI Median",
       x = "Date",
       y = "CPI",
       color = "Legend") +
  theme_minimal()

ggplot(comparison_data, aes(x = date)) +
  geom_line(aes(y = unemployment, color = "Actual Unemployment")) +
  geom_line(aes(y = unemp_exp_median,color = "Forecasted Unemployment Median")) +
  labs(title = "Actual Unemployment vs Forecasted Unemployment Median",
       x = "Date",
       y = "Unemployment",
       color = "Legend") +
  theme_minimal()

ggplot(comparison_data, aes(x = date)) +
  geom_line(aes(y = breakeven_inflation, color = "Actual Breakeven Inflation")) +
  geom_line(aes(y = pce_exp_median,color = "Forecasted PCE Inflation Median")) +
  labs(title = "Actual PCE vs Forecasted Inflation Median",
       x = "Date",
       y = "Inflation",
       color = "Legend") +
  theme_minimal()

ggplot(comparison_data, aes(x = date)) +
  geom_line(aes(y = gdp, color = "Actual GDP")) +
  geom_line(aes(y = rgdp_exp_median,color = "Forecasted GDP Median")) +
  labs(title = "Actual GDP vs Forecasted GDP Median",
       x = "Date",
       y = "GDP",
       color = "Legend") +
  theme_minimal()


#check for Bias in the forecast
mincer_model_cpi <- lm(inflation_qoq ~ cpi_exp_median, data = comparison_data)
summary(mincer_model_cpi)
mincer_model_gdp <- lm(gdp ~ rgdp_exp_median, data = comparison_data)
summary(mincer_model_gdp)
mincer_model_unemp <- lm(unemployment ~ unemp_exp_median, data = comparison_data)
summary(mincer_model_unemp)
mincer_model_pce <- lm(breakeven_inflation ~ pce_exp_median, data = comparison_data)
summary(mincer_model_pce)

#RMSE and MAE
comparison_data <- comparison_data %>%
  mutate(cpi_forecast_error = inflation_qoq - cpi_exp_median) %>%
  mutate(gdp_forecast_error = gdp - rgdp_exp_median) %>%
  mutate(unemp_forecast_error = unemployment - unemp_exp_median) %>%
  mutate(inflation_forecast_error = breakeven_inflation - pce_exp_median)
rmse_cpi <- sqrt(mean(comparison_data$cpi_forecast_error^2, na.rm = TRUE))
mae_cpi <- mean(abs(comparison_data$cpi_forecast_error), na.rm = TRUE)
rmse_unemp <- sqrt(mean(comparison_data$unemp_forecast_error^2, na.rm = TRUE))
mae_unemp <- mean(abs(comparison_data$unemp_forecast_error), na.rm = TRUE)
rmse_gdp <- sqrt(mean(comparison_data$gdp_forecast_error^2, na.rm = TRUE))
mae_gdp <- mean(abs(comparison_data$gdp_forecast_error), na.rm = TRUE)
rmse_inflation<- sqrt(mean(comparison_data$inflation_forecast_error^2, na.rm = TRUE))
mae_inflation <- mean(abs(comparison_data$inflation_forecast_error), na.rm = TRUE)
cat("RMSE for CPI:", rmse_cpi, "\n")
cat("MAE for CPI:", mae_cpi, "\n")
cat("RMSE for Unemployment:", rmse_unemp, "\n")
cat("MAE for Unemployment:", mae_unemp, "\n")
cat("RMSE for GDP:", rmse_gdp, "\n")
cat("MAE for GDP:", mae_gdp, "\n")
cat("RMSE for Inflation:", rmse_inflation, "\n")
cat("MAE for Inflation:", mae_inflation, "\n")
error_metrics <- tibble(
  Variable = c("CPI", "Unemployment", "GDP", "Inflation"),
  RMSE = c(rmse_cpi, rmse_unemp, rmse_gdp, rmse_inflation),
  MAE = c(mae_cpi, mae_unemp, mae_gdp, mae_inflation)
)

#VAR Modeling
var_data <- comparison_data %>%
  select(inflation_qoq, unemployment, gdp, breakeven_inflation, cpi_exp_median, unemp_exp_median, rgdp_exp_median,pce_exp_median, date) %>%
  na.omit()
var_data$date <- as.Date(var_data$date)
lag_order <- 2
var_model <- VAR(var_data, p = lag_order)
summary(var_model)
var_forecast <- predict(var_model, n.ahead = 12)
plot(var_forecast)
var_residuals <- residuals(var_model)
residuals_df <- as.data.frame(var_residuals)
residuals_df$date <- comparison_data$date[1:nrow(residuals_df)]
residuals_long <- residuals_df %>%
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Residual")
par(mfrow = c(1, 1))
for (i in 1:ncol(var_residuals)) {
  var_name <- colnames(var_residuals)[i]
  plot(var_residuals[, i], type = "l", main = paste("Residuals:", var_name),
       xlab = "Time", ylab = "Residuals")
  ylim_range <- range(var_residuals[, i]) * 0.5 
  coord_cartesian(ylim = ylim_range)
}
ggplot(residuals_long, aes(x = date, y = Residual, color = Variable)) +
  geom_line() +
  labs(title = "VAR Model Residuals",
       x = "Date",
       y = "Residuals") +
  theme_minimal() +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1)

#Boosting Data
boost_data <- var_data
x_data <- as.matrix(boost_data %>% select(-inflation_yoy))
y_data <- boost_data$inflation_yoy
set.seed(114)
train_index <- sample(1:nrow(boost_data), 0.8 * nrow(boost_data))
train_data <- x_data[train_index, ]
train_label <- y_data[train_index]
test_data <- x_data[-train_index, ]
test_label <- y_data[-train_index]
boost_model <- xgboost(data = train_data,
                       label = train_label,
                       nrounds = 100,
                       objective = "reg:squarederror",
                       eta = 0.1,
                       max_depth = 3)
importance_matrix <- xgb.importance(model = boost_model)
xgb.plot.importance(importance_matrix)

x_data <- as.matrix(boost_data %>% select(-unemployment))
y_data <- boost_data$unemployment
set.seed(114)
train_index <- sample(1:nrow(boost_data), 0.8 * nrow(boost_data))
train_data <- x_data[train_index, ]
train_label <- y_data[train_index]
test_data <- x_data[-train_index, ]
test_label <- y_data[-train_index]
boost_model <- xgboost(data = train_data,
                       label = train_label,
                       nrounds = 100,
                       objective = "reg:squarederror",
                       eta = 0.1,
                       max_depth = 3)
importance_matrix <- xgb.importance(model = boost_model)
xgb.plot.importance(importance_matrix)

x_data <- as.matrix(boost_data %>% select(-gdp))
y_data <- boost_data$gdp
set.seed(114)
train_index <- sample(1:nrow(boost_data), 0.8 * nrow(boost_data))
train_data <- x_data[train_index, ]
train_label <- y_data[train_index]
test_data <- x_data[-train_index, ]
test_label <- y_data[-train_index]
boost_model <- xgboost(data = train_data,
                       label = train_label,
                       nrounds = 100,
                       objective = "reg:squarederror",
                       eta = 0.1,
                       max_depth = 3)
importance_matrix <- xgb.importance(model = boost_model)
xgb.plot.importance(importance_matrix)

boost_data <- var_data
x_data <- as.matrix(boost_data %>% select(-breakeven_inflation))
y_data <- boost_data$breakeven_inflation
set.seed(114)
train_index <- sample(1:nrow(boost_data), 0.8 * nrow(boost_data))
train_data <- x_data[train_index, ]
train_label <- y_data[train_index]
test_data <- x_data[-train_index, ]
test_label <- y_data[-train_index]
boost_model <- xgboost(data = train_data,
                       label = train_label,
                       nrounds = 100,
                       objective = "reg:squarederror",
                       eta = 0.1,
                       max_depth = 3)
importance_matrix <- xgb.importance(model = boost_model)
xgb.plot.importance(importance_matrix)


#ARMA Modeling
arma_model_cpi <- auto.arima(comparison_data$inflation_yoy, max.p = 1, max.q = 1)
summary(arma_model_cpi)
cpi_forecast_arma <- forecast(arma_model_cpi, h = 12)
plot(cpi_forecast_arma, main = "ARMA(1,1) Forecast for CPI")
arma_model_unemp <- auto.arima(comparison_data$unemployment, max.p = 1, max.q = 1)
summary(arma_model_unemp)
unemp_forecast_arma <- forecast(arma_model_unemp, h = 12)
plot(unemp_forecast_arma, main = "ARMA(1,1) Forecast for Unemployment Rate")
arma_model_gdp <- auto.arima(comparison_data$gdp, max.p = 1, max.q = 1)
summary(arma_model_gdp)
gdp_forecast_arma <- forecast(arma_model_gdp, h = 12)
plot(gdp_forecast_arma, main = "ARMA(1,1) Forecast for GDP")
cpi_forecast_df <- fit_arima_forecast(comparison_data, "inflation_yoy")
unemp_forecast_df <- fit_arima_forecast(comparison_data, "unemployment")
gdp_forecast_df <- fit_arima_forecast(comparison_data, "gdp")
inflation_forecast_df <- fit_arima_forecast(comparison_data,"breakeven_inflation")
combined_forecast_df <- bind_rows(cpi_forecast_df, unemp_forecast_df, gdp_forecast_df)
write.csv(combined_forecast_df, "C:/Users/jeesh/OneDrive/Documents/FMCapstone/combined_forecast_df.csv", row.names = FALSE)

#BVAR Model
bvar_model <- bvar(var_data_diff, lags = 2)
summary(bvar_model)

#UC-SV test
uc_sv_code <- "
data {
  int<lower=1> T;
  real y[T];
}
parameters {
  real<lower=0> sigma_trend;
  real<lower=0> sigma_cycle;
  vector[T] trend;
  vector[T] cycle;
}
model {
  sigma_trend ~ cauchy(0, 2.5);
  sigma_cycle ~ cauchy(0, 2.5);
  
  trend[1] ~ normal(0, 1);
  cycle[1] ~ normal(0, 1);
  
  for (t in 2:T) {
    trend[t] ~ normal(trend[t-1], sigma_trend);
    cycle[t] ~ normal(0, sigma_cycle);
  }
  
  for (t in 1:T) {
    y[t] ~ normal(trend[t] + cycle[t], 1);
  }
}
"
uc_sv_code_ppc <- "
data {
  int<lower=1> T;
  real y[T];
}
parameters {
  real<lower=0> sigma_trend;
  real<lower=0> sigma_cycle;
  vector[T] trend;
  vector[T] cycle;
}
model {
  sigma_trend ~ cauchy(0, 2.5);
  sigma_cycle ~ cauchy(0, 2.5);
  
  trend[1] ~ normal(0, 1);
  cycle[1] ~ normal(0, 1);
  
  for (t in 2:T) {
    trend[t] ~ normal(trend[t-1], sigma_trend);
    cycle[t] ~ normal(0, sigma_cycle);
  }
  
  for (t in 1:T) {
    y[t] ~ normal(trend[t] + cycle[t], 1);
  }
}
generated quantities {
  vector[T] y_rep;
  for (t in 1:T) {
    y_rep[t] = normal_rng(trend[t] + cycle[t], 1);
  }
}
"

stan_data_cpi <- list(T = length(var_data$inflation_yoy), y = var_data$inflation_yoy)
uc_sv_model_cpi <- stan(
  model_code = uc_sv_code,
  data = stan_data_cpi,
  iter = 20000,                     
  chains = 4,
  control = list(
    adapt_delta = 0.995,             
    max_treedepth = 15              
  )
)
uc_sv_model_cpi_ppc <- stan(
  model_code = uc_sv_code_ppc,
  data = stan_data_cpi,
  iter = 20000,
  chains = 4,
  control = list(
    adapt_delta = 0.995,
    max_treedepth = 15
  )
)
pairs(uc_sv_model_cpi, pars = c("sigma_trend", "sigma_cycle", "trend", "cycle"))
print(uc_sv_model_cpi)
stan_data_gdp <- list(T = length(comparison_data$gdp), y = comparison_data$gdp)
uc_sv_model_gdp <- stan(
  model_code = uc_sv_code,
  data = stan_data_gdp,
  iter = 20000,
  chains = 4,
  control = list(
    adapt_delta = .995,
    max_treedepth = 15
  )
)
uc_sv_model_gdp_ppc <- stan(
  model_code = uc_sv_code_ppc,
  data = stan_data_gdp,
  iter = 20000,
  chains = 4,
  control = list(
    adapt_delta = 0.995,
    max_treedepth = 15
  )
)
pairs(uc_sv_model_gdp, pars = c("sigma_trend", "sigma_cycle", "trend", "cycle"))
print(uc_sv_model_gdp)
stan_data_unemp <- list(T = length(comparison_data$unemployment), y = comparison_data$unemployment)
uc_sv_model_unemp <- stan(
  model_code = uc_sv_code,
  data = stan_data_unemp,
  iter = 20000,
  chains = 4,
  control= list(
    adapt_delta = .995,
    max_treedepth = 15
  ) 
)
uc_sv_model_unemp_ppc <- stan(
  model_code = uc_sv_code_ppc,
  data = stan_data_unemp,
  iter = 20000,
  chains = 4,
  control = list(
    adapt_delta = 0.995,
    max_treedepth = 15
  )
)
pairs(uc_sv_model_unemp, pars = c("sigma_trend", "sigma_cycle", "trend", "cycle"))
print(uc_sv_model_unemp)
stan_data_inflation <- list(T = length(comparison_data$breakeven_inflation), y = comparison_data$breakeven_inflation)
uc_sv_model_inflation <- stan(
  model_code = uc_sv_code,
  data = stan_data_inflation,
  iter = 20000,
  chains = 4,
  control= list(
    adapt_delta = .995,
    max_treedepth = 15
  ) 
)
uc_sv_model_inflation_ppc <- stan(
  model_code = uc_sv_code_ppc,
  data = stan_data_inflation,
  iter = 20000,
  chains = 4,
  control = list(
    adapt_delta = 0.995,
    max_treedepth = 15
  )
)
pairs(uc_sv_model_inflation, pars = c("sigma_trend", "sigma_cycle", "trend", "cycle"))
print(uc_sv_model_inflation)

#plots of the uc-sv regression
stan_samples_cpi <- extract(uc_sv_model_cpi)
trend_forecast_cpi <- apply(stan_samples_cpi$trend, 2, mean)
cycle_forecast_cpi <- apply(stan_samples_cpi$cycle, 2, mean)
predicted_cpi <- trend_forecast_cpi + cycle_forecast_cpi
forecast_dates_cpi <- seq(from = max(comparison_data$date) + months(3), length.out = length(predicted_cpi), by = "quarter")
forecasted_cpi_df <- data.frame(date = forecast_dates_cpi, trend = trend_forecast_cpi, cycle = cycle_forecast_cpi, cpi_forecast = predicted_cpi)
ggplot(forecasted_cpi_df, aes(x = date, y = cpi_forecast)) +
  geom_line(color = "blue") +
  labs(title = "Forecasted CPI",
       x = "Date",
       y = "Forecasted CPI (%)") +
  theme_minimal()
stan_samples_unemp <- extract(uc_sv_model_unemp)
trend_forecast_unemp <- apply(stan_samples_unemp$trend, 2, mean)
cycle_forecast_unemp <- apply(stan_samples_unemp$cycle, 2, mean)
predicted_unemp <- trend_forecast_unemp + cycle_forecast_unemp
forecast_dates_unemp <- seq(from = max(comparison_data$date) + month(3), length.out = length(predicted_unemp), by = "quarter")
forecasted_unemp_df <- data.frame(date = forecast_dates_unemp, trend = trend_forecast_unemp, cycle = cycle_forecast_unemp, unemp_forecast = predicted_unemp)
ggplot(forecasted_unemp_df, aes(x = date, y = unemp_forecast)) +
  geom_line(color = "blue") +
  labs(title = "Forecasted Unemployment",
       x = "Date",
       y = "Forecasted Unemployment (%)") + 
  theme_minimal()
stan_samples_gdp <- extract(uc_sv_model_gdp)
trend_forecast_gdp <- apply(stan_samples_gdp$trend, 2, mean)
cycle_forecast_gdp <- apply(stan_samples_gdp$cycle, 2, mean)
predicted_gdp <- trend_forecast_gdp + cycle_forecast_gdp
forecast_dates_gdp <- seq(from = max(comparison_data$date) + month(3), length.out = length(predicted_gdp), by = "quarter")
forecasted_gdp_df <- data.frame(date = forecast_dates_gdp, trend = trend_forecast_gdp, cycle = cycle_forecast_gdp, gdp_forecast = predicted_gdp)
ggplot(forecasted_gdp_df, aes(x = date, y = gdp_forecast)) +
  geom_line(color = "blue") +
  labs(title = "Forecasted GDP",
       x = "Date",
       y = "Forecasted GDP") + 
  theme_minimal()
stan_samples_inflation <- extract(uc_sv_model_inflation)
trend_forecast_inflation <- apply(stan_samples_inflation$trend, 2, mean)
cycle_forecast_inflation <- apply(stan_samples_inflation$cycle, 2, mean)
predicted_inflation <- trend_forecast_inflation + cycle_forecast_inflation
forecast_dates_inflation <- seq(from = max(comparison_data$date) + month(3), length.out = length(predicted_inflation), by = "quarter")
forecasted_inflation_df <- data.frame(date = forecast_dates_inflation, trend = trend_forecast_inflation, cycle = cycle_forecast_inflation, inflation_forecast = predicted_inflation)
ggplot(forecasted_inflation_df, aes(x = date, y = inflation_forecast)) +
  geom_line(color = "blue") +
  labs(title = "Forecasted Inflation",
       x = "Date",
       y = "Forecasted Inflation (%)") + 
  theme_minimal()
View(stan_samples_cpi)
posterior_predictive <- stan_samples_cpi$y_rep
if (!is.null(dim(posterior_predictive)) && length(dim(posterior_predictive)) == 2) {
  predicted_means <- apply(posterior_predictive, 2, mean)
} else {
  stop("The dimension of `posterior_predictive` is not as expected.")
}
ggplot() +
  geom_density(aes(x = comparison_data$inflation_yoy), fill = "blue", alpha = 0.3) +
  geom_density(aes(x = apply(posterior_predictive, 2, mean)), fill = "red", alpha = 0.3) +
  labs(title = "Posterior Predictive Check: Actual vs. Predicted",
       x = "Inflation YoY",
       y = "Density") +
  theme_minimal()

#Philips Curve Models
philips_curve_model_cpi <- lm(cpi ~ unemployment + gdp + breakeven_inflation + lag(cpi, 1) +
                                unemployment:breakeven_inflation + lag(cpi, 2), data = comparison_data)
philips_curve_model_unemp <- lm(unemployment ~ cpi + gdp + breakeven_inflation + lag(unemployment, 1) +
                                  gdp:breakeven_inflation + lag(unemployment, 2), data = comparison_data)
philips_curve_model_gdp <- lm(gdp ~ unemployment + cpi + breakeven_inflation + lag(gdp, 1) +
                                unemployment:gdp + lag(gdp, 2), data = comparison_data)
philips_curve_model_inflation <- lm(breakeven_inflation ~ unemployment + cpi + gdp + lag(breakeven_inflation, 1) +
                                      cpi:breakeven_inflation + lag(breakeven_inflation, 2), data = comparison_data)
summary(philips_curve_model_cpi)
summary(philips_curve_model_unemp)
summary(philips_curve_model_gdp)
summary(philips_curve_model_inflation)
par(mfrow = c(2, 2))
plot(philips_curve_model_cpi)
dwtest(philips_curve_model_cpi)
dwtest(philips_curve_model_unemp)
dwtest(philips_curve_model_gdp)
dwtest(philips_curve_model_inflation)
predicted_data <- comparison_data %>%
  mutate(predicted_cpi = predict(philips_curve_model_cpi, newdata = comparison_data),
         predicted_unemp = predict(philips_curve_model_unemp, newdata = comparison_data),
         predicted_gdp = predict(philips_curve_model_gdp, newdata = comparison_data),
         predicted_inflation = predict(philips_curve_model_inflation, newdata = comparison_data)) %>%
  na.omit()
ggplot(predicted_data, aes(x = unemployment, y = predicted_cpi)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'red') +
  labs(title = "Partial Effect of Unemployment on CPI",
       x = "Unemployment Rate",
       y = "Predicted CPI") +
  theme_minimal()
ggplot(predicted_data, aes(x = cpi, y = predicted_unemp)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'red') +
  labs(title = "Partial Effect of CPI on Unemployment",
       x = "CPI",
       y = "Predicted Unemployment Rate") +
  theme_minimal()
ggplot(predicted_data, aes(x = gdp, y = predicted_inflation)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'red') +
  labs(title = "Partial Effect of GDP on Breakeven Inflation",
       x = "Real GDP",
       y = "Predicted Breakeven Inflation") +
  theme_minimal()
ggplot(predicted_data, aes(x = gdp, y = predicted_cpi)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'red') +
  labs(title = "Partial Effect of GDP on CPI",
       x = "Real GDP",
       y = "Predicted CPI") +
  theme_minimal()
ggplot(predicted_data, aes(x = gdp, y = predicted_cpi)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'red') +
  labs(title = "Partial Effect of GDP on CPI",
       x = "Real GDP",
       y = "Predicted CPI") +
  theme_minimal()
ggplot(predicted_data, aes(x = gdp, y = predicted_unemp)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'red') +
  labs(title = "Partial Effect of GDP on Unemployment",
       x = "Real GDP",
       y = "Predicted Unemployment Rate") +
  theme_minimal()
ggplot(predicted_data, aes(x = unemployment, y = predicted_gdp)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'red') +
  labs(title = "Partial Effect of Unemployment on GDP",
       x = "Unemployment Rate",
       y = "Predicted GDP") +
  theme_minimal()
ggplot(predicted_data, aes(x = cpi, y = predicted_gdp)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'red') +
  labs(title = "Partial Effect of CPI on GDP",
       x = "CPI",
       y = "Predicted GDP") +
  theme_minimal()
ggplot(predicted_data, aes(x = gdp, y = predicted_inflation)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'red') +
  labs(title = "Partial Effect of GDP on Breakeven Inflation",
       x = "Real GDP",
       y = "Predicted Breakeven Inflation") +
  theme_minimal()
ggplot(predicted_data, aes(x = unemployment, y = predicted_inflation)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'red') +
  labs(title = "Partial Effect of Unemployment on Breakeven Inflation",
       x = "Unemployment Rate",
       y = "Predicted Breakeven Inflation") +
  theme_minimal()
ggplot(predicted_data, aes(x = cpi, y = predicted_inflation)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'red') +
  labs(title = "Partial Effect of CPI on Breakeven Inflation",
       x = "CPI",
       y = "Predicted Breakeven Inflation") +
  theme_minimal()
cpi_summary <- summary(philips_curve_model_cpi)$coefficients
unemp_summary <- summary(philips_curve_model_unemp)$coefficients
gdp_summary <- summary(philips_curve_model_gdp)$coefficients
inflation_summary <- summary(philips_curve_model_inflation)$coefficients
cpi_summary_df <- as.data.frame(cpi_summary)
cpi_summary_df$Model <- "CPI Model"
cpi_summary_df$Predictor <- rownames(cpi_summary_df)
unemp_summary_df <- as.data.frame(unemp_summary)
unemp_summary_df$Model <- "Unemployment Model"
unemp_summary_df$Predictor <- rownames(unemp_summary_df)
gdp_summary_df <- as.data.frame(gdp_summary)
gdp_summary_df$Model <- "GDP Model"
gdp_summary_df$Predictor <- rownames(gdp_summary_df)
inflation_summary_df <- as.data.frame(inflation_summary)
inflation_summary_df$Model <- "Inflation Model"
inflation_summary_df$Predictor <- rownames(inflation_summary_df)
combined_summary_df <- rbind(cpi_summary_df, unemp_summary_df, gdp_summary_df, inflation_summary_df)
colnames(combined_summary_df) <- c("Estimate", "Std.Error", "t_value", "P_value", "Model", "Predictor")
combined_summary_df <- combined_summary_df[, c("Model", "Predictor", "Estimate", "Std.Error", "t_value", "P_value")]
combined_summary_df <- combined_summary_df %>%
  mutate(across(c(Estimate, Std.Error, t_value, P_value), round, 3)) %>%
  na.omit
combined_summary_df
write.csv(combined_summary_df, "C:/Users/jeesh/OneDrive/Documents/FMCapstone/Philips_summary.csv", row.names = FALSE)