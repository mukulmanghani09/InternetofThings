#Applications of IoT in Manufacturing
# Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(forecast)

# Simulate IoT sensor data
iot_data <- tibble(
  timestamp = seq(from = as.POSIXct("2025-02-12 08:00:00"), by = "min", length.out = 1000),
  temperature = rnorm(1000, mean = 75, sd = 5),
  vibration = rnorm(1000, mean = 10, sd = 2),
  pressure = rnorm(1000, mean = 50, sd = 3)
)

# Introduce anomalies for predictive maintenance
iot_data$vibration[900:920] <- iot_data$vibration[900:920] + 10 # Simulate abnormal vibration

iot_data %>% 
  pivot_longer(cols = c(temperature, vibration, pressure), names_to = "Sensor", values_to = "Value") %>%
  ggplot(aes(x = timestamp, y = Value, color = Sensor)) +
  geom_line() +
  theme_minimal() +
  labs(title = "IoT Sensor Data for Manufacturing Analytics", x = "Time", y = "Sensor Readings")

# Anomaly detection
library(anomalize)
anomalies <- iot_data %>% 
  select(timestamp, vibration) %>% 
  time_decompose(vibration) %>% 
  anomalize(remainder) %>% 
  time_recompose()

print(anomalies)

# Predictive maintenance using ARIMA
vibration_ts <- ts(iot_data$vibration, frequency = 60)
fit <- auto.arima(vibration_ts)
forecasted <- forecast(fit, h = 60)
plot(forecasted, main = "Vibration Forecast for Predictive Maintenance")
