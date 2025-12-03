library(jsonlite)
library(dplyr)
library(stringr)
library(glue)
library(purrr)

make_location_summary <- function(lat, lon, name = NULL) {
  url <- glue("https://forecast.weather.gov/MapClick.php?lat={lat}&lon={lon}&FcstType=json")
  dat <- fromJSON(url)
  
  temp_label <- if ("tempLabel" %in% names(dat$data)) dat$data$tempLabel else rep(NA, length(dat$data$temperature))
  
  forecast_df <- tibble(
    period = dat$time$startPeriodName,
    temp_label = temp_label,
    temperature = as.numeric(dat$data$temperature),
    weather = dat$data$weather,
    text = dat$data$text
  )
  
  precip <- forecast_df %>%
    filter(str_detect(str_to_lower(weather), "rain|snow|shower|precip")) %>%
    mutate(summary = glue("🌧 {period}: {text}")) %>%
    pull(summary)
  
  wind <- forecast_df %>%
    mutate(wind_speed = as.numeric(str_extract(text, "(?<=\\bwind\\s)(\\d{1,2})"))) %>%
    filter(!is.na(wind_speed) & wind_speed > 15) %>%
    mutate(summary = glue("💨 {period}: {text}")) %>%
    pull(summary)
  
  extreme <- forecast_df %>%
    filter((temp_label == "High" & temperature >= 90) |
             (temp_label == "Low" & temperature <= 32)) %>%
    mutate(summary = glue("🌡 {period}: {text}")) %>%
    pull(summary)
  
  bullets <- c()
  if (length(precip) > 0) bullets <- c(bullets, precip[1:min(2, length(precip))])
  if (length(wind) > 0) bullets <- c(bullets, wind[1])
  if (length(extreme) > 0) bullets <- c(bullets, extreme[1])
  
  
  # Add weekly temperature range if quiet forecast
  if (length(bullets) == 0) {
    temp_min <- min(forecast_df$temperature, na.rm = TRUE)
    temp_max <- max(forecast_df$temperature, na.rm = TRUE)
    bullets <- glue("No precipitation, high winds, or extreme temps expected. Temperature range is {temp_min}–{temp_max}°F.")
  }
  
  header <- if (!is.null(name)) glue("**{name}**:") else ""
  bullet_lines <- paste("-", bullets, collapse = "\n")
  paste(header, "\n", bullet_lines)
}


# Use it for your two points
weather_stockton <- make_location_summary(37.9537, -121.2905, "Stockton, CA")
weather_antioch <- make_location_summary(38.0169, -121.8138, "Antioch, CA")