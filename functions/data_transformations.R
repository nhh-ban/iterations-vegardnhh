

transform_metadata_to_df <- function(stations_metadata) {
  df <- 
    stations_metadata[[1]] %>%
    map(as_tibble) %>%  
    list_rbind() %>%  
    mutate(latestData = map_chr(latestData, 1, .default = "")) %>%  
    mutate(latestData = as_datetime(latestData)) %>% 
    mutate(latestData = force_tz(latestData, tzone = "UTC")) %>%
    mutate(location = map(location, unlist)) %>%   
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location)
  
  return(df)
}

to_iso8601 <- function(datetime, offset_days) {
  # Add offset to the original datetime
  adjusted_datetime <- datetime + lubridate::days(offset_days)
  
  # Convert to ISO8601 format
  iso_datetime <- format(adjusted_datetime, format = "%Y-%m-%dT%H:%M:%SZ")
  
  return(iso_datetime)
}

