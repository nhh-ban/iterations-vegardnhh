

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

transform_volumes <- function(volumes) {
  df <- volumes$trafficData$volume$byHour$edges %>%
    map_df(~{
      tibble(
        from = .x$node$from %>% as_datetime() %>% force_tz(tzone = "UTC"),
        to = .x$node$to %>% as_datetime() %>% force_tz(tzone = "UTC"),
        volume = .x$node$total$volumeNumbers$volume
      )
    })
  
  return(df)
}

test <- GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)

transform_volumes(test)
