library(readr)
library(dplyr)

coords_df <- tibble()  # initialize empty tibble

for (m in 1:12) {
  month_str <- sprintf("%02d", m)  # ensures leading zero (e.g., "01", "02", ..., "12")
  file_path <- paste0("../all_data/chicago_taxi_2024_", month_str, ".csv")
  temp <- read_csv(file_path)
  coords_df <- bind_rows(coords_df, temp)
}

# Combine pickup and dropoff coordinates, filter unique
#points_df <- coords_df %>%
#  select(pickup_centroid_latitude, pickup_centroid_longitude,
#         dropoff_centroid_latitude, dropoff_centroid_longitude) %>%
#  pivot_longer(cols = everything(),
#               names_to = c("type", ".value"),
#               names_pattern = "(pickup|dropoff)_centroid_(latitude|longitude)") %>%
#  distinct(latitude, longitude) %>% filter(!is.na(latitude) & !is.na(longitude))

# Convert to sf object (points)
#points_sf <- st_as_sf(points_df, coords = c("longitude", "latitude"), crs = 4326)

# Plot polygons and points
#leaflet() %>%
#  addTiles() %>%
#  addPolygons(data = sf_df, color = "blue", weight = 2, fillOpacity = 0.3, popup = ~NAME10) %>%
#  addCircleMarkers(data = points_sf, radius = 4, color = "red", stroke = FALSE, fillOpacity = 0.8)


lookup <- coords_df %>%
 select(centroid_latitude = pickup_centroid_latitude,
         centroid_longitude = pickup_centroid_longitude,
         census_tract = pickup_census_tract) %>%
  bind_rows(
    coords_df %>%
      select(centroid_latitude = dropoff_centroid_latitude,
             centroid_longitude = dropoff_centroid_longitude,
             census_tract = dropoff_census_tract)
  ) %>%
  filter(!is.na(census_tract)) %>%
  distinct(census_tract, .keep_all = TRUE) %>%
  arrange(census_tract)

# View the result
print(lookup)



