library(ggplot2)
library(maps)
library(ggmap)
library(dplyr)
library(tidygeocoder)
library(viridis)
library(sf)

setwd("C:/Users/llaur/Downloads")
tjs = read_csv("store_info.csv")
zips = unique(tjs$zip)

# drop phone numbers
zips = zips[which(nchar(zips) < 11)]

# drop ? and extensions
zips = substr(zips,1,5)

# make dataframe to plot
uszips = read_csv("uszips.csv")
uszips$zip = as.character(uszips$zip)

# add leading 0's
uszips$zip <- ifelse(
  nchar(uszips$zip) == 3,
  paste0("00", uszips$zip), 
  ifelse(nchar(uszips$zip) == 4,
         paste0("1", uszips$zip), 
         uszips$zip)
)

tjzips = uszips[uszips$zip %in% zips, ]

#make map
us_map = map_data("state")
ggplot() +
  geom_polygon(data=us_map, aes(x = long, y = lat, group = group),
               fill = "gray80", color = "white") +
  geom_point(data = tjzips, aes(x = lng, y = lat), color = "red", size = 2) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Trader Joe's locations")

# visualize pop densities on map
#restrict to continental us
lat_min <- 22.0
lat_max <- 49.0
lon_min <- -125.0
lon_max <- -65.

uszips <- uszips[
  uszips$lat >= lat_min & uszips$lat <= lat_max &
    uszips$lng >= lon_min & uszips$lng <= lon_max, ]

# us census zip map shapes
zip_shapefile <- st_read("zipshapefiles/cb_2018_us_zcta510_500k.shp")
zip_shapefile <- zip_shapefile %>%
  left_join(uszips, by = c("ZCTA5CE10" = "zip"))

ggplot(zip_shapefile) +
  geom_sf(aes(fill = density), color = "white", size = 0.1) + 
  scale_fill_viridis(option = "D", direction = -1, name = "Pop Density") + 
  labs(title = "Pop Density by Zip Code in the Continental US") +
  #geom_point(data = tjzips, aes(x = lng, y = lat), color = "red", size = 1, alpha = 0.7) + 
  theme_minimal() +
  theme(legend.position = "bottom") + 
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max))

# census data
agedf = read_csv("ACSST5Y2017.S0101_2024-10-06T020720/ACSST5Y2017.S0101-Data.csv")

#NAME col is zipcode
agedf$NAME = substr(agedf$NAME,7,11)

#                 %zip      % under 18          %18-24           % 25-29          % 30-34             % 65 and older    % median age
age = agedf[, c("NAME", "S0101_C02_022E", "S0101_C02_023E","S0101_C02_007E", "S0101_C02_008E" , "S0101_C02_030E", "S0101_C01_032E")]

#drop nonumeric

age$midage = 100 - (age$S0101_C02_022E + age$S0101_C02_023E + age$S0101_C02_007E + age$S0101_C02_008E + age$S0101_C02_030E)

#
