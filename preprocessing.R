library(ggplot2)
library(readr)
library(maps)
library(ggmap)
library(dplyr)
library(geosphere)
library(tidygeocoder)
library(tidyr)
library(viridis)
library(sf)
library(purrr)

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
uszips = uszips[,c("zip", "lat", "lng", "city", "state_name")]

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
  geom_sf(aes(fill = density), color = "white", size = 0.01) + 
  scale_fill_viridis(option = "D", direction = -1, name = "Pop Density") + 
  labs(title = "Pop Density by Zip Code in the Continental US") +
  #geom_point(data = tjzips, aes(x = lng, y = lat), color = "red", size = 1, alpha = 0.7) + 
  theme_minimal() +
  theme(legend.position = "bottom") + 
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max))

# age census data
agedf = read_csv("ACSST5Y2017.S0101_2024-10-06T020720/ACSST5Y2017.S0101-Data.csv")

#NAME col is zipcode
agedf$NAME = substr(agedf$NAME,7,11)

head(agedf)
#                 %zip      % under 18          %18-24           % 25-29          % 30-34             % 65 and older    % median age
age = agedf[, c("NAME", "S0101_C02_022E", "S0101_C02_023E","S0101_C02_007E", "S0101_C02_008E" , "S0101_C02_030E", "S0101_C01_032E")]

#drop censored 
dim(age)
age_clean <- age[!apply(age[, c("S0101_C02_022E", "S0101_C02_023E","S0101_C02_007E", "S0101_C02_008E" , "S0101_C02_030E", "S0101_C01_032E")],
                        1, function(x) any(grepl("[^0-9.]", x))), ]
dim(age_clean)

age_clean$S0101_C02_022E <- as.numeric(age_clean$S0101_C02_022E)
age_clean$S0101_C02_023E <- as.numeric(age_clean$S0101_C02_023E)
age_clean$S0101_C02_007E <- as.numeric(age_clean$S0101_C02_007E)
age_clean$S0101_C02_008E <- as.numeric(age_clean$S0101_C02_008E)
age_clean$S0101_C02_030E <- as.numeric(age_clean$S0101_C02_030E)

age_clean$midage = 100 - (age_clean$S0101_C02_022E + age_clean$S0101_C02_023E + age_clean$S0101_C02_007E + age_clean$S0101_C02_008E + age_clean$S0101_C02_030E)

head(age_clean)

#employment census data

empldf = read_csv("ACSST5Y2017.S2406_2024-10-07T194504/ACSST5Y2017.S2406-Data.csv")
empl_cols = c("S2406_C02_004E", # Employee of private company workers, Civilian employed population 16 years and over!!Sales and office occupations
            "S2406_C02_003E", # Employee of private company workers!!Civilian employed population 16 years and over!!Service occupations
            "S2406_C02_005E", # Employee of private company workers!!Civilian employed population 16 years and over!!Natural resources, construction, and maintenance occupations
            "S2406_C02_001E" # Estimate!!Employee of private company workers!!Civilian employed population 16 years and over
            )
empldf$NAME = substr(empldf$NAME,7,11)
empl = empldf[, c("NAME", empl_cols)]

#drop
dim(empl)
empl_clean <- empl[!apply(empl[, empl_cols],
                        1, function(x) any(grepl("[^0-9.]", x))), ]
dim(empl_clean)
empl_clean[empl_cols] <- lapply(empl_clean[empl_cols], as.numeric)

head(empl_clean)

#income census data

incdf = read_csv("ACSST5Y2017.S1903_2024-10-07T194909/ACSST5Y2017.S1903-Data.csv")
inc_cols = c("S1903_C03_001E", #Median household income
            "S1903_C02_002E", # %white household
            "S1903_C02_003E", # %black
            "S1903_C02_004E", # %indigenous
            "S1903_C02_005E", # %asian
            "S1903_C02_009E" # %hispanic/latino
            )
incdf$NAME = substr(incdf$NAME,7,11)
inc = incdf[, c("NAME", inc_cols)]
dim(inc)
inc_clean <- inc[!apply(inc[, inc_cols],
                          1, function(x) any(grepl("[^0-9.]", x))), ]
dim(inc_clean)
inc_clean[inc_cols] <- lapply(inc_clean[inc_cols], as.numeric)
head(inc_clean)

# age/educ census data

demodf = read_csv("ACSDT5Y2017.B15002_2024-10-07T195706/ACSDT5Y2017.B15002-Data.csv")
demo_cols = c("B15002_019E", #Estimate!!Total!!Female
              "B15002_028E", #Estimate!!Total!!Female!!High school graduate (includes equivalency)
              "B15002_002E", #Estimate!!Total!!Male
              "B15002_011E", #Estimate!!Total!!Male!!High school graduate (includes equivalency)
              "B15002_015E", #Estimate!!Total!!Male!!Bachelor's degree
              "B15002_032E" #Estimate!!Total!!Female!!Bachelor's degree
              )
demodf$NAME = substr(demodf$NAME,7,11)
demo = demodf[, c("NAME", demo_cols)]
dim(demo)
demo_clean <- demo[!apply(demo[, demo_cols],
                        1, function(x) any(grepl("[^0-9.]", x))), ]
dim(demo_clean)
demo_clean[demo_cols] <- lapply(demo_clean[demo_cols], as.numeric)

# get percentages
demo_clean$femhs = demo_clean[["B15002_028E"]] / demo_clean[["B15002_019E"]]
demo_clean$malehs = demo_clean[["B15002_011E"]] / demo_clean[["B15002_002E"]]
demo_clean$mpct = demo_clean[["B15002_002E"]] / (demo_clean[["B15002_002E"]] + demo_clean[["B15002_019E"]]) #percentage male
demo_clean$fembach = demo_clean[["B15002_032E"]] / demo_clean[["B15002_019E"]]
demo_clean$malebach = demo_clean[["B15002_015E"]] / demo_clean[["B15002_002E"]]
demo_clean$hs = with(demo_clean, 
   (mpct * malehs + (1-mpct) * femhs)*100
)
demo_clean$bach = with(demo_clean, 
   (mpct * malebach + (1-mpct) * fembach)*100
)
head(demo_clean[, c("bach", "hs")])
summary(demo_clean[, c("bach", "hs")]) # check valid pct

# zillow rent data
rent = read_csv("pricepersqft.csv")
head(rent)
dim(rent)
rent_cols = c("May 2012", "December 2012",
              "May 2013", "December 2013",
              "May 2014", "December 2014",
              "May 2015", "December 2015",
              "May 2016", "December 2016"
              )
rent = rent[, c("City", "State", rent_cols)]
rent$min_rent = apply(rent[, rent_cols], 1, min)
rent$overall_diff = rent[["December 2016"]]-rent[["May 2012"]]
rent$avg_diff = (1/9)*((rent[["December 2016"]]-rent[["May 2016"]])+
                     (rent[["May 2016"]]-rent[["December 2015"]])+
                      (rent[["December 2015"]]-rent[["May 2015"]])+
                      (rent[["May 2015"]]-rent[["December 2014"]])+
                      (rent[["December 2014"]]-rent[["May 2014"]])+
                      (rent[["May 2014"]]-rent[["December 2013"]])+
                      (rent[["December 2013"]]-rent[["May 2013"]])+
                      (rent[["May 2013"]]-rent[["December 2012"]])+
                      (rent[["December 2012"]]-rent[["May 2012"]]))
summary(rent$avg_diff)
summary(rent$overall_diff)

rent = rent[, c("City", "State", "overall_diff", "avg_diff")]

# get latitude and longitude of each city
cities = read.csv('worldcities.csv')
cities = cities[cities$country=="United States", c("city", "lat", "lng")]
dim(cities)
rent <- inner_join(rent, cities, by = c("City" = "city"))
head(rent)

# join on rent from nearest city

find_closest_rent <- function(lat, lon, rent_data) {
  distances <- distHaversine(cbind(lon, lat), rent_data[, c("lng", "lat")])
  closest_index <- which.min(distances)
  rent_data[closest_index, ]
}

matched_rent <- uszips %>%
  rowwise() %>%
  mutate(
    closest_rent = list(find_closest_rent(lat, lng, rent))
  ) %>%
  unnest_wider(closest_rent, names_sep = "_closest")

head(matched_rent)
exclude <- c("Puerto Rico", "Virgin Islands", "Guam", "American Samoa", "Northern Mariana Islands")
rent_final = matched_rent[!matched_rent$state_name%in%exclude,]
head(rent_final)
rent_final <- rent_final %>% rename (
  closest_city = closest_rent_closestCity,
  overall_diff = closest_rent_closestoverall_diff,
  avg_diff = closest_rent_closestavg_diff
)
rent_final <- rent_final %>% select(-closest_rent_closestState, -closest_rent_closestlat, -closest_rent_closestlng)
head(rent_final)

write.csv(rent_final, "rent.csv", row.names = FALSE)

#inner join into 1 big dataset
names(uszips)
dim(uszips)

# rename colnames lol
# ALL PERCENTAGES (except where listed as median)
age_final <- age_clean %>% rename(
  au18 = S0101_C02_022E, # age over 18
  a18to24 = S0101_C02_023E, # age 18-24
  a25to29 = S0101_C02_007E, 
  a30to34 = S0101_C02_008E,
  ao65 = S0101_C02_030E, # age over 65
  medage = S0101_C01_032E # age median
)
age_final$medage <- as.numeric(age_final$medage)
head(age_final)
dim(age_final)

empl_final <- empl_clean %>% rename(
  sales = S2406_C02_004E, # over 16yo employed sales and office occupations
  serv = S2406_C02_003E, # ''' service occupations
  labor = S2406_C02_005E, # ''' construction, maintenance occupations
  eo16 = S2406_C02_001E, # employed 16 over
)
head(empl_final)
dim(empl_final)

inc_final <- inc_clean %>% rename (
  medinc = S1903_C03_001E, #Median household income
  white = S1903_C02_002E, # %white household
  black = S1903_C02_003E, # %black
  indg = S1903_C02_004E, # %indigenous
  asian = S1903_C02_005E, # %asian
  hisp = S1903_C02_009E # %hispanic/latino
)
head(inc_final)
dim(inc_final)

head(demo_clean)
demo_final <- demo_clean[, c('NAME', 'mpct', 'hs', 'bach')]
demo_final$mpct = demo_final$mpct*100
head(demo_final)
dim(demo_final)

df_list <- list(age_final, empl_final, inc_final, demo_final)
result <- reduce(df_list, ~ inner_join(.x, .y, by = "NAME"))
names(result)
dim(result)

head(rent_final)
df <- inner_join(rent_final, result, by = c("zip" = "NAME"))

head(df)
dim(df)

write.csv(df, "data.csv", row.names = FALSE)
