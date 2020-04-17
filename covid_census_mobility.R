rm(list = ls())
library(RPostgreSQL)
library(tidycensus)
library(tidyverse)
library(viridis)
library(ggmap)
library(tidycensus)
library(sf)
library(maps)
library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)
library(nngeo)
library(lubridate)
library(DescTools)
library(gridExtra)

#check out avialable variables from the American Community Survey 2017
v17 <- load_variables(2017, "acs5", cache = TRUE)
view(v17) # var for median_income = B07011_001 and median_age = B01002_001

co_census_median_age<- get_acs(geography = "block group", var = c(median_age = "B01002_001"), state = "CO", geometry = TRUE)
co_census_median_age_county<- get_acs(geography = "county", var = c(median_age = "B01002_001"), state = "CO", geometry = TRUE)

co_census_median_age_county<- get_acs(geography = "block group", var = c(median_age = "B01002_001"), state = "CO", geometry = TRUE)
#split out combined geos from `NAME` into distinct columns
co_census_median_age<-co_census_median_age %>% separate(NAME, c("block_group", "census_tract", "county", "state"), sep = ", ", convert = TRUE)

#find centroids of blocks for later use and create separate columns for lat (X) and long (Y), ignore warning as centroid can be approximate
co_census_median_age$centroid<-st_centroid(co_census_medage$geometry)
co_census_median_age<-cbind(co_census_median_age,st_coordinates(co_census_median_age$centroid))
co_census_median_age<-rename(co_census_median_age, "lat" = Y, "long" = X)

#grab the world map
world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)

#bin ages for cleaner visuals
co_census_median_age$age_bin <- cut(co_census_median_age$estimate, breaks=c(0,33,39,45,60,90), labels=c("<33yo","33-39yo","39-45", "45-60yo", "60+"))

#map median age groups by census block
co_age_plot<-world%>%
  ggplot() +
  theme_void() +
  geom_sf(data = na.omit(co_census_median_age), aes(fill = factor(age_bin))) +
  coord_sf(xlim = c(-109.5, -101.5), ylim = c(36.5, 41.25), expand = FALSE)+
  scale_color_viridis(discrete = TRUE)+
  labs(title = "Median Age in Colorado Block Tracts", colour = co_census_median_age$age_bin)

co_age_plot

#Plot by county for contrast with lower resolution
co_census_median_age_county$age_bin <- cut(co_census_median_age_county$estimate, breaks=c(0,33,39,45,60,90), labels=c("<33yo","33-39yo","39-45", "45-60yo", "60+"))

co_age_county_plot<-world%>%
  ggplot() +
  theme_void() +
  geom_sf(data = co_census_median_age_county, aes(fill = factor(age_bin)), show.legend = FALSE) +
  coord_sf(xlim = c(-109.5, -101.5), ylim = c(36.5, 41.25), expand = FALSE)+
  scale_color_viridis(discrete = TRUE)+
  labs(title = "Median Age in Colorado Counties") 


grid.arrange(co_age_county_plot, co_age_plot, ncol=2, nrow=1, widths= c(4,5), heights= 4)


#Bring in some descrate mobility data
# initialize the driver to PostgreSQL
drv <- dbDriver("PostgreSQL")

#Connect to local postges db estimize
## create a connection to a PostgreSQL server
covid <- dbConnect(drv, user="", password="",
                   dbname="covid_dwh", host="covid-dwh.cjzjn92qszxx.us-east-1.rds.amazonaws.com")

co_muni<- dbGetQuery(covid, "SELECT DISTINCT municipality, county, concat(municipality, ' ', state) muni_state
                             FROM descartes.municipality_mobility")

muni_mobility <- dbGetQuery(covid, "SELECT county, municipality, date, median_distance_traveled_km
                                    FROM descartes.municipality_mobility
                                    ORDER BY municipality, date")
#get lat long coordinates for municipalities
geocoded <- data.frame(stringsAsFactors = FALSE)
for(i in 1:nrow(co_muni))
{
  # Print("Working...")
  result <- geocode(co_muni$muni_state[i],output = "latlona", source = "google")
  co_muni$lon[i] <- as.numeric(result[1])
  co_muni$lat[i] <- as.numeric(result[2])
  co_muni$geoAddress[i] <- as.character(result[3])
}

#rejoin mobility data with coodinates for each municipality
muni_mobility<- co_muni %>%inner_join(muni_mobility,  by ="municipality")
summary(muni_mobility)
summary(co_census_medage)
#
muni_mobility_geo <- st_as_sf(na.omit(muni_mobility), coords = c("lon", "lat"), crs = 4326)
co_census_medage_geo <- st_as_sf(na.omit(co_census_medage), coords = c("X", "Y"), crs = 4326)

co_census_medage_geo<-st_transform(co_census_medage_geo, 4326)
st_crs(muni_mobility_geo)
st_crs(co_census_medage_geo)

#join muni descrates data with census data
mobility_median_age<-st_join(muni_mobility_geo,co_census_medage_geo, st_nn, k = 1, maxdis=50000)


mobility_median_age$week_start<-floor_date(mobility_median_age$date, "week")

#group by age and date for plotting
mobility_median_age_grouped<-na.omit(mobility_median_age) %>%  st_set_geometry(NULL) %>%
  group_by(date, age_bin) %>% 
  summarise(
    mean_median_distance_traveled_km = mean(median_distance_traveled_km),
    n = n(), 
    sd = sd(median_distance_traveled_km), se = sd/sqrt(n)
  )

mobility_median_age_grouped%>% pivot_wider(age_bin, mean_median_distance_traveled_km)

#boxplot
mobility_age_box<-
  ggplot(mobility_median_age, aes(x=week_start,y=median_distance_traveled_km,group = interaction(age_bin, week_start))) +
  geom_boxplot(aes(fill=age_bin)) +
  scale_y_continuous(labels = scales::comma)+
  scale_x_date(date_breaks="1 week", date_minor_breaks = "1 day", date_labels="%b %d")+
  labs(title = "Mobility by Age over Time (based on Census Block and Descartes)", y = "median_distance_traveled_km", x = "date", colour = "age group")

mobility_age_box


#jitter plot
mobility_age_jitter<-
  ggplot(mobility_median_age, aes(x=date,y=median_distance_traveled_km, color = age_bin)) +
  geom_jitter(aes(color=age_bin), size=1) +
  scale_y_continuous(labels = scales::comma)+
  scale_x_date(date_breaks="1 week", date_minor_breaks = "1 day", date_labels="%b %d")+
  labs(title = "Mobility by Age over Time (based on Census Block and Descartes", y = "median_distance_traveled_km", x = "date", colour = "age group", shape = "population density") 

mobility_age_jitter

#grroped line chart
mobility_age_line_grouped<-
  ggplot(mobility_median_age_grouped, aes(x=date,y=mean_median_distance_traveled_km, color = age_bin)) +
  geom_line(aes(color=age_bin), size=1) +
  scale_y_continuous(labels = scales::comma)+
  scale_x_date(date_breaks="1 week", date_minor_breaks = "1 day", date_labels="%b %d")+
  labs(title = "Mobility by Age over Time (based on Census Block and Descartes", y = "mean_median_distance_traveled_km", x = "date", colour = "age group", shape = "population density") 

mobility_age_line_grouped

#standard deviations plot
mobility_age_plot_sd<-
  ggplot(mobility_median_age_grouped, aes(x=date,y=se/mean_median_distance_traveled_km, color = age_bin)) +
  geom_line(aes(color=age_bin), size=1) +
  scale_y_continuous(labels = scales::comma)+
  scale_x_date(date_breaks="1 week", date_minor_breaks = "1 day", date_labels="%b %d")+
  labs(title = "Mobility by Age over Time (based on Census Block and Descartes", y = "mean_median_distance_traveled_km", x = "date", colour = "age group", shape = "population density") 

mobility_age_plot_sd


mobility_age_plot<-
  ggplot(mobility_median_age, aes(x=week_start,y=median_distance_traveled_km,group = interaction(age_bin, week_start))) +
  geom_boxplot(aes(fill=age_bin)) +
  geom_jitter(aes(color=age_bin), size=1) +
  scale_y_continuous(labels = scales::comma)+
  scale_x_date(date_breaks="1 week", date_minor_breaks = "1 day", date_labels="%b %d")+
  labs(title = "Mobility by Age over Time (based on Census Block and Descartes", y = "median_distance_traveled_km", x = "date", colour = "age group")

mobility_age_plot




