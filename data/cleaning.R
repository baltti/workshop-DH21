
library(tidyverse)
library(sf)

airports<-read.delim("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
                     header = FALSE, sep = ",") %>% 
  rename(lat=V7, lon=V8,name=V2,city=V3,short_name=V5) %>% 
  filter(V4=="Russia") %>% 
  select(-V1,-V4,-V6,-V9,-V10,-V11,-V12,-V13,-V14)

routes<-read.delim("https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat",
                   header = FALSE, sep = ",") %>% 
  rename(orig=city, dest=short_name) 

flights<-left_join(routes,airports, by=c("orig"="short_name"))

flights<-left_join(flights,airports, by=c("dest"="short_name")) %>% 
  dplyr::select(orig, dest, lat.x, lon.x, lat.y, lon.y) %>% 
  drop_na(lat.y, lon.y,lat.x,lon.x) %>% 
  rename(origin.lat=lat.x,origin.lon=lon.x,
         destination.lat=lat.y,destination.lon=lon.y)

make_line <- function(origin.lon, origin.lat, destination.lon, destination.lat) {
  st_linestring(matrix(c(origin.lon, destination.lon, origin.lat, destination.lat), 2, 2))
}

flights1<-flights %>%
  select(-orig,-dest) %>% 
  pmap(make_line) %>% 
  st_as_sfc(crs = 4326) %>% 
  {tibble(orig = flights$orig, dest=flights$dest,
          origin.lon=flights$origin.lon, origin.lat=flights$origin.lat, 
          destination.lon=flights$destination.lon, 
          destination.lat=flights$destination.lat, geometry = .)} %>% 
  st_sf()

st_write(flights1,"flights.geojson",append = FALSE)

write_csv(airports,"airports.csv")
