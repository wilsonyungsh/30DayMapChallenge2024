
pacman::p_load(XML,mapdeck,tidyverse,sf,lubridate)

## Parse the gpx file
gpx_parsed <- htmlTreeParse(file = "data/Afternoon_Ride.gpx", useInternalNodes = TRUE)
gpx_parsed
# extract loc,elevation, timestamp
loc <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)
time <- xpathApply(doc = gpx_parsed,path = "//trkpt/time", fun = xmlValue) %>% unlist()



## produce the journey df
journey <- 
data.frame(
  lat = loc['lat',],
  lon = loc['lon',],
  elevation = as.numeric(elevation),
  utc_time = time
) %>% 
  mutate(id = row_number(),local_time = ymd_hms(time,tz = "Australia/Brisbane")) %>% 
  st_as_sf(coords = c("lon","lat"),crs = 4326,remove = FALSE) %>% 
  select(id,local_time,elevation,geometry,lon,lat,utc_time) %>% 
  mutate(
         dist_travelled = as.numeric(st_distance(by_element = TRUE,geometry, lag(geometry))),
         tp = paste0("seqenece: ",id,"<br>timestamp : ",local_time,"<br> Distance Travelled(m) : ",dist_travelled))


## Calculate Stats
total_dist <-
  round(journey$dist_travelled %>% sum(na.rm = TRUE),2) 

journey_time <- round(max(journey$local_time) -min(journey$local_time),2)
dt <- unique(as.Date(journey$local_time))


## map
mapdeck(style = mapdeck_style("dark"),pitch = 43,zoom = 20,location = c(152.9765855270416,-27.488362829451493)) %>%
  add_pointcloud(data = journey,fill_colour = "elevation",palette = "terrain_hcl",elevation =journey$elevation*3,
                 legend = TRUE,
                 tooltip = "tp") %>% 
  add_title(title = paste0("First Cycle Journey In Brisbane - School Pickup<br> Total Distance(m) : ",
                          total_dist,"<br> Journey Duration(Minutes) : ",journey_time,
                          "<br> Date : ",dt)) 
                 

