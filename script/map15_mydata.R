pacman::p_load(XML,mapdeck,tidyverse,sf,lubridate)

## Parse the gpx file
gpx_parsed <- htmlTreeParse(file = "data/Appen Travel.gpx", useInternalNodes = TRUE)

# extract loc,elevation, timestamp
loc <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)
time <- xpathApply(doc = gpx_parsed,path = "//trkpt/time", fun = xmlValue) %>% unlist()



fieldwork_trace <- 
  data.frame(
    lat = loc['lat',],
    lon = loc['lon',],
    elevation = as.numeric(elevation),
    utc_time = time
  ) %>% 
  mutate(local_time = ymd_hms(time,tz = "Australia/Sydney"),
         date = date(local_time),
         trip = case_when(between(date,ymd("20131202"),ymd("20131212")) ~ "Goulbourn_Canberra_SnowyMountains",
                          between(date,ymd("20131216"),ymd("20131218")) ~ "HunterValley_BlueMountains",
                          between(date,ymd("20140113"),ymd("20140123")) ~ "Yass_Cootamundra_Wagga_Griffith",
                          between(date,ymd("20140128"),ymd("20140201")) ~ "Booral_Batlow_Kosciuszko",
                          between(date,ymd("20140204"),ymd("20140206")) ~ "Bilpin",
                          between(date,ymd("20140211"),ymd("20140215")) ~ "Dubbo_LakeCargelligo_Sherparton_Leeton",
                          between(date,ymd("20140218"),ymd("20140221")) ~ "Blackheath_Wollombi",
                          date == ymd(20131127) ~ "shortrip1",
                          between(date,ymd("20140109"),ymd("20140110")) ~ "shortrip2",
                           ))

grouped_trip <- fieldwork_trace %>% filter(trip != "shortrip1") %>% group_by(trip) %>% arrange(local_time) %>% 
  st_as_sf(coords = c("lon","lat"),crs = 4326,remove = FALSE) %>% group_by(trip) %>% arrange(trip,local_time) %>% 
  summarise(period = paste0(min(date),"-",max(date)),
            days = n_distinct(date),
            ele_range = paste0(min(elevation),"-",max(elevation)),
            geometry = st_cast(st_combine(geometry),"LINESTRING")) %>% 
  mutate(dist_m = st_length(geometry))

grouped_trip %>% mutate(tp = paste0("Trip :",trip,"<br> Trip Period: ",period,"<br> Elevation Range:",
                                    ele_range,"<br>Trip Length (days) :",days, "<br>Travelled Distance in KM : ",round(dist_m/1000,2)),
                        days_sqrt = days^3) %>% 
mapdeck::mapdeck(style = mapdeck_style(style = "dark")) %>% 
  # add_pointcloud(fill_colour = "trip")
  add_path(stroke_colour = "trip",tooltip = "tp",stroke_width = "days_sqrt",legend = TRUE) %>% 
  add_title(title = "Field Trips traces")

