pacman::p_load(tidyverse,sf,deckgl,htmlwidgets,strayr,tidytransit)

# source function to download gtfs
source("r/get_seq_gfts.R")

# 1. Download GTFS data
get_seq_gtfs()


# 2. Define Greater Brisbane Area
aoi_sa4<-strayr::read_absmap(name = "sa42021") %>% filter(sa4_code_2021 %in% 301:305) %>% mutate(lbl = str_remove_all(sa4_name_2021,"Brisbane - "))
aoi_bbox <- aoi_sa4 %>% filter(sa4_code_2021 !=301) %>% st_bbox()
## Create PT stops by type
seq<-read_gtfs("data/seq_gtfs.zip_20241031")
stop_sf<-seq$stops %>% select(stop_id,stop_name,stop_lat,stop_lon,parent_station) %>% 
  st_as_sf(coords = c("stop_lon","stop_lat"),crs = 4326) %>% 
  mutate(x = st_coordinates(.)[,1],y = st_coordinates(.)[,2])


## Enrich PT stops by stop types from routes and trips
stop_summary<-
  seq$stop_times %>% distinct(stop_id,trip_id) %>% 
  left_join(seq$trips %>% distinct(route_id,service_id,trip_id),by = "trip_id") %>% 
  left_join(seq$routes %>% distinct(route_id,route_type) %>% 
              mutate(stop_type = case_match(route_type, 0 ~ "TRAM",2 ~ "RAIL", 3 ~ "BUS")),by = "route_id") %>% 
  group_by(stop_id,stop_type) %>% summarise(stop_type = paste0(unique(stop_type),collapse = "|"),
                                            across(c("trip_id","route_id"),.fns = list(unique_cnt = ~ n_distinct(.x)))) 

seq_pt_stops<-
  stop_sf %>% inner_join(stop_summary,by = "stop_id") %>% 
  group_by(stop_type) %>% mutate(service_cnt_ranking_percentile = ntile(desc(trip_id_unique_cnt),n = 100))


seq_rail_summary <- 
  seq_pt_stops %>% filter(stop_type %in% "RAIL") %>% mutate(stn_name = str_remove_all(stop_name,", .*")) %>% 
  st_drop_geometry() %>% 
  group_by(parent_station,stn_name,stop_type) %>% summarise(platform_ids = paste0(stop_id,collapse = "|"),
                                                            platform_cnt = n(),
                                                            across(c(x,y),.fns = mean),
                                                            service_cnt = sum(trip_id_unique_cnt)) 
  st_as_sf(coords = c("x","y"),crs = 4326,remove = FALSE)

  seq_bus_summary <- 
    seq_pt_stops %>% filter(stop_type %in% "BUS") %>% mutate(stn_name = str_remove_all(stop_name,", .*")) %>% 
    st_drop_geometry() %>% 
    group_by(parent_station,stn_name,stop_type) %>% summarise(platform_ids = paste0(stop_id,collapse = "|"),
                                                              platform_cnt = n(),
                                                              across(c(x,y),.fns = mean),
                                                              service_cnt = sum(trip_id_unique_cnt)) 
  st_as_sf(coords = c("x","y"),crs = 4326,remove = FALSE)
  

##

# 4. set color PALETTE

pal <- scales::col_quantile(
  "viridis",
  seq_bus_summary$service_cnt,
  n = 8
)

seq_bus_summary$color <- pal(
  seq_bus_summary$service_cnt
)


seq_bus <- seq_bus_summary %>%  ungroup() %>% 
  select(stn_name,service_cnt,x,y,color) %>% st_drop_geometry%>% 
  mutate(ht = service_cnt/100,
    tp = paste0(
    "Stop: ", stn_name, "<br>",
    "Service Count: ", service_cnt
  ))


# Deckgl viz  
map <- deckgl::deckgl(
    latitude = -27.470409456759125,
    longitude = 153.01213937458263,
    zoom = 10,
    pitch = 45
  ) |>
    deckgl::add_column_layer(
      data = seq_bus,
      properties = list(
        getPosition = ~c(x, y),    # Position from x, y
        getElevation = ~ht,       # Extrusion height based on `ht`
        getFillColor = ~color,    # Color mapped to `color`
        getLineColor = c(0, 0, 0), # Optional: black border
        radius = 100,             # Radius of columns
        extruded = TRUE,          # Enable 3D extrusion
        elevationScale = 50,      # Scale extrusion for better visibility
        tooltip = "Stop : {{stn_name}}<br> Services : {{service_cnt}}"        # Tooltip with station info
      )) %>% 
    deckgl::add_basemap(deckgl::use_carto_style())

  # Wrap with an HTML title and full page
  html_page <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$style(
        "html, body { margin: 0; padding: 0; width: 100%; height: 100%; overflow: hidden; } 
      #map-container { position: absolute; top: 10%; left: 0; right: 0; bottom: 0; }
      h1 { position: absolute; top: 0; width: 100%; text-align: center; margin: 0; 
      padding: 10px; background: white; z-index: 1000; font-family: Arial; }"
      )
    ),
    htmltools::tags$h1("SEQ Bus Stop Service Density"),
    htmltools::tags$div(id = "map-container", map)
  )
  # Display the map with the title
  htmltools::browsable(html_page)
  
#save
  htmltools::save_html(html_page,file = "maps/map18_seq_bus_service_density.html")
  # htmlwidgets::saveWidget(
  #   map, file = "maps/map18_seq_bus_service_density.html",
  #   selfcontained = TRUE
  # )

