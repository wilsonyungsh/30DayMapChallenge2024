pacman::p_load(tidyverse,sf,osmdata,mapgl)


brisbane_toilets<-
opq("Brisbane",osm_types = "node") %>% 
  osmdata::add_osm_feature(key = "amenity",value = "toilets",) %>% 
  osmdata::osmdata_sf() %>% 
  pluck("osm_points") %>% 
  select(osm_id,geometry)

mapboxgl(bounds = brisbane_toilets) %>% 
  add_circle_layer(id = "toilet",source = brisbane_toilets,circle_color = "white",circle_radius = 5,circle_stroke_color = "black",circle_stroke_width = 1,
                   cluster_options = list(
                     max_zoom = 13,
                     cluster_radius = 20,
                     color_stops = c("#51bbd6", "#f1f075", "#f28cb1"),
                     radius_stops = c(20, 30, 40),
                     count_stops = c(0, 50, 100),
                     circle_blur = NULL,
                     circle_opacity = NULL,
                     circle_stroke_color = NULL,
                     circle_stroke_opacity = NULL,
                     circle_stroke_width = NULL
                   ))
