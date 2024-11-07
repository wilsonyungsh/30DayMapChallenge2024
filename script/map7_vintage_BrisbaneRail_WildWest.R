
#### Create a vintage style map looking for train station and train line in Brisbane


#load library
pacman::p_load(osmdata,tidyverse,sf,mapgl,terra,raster)
source("r/get_asgs21_boundary.R")

## get Greater Brisbane Area boundary 
gccsa<-get_asgs21_boundary(structure = "ABS",level = "gccsa") %>% filter(gccsa_code_2021 == "3GBRI")

# osm train station extraction
train_stns<-
  osmdata::opq(bbox = unname(st_bbox(gccsa)),nodes_only = TRUE) %>% 
  osmdata::add_osm_feature (key = "railway",value = c("station")) %>% 
  osmdata::osmdata_sf (quiet = FALSE) %>% 
  pluck("osm_points") %>% 
  dplyr::select(name,network,operator)

# osm train line extraction
train_lines<-osmdata::opq(bbox = unname(st_bbox(gccsa)),osm_types = "way") %>% 
  osmdata::add_osm_feature (key = "railway",value = "rail") %>% 
  osmdata::osmdata_sf (quiet = FALSE) %>% 
  pluck("osm_lines") %>% 
  dplyr::select(osm_id,name,usage)

## osm road extraction
rd <- osmdata::opq(bbox = unname(st_bbox(gccsa)),osm_types = "way") %>% 
  osmdata::add_osm_feature (key = "highway",value = c("motorway","trunk","primary","secondary")) %>% 
  osmdata::osmdata_sf (quiet = FALSE) %>% 
  pluck("osm_lines") %>% 
  dplyr::select(osm_id,name,highway)

## place name from osm

aoi_osm_place_extract<-
  osmdata::opq(bbox = bx[c(1,3,2,4)],osm_types = "node",nodes_only = TRUE) %>% 
  osmdata::add_osm_feature (key = "place",value = "suburb") %>% 
  osmdata::osmdata_sf (quiet = FALSE) %>% 
  pluck('osm_points') %>% dplyr::select(osm_id,name,highway)

list(road = rd,stn = train_stns,
     in_stns,railine = train_lines) %>% imap(~st_write(.x,"data/mapchallenge.gpkg",layer = .y,append = FALSE))

# quick vis via mapgl
mapgl::mapboxgl(style = carto_style(style_name = "positron"),bounds = gccsa) %>% 
  mapgl::add_line_layer(id = "railway",source = train_lines %>% filter(usage == "main"),line_gap_width = 5,line_width = 2,) %>% 
  mapgl::add_circle_layer(source = train_stns,id = "stns",circle_radius = 8) %>%
  mapgl::add_symbol_layer(id = "stn_symbols",source = train_stns,icon_image = "marker-15",icon_allow_overlap = TRUE,
                          tooltip = "name") %>% 
  mapgl::add_line_layer(source = rd,id = "road")




