

## data souce NGA Arctic data application
# Arctic DEM products 
# 1. https://arctic-nga.opendata.arcgis.com/datasets/1079fa1d09954781878e2437dbab87d2_0/explore
# 2. Subset for Arctic region of World Martime Boundaries https://arctic-nga.opendata.arcgis.com/datasets/193c4fdcfb9c49acb667e81040b5e19b_0/explore
# 3. Daily Arctic Shapefile - https://usicecenter.gov/File/DownloadCurrent?pId=2

pacman::p_load(tidyverse,sf,mapgl)

## see layer names
st_layers("data/arctic/Arctice_DEM_Products_NEW_2858859717152080327.gpkg")
st_layers("data/arctic/Arctic_Maritime_Boundaries_-3785870301411136865.gpkg")

# load data
arctic <- st_read("data/arctic/Arctice_DEM_Products_NEW_2858859717152080327.gpkg",layer = "Arctic_DEMs")
arctic_marine_boundary <- st_read("data/arctic/Arctic_Maritime_Boundaries_-3785870301411136865.gpkg",layer = "Arctic_Maritime_Boundaries")
ice<-st_read("data/arctic/nic_autoc2024316n_pl_a/nic_autoc2024316n_pl_a.shp")

mapgl::mapboxgl(bounds = arctic,style = mapbox_style(style_name = "dark")) %>% 
  add_fill_layer(id = "ice",source = ice,fill_color = "white",fill_opacity = 0.5) %>% 
  add_line_layer(id = "maritime",source = arctic_marine_boundary,line_color = "white",tooltip = "Boundary",line_width = 4,
                 hover_options = list(
                   line_color = "yellow",
                   fill_opacity = 1)) %>% 
  add_circle_layer(id = "dem",source = arctic,circle_radius = 4,circle_color = "red",circle_stroke_color = "black",circle_stroke_width = 5,
                   tooltip = "description",
                   hover_options = list(
                     circle_color = "yellow",
                     circle_radius = 12)) 

