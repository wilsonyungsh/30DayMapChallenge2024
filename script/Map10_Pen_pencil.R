pacman::p_load(tidyverse,osmdata,sf,mapgl)
source("r/get_asgs21_boundary.R")

aus<-get_asgs21_boundary(level = "ste") %>% filter(state_code_2021 %in% 1:9) %>% 
  dplyr::select(state_name_2021,area_albers_sqkm) %>% st_transform(7845) %>% 
  st_simplify(preserveTopology = FALSE,dTolerance = 0.01)

place <- get_asgs21_boundary(structure = "NON_ABS",level = "sal_2022") %>% dplyr::select(sal_name_2021,sal_code_2021, area_albers_sqkm)


place<- 
opq("Australia") %>% 
  add_osm_feature(key = "place",value = "city") %>% 
  osmdata::osmdata_sf() %>% pluck("osm_points")

place_names <- c("sydney","melbourne","brisbane","adeliade","perth","cairns","darwin","warrnambool")
place %>% filter()

aus  %>% 
ggplot() +
  geom_sf(fill = "grey50") +
  theme_minimal() +
  labs(title = "Australian City Map") +
  coord_sf(crs = 7854)


mapgl::mapboxgl(bounds = aus)
  

  