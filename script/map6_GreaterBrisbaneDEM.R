## Download the data from GeoScience Australia 
  ## Queensland LiDAR Data - Brisbane 2019 Project - 
## Resolution: 1M grid
# https://qldspatial.information.qld.gov.au/catalogue/custom/viewMetadataDetails.page?uuid=%7BE5EAAFE5-3EDA-4FC6-B41E-78237CDEC0BE%7D

pacman::p_load(
  tidyverse,terra,sf,strayr,raster,ggrepel,ggspatial
)


# load the all rasters 
bne_1m_dem_list <- lapply(list.files("data/QLD Government/DEM/1 Metre/",pattern = ".tif",full.names = TRUE) , 
                                terra::rast)

# read area of interest sf, transform into GDA94 / MGA zone 56 (EPSG:28356) 
sa2<- c(304021088:304021090,304031092,304031094:304031097,305041137)
sa2_bne<- read_absmap("sa22021") %>% filter(sa2_code_2021 %in% sa2) %>% 
  dplyr::select(sa2_code_2021,sa2_name_2021,areasqkm_2021) %>% 
  st_transform(28356)


# combine raster 
combine_raster <- do.call(
  terra::mosaic,
  bne_1m_dem_list
)
# crop and mask area of interest
aoi_raster<- 
  terra::crop(combine_raster,terra::vect(sa2_bne),
              snap = "in", # only inside
              mask = TRUE ) 


## re-sampling to reduce size, usually lower by 10 times and convert to dataframe - for ggplot
bne_1m <- aoi_raster |>
  terra::aggregate(
    fact = 10 # both dimension reduce how much 
  ) |>
  as.data.frame(
    xy = T
  ) %>% setNames(c("x","y","elevation"))


color_break <- round(
  classInt::classIntervals(
    bne_1m$elevation,
    n = 10,
    style = "pretty"
  )$brks, 0)
## plot using ggplot2
m6 <- ggplot(bne_1m) +
  geom_tile(
    aes(x = x,y = y,fill = elevation)) + 
  guides(col = guide_legend(title = "",alpha = "none")) +
  labs(title = "Brisbane Western Suburb Elevation Map", x = "", y = "",
    caption  = "Data source: Queensland LiDAR Data Brisbane 2019 1M grid") +
  scale_fill_gradientn(
    name = "Elevation (m)",
    colors = terrain.colors(n = 10),
    breaks = color_break,
    labels =ifelse(color_break %in% c(0, 50, 100), as.character(color_break), "")
  ) +
  geom_sf(data = sa2_bne,fill = NA, col = "black", size = 4) +
  geom_sf_label(data = sa2_bne,aes(label = sa2_name_2021)) +
  coord_sf(crs = 28356) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "grey90"),
        panel.grid = element_blank()) +
  annotation_north_arrow(
    location = "tl",  # Top-left corner
    which_north = "true",  # True north
    style = north_arrow_fancy_orienteering()  # Style of the north arrow
  ) +
  annotation_scale(location = "tr", width_hint = 0.4)  # Bottom-left corner with a width hint

ggsave(plot = m6,filename = "maps/BrisbaneElevation_map6_2.png",dpi = 400,device = "png",width = 20, height = 10,units = "in") 


### Below was my failed attempt to render terrain on mapboxgl

# Test on mapgl

# bne_1mraster_84<-project(combine_raster,'EPSG:4326') 
out_path <- "data/bne_1m_raster.tif"
terra::writeRaster(combine_raster, out_path, overwrite = TRUE)
plot(raster::raster(out_path))

# Use the saved GeoTIFF in the mapgl map
mapgl::mapboxgl(center = c(153.00852180109464, -27.482170216588077), zoom = 10) %>%
  mapgl::add_raster_layer(id = "dem", source = out_path, raster_opacity = 0.8)

mapgl::mapboxgl(center = c(153.00852180109464, -27.482170216588077), zoom = 10) %>% 
  mapgl::add_raster_source(id = "dem_source",url = "https://shorturl.at/9hIbp") %>% 
  mapgl::add_raster_layer(id = "raster_layer",source = "dem_source")

library(mapdeck)

bne_1m_84_xy <- aoi_raster |>project('EPSG:4326') %>% 
  terra::aggregate(
    fact = 10 # both dimension reduce how much 
  ) |>
  as.data.frame(
    xy = T
  ) %>% setNames(c("x","y","elevation"))
mapdeck::mapdeck(style = mapdeck_style(style = "dark"),pitch = 45,bearing = 50) %>% 
  add_grid(data = bne_1m_84_xy,lon = "x",lat = "y",elevation = "elevation")
