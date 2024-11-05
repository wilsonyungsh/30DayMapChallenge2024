# load library
pacman::p_load(tidyverse,sf,strayr,readabs,tidytransit,ggrepel,ggspatial)
source("r/get_seq_gfts.R")
aoi_sa4<-strayr::read_absmap(name = "sa42021") %>% 
  filter(sa4_code_2021 %in% 301:305) %>% mutate(lbl = str_remove_all(sa4_name_2021,"Brisbane - ")) %>% 
  st_transform(4283)
aoi_bbox <- aoi_sa4 %>% filter(sa4_code_2021 !=301) %>% st_bbox()
## Create PT routes
seq<-read_gtfs("data/seq_gtfs.zip_20241031")


seq$routes$route_type %>% unique()

# get pt routes shape
shape_sf <- seq$shapes %>% st_as_sf(coords = c("shape_pt_lon","shape_pt_lat"),crs = 4283) %>% group_by(shape_id) %>% 
  summarise(geometry = st_cast(st_combine(geometry),"LINESTRING"))

brisbane_pt_route<-
  seq$trips %>% distinct(route_id,shape_id,trip_headsign,direction_id) %>% 
  left_join(seq$routes %>% distinct(route_id,route_type,route_short_name,route_color,route_text_color) %>% 
              mutate(route_type = case_match(route_type, 0 ~ "TRAM",2 ~ "RAIL", 3 ~ "BUS", 4 ~ "FERRY")),by = "route_id") %>% 
  left_join(shape_sf,by = "shape_id") %>% st_as_sf() %>% 
  st_filter(aoi_sa4)


## mapping
m2<-brisbane_pt_route %>% 
  ggplot() +
  geom_sf(data = aoi_sa4,fill = "grey80",   # Light grey fill for the polygons
          color = "white",  # Border color, slightly darker grey
          linewidth = 0.8
          ) +
  geom_sf(aes(col = route_type,linewidth = route_type),   # Light grey fill for the polygons
          alpha = 0.5) +
  scale_linewidth_manual(values = c("BUS" = 0.1, "RAIL" = 1.5, "FERRY" = 1)) +
  guides(linewidth = "none") +  # Remove linewidth legend
  scale_color_manual(
    name = "PT Route Type",
    values = c("RAIL" = "#ff8000",
               "FERRY" = "#002aff",
               "BUS" = "#000000")
  ) +
  geom_sf_text(data = aoi_sa4, 
               aes(label = str_replace_all(lbl," ","\n")), # Replace with your label column name
               color = "black",                # Label color
               size = 5,                       # Font size for labels
               fontface = "bold",              # Font style
               # nudge_y = 0.2                   # Adjust label position if needed
  ) +
  coord_sf(xlim = c(aoi_bbox[1],aoi_bbox[3]+0.08),
           ylim = aoi_bbox[c(2,4)],
           expand = FALSE) +
  labs(x ="", y = "", title = "30 Day Map Challenge #2 Line : \nBrisbane Public Transport Coverage",
       caption = "Data Source : Translink ; ABS 2021 \nAuthor : Wilson Yung") +
  annotation_north_arrow(
    location = "tl",  # Top-left corner
    which_north = "true",  # True north
    style = north_arrow_fancy_orienteering()  # Style of the north arrow
  ) +
  annotation_scale(location = "tr", width_hint = 0.4) +  # Bottom-left corner with a width hint
  theme(
    legend.position = c(0.8,0.9),  # Place legend at the bottom
    # plot.background = element_rect(fill = "grey70", color = NA),
    panel.background = element_rect(fill = "grey70", color = NA),
    legend.box = "horizontal",   # Horizontal layout for legend items
    legend.background = element_rect(fill = "grey70", color = NA),  # Light background
    legend.title = element_text(face = "bold", size = 10),  # Bold and larger title
    legend.text = element_text(size = 9),  # Larger legend text
    legend.key = element_rect(fill = "white", color = NA),  # White legend keys
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
  )

ggsave(plot = m2,filename = "maps/BrisbanePTRoutes#2.png",dpi = 400,device = "png",width = 20, height = 10,units = "in") 


