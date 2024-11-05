# load library
pacman::p_load(tidyverse,sf,strayr,readabs,tidytransit,ggrepel,ggspatial)
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
seq_pt_stops %>% filter(stop_type == "RAIL") %>% mutate(stn_name = str_remove_all(stop_name,", .*")) %>% 
  st_drop_geometry() %>% 
  group_by(parent_station,stn_name,stop_type) %>% summarise(platform_ids = paste0(stop_id,collapse = "|"),
                                                            platform_cnt = n(),
                                                            across(c(x,y),.fns = mean),
                                                            service_cnt = sum(trip_id_unique_cnt)) %>% 
  ungroup() %>% mutate(ratio = service_cnt/sum(service_cnt),
                       service_cnt_ranking_percentile = ntile(desc(service_cnt),n = 100)) %>% 
  arrange(desc(ratio)) %>% 
  st_as_sf(coords = c("x","y"),crs = 4283,remove = FALSE)

seq_bus_summary <- 
  seq_pt_stops %>% filter(stop_type == "BUS") 


seq_rail_summary %>% st_write("data/seq.gpkg",layer = "seq_rail_summary_20241031",append = FALSE)
seq_bus_summary %>% st_write("data/seq.gpkg",layer = "seq_bus_summary_20241031",append = FALSE)
mapview::mapview(seq_rail_summary)

## Vis maps
sa4_union <- aoi_sa4 %>% summarise() %>% st_transform(4283)

st_filter(seq_rail_summary , sa4_union,.predicate = st_within)
mapview::mapview()



# set colors
cols <- hcl.colors(
  6, "Viridis"
) 
## set minimum
theme_for_the_win <- function() {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = c(0.8, 0.2),  # Position the legend inside the map (adjust coordinates as needed)
      legend.background = element_rect(fill = alpha("white", 0.5)),  # Optional: makes the legend background semi-transparent
      # axis.ticks = element_blank(),
      legend.position = c(.8, .1),
      legend.title = element_text(
        size = 8, color = "grey80",
        vjust = -20
      ),
      legend.text = element_text(
        size = 7, color = "grey80"
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(
        fill = "black"
      ),
      plot.margin = unit(
        c(
          t = -1, r = -1,
          b = -1, l = -1
        ), "cm"
      )
    )
}

## plotting

seq_rail_summary %>% ggplot() +
  geom_sf(data = aoi_sa4,fill = "grey80",   # Light grey fill for the polygons
          color = "grey50",  # Border color, slightly darker grey
          alpha = 0.5) +
  geom_sf_text(data = aoi_sa4, 
               aes(label = str_replace_all(lbl," ","\n")), # Replace with your label column name
               color = "black",                # Label color
               size = 3,                       # Font size for labels
               fontface = "bold",              # Font style
               # nudge_y = 0.2                   # Adjust label position if needed
  ) +
  geom_sf(aes(col = service_cnt_ranking_percentile),size = 4,
          linewidth = 0) +
  geom_label_repel(data = seq_rail_summary %>% filter(service_cnt_ranking_percentile<4), 
                 aes(x = x, 
                     y = y, 
                     label = stn_name), # Replace 'station_name' with your station label column
                 color = "black",
                 size = 3.5,
                 segment.color = "black",    # Color of the leader line
                 segment.size = 0.5,         # Size of the leader line
                 box.padding = 0.5,          # Space around the label box
                 point.padding = 0.5,        # Space around the points
                 nudge_y = -0.05,               # Adjust position if needed
                 nudge_x = 0.1
) +
  scale_color_gradientn(
    name = "Services Count",
    colors = cols,
    na.value = cols[1],
    labels = c("High","Low"),
    breaks = range(seq_rail_summary$service_cnt_ranking_percentile, na.rm = TRUE),
    trans = "reverse"
  ) +
  coord_sf(xlim = c(aoi_bbox[1],aoi_bbox[3]+0.08),
           ylim = aoi_bbox[c(2,4)],
           expand = FALSE) +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barheight = unit(
        .2,
        units = "cm"
      ),
      barwidth = unit(
        5,
        units = "cm"
      ),
      label.position = "bottom",
      title.position = "top",
      label.hjust = .15,
      drop = FALSE
    )
  ) +
  labs(x ="", y = "", title = "30 Day Map Challenge #1 Point : \nBrisbane Train Services density",caption = "Data Source : SEQ GTFS data extracted from Translink on 31st Oct 2024\nAuthor : Wilson Yung") +
  theme_minimal() +
  annotation_north_arrow(
    location = "tl",  # Top-left corner
    which_north = "true",  # True north
    style = north_arrow_fancy_orienteering()  # Style of the north arrow
  ) +
  annotation_scale(location = "tr", width_hint = 0.4)  # Bottom-left corner with a width hint
 
  



