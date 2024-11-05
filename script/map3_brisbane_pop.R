# load library
pacman::p_load(tidyverse,sf,strayr,readabs,tidytransit,ggrepel,ggspatial,cowplot)

aoi_sa2<-strayr::read_absmap(name = "sa22021") %>% 
  filter(sa4_code_2021 %in% c(301:305,310,311,314)) %>% mutate(lbl = str_remove_all(sa4_name_2021,"Brisbane - ")) %>% 
  st_transform(4283) %>% 
  select(starts_with("sa2"),lbl,areasqkm_2021,cent_lat,cent_long)

aoi_sa4<-strayr::read_absmap(name = "sa42021") %>% 
  filter(sa4_code_2021 %in% c(301:305)) %>% 
  mutate(lbl = str_remove_all(sa4_name_2021,"Brisbane - "))
aoi_bbox <- aoi_sa4 %>% filter(sa4_code_2021 !=301) %>% st_bbox()
# construct abs api query
read_api_dataflows() %>% view()
meta <- read_api_datastructure(id = "C21_G01_SA2") 
aoi_sa2_pop<- read_api(id = "C21_G01_SA2",
                       datakey = list(sexp = 3,
                                      pchar = c("P_1",meta %>% filter(grepl(pattern = "Age groups",label)) %>% .$code),
                                      region_type = "SA2")) %>% 
  filter(substr(region,1,3) %in% c(301:305,310,311,314)) %>% 
  mutate(age_group = case_match(pchar, c("0_4","5_14") ~ "under_15", 
                                c("65_74","75_84","GE85") ~ "over_65",
                                "P_1" ~ "Total", 
                                .default =  "16to64")) %>% 
  group_by(region,region_type,age_group) %>% 
  summarise(pop = sum(obs_value)) %>% ungroup() %>% 
  # mutate(sa2_name = labelled::to_character(region)) %>% 
  select(-region_type) %>%   rename(sa2 = region)


aoi_pop_sa2_sf<-
aoi_sa2 %>% left_join(aoi_sa2_pop,by = c("sa2_code_2021" = "sa2")) %>% st_simplify(dTolerance = 0.0001)

# prepare data for mapping
map_pop <- 
aoi_pop_sa2_sf %>% filter(age_group == "Total") %>% mutate(pop_rank = ntile(pop,100)) 
  

aoi_sa4_map <- aoi_sa4 %>% inner_join(map_pop %>% st_drop_geometry %>% group_by(lbl) %>% 
                                    summarise(pop = sum(pop)),by = "lbl") 
sa4_tbl <- 
aoi_sa4_map %>% st_drop_geometry() %>% select(lbl,pop) %>% arrange(desc(pop)) %>%
  mutate(pop = scales::comma(pop),lbl = str_replace_all(lbl," ","\n")) %>% 
  rename(Region = lbl)

m3 <-  ggplot(data = map_pop) +
  geom_sf(aes(fill = pop_rank),col = NA,alpha = 0.9) +
  scale_fill_gradientn(
    name = "Population Count",
    colors = RColorBrewer::brewer.pal(name = "BuPu",n = 8),
    labels = c("Low","High"),
    breaks = range(map_pop$pop_rank, na.rm = TRUE),
    # trans = "reverse"
  ) +
    geom_label_repel(data = map_pop %>% filter(pop_rank>90) , 
                     aes(label = sa2_name_2021,
                         x = cent_long,
                         y = cent_lat), # Replace 'station_name' with your station label column
                     color = "black",
                     size = 2.5,
                     segment.color = "black",    # Color of the leader line
                     segment.size = 0.5,         # Size of the leader line
                     box.padding = 0.5,          # Space around the label box
                     point.padding = 0.5,        # Space around the points
                     nudge_y = 0.05,               # Adjust position if needed
                     nudge_x = 0.05
    ) +
  geom_sf(data = aoi_sa4_map,fill = NA,   # Light grey fill for the polygons
          color = "black",  # Border color, slightly darker grey
          linewidth = 0.5
  ) +
  geom_sf_text(data = aoi_sa4_map, 
               aes(label = str_replace_all(lbl," ","\n")), # Replace with your label column name
               color = "black",                # Label color
               size = 5,                       # Font size for labels
               fontface = "bold",              # Font style
               nudge_x = -0.02                   # Adjust label position if needed
  ) +
  coord_sf(xlim = c(aoi_bbox[1]-0.2,aoi_bbox[3] + 0.2),
           ylim = c(aoi_bbox[2],aoi_bbox[4] + 0.1),
           expand = FALSE) +
    theme(
      panel.background = element_rect(fill = "#00bfff50", color = NA),
      legend.position = c(0.92,0.85),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
    ) +
  labs(x ="", y = "", title = "30 Day Map Challenge #3 Polygon : \nBrisbane Population by SA2",
       caption = "Data Source : ABS 2021 \nAuthor : Wilson Yung") +
    annotation_north_arrow(
      location = "tl",  # Top-left corner
      which_north = "true",  # True north
      style = north_arrow_fancy_orienteering()  # Style of the north arrow
    ) +
    annotation_scale(location = "tr", width_hint = 0.4) 
ggsave(plot = m3,filename = "maps/BrisbanePopSA2#3.png",dpi = 400,device = "png",width = 20, height = 10,units = "in") 

  
