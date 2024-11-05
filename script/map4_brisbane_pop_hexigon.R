pacman::p_load(tidyverse,sf,mapgl,mapdeck,htmltools)
invisible(source("r/get_asgs21_boundary.R"))
# get mb data
mb<-get_asgs21_boundary(structure = "ABS",level = "MB")
#get mb level population count
mb_pop_cnt_url <- "https://www.abs.gov.au/census/guide-census-data/mesh-block-counts/2021/Mesh%20Block%20Counts%2C%202021.xlsx"
curl::curl_download(url = mb_pop_cnt_url,destfile = "data/au_mb_2021_pop_cnt.xlsx")
mb_pop<- map(c("Table 3","Table 3.1"),
             ~ readxl::read_xlsx("data/au_mb_2021_pop_cnt.xlsx",sheet = .x,skip = 6)) %>% bind_rows() %>% 
  janitor::clean_names() %>% 
  filter(!is.na(dwelling))


## join data for mapping
aoi_sa4<-strayr::read_absmap(name = "sa42021") %>% 
  filter(sa4_code_2021 %in% c(301:305)) %>% 
  mutate(lbl = str_remove_all(sa4_name_2021,"Brisbane - ")) %>% st_transform(7844)
aoi_bbox <- aoi_sa4 %>% filter(sa4_code_2021 !=301) %>% st_bbox()

aoi_bne_mb_pop <- 
mb %>% select(-c(change_flag_2021,change_label_2021,sa2_code_2021,sa3_code_2021,aus_code_2021,aus_name_2021,asgs_loci_uri_2021,area_albers_sqkm)) %>% 
  inner_join(mb_pop, by = "mb_code_2021") %>% filter(sa4_code_2021 %in% c(301:305)) %>% 
  mutate(across(c(person,dwelling), list(rank = ~ ntile(n = 10,.x))))


## make grid cover area of interest
hex_grid <- st_make_grid(aoi_bne_mb_pop, cellsize = 0.01, square = FALSE) %>% 
  st_as_sf() %>% st_filter(aoi_sa4) %>% mutate(hex_id = row_number())



## need to properly distribute population numbers and dwellings by area proportion (WIP)
mb_hex_b4_agg<-
  hex_grid %>% st_join(aoi_bne_mb_pop %>% filter(sa4_code_2021 %in% 301:305),join = st_contains)

hex_agg<-mb_hex_b4_agg %>% group_by(hex_id) %>% summarise(across(c(dwelling,person),sum)) %>% rename(geom = x) %>% 
  mutate(across(c(person,dwelling), list(rank = ~ ntile(n = 10,.x))),
         tp = paste0("Total Pop : ",person,
                     "<br>Total Dwellings : ",dwelling))

# Intersect the hex grid with the polygon to get hexagons within the polygon


# aoi_bne_mb_pop %>% 
#   ggplot() +
#   geom_sf(aes(fill = person)) +
#   theme_minimal() +
#   coord_sf(xlim = c(aoi_bbox[1]-0.2,aoi_bbox[3] + 0.2),
#            ylim = c(aoi_bbox[2],aoi_bbox[4] + 0.1),
#            expand = FALSE) 

hex_agg %>% ggplot() + geom_sf(aes(fill = person),alpha = 0.5,col = NA)  +
  scale_fill_gradientn(
    name = "Population Count",
    colors = hcl.colors(palette = "Inferno",n = 10),
    labels = c("Low","Medium","High"),
    breaks = c(1,5,10)
  ) +
  theme(
    legend.direction = "vertical",
    legend.text = element_text(margin = margin(t = 5, b = 5))  # Add spacing above and below labels
  )
    coord_sf(xlim = c(aoi_bbox[1]-0.2,aoi_bbox[3] + 0.2),
             ylim = c(aoi_bbox[2],aoi_bbox[4] + 0.1),
             expand = FALSE) +




mapboxgl(bounds = hex_agg,style = carto_style(style_name = "positron")) %>% 
  # add_fill_layer(source = aoi_sa4,fill_color = "grey",fill_opacity = 0.2,id = "base",
  #                fill_outline_color = "black") %>% 
  add_fill_layer(source = hex_agg, fill_opacity = 0.4,
                 fill_color = match_expr(
                   "dwelling_rank",
                   values = 1:10,
                   stops = hcl.colors(palette = "Inferno",n = 10),
                 ),
                 tooltip = "tp",
                 id = "person",
                 hover_options = list(
                   fill_color = "#FF0000",  # Red color on hover
                   fill_opacity = 0.7      # Increase opacity on hover for emphasis
                 )) %>% 
 add_legend(legend_title = "Brisbane Dwelling Concentration",position = "top-right",
            values = c("low","medium","high"),colors = hcl.colors(palette = "Inferno",n = 10),type = "continuous") 




# use mapdeck

aoi_bne_mb_pop %>% mapdeck(style = mapdeck_style(style = "dark")) %>% 
  add_hexagon(radius = 500,colour = "person")
