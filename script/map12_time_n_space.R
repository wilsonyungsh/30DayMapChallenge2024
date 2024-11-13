## 2021 and 2016 MB land use change

pacman::p_load(tidyverse,sf,mapgl,htmlwidgets)

invisible(map(list.files("r/",full.names = TRUE),source))
## get mb 2021 and 2016 geom

gcc <- paste0("Greater ",c("Sydney","Melbourne","Brisbane","Adelaide","Perth","Hobart","Darwin"))

mb_2021_gcc<-get_asgs21_boundary(level = "mb") %>% filter(gccsa_name_2021 %in% gcc) %>% select(contains(c("category","gccsa_name","sqkm"))) %>% 
  setNames(c("landuse","name","areasqkm","geom"))
mb_2016_gcc <- get_asgs2016_boundary(level = "MB")%>% filter(gccsa_name_2016 %in% gcc) %>% select(contains(c("category","gccsa_name","sqkm"))) %>% 
  setNames(c("landuse","name","areasqkm","geom"))

mb_2016_gcc$landuse %>% unique
mb_2021_gcc$landuse %>% unique

stats<-list(yr2021 = mb_2021_gcc,
     yr2016 = mb_2016_gcc) %>% lapply(function(x) x %>% st_drop_geometry %>% group_by(landuse,name) %>% 
                                        summarise(areasqkm = sum(areasqkm))) %>% bind_rows(.id = "year")

res_parkland_stats<-stats %>% filter(landuse %in% c("Residential","Parkland")) %>% 
  pivot_wider(names_from = year,values_from = areasqkm) %>% 
  mutate(change_pr = 100*(yr2021-yr2016)/yr2016)


mb_16_res_parklands<- mb_2016_gcc %>% filter(landuse %in% c("Residential","Parkland")) 
mb_21_res_parklands<- mb_2021_gcc %>% filter(landuse %in% c("Residential","Parkland")) 


sydney_nw <- c(150.96010631536652,-33.696669141153095)

m2016 <- 
mapboxgl(center = sydney_nw,,zoom = 8) %>% 
  add_fill_layer(id = "mb2016",source = mb_16_res_parklands %>% mutate(year = "2016"),
                 fill_color = match_expr(
                   "landuse",
                   values = c("Residential","Parkland"),
                   stops = c("#db254770","#08803290")),
                 hover_options = list(fill_color = "yellow"),
                 tooltip = "year")%>% 
  add_categorical_legend(legend_title = "Landuse",                   
                         values = c("Residential","Parkland"),
                         colors = c("#db254770","#08803290"))

m2021 <- 
  mapboxgl(center = sydney_nw,,zoom = 8) %>% 
  add_fill_layer(id = "mb2021",source = mb_21_res_parklands %>% mutate(year = "2021"),
                 fill_color = match_expr(
                   "landuse",
                   values = c("Residential","Parkland"),
                   stops = c("#db254770","#08803290")),
                 hover_options = list(fill_color = "yellow"),
                 tooltip = "year")%>% 
  add_categorical_legend(legend_title = "Landuse",                   
                         values = c("Residential","Parkland"),
                         colors = c("#db254770","#08803290"))


comparison_map <- compare(m2016,m2021)
# Export the map as an HTML file
saveWidget(comparison_map, "data/Map12_landuse_change.html",selfcontained = TRUE)

