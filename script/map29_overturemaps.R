# Load package
pacman::p_load(sf,tidyverse,deckgl,mapdeck,jsonlite)

# manually got bbox for greater taipei
greaterTaipei_bbox <- c(121.41100382712675,24.962333771835905,121.59908493722324,25.20048183516369)

#put this into bbox parameter
paste0(greaterTaipei_bbox,collapse = ",")
# Python command line = run in terminal
# https://docs.overturemaps.org/getting-data/overturemaps-py/
#overturemaps download --bbox=121.411003827127,24.9623337718359,121.599084937223,25.2004818351637 -f geojson --type=building -o data/greaterTaipei.geojson

tpe<-c(121.53237725750728,25.068595184751306)
# read the geojson and parse names
tpe_building<-st_read("data/greaterTaipei.geojson") %>%  select(id,names,height,num_floors) %>% 
  mutate(name = sapply(names, function(x) {
    # Parse JSON safely
    tryCatch(fromJSON(x)$primary, error = function(e) NA)
  })) %>% select(-names)

# 
# pal <- scales::col_quantile(
#   "viridis",
#   tpe_building$height,
#   n = 5
# )
# 
# tpe_building$color <- pal(
#   tpe_building$height
# )


mapdeck(style = mapdeck_style(style = "light"),zoom = 12,location =tpe,pitch = 45, ) %>% 
  add_polygon(data = tpe_building %>% mutate(tp = paste0("Building Name : ",name,"<br>Height(m) : ",height,"<br> floor : ",num_floors)),
              fill_colour = "height",elevation = "height",legend = TRUE,update_view = FALSE,elevation_scale = 10,
              tooltip = "tp") %>% 
  add_title("Greater Taipei Metro Area Building Height and floor number <br> Data sourced from overturemaps")
