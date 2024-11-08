## landing page : https://data.humdata.org/dataset/movement-distribution
pacman::p_load(tidyverse,sf,geodata,curl,mapgl,mapdeck)
# Download data and metadata
data_url <- "https://data.humdata.org/dataset/32167ba5-ef67-4254-8eaf-04cdb8b90c1d/resource/92cddd47-78b8-4ea1-b712-16c39ecf6b49/download/movement-distribution-data-for-good-at-meta_2024-10-01_2024-11-01_csv.zip"
meta_url <- "https://data.humdata.org/dataset/32167ba5-ef67-4254-8eaf-04cdb8b90c1d/resource/767a05fe-0bf6-4a0b-96dc-8525baefa327/download/movement-distribution-readme-data-for-good-at-meta.pdf"

curl::multi_download(urls = c(data_url,meta_url),destfiles = file.path("data/", basename(c(data_url, meta_url))))

## load data
#unzip files
unzip("data/movement-distribution-data-for-good-at-meta_2024-10-01_2024-11-01_csv.zip",exdir = "data/movement/",)


# Read the CSV directly from the zip file
anz_movement_data <- map(list.files("data/movement/movement-distribution-data-for-good-at-meta_2024-10-01_2024-11-01_csv/",full.names = TRUE),
                         ~ read_csv(.x) %>% filter(country %in% c("AUS","NZL"))) %>% bind_rows() 
## download geodata
map(c("AUS","NZL"), ~ geodata::gadm(country = .x,level = 2, path = ("data/")))

## read map data
anz_geo <- map(list.files("data/gadm/",full.names = TRUE), ~ readRDS(.x) %>% st_as_sf() ) %>% bind_rows() %>% 
  janitor::clean_names()


## aggregate data to monthly and join data together

monthly_median <- anz_movement_data %>% group_by(gadm_id,home_to_ping_distance_category) %>% 
  reframe(distance_category_ping_fraction = median(distance_category_ping_fraction))

anz_movement_joined <-
anz_geo %>% rename( gadm_id= gid_2) %>% select(gadm_id,country,name_1,name_2) %>% 
  inner_join(monthly_median,by = "gadm_id") %>% filter(home_to_ping_distance_category == "(0, 10)") 
  

## Viz
anz_move_mapping <-  
  anz_movement_joined %>% mutate(tp = paste0("State:",name_1, "<br>Area:",name_2,
                                             "<br>fration_within10kmFromHome:",scales::percent(distance_category_ping_fraction,accuracy = 0.01)),
                                 distance_category_ping_fraction = round(distance_category_ping_fraction,4)*100)
anz_move_mapping %>% 
mapdeck::mapdeck(style = mapdeck_style(style = "light"),location = c(174.79753867603705,-41.82106332547732),
                 zoom = 3,pitch = 70,bearing = 300) %>% 
  add_polygon(fill_colour = "distance_category_ping_fraction",
              stroke_colour = "#ffffff",
              fill_opacity = 200,
              stroke_width = 10,
              tooltip = "tp",palette = "plasma",update_view = FALSE,
              legend = list(stroke_colour = FALSE,fill_colour = TRUE),auto_highlight = TRUE,highlight_colour = "#ffffff80",
              legend_options = list(
                fill_colour = list(title = "% people stay within 10km from home",
                                   ticks = 5,  # Set number of legend ticks for a smoother gradient
                                   tick_format = ".2f"  # Format for tick labels)
              ))) %>% 
  add_title(title = "Meta Movement Distribution for Australia and New Zealand <br>Time: 2024 Oct")


  











