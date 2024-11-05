#' This function is to download geopackages from ABS website and load 2021 ASGS geographies or None-ABS structure with precise geometry. 
#' 
#' @param structure Default to "ABS" structure geographies, alternatively accept "NON_ABS" value.
#' @param level Pick appropriate level under different structures, examples for ABS level includes MB,SA1-SA4 and so on. For Non_ABS geometries, it will include region type and year as prompted.
#' @examples
#' lga_2024<-get_asgs21_boundary(structure = "NON_ABS",level = "LGA_2024")
#' sa1<-get_asgs21_boundary(structure = "ABS",level = "sa1")

get_asgs21_boundary <- function(structure = 'ABS',level){
  # normalise input argument value
  structure <- toupper(structure)
  rlang::arg_match(structure,values = c("ABS","NON_ABS"))
  
  # create folder if not exist
  if (!dir.exists("ASGS_DATA")) dir.create("ASGS_DATA") 
  
  # base url
  base <- "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/"
  
  target_url <- switch(structure,
                       "ABS" = paste0(base,"ASGS_2021_MAIN_STRUCTURE_GPKG_GDA2020.zip"),
                       "NON_ABS" = paste0(base,"ASGS_Ed3_Non_ABS_Structures_GDA2020_updated_2024.zip"))
  
  downloaded_file_path <-paste0("ASGS_DATA/",basename(target_url))
  if (!file.exists(paste0("ASGS_DATA/",basename(target_url)))){
    print(str_c("Downloading data from ",target_url))
    curl::multi_download(urls = target_url,destfiles = downloaded_file_path,progress = TRUE)
  }
  else message("file exists! skip downloading")
  
  ### read corresponding file ###
  print(downloaded_file_path)
  #unzip files
  unzip(downloaded_file_path,exdir = "ASGS_DATA/",overwrite = FALSE)
  # remove superfluous .xml files
  list.files("ASGS_DATA/",pattern = ".xml$",full.names = TRUE) %>% purrr::map(file.remove)
  #GPKG name and url
  gpkgname <- unzip(zipfile = downloaded_file_path,list = TRUE)$Name %>% grep(pattern = ".gpkg$",value = TRUE)
  gpkg_url <- paste0("ASGS_DATA/",gpkgname)
  
  if(structure == "ABS"){
    
    # avaliable geometries in ABS structure
    level <- toupper(level)
    rlang::arg_match(level,
                     sf::st_layers(gpkg_url)$name %>% str_remove_all("_2021_AUST_GDA2020"))
    sf::st_read(gpkg_url,layer = paste0(toupper(level),"_2021_AUST_GDA2020")) %>% janitor::clean_names()}
  
  else {
    ref_input <- sf::st_layers(gpkg_url)$name %>% 
      as_tibble() %>% dplyr::mutate(entry = str_remove_all(value,"_AUST.*"))
    # avaliable geometries in NON-ABS structure
    rlang::arg_match(level,
                     ref_input$entry)
    layer_name <- ref_input %>% dplyr::filter(entry == level) %>% .$value
    
    sf::st_read(dsn = gpkg_url,layer = layer_name) %>% janitor::clean_names()}
  
}