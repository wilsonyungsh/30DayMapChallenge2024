#' This function is to download geopackages from ABS website and load 2021 ASGS geographies or None-ABS structure with precise geometry. 
#' 
#' @param structure Default to "ABS" structure geographies, alternatively accept "NON_ABS" value.
#' @param level Pick appropriate level under different structures, examples for ABS level includes MB,SA1-SA4 and so on. For Non_ABS geometries, it will include region type and year as prompted.
#' @examples
#' lga_2024<-get_asgs2016_boundary(structure = "NON_ABS",level = "LGA_2024")
#' sa1<-get_asgs2016_boundary(structure = "ABS",level = "sa1")

get_asgs2016_boundary <- function(level){

  # create folder if not exist
  if (!dir.exists("ASGS_DATA")) dir.create("ASGS_DATA") 
  
  # base url
  target_url <- "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_ASGS_2016_vol_1_geopackage.zip&1270.0.55.001&Data%20Cubes&C406A18CE1A6A50ACA257FED00145B1D&0&July%202016&12.07.2016&Latest"
  

  
  downloaded_file_path <-"ASGS_DATA/asgs_2016.gpkg"
  if (!file.exists(downloaded_file_path)){
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
  
    
    # avaliable geometries in ABS structure
    level <- toupper(level)
    rlang::arg_match(level,
                     sf::st_layers(gpkg_url)$name %>% str_remove_all("_2016_AUST"))
    sf::st_read(gpkg_url,layer = paste0(toupper(level),"_2016_AUST")) %>% janitor::clean_names()
}
