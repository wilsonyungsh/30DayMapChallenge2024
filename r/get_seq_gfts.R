get_seq_gtfs <- function(x){
  if (!dir.exists("data")) {
    message("data directory doesn't exist") 
    dir.create("data")
    message("data folder created....")
  }
  gtfs_url <-"https://gtfsrt.api.translink.com.au/GTFS/SEQ_GTFS.zip"
  file_name <- str_c(tolower(basename(gtfs_url)),"_",str_remove_all(Sys.Date(),"-"))
  curl::curl_download(url = gtfs_url,destfile = str_c("data/",file_name),quiet = FALSE)
}