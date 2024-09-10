read_kmz <- function(path){
  
  tdir <- tempdir()
  
  tdir <- paste0(tdir,'\\',gsub('-|:','',Sys.time()))
  
  dir.create(tdir)
  
  utils::unzip(zipfile = path,exdir = tdir)
  
  read_sf(list.files(path = tdir, pattern = '.kml',full.names = T)) %>% 
    return()
  
}
