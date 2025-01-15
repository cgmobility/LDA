#' Get Fortaleza cycle network from MyMaps
#'
#' `get_cycle_net` download kml objects from mymaps and read into R.
#'
#'
#' @param forceDownload A logical. When TRUE force download of the kml from google. Default is FALSE, in this case try use cache kml downloaded if available.
#'
#' @return An object of class `sf`, which includes the mymaps columns and a additional column 'layer'.
#'
#' @details
#' - geometries downloaded from 'https://bit.ly/ciclofortaleza'
#'
#'
#' @examples
#' 
#' net <- get_cycle_net()
#'
#'
#' @export
get_cycle_net<-function(forceDownload = FALSE){
  
  checkmate::assert_logical(forceDownload)
  if(forceDownload){
    
    kml_f <- download_kml_ciclo()
    
    Sys.setenv(`ciclo_net_path`=list(path = kml_f,time = Sys.time()))
  }else{
    if(Sys.getenv('ciclo_net_path')==""){
      
      kml_f <- download_kml_ciclo()
      
      Sys.setenv(`ciclo_net_path`=list(path = kml_f,time = Sys.time()))
      
    }else{
      if(as.numeric(Sys.time()-Sys.getenv('ciclo_net_path')$time, units = 'secs') > 3600){
        
        kml_f <- tryCatch({
          download_kml_ciclo()
        },error = function(e){
          message('Failed connection with mymaps, using cache data\n',e)
          return(Sys.getenv('ciclo_net_path')$path)
        })
        
        Sys.setenv(`ciclo_net_path`=list(path = kml_f,time = Sys.time()))
        
        
      }else{
        kml_f <- Sys.getenv('ciclo_net_path')$path
      }
      
    }
  }
  
  
  layers <- st_layers(kml_f)
  
  layers <- layers$name[str_detect(unlist(layers$geomtype),"Line")]
  
  layers <- layers[layers!="Fortaleza"]
  
  df <- lapply(layers, function(x){
    read_sf(kml_f,layer = x) %>% 
      mutate(layer = x) %>% 
      st_zm() %>% suppressMessages()
  }) %>% rbindlist(fill = T) %>% 
    st_as_sf()
  
  unnest_description <- function(x){
    splitted <- x %>% 
      str_remove("descrição: ") %>% 
      str_split(pattern="<br>") %>% 
      .[[1]]
    if(length(splitted)==1){
      return(tibble(
        Tipologia=splitted
      ))
    }else{
      splitted %>% 
        unique() %>% 
        .[str_detect(.,"img src=",negate = T)] %>% 
        .[.!=""] %>% 
        lapply(FUN = function(y){
          y <- str_replace(y,":",": ")
          ls = str_split(y,":")[[1]]
          d <- tibble(ls[2])
          names(d) <- ls[1]
          return(d)
        }) %>% 
        bind_cols() %>% 
        as_tibble() %>% 
        select(Id,Tipologia,`Extensão (km)`,Trecho,`Posição na via`,`Posição (agregado)`,
               `Sentido de circulação`,Pavimento,`Elementos de separação`,`Separação (agregado)`,
               `Data de implantação`,Bairros,anoref) %>% 
        return()
    }
    
  }
  
  df$Description2 <- lapply(df$Description,unnest_description)
  
  df <- df %>% 
    unnest(cols = "Description2",keep_empty=T) %>% 
    mutate(Description = str_remove(Description,'.*?/>')) %>%
    mutate(Description = str_remove(Description,"<img.*")) %>% 
    mutate(Description = str_replace_all(Description,'<br><br>','<br>'))
  
  df <- df %>% 
    mutate(`Data de implantação` = as.POSIXct(str_trim(`Data de implantação`),format = c("%d/%m/%Y")))
  return(df)
}


download_kml_ciclo <- function(){
  dir<-tempdir()
  
  dir <- paste0(dir,'\\',gsub('-|:|-','',Sys.time()))
  dir.create(dir)
  file1<-paste0(dir,'\\layer.kmz')
  file.create(file1)
  
  
  httr::GET("http://maps.google.com/maps/ms?ie=UTF8&hl=en&vps=1&jsv=206b&msa=0&output=kml&msid=1eqNX-fl3ENPC8_1tqzbRDYZFQmA",
            httr::write_disk(path = file1,overwrite = T))
  
  
  utils::unzip(file1,exdir = dir)
  
  return(paste0(dir,"\\doc.kml"))
}
