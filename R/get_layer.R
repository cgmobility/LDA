

get_layer <- function(nm_layer,base.path = .Options$lda_base.path,
                      layers.path = .Options$lda_layers.path){
  checkmate::assert_character(nm_layer,len = 1)
  cur_wd <- rstudioapi::getActiveDocumentContext()$path %>% 
    dirname() %>% 
    str_sub(end = str_locate(.,base.path)[,2])
  
  layers_path <- list.dirs(
    cur_wd,
    recursive = F
  ) %>% Filter2(function(x) basename(x) == layers.path)
  
  list.files(
    path = layers_path,
    pattern = nm_layer,
    full.names = T,
    recursive = T
  )[1] %>% return()
  
}





