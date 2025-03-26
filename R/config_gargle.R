

config_gargle_sheets <- function(
  email = 'caiogcg.mobilidade@gmail.com', path = 'secrets'
){
  if(!dir.exists(path)){
    paths <- list.dirs(path = '..')
    path <- paths[stringr::str_detect(paths,path)]
  }
  if(length(path)>1){
    stop('Could not set the cache directory\n',paste0(path,collapse = '\n'))
  }
  if(length(path)==0){
    stop('Could not set the cache directory\n','Empty path')
  }
  options(gargle_oauth_cache = path)
  
  gs4_auth(
    email = email
  )
}


config_gargle_drive <- function(
    email = 'caiogcg.mobilidade@gmail.com', path = 'secrets'
){
  if(!dir.exists(path)){
    paths <- list.dirs(path = '..')
    path <- paths[stringr::str_detect(paths,path)]
  }
  if(length(path)>1){
    stop('Could not set the cache directory\n',paste0(path,collapse = '\n'))
  }
  if(length(path)==0){
    stop('Could not set the cache directory\n','Empty path')
  }
  
  options(
    gargle_oauth_email = TRUE,
    gargle_oauth_cache = path
  )
  
  drive_auth(
    email = email
  )
}




