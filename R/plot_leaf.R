
plot_leaf <- function(dt_map,fun = NULL,data){
  UseMethod('plot_leaf')
}

plot_leaf.sf <- function(dt_map,fun = NULL,data){
  leafmap <- leaflet::leaflet()
  if(is.null(fun)){
   geom_type <- st_geometry_type(dt_map,F) %>% as.character()
   if(geom_type %in% c('POINT','MULTIPOINT')){
     fun <-  leaflet::addMarkers
   }
   if(geom_type %in% c('LINESTRING','MULTILINESTRING')){
     fun <-  leaflet::addPolylines
   }
   if(geom_type %in% c('POLYGON','MULTIPOLYGON')){
     fun <-  leaflet::addPolygons
   }
  }
  checkmate::assert_function(fun)
  leafmap <- leafmap %>% 
    fun(data = dt_map) %>% 
    leaflet::addTiles()
  return(leafmap)
}

plot_leaf.leaflet <- function(dt_map,fun = NULL,data){
  if(is.null(fun)){
    geom_type <- st_geometry_type(data,F) %>% as.character()
    if(geom_type %in% c('POINT','MULTIPOINT')){
      fun <-  leaflet::addMarkers
    }
    if(geom_type %in% c('LINESTRING','MULTILINESTRING')){
      fun <-  leaflet::addPolylines
    }
    if(geom_type %in% c('POLYGON','MULTIPOLYGON')){
      fun <-  leaflet::addPolygons
    }
  }
  checkmate::assert_function(fun)
  dt_map %>% 
    fun(data = data) %>% 
    return()
}


