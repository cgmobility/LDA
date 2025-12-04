
read_kml2 <- function(path){
  doc <- xml2::read_xml(path)
  ns <- xml_ns(doc)
  elements <- xml_find_all(doc,'.//d1:Placemark',ns)
  ns_id <- names(ns)
  names_attr <- xml_find_all(elements[[1]],'.//d1:SimpleData',ns) %>%
    lapply(function(x) xml_attr(x,'name')) %>% unlist()
  ".//d1:SimpleData[@name='Bairro']"
  columns <- paste0(".//",ns_id,":SimpleData[@name='",
                    names_attr,"']")
  data <- lapply(elements, function(elem){
    columns <- sapply(columns, function(col){
      xml_text(xml_find_first(elem,col,ns))
    })
    columns <- c(columns,xml_text(xml_find_first(
      elem,paste0(".//",ns_id,":coordinates"),
      ns
    )))
    as_tibble(t(columns)) %>% 
      setNames(c(names_attr,'geometry')) %>% 
      return()
  }) %>% bind_rows()
  
  data <- data %>% 
    mutate(geometry = str_replace_all(geometry,' ',';') %>% 
             str_replace_all(',',' ') %>% str_replace_all(';',',') %>% 
             paste0('POLYGON((',.,'))'))
  
  return(data)
}
