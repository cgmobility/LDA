

table_perc <- function(x){
  x <- table(x) %>% sort()
  nm <- names(x)
  x <- x/sum(x)
  x <- percent(as.numeric(x),.1)
  names(x) <- nm
  return(x)
}

