

table_perc <- function(x,chr =  TRUE){
  checkmate::assert_logical(chr)
  if(chr){
    x <- table(x) %>% sort()
    nm <- names(x)
    x <- x/sum(x)
    x <- percent(as.numeric(x),.1)
    names(x) <- nm
  }else{
    x <- table(x) %>% sort()
    x <- x/sum(x)
  }
  
  return(x)
}

