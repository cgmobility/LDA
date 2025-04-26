

view_plot <- function(p,w = 10,h = 8,dpi = 110){
  UseMethod('view_plot')
}

view_plot.ggplot <- function(p,w = 10,h = 8,dpi = 110){
  tfile <- tempfile(fileext = '.png')
  ggplot2::ggsave(tfile,plot = p,width = w,height = h,dpi = dpi)
  shell.exec(tfile)
  return(tfile)
}

view_plot.default <- function(p,w = 10,h = 8,dpi = 110){
  tfile <- tempfile(fileext = '.png')
  ggplot2::ggsave(tfile,plot = p,width = w,height = h,dpi = dpi)
  shell.exec(tfile)
  return(tfile)
}


