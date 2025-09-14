

.onLoad <- function(libname,pkgname){
  invisible(
    suppressMessages(
      suppressWarnings(
        
        lapply(
          c('sf', 'rjson', 'data.table', 'plotly' ,'lubridate','scales', 'tidyverse', 'rstudioapi',
            'readxl', 'ggrepel', 'hrbrthemes', 'ggpubr', 'shadowtext', 'ggnewscale',
            'janitor','ggsvg','viridis','KeyboardSimulator'),
          function(x){
            invisible(
              suppressPackageStartupMessages(
                library(x,quietly = TRUE,character.only = TRUE)
              )
            )
          })
        
      )
    )
  )
  
  invisible(options(lda_base.path = '06_Eixo Dados'))
  invisible(options(lda_layers.path = 'layers'))
}

