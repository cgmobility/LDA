

.onLoad <- function(libname,pkgname){
  invisible(
    suppressMessages(
      suppressWarnings(
        
        lapply(
          c('sf', 'rjson', 'data.table', 'plotly' ,'lubridate','scales', 'tidyverse', 'rstudioapi',
            'readxl', 'ggrepel', 'hrbrthemes', 'ggpubr', 'shadowtext', 'ggnewscale',
            'janitor','ggsvg','viridis'),
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
  
  invisible(options(lda_base.path = '2026_02 - Eusébio em Movimento'))
  invisible(options(lda_layers.path = '2_Dados Gerais'))
}
