#' Load Futureverse Packages
#'
#' `ld_future_pkgs` loads all packages from the `futureverse` ecosystem in the current R session.
#'
#' This function retrieves the list of packages from `futureverse::futureverse_packages()` and loads them using `library()` with the `character.only = TRUE` option.
#'
#' @return `NULL` (the function is used for its side effect of loading packages)
#' @examples
#' # Load all futureverse packages
#' ld_future_pkgs()
ld_future_pkgs <- function(){
  sapply(futureverse::futureverse_packages(), function(x) {
    library(x, character.only = TRUE)
    return(NULL)
  })
}
