#' @import tidyverse httr
.onLoad <- function(libname, pkgname){
  season_state <<- httr::GET("https://api.sleeper.app/v1/state/nfl") %>% httr::content()
}
