
#' Use the weathercan package to download data
#'
#' @param station_list a single station_id or a vector of stations to download from. Stations can be found using \code{\link[weathercan]{stations_search}}.
#' @param ... additional parameters from weathercan. See \code{\link[weathercan]{weather_dl}} for more information.
#'
#' @return the downloaded weather data.
#' @export
#'
#' @examples
load_weather_station_data <- function(station_list, ...){
  weather_data<-weathercan::weather_dl(station_list, ...)
  return(weather_data)
}

