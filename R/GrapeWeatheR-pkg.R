#' @title GrapeWeatheR
#'
#' @description Analysis of Environment Canada Climate Data for Grape Grower and Agriculture Information
#'
#' @references
#' Gladstones, J. 1992. Viticulture and Environment. Winetitles, Adelaide.
#' Huglin, P. 1978. Nouveau Mode d'Evaluation des Possibilites Heliothermiques d'un Milieu Viticole. C.R. Acad. Agr. France 64:1117-1126
#' Winkler, A.J., J.A. Cook, W.M. Kliewer, and L.A. Lider. 1974. General Viticulture. 4th ed. University of California Press, Berkeley.
#'
#' @docType package
#' @name GrapeWeatheR-package
#' @aliases GrapeWeatheR GrapeWeatheR-package
#' @importFrom dplyr "%>%"
NULL

# Dealing with CRAN Notes due to NSE
.onLoad <- function(libname = find.package("GrapeWeatheR"),
                    pkgname = "GrapeWeatheR"){
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      # Variables used in NSE, declared here to
      # avoid CRAN warnings
      c("TC_id", "WMO_id", "climate_id", "day", "elev", "lat",
        "lon", "month", "prov","station_id", "station_name",
        "station_operator", "temp", "year",
        "." # piping requires '.' at times
      )
    )
  invisible()
}
