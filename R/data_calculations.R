
#' Calculate Latitude Correction Values for HI and BEDD. HI = 1.02 for 40, 1.06 for 50. Calculated for Apr 01-Sept 31
#'
#' @param lat latitude to calculate K
#'
#' @return named list of HI and BEDD K values for the specified latitude
#' @export
lat_correction <- function(lat, ...){
  rlat<-lat * pi/180
  mlat_jday <- function(rlat, jday, axis=0.409){
    m<-1-tan(rlat)*tan(axis * cos((pi * jday)/182.625))
    return(m)
  }

  day_length <- function(rlat, jday, ...){
    d <- (acos(1-mlat_jday(rlat, jday, ...))/pi) * 24
    return(d)
  }

  total_season_day_length <- function(rlat, ...){
    l<-0
    for (i in 101:283) {
      l<-l+day_length(rlat, i, ...)
    }
    return(l)
  }

  tsdl <- total_season_day_length(rlat, ...)

  k_hi<-2.8311e-4*tsdl + 0.30834
  k_bedd<-1.1135*k_hi - 0.13520

  return(list(K_HI = k_hi, K_BEDD = k_bedd))
}

slice_data_to_growing_season<-function(data, months=(4:10)){
  data$month<-as.integer(data$month)
  data<-data[data$month %in% months, ]
  return(data)
}

#' Calculate Growing Season Temperature average, max and min (GST)
#'
#' @param data data frame or tibble as downloaded with \code{\link[weathercan]{weather_dl}}. Provide a single measurement site for a single year only.
#'
#' @return named list of GST average temperature, GST max temperature and GST min temperature, plus GST binned 'Region'
#' @export
gst <- function(data){
  data<-slice_data_to_growing_season(data)

  gst_avg<-mean(data$mean_temp, na.rm = TRUE)
  gst_max<-mean(data$max_temp, na.rm = TRUE)
  gst_min<-mean(data$min_temp, na.rm = TRUE)

  region <- get_region("GST", gst_avg)
  return(list("GSTavg" = gst_avg, "GSTmax" = gst_max, "GSTmin" = gst_min, "GST_region" = region))
}


#' Calculate Winkler Index (Growing Degree Days) (WI/GDD)
#'
#' @param data data frame or tibble as downloaded with \code{\link[weathercan]{weather_dl}}. Provide a single measurement site for a single year only.
#'
#' @return a named list including the Winkler Index value and binned 'Region'. See Winkler et. al. (1974)
#' @export
winkler_index <- function(data){
  data<-slice_data_to_growing_season(data)

  wi<-sum(pmax(data$mean_temp-10, ((data$max_temp+data$min_temp)/2)-10, 0, na.rm=TRUE))
  region <- get_region("WI", wi)
  return(list(WI=wi, WI_region=region))
}

#' Calculate Huglin Index (HI)
#'
#' @param data data frame or tibble as downloaded with \code{\link[weathercan]{weather_dl}}. Provide a single measurement site for a single year only.
#'
#' @return a named list including the Huglin Index value and binned 'Region'. See Huglin (1978)
#' @export
#'
#' @examples
huglin_index <- function(data){
  data<-slice_data_to_growing_season(data, months = 4:9)
  k<-lat_correction(data$lat[1])$K_HI

  hi <- sum(pmax(((data$mean_temp-10)+(data$max_temp-10))/2, 0, na.rm=TRUE)*k)
  region <- get_region("HI", hi)
  return(list(HI=hi, HI_region=region))
}

#' Calculate Biologically Effective Growing Days value (BEDD)
#'
#' @param data data frame or tibble as downloaded with \code{\link[weathercan]{weather_dl}}. Provide a single measurement site for a single year only.
#'
#' @return a named list including the Biologically Effective Degere Days and binned 'Region'. See Gladstones (1992)
#' @export
#'
#' @examples
bedd <- function(data){
  data<-slice_data_to_growing_season(data)
  k<-lat_correction(data$lat[1])$K_BEDD

  tradj<-ifelse(
    (data$max_temp-data$min_temp-13) > 13,
    0.25*(data$max_temp-data$min_temp),
    ifelse(
      (data$max_temp-data$min_temp-10) < 10,
      0.25*(data$max_temp-data$min_temp),
      0))
  tradj[is.na(tradj)] <- 0
  data$tradj<-tradj

  bedd <- sum(pmin(pmax(data$mean_temp-10, ((data$max_temp+data$min_temp)/2)-10, 0)*k + data$tradj, 9, na.rm=TRUE))

  region <- get_region("BEDD", bedd)
  return(list(BEDD = bedd, BEDD_region = region))
}

#' Get Region classification
#'
#' @param calculation Which calculation's value of which to look up the region result.
#' @param value The value to look up
#'
#' @return The region (from Too Cool to Too Hot)
#' @export
get_region<-function(calculation="WI", value) {
  region <- ""
  if (calculation == "WI"){
    if(value < 850){1
      region <- "Too Cool"
    } else if (value < 1390){
      region <- "Region I"
    } else if (value < 1668){
      region <- "Region II"
    } else if (value < 1945){
      region <- "Region III"
    } else if (value < 2223){
      region <- "Region IV"
    } else if (value < 2701){
      region <- "Region V"
    } else {
      region <- "Too Hot"
    }
  } else if (calculation == "HI"){
    if(value < 1200){
      region <- "Too Cool"
    } else if (value < 1500){
      region <- "Very Cool"
    } else if (value < 1800){
      region <- "Cool"
    } else if (value < 2100){
      region <- "Temperate"
    } else if (value < 2400){
      region <- "Warm Temperate"
    } else if (value < 2700){
      region <- "Warm"
    } else if (value < 3000){
      region <- "Very Warm"
    } else {
      region <- "Too Hot"
    }
  } else if (calculation == "GST"){
    if(value < 13){
      region <- "Too Cool"
    } else if (value < 15){
      region <- "Cool"
    } else if (value < 17){
      region <- "Intermediate"
    } else if (value < 19){
      region <- "Warm"
    } else if (value < 21){
      region <- "Hot"
    } else if (value < 24){
      region <- "Very Hot"
    } else {
      region <- "Too Hot"
    }
  } else if (calculation == "BEDD"){
    if(value < 1000){
      region <- "Too Cool"
    } else if (value < 1200){
      region <- "Very Cool"
    } else if (value < 1400){
      region <- "Cool"
    } else if (value < 1600){
      region <- "Temperate"
    } else if (value < 1800){
      region <- "Warm Temperate"
    } else if (value < 2000){
      region <- "Warm"
    } else if (value < 2200){
      region <- "Very Warm"
    } else {
      region <- "Too Hot"
    }
  }
  return(region)
}
