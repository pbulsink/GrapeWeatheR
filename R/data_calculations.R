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
  data$year<-as.integer(data$year)
  data$month<-as.integer(data$month)
  data$day<-as.integer(data$day)
  data<-data[data$month %in% months, ]
  return(data)
}

turn_hour_data_to_daily<-function(data){
  #Do nothing if it's not hourly data.
  if(!'time' %in% colnames(data)) return(data)

  daily<- data %>%
    dplyr::group_by(station_id, date) %>%
    dplyr::summarise(
      station_name=station_name[1],
      prov=prov[1],
      lat=lat[1],
      lon=lon[1],
      elev=elev[1],
      climate_id=climate_id[1],
      WMO_id=WMO_id[1],
      TC_id=TC_id[1],
      year=year[1],
      month=month[1],
      day=day[1],
      max_temp = max(temp, na.rm = TRUE),
      min_temp = min(temp, na.rm = TRUE),
      mean_temp = mean(temp, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  return(daily)
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

  region <- get_region_classification("GST", gst_avg)
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
  region <- get_region_classification("WI", wi)
  return(list(WI=wi, WI_region=region))
}

#' Calculate Huglin Index (HI)
#'
#' @param data data frame or tibble as downloaded with \code{\link[weathercan]{weather_dl}}. Provide a single measurement site for a single year only.
#'
#' @return a named list including the Huglin Index value and binned 'Region'. See Huglin (1978)
#' @export
huglin_index <- function(data){
  data<-slice_data_to_growing_season(data, months = 4:9)
  k<-lat_correction(data$lat[1])$K_HI

  hi <- sum(pmax(((data$mean_temp-10)+(data$max_temp-10))/2, 0, na.rm=TRUE)*k)
  region <- get_region_classification("HI", hi)
  return(list(HI=hi, HI_region=region))
}

#' Calculate Biologically Effective Growing Days value (BEDD)
#'
#' @param data data frame or tibble as downloaded with \code{\link[weathercan]{weather_dl}}. Provide a single measurement site for a single year only.
#'
#' @return a named list including the Biologically Effective Degere Days and binned 'Region'. See Gladstones (1992)
#' @export
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

  region <- get_region_classification("BEDD", bedd)
  return(list(BEDD = bedd, BEDD_region = region))
}

#' Frost Free Length & Frosty Days
#'
#' @param data data frame or tibble as downloaded with \code{\link[weathercan]{weather_dl}}. Provide a single measurement site for a single year only.
#'
#' @return The longest number of days between dates with min_temp < 0 C, and number of Frosty Days
#' @export
frost<-function(data){
  data<-slice_data_to_growing_season(data, 1:12)

  f_annual<-nrow(data[data$min_temp <= 0, ])
  since_frost<-0
  largest_no_frost<-0
  for(i in unique(data$date)){
    if(data[data$date == i, ]$min_temp > 0){
      since_frost<-since_frost + 1
    } else {
      if(since_frost != 0){
        if(since_frost > largest_no_frost){
          largest_no_frost <- since_frost
        }
        since_frost <- 0
      }
    }
  }

  d<-slice_data_to_growing_season(data, 4:10)
  f_growing<-nrow(data[data$min_temp <= 0, ])
  return(list(FFD=largest_no_frost, F_Annual=f_annual, F_Growing=f_growing))
}

#' Number of days related to temperature (percentile, temperature)
#'
#' @param data data frame or tibble as downloaded with \code{\link[weathercan]{weather_dl}}. Provide a single measurement site for a single year only.
#'
#' @return a named list of:
#' ND_25 = number of days with t>25 C
#' ND_30 = number of days with t>30
#' NDT_Min_90p = number of days with minimum temperature > 90th percentile
#' NDT_Max_90p = number of days with maximum temperature > 90th percentile
#' NDT_Min_10p = number of days with minimum temperature < 10th percentile
#' NDT_Max_10p = number of days with maximum temperature > 10th percentile
#'
#' @export
nd_temp <- function(data){
  data <- slice_data_to_growing_season(data, 1:12)

  nd25 <- nrow(data[data$max_temp >= 25, ])
  nd30 <- nrow(data[data$max_temp >= 30, ])
  ndtMin90p <- nrow(data[data$min_temp > stats::quantile(data$min_temp, 0.9, na.rm = TRUE),])
  ndtMax90p <- nrow(data[data$max_temp > stats::quantile(data$max_temp, 0.9, na.rm = TRUE),])
  ndtMin10p <- nrow(data[data$min_temp < stats::quantile(data$min_temp, 0.1, na.rm = TRUE),])
  ndtMax10p <- nrow(data[data$max_temp < stats::quantile(data$max_temp, 0.1, na.rm = TRUE),])

  d<-slice_data_to_growing_season(data, 8:9)

  dtr <- mean(d$max_temp-d$min_temp)

  return(list(ND_25 = nd25, ND_30 = nd30, NDT_Min_90p = ndtMin90p,
              NDT_Max_90p = ndtMax90p, NDT_Min_10p = ndtMin10p, NDT_Max_10p = ndtMax10p, DTR=dtr))
}

#' Calculate Precipitation Statistics
#'
#' @param data data frame or tibble as downloaded with \code{\link[weathercan]{weather_dl}}. Provide a single measurement site for a single year only.
#'
#' @return precipitation values for the site.
#' @export
precip<-function(data){
  data <- slice_data_to_growing_season(data, 1:12)

  p_annual <- sum(data$total_precip, na.rm = TRUE)
  p_max <- max(data$total_precip, na.rm = TRUE)
  np95p <- nrow(data[data$total_precip > stats::quantile(data$total_precip, 0.95, na.rm = TRUE),])
  pp95p <- sum(data[data$total_precip > stats::quantile(data$total_precip, 0.95, na.rm = TRUE),]$total_precip,
               na.rm = TRUE)

  d<-slice_data_to_growing_season(data, 4:10)
  p_growing <- sum(d$total_precip, na.rm = TRUE)

  d_i <- dplyr::bind_rows(data[data$month == 4, ], data[(data$month == 5 & data$day < 11), ])
  p_i <- sum(d_i$total_precip, na.rm = TRUE)
  d_ii <- dplyr::bind_rows(data[(data$month == 5 & data$day > 10), ], data[(data$month == 6), ])
  p_ii <- sum(d_ii$total_precip, na.rm = TRUE)
  d_iii <- dplyr::bind_rows(data[(data$month == 7), ],
                            data[(data$month == 8), ],
                            data[(data$month == 9 & data$day < 16), ])
  p_iii <- sum(d_iii$total_precip, na.rm = TRUE)
  d_iv <- dplyr::bind_rows(data[(data$month == 9 & data$day > 15), ],
                           data[(data$month == 10), ])
  p_iv <- sum(d_iv$total_precip, na.rm = TRUE)
  p_v <- p_annual - p_growing

  # 7 DPL Maximum annual drought period
  since_precip<-0
  largest_no_precip<-0
  for(i in 1:nrow(data)){
    if(!is.na(data[i,]$total_precip) && data[i, ]$total_precip == 0){
      since_precip<-since_precip + 1
    } else {
      if(since_precip != 0){
        if(since_precip > largest_no_precip){
          largest_no_precip <- since_precip
        }
        since_precip <- 0
      }
    }
  }

  return(list(P_Annual = p_annual, P_Growing = p_growing, P_Max = p_max, NP95p = np95p,
              PP95p = pp95p, P_I = p_i, P_II = p_ii, P_III = p_iii, P_IV = p_iv,
              P_V = p_v, DPL = largest_no_precip))
}

#' Get Region classification
#'
#' @param calculation Which calculation's value of which to look up the region result.
#' @param value The value to look up
#'
#' @return The region (from Too Cool to Too Hot)
#' @export
get_region_classification<-function(calculation="WI", value) {
  if(!is.finite(value)) return("")
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

#' Calculate Annual Temperature Indices
#'
#' @description Calculate GST, WI, HI, and BEDD values for each year of data provided, at each individual climate station (separated by ID).
#'
#' @param data data frame or tibble as downloaded with \code{\link[weathercan]{weather_dl}}. Multiple stations or years are accepted. Daily data required.
#'
#' @return a tibble with climate indicies and region classification for each climate station for each year.
#' @export
calculate_annual_indicies <- function(data){
  data<-turn_hour_data_to_daily(data)
  data$year<-as.integer(data$year)
  data$month<-as.integer(data$month)
  data$day<-as.integer(data$day)
  stations<-unique(data$station_id)
  years<-unique(data$year)
  results <- dplyr::tibble()
  for(s in stations){
    for (y in years){
      d<-data[(data$station_id == s & data$year == y),]
      g<-gst(d)
      w<-winkler_index(d)
      h<-huglin_index(d)
      b<-bedd(d)
      n<-nd_temp(d)
      f<-frost(d)
      p<-precip(d)

      r<-dplyr::tibble(station_name = d$station_name[1], station_id = s,
                        climate_id = d$climate_id[1], WMO_id = d$WMO_id[1], TC_id = d$TC_id[1],
                        elev = d$elev[1], lat = d$lat[1], lon = d$lon[1], year = y,
                        GSTavg = g$GSTavg, GSTmax = g$GSTmax, GSTmin = g$GSTmin,
                        GST_region = g$GST_region, WI = w$WI, WI_region = w$WI_region, HI = h$HI,
                        HI_region = h$HI_region, BEDD = b$BEDD, BEDD_region = b$BEDD_region,
                        FFD = f$FFD, F_Growing = f$F_Growing, F_Annual = f$F_Annual,
                        ND_25 = n$ND_25, ND_30 = n$ND_30, NDT_Min_90p = n$NDT_Min_90p,
                        NDT_Max_90p = n$NDT_Max_90p, NDT_Min_10p = n$NDT_Min_10p,
                        NDT_Max_10p = n$NDT_Max_10p, DTR = n$DTR, P_Annual = p$P_Annual,
                        P_Growing = p$P_Growing, P_Max = p$P_Max, NP95p = p$NP95p,
                        PP95p = p$PP95p, P_I = p$P_I, P_II = p$P_II, P_III = p$P_III,
                        P_IV = p$P_IV, P_V = p$P_V, DPL = p$DPL
                        )
      results<-dplyr::bind_rows(results, r)
    }
  }
  return(results)
}


