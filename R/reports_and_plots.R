rolling <- function(v, n){
  if(n %% 2 == 0){
    message("Odd rolling window required. Using n=", n, "+1.")
  }

  half_window<-floor(n/2)

  results<-rep(0, length(v))

  for (i in 1:half_window){
    results[i] <- mean(v[1:i], na.rm = TRUE)
  }

  for (i in half_window:(length(v)-half_window)){
    results[i] <- mean(v[(i-half_window):(i+half_window)], na.rm = TRUE)
  }

  for (i in (length(v)-half_window):length(v)){
    results[i] <- mean(v[(i-half_window):(i+half_window)], na.rm = TRUE)
  }

  results[is.nan(results)] <- NA
  return(results)
  #return(as.numeric(filter(v,rep(1/n,n), sides=2))) #Doesn't handle NA
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#' Plot Temperature History
#'
#' @param data Data frame from which to plot.
#' @param average_n The number of items to include in a rolling average
#' @param interval One of 'day', 'week', 'month'
#' @param what One of 'max', 'mean', 'min'
#' @param trim_years Number of recent years to show. Set 0 for all data.
#'
#' @return a plot
#' @export
plot_temp_history<-function(data, average_n=1, interval='day', what = 'max', trim_years=5){
  if (!(interval %in% c('day', 'week', 'month'))){
    stop("Interval must be one of 'day', 'week', or 'month'.")
  }
  if (!(what %in% c('max','min','mean'))){
    stop("What must be one of 'max', 'min', 'mean'.")
  }

  if (average_n%%2 == 0){
    message('Even numbered rolling window is not possible. Using ', average_n, '+1.')
    average_n <- average_n + 1
  }

  if(trim_years > 0){
    data<-data[data$date > Sys.Date()-trim_years*365.25,]
  }

  data$week <- strftime(data$date, format='%V')
  if(interval == 'week'){
    data <- data %>%
      dplyr::group_by(!!dplyr::sym('year'), !!dplyr::sym('week')) %>%
      dplyr::summarise(date = mean(!!dplyr::sym('date'), na.rm = TRUE),
                       max_temp = mean(!!dplyr::sym('max_temp'), na.rm = TRUE),
                       min_temp = mean(!!dplyr::sym('min_temp'), na.rm = TRUE),
                       mean_temp = mean(!!dplyr::sym('mean_temp'), na.rm = TRUE))

  } else if (interval == 'month'){
    data <- data %>%
      dplyr::group_by(!!dplyr::sym('year'), !!dplyr::sym('month')) %>%
      dplyr::summarise(date = mean(!!dplyr::sym('date'), na.rm = TRUE),
                       max_temp = mean(!!dplyr::sym('max_temp'), na.rm = TRUE),
                       min_temp = mean(!!dplyr::sym('min_temp'), na.rm = TRUE),
                       mean_temp = mean(!!dplyr::sym('mean_temp'), na.rm = TRUE))
  }

  if (average_n > 1){
    data$rolling <- rolling(data[[paste0(what,'_temp')]], average_n)
  } else {
    data$rolling <- data[[paste0(what,'_temp')]]
  }

  p <- ggplot2::ggplot(data = data,
                       ggplot2::aes_(x=quote(date), y = quote(rolling))) +
    ggplot2::geom_line(na.rm=TRUE) +
    ggplot2::ggtitle(paste0(simpleCap(what), ' Temperature History')) +
    ggplot2::xlab('Date') +
    ggplot2::ylab('Temperature (\u00B0C)') +
    ggplot2::theme_minimal()
  p
}

#' Plot Precipitation History
#'
#' @param data Data frame from which to plot.
#' @param interval Whether to plot `day` or `week` or `month` or `year` amounts
#' @param trim_years the number of years of data to plot.
#'
#' @return a ggplot object
#' @export
plot_precip_history<-function(data, interval = 'day', trim_years=5){
  if (!(interval %in% c('day', 'week', 'month', 'year'))){
    stop("Interval must be one of 'day', 'week', 'month', or 'year'.")
  }

  if(trim_years > 0){
    data<-data[data$date > Sys.Date()-trim_years*365.25,]
  }

  if(interval == 'week'){
    data$week <- strftime(data$date, format='%V')
    data <- data %>%
      dplyr::group_by(!!dplyr::sym('year'), !!dplyr::sym('week')) %>%
      dplyr::summarise(date = mean(!!dplyr::sym('date'), na.rm = TRUE),
                       total_precip = sum(!!dplyr::sym('total_precip'), na.rm=TRUE))

  } else if (interval == 'month'){
    data <- data %>%
      dplyr::group_by(!!dplyr::sym('year'), !!dplyr::sym('month')) %>%
      dplyr::summarise(date = mean(!!dplyr::sym('date'), na.rm = TRUE),
                       total_precip = sum(!!dplyr::sym('total_precip'), na.rm=TRUE))
  } else if (interval == 'year'){
    data <- data %>%
      dplyr::group_by(!!dplyr::sym('year')) %>%
      dplyr::summarise(date = mean(!!dplyr::sym('date'), na.rm = TRUE),
                       total_precip = sum(!!dplyr::sym('total_precip'), na.rm=TRUE))

  }

  p <- ggplot2::ggplot(data = data,
                       ggplot2::aes_(x=quote(date), y = quote(total_precip))) +
    ggplot2::geom_line(na.rm=TRUE) +
    ggplot2::ggtitle(paste0('Precipitation History (Total per ', simpleCap(interval), ')')) +
    ggplot2::xlab('Date') +
    ggplot2::ylab('Precipitation (mm)') +
    ggplot2::theme_minimal()
  p
}

#' Plot Precipitation Progress
#'
#' @param data Data from which to plot
#' @param year year to plot, optional (NULL to plot all)
#'
#' @return a ggplot plot
#' @export
plot_precip_progress <- function(data, year=NULL){
  if(!is.null(year)){
    yearrange<-unique(data$year)
    stopifnot(year %in% yearrange)
    year<-year[year%in%yearrange]
  } else {
    year<-unique(data$year)
  }

  data<-data[data$year %in% year, ]
  data$jd<-as.numeric(strftime(data$date, format = "%j"))-1
  data$refdate<-as.Date(data$jd, format = '%j', origin = as.Date("2017-01-01"))
  data$stn_year <- paste(data$station_id, data$year, sep = ", ")

  #cumsum trips over NA. Substitute 0 so that we can see the rest of the year
  miss <- is.na(data$total_precip)
  data$total_precip[miss] <- 0

  data <- data %>%
    dplyr::group_by(year, station_id) %>%
    dplyr::mutate(cumulate = cumsum(!!dplyr::sym('total_precip')))

  p<-ggplot2::ggplot(data=data,
                     ggplot2::aes_(x=quote(refdate), y=quote(cumulate), colour = quote(stn_year))) +
    ggplot2::geom_line(na.rm=TRUE) +
    gghighlight::gghighlight(year == max(year), max_highlight = 1L, use_direct_label = FALSE, use_group_by = FALSE) +
    ggplot2::ggtitle('Cumulative Precipitation Progress Plot') +
    ggplot2::xlab('Date') +
    ggplot2::ylab('Precipitation Total (mm)') +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = 'top') +
    ggplot2::scale_color_discrete(name = 'Station ID, Year') +
    ggplot2::scale_x_date(date_breaks = '1 month', date_labels = "%b", date_minor_breaks = '1 month')

  p
}


#' Plot the annual index value over time
#'
#' @param data Data frame wfrom which to plot
#' @param index The index of which to plot annual values (one of 'WI', 'HI', 'BEDD', or 'GST', default 'WI')
#'
#' @return a plot
#' @export
plot_index_history<-function(data, index='Winkler'){
  if (!index %in% c('Winkler', 'Huglin', 'BEDD', 'GST')){
    stop('Index must be one of Winkler, Huglin, BEDD, or GST')
  }
  id<-list('Winkler' = 'WI', 'Huglin' = 'HI', 'BEDD' = 'BEDD', 'GST'= 'GSTavg')
  id<-id[[index]]
  if(id == 'WI'){
    limits <- c(850, 1390, 1668, 1945, 2223, 2701)
    limits_text <- c("Too cool", "Region I", "Region II", "Region III", "Region IV", "Region V", "Too Hot")
  } else if (id == 'HI'){
    limits <- c(1200, 1500, 1800, 2100, 2400, 2700, 3000)
    limits_text <- c("Too cool", "Very Cool", "Cool", "Temperate", "Warm Temperate", "Warm", "Very Warm", "Too Hot")
  } else if (id == 'BEDD'){
    limits <- c(1000, 1200, 1400, 1600, 1800, 2000, 2200)
    limits_text <- c("Too cool", "Very Cool", "Cool", "Temperate", "Warm Temperate", "Warm", "Very Warm", "Too Hot")
  } else {  #GSTavg
    limits <- c(13, 15, 17, 19, 21, 24)
    limits_text <- c("Too cool", "Cool", "Intermediate", "Warm", "Hot", "Very Hot", "Too Hot")
  }


  suppressWarnings(annual_data<-calculate_annual_indicies(data = data))

  annual_data <- annual_data[,c('year', id)]
  colnames(annual_data)<-c('year', 'index')

  p <- ggplot2::ggplot(data = annual_data,
                       ggplot2::aes_(x = quote(year), y = quote(index))) +
    ggplot2::geom_point(na.rm=TRUE) +
    ggplot2::ggtitle(paste0('Annual ', index, ' Values Plot')) +
    ggplot2::xlab('Year') +
    ggplot2::ylab(paste0(index, ' Value')) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(10,100,10,10, unit = 'pt')) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = nrow(annual_data)))

  x.range<-ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  for(i in 1:length(limits)){
    p <- p +
      ggplot2::geom_hline(yintercept = limits[[i]]) +
      ggplot2::annotate(geom = 'label', label = limits_text[[i]], x = min(x.range), y = limits[[i]], hjust = 0)
  }

  p

}

#' Plot index progress
#'
#' @param data Raw daily data returned from weathercan
#' @param index The index of which to plot accumilation (one of 'Winkler', 'Huglin', or 'BEDD')
#' @param year year to plot, optional (or leave NULL for all)
#'
#' @return an annual accumilation plot of the specified index
#' @export
plot_index_progress<-function(data, index='Winkler', year=NULL){

  if (!index %in% c('Winkler', 'Huglin', 'BEDD')){
    stop('Index must be one of Winkler, Huglin, or BEDD')
  }
  if(index == 'Winkler'){
    limits <- c(850, 1390, 1668, 1945, 2223, 2701)
    limits_text <- c("Too cool", "Region I", "Region II", "Region III", "Region IV", "Region V", "Too Hot")
  } else if (index == 'Huglin'){
    limits <- c(1200, 1500, 1800, 2100, 2400, 2700, 3000)
    limits_text <- c("Too cool", "Very Cool", "Cool", "Temperate", "Warm Temperate", "Warm", "Very Warm", "Too Hot")
  } else {
    limits <- c(1000, 1200, 1400, 1600, 1800, 2000, 2200)
    limits_text <- c("Too cool", "Very Cool", "Cool", "Temperate", "Warm Temperate", "Warm", "Very Warm", "Too Hot")
  }

  if(!is.null(year)){
    yearrange<-unique(data$year)
    stopifnot(year %in% yearrange)
    year<-year[year%in%yearrange]
  } else {
    year<-unique(data$year)
  }

  data<-data[data$year %in% year, ]
  data$jd<-as.numeric(strftime(data$date, format = "%j"))-1
  data$refdate<-as.Date(data$jd, format = '%j', origin = as.Date("2017-01-01"))
  data$stn_year <- paste(data$station_id, data$year, sep = ", ")

  #Winkler & Huglin use subset of months.
  if(index == 'Winkler'){
    data$month<-as.numeric(data$month)
    data<-data[data$month %in% c(4:10), ]
    data$index<-winkler_day(data)
  } else if (index == 'Huglin'){
    data$month<-as.numeric(data$month)
    data<-data[data$month %in% c(4:9), ]
    data$index<-huglin_day(data)
  } else {
    data$index<-bedd_day(data)
  }

  data <- data %>%
    dplyr::group_by(year, station_id) %>%
    dplyr::mutate(cumulate = cumsum(index))

  p<-ggplot2::ggplot(data=data,
                     ggplot2::aes_(x=quote(refdate), y=quote(cumulate), colour = quote(stn_year))) +
    ggplot2::geom_line(na.rm=TRUE) +
    gghighlight::gghighlight(year == max(year), max_highlight = 1L, use_direct_label = FALSE, use_group_by = FALSE) +
    ggplot2::ggtitle(paste0('Cumulative ', index, ' Progress Plot')) +
    ggplot2::xlab('Date') +
    ggplot2::ylab(paste0(index, ' Value')) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = 'top') +
    ggplot2::scale_color_discrete(name = 'Station ID, Year') +
    ggplot2::scale_x_date(date_breaks = '1 month', date_labels = "%b")

  for(i in 1:length(limits)){
    p <- p +
      ggplot2::geom_hline(yintercept = limits[[i]]) +
      ggplot2::annotate(geom = 'label', label = limits_text[[i]], x = min(data$refdate), y = limits[[i]], hjust = 0)
  }

  p

}
