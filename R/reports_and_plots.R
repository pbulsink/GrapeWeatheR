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

#' Plot Annual Temperature History
#'
#' @param data Data frame from which to plot.
#' @param average_n The number of items to include in a rolling average
#' @param interval One of 'day', 'week', 'month'
#' @param what One of 'max', 'mean', 'min'
#'
#' @return a plot
#' @export
plot_temp_history<-function(data, average_n=1, interval='day', what = 'max'){
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

  data$week <- strftime(data$date, format='%V')
  if(interval == 'week'){
    data %>%
      dplyr::group_by(year, week) %>%
      dplyr::summarise(max_temp = mean(max_temp),
                       min_temp = mean(min_temp),
                       mean_teamp = mean(mean_temp))

  } else if (interval == 'month'){
    data %>%
      dplyr::group_by(year, month) %>%
      dplyr::summarise(max_temp = mean(max_temp),
                       min_temp = mean(min_temp),
                       mean_teamp = mean(mean_temp))
  }

  data$rolling <- rolling(data[,paste0(what,'_temp')], average_n)

  p <- ggplot2::ggplot(data = data,
                       aes_(x=quote(date), y = rolling))
}



#precip plot
#' Title
#'
#' @param data Data frame from which to plot.
#'
#' @return a ggplot object
#' @export
plot_precip_history<-function(data){

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
    ggplot2::geom_point() +
    ggplot2::ggtitle(paste0('Annual ', index, ' Values Plot')) +
    ggplot2::xlab('Year') +
    ggplot2::ylab(paste0(index, ' Value')) +
    #ggplot2::theme_bw() +
    ggplot2::theme(plot.margin = ggplot2::margin(10,100,10,10, unit = 'pt'))
  max_year<-ceiling(max(ggplot2::ggplot_build(p)$layout$panel_ranges[[1]]$x.range))
  for(i in 1:length(limits)){
    p <- p +
      ggplot2::geom_hline(yintercept = limits[[i]]) +
      ggplot2::annotation_custom(grob = grid::textGrob(limits_text[[i]],
                                                       hjust = 0),
                                 xmin = max_year+1,
                                 xmax = max_year+1,
                                 ymin = limits[[i]],
                                 ymax = limits[[i]])
  }

  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"

  grid::grid.draw(gt)

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

  #Winkler & Huglin use subset of months.
  if(index == 'Winkler'){
    data$month<-as.numeric(data$month)
    data<-data[data$month %in% c(4:10), ]
    data$index<-GrapeWeatheR:::winkler_day(data)
  } else if (index == 'Huglin'){
    data$month<-as.numeric(data$month)
    data<-data[data$month %in% c(4:9), ]
    data$index<-GrapeWeatheR:::huglin_day(data)
  } else {
    data$index<-GrapeWeatheR:::bedd_day(data)
  }

  data$cumulate <- 0

  for (y in year){
    data[data$year == y, 'cumulate']<-cumsum(data[data$year==y, 'index'])
  }

  p<-ggplot2::ggplot(data=data,
                     ggplot2::aes_(x=quote(refdate), y=quote(cumulate))) +
    ggplot2::geom_line() +
    gghighlight::gghighlight(max(year), max_highlight = 1L, use_direct_label = FALSE)
    ggplot2::ggtitle(paste0('Cumulative ', index, ' Progress Plot')) +
    ggplot2::xlab('Date') +
    ggplot2::ylab(paste0(index, ' Value')) +
    ggplot2::theme(plot.margin = ggplot2::margin(10,100,10,10, unit = 'pt'),
                   legend.position = 'top')
  max_date<-ceiling(max(ggplot2::ggplot_build(p)$layout$panel_ranges[[1]]$x.range))
  for(i in 1:length(limits)){
    p <- p +
      ggplot2::geom_hline(yintercept = limits[[i]]) +
      ggplot2::annotation_custom(grob = grid::textGrob(limits_text[[i]],
                                                       hjust = 0),
                                 xmin = max_date+1,
                                 xmax = max_date+1,
                                 ymin = limits[[i]],
                                 ymax = limits[[i]])
  }

  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"

  grid::grid.draw(gt)

}
