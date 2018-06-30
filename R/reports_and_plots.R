#' Plot Annual Temperature History
#'
#' @param data Data frame from which to plot.
#'
#' @return a ggplot object
#' @export
plot_temp_history<-function(data){

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

#' Plot index accumilation
#'
#' @param data Raw hourly or daily data returned from weathercan
#' @param index The index of which to plot accumilation (one of 'Winkler', 'Huglin', or 'BEDD')
#' @param year year to plot, optional (or leave NULL for all)
#'
#' @return an annual accumilation plot of the specified index
#' @export
plot_index_accumilation<-function(data, index='Winkler', year=NULL){
  stopifnot(index %in% c('Winkler', 'Huglin', 'BEDD'))

  if(!is.null(year)){
    yearrange<-unique(data$year)
    stopifnot(year %in% yearrange)
    year<-year[year%in%yearrange]
  } else {
    year<-unique(data$year)
  }
}
