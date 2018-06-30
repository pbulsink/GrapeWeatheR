#temp plot
#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_temp_history<-function(data){
  data %>%
    subset(WI > 500) %>%
    ggplot(aes(x=year)) + geom_line(aes(y=WI, color = 'wI')) + geom_line(aes(y=HI, color = 'HI')) + geom_line(aes(y=BEDD, color='BEDD')) + geom_line(aes(y=FFD, color = 'FFD')) +  geom_smooth(aes(y=WI), method = 'lm') + theme_bw()
}



#precip plot
#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_precip_history<-function(data){}



#index plot
#' Title
#'
#' @param data
#' @param index
#'
#' @return
#' @export
#'
#' @examples
plot_index_history<-function(data, index='Winkler'){
  stopifnot(index %in% c('Winkler', 'Huglin', 'BEDD'))

  annual_data<-calculate_annual_indicies(data = data)
}

#' Title
#'
#' @param data Raw hourly or daily data returned from weathercan
#' @param index Which index of which to plot accumilation (one of 'Winkler', 'Huglin', or 'BEDD')
#' @param year year to plot, optional (or leave NULL for all)
#'
#' @return an annual accumilation plot of the specified
#' @export
#'
#' @examples
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
