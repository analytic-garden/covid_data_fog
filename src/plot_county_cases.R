#' plot_county_cases - plot case moving averages for selected counties
#'
#' @param state - selected state, New York is default,
#' @param counties - a list of county names. Capital District counties are default.
#' @param lag - number of days for moving average. Default is 7
#'
#' @return a data frame with columns
#'    date
#'    county - selected counties
#'    state - selected state
#'    fips - standard location data, see https://www.census.gov/prod/techdoc/cbp/95-96cd/fips-st.pdf
#'    cases - cases lagged by lag days
#'    deaths - cumulative deaths
#'    daily - new cases
#'    avg - moving average
#'    
#' @requires
#' counties must be in state or an error is thrown.

plot_county_cases <- function(state = 'New York',
                              counties = c('Albany', 'Columbia', 'Rensselaer', 'Saratoga', 'Schenectady'),
                              lag = 7) {
  require(tidyverse)
  require(grid)
  require(gridExtra)
  require(zoo)
  require(rlist)
  
  url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  
  df <- read_csv(url)
  df <- df %>% 
    filter(county %in% {{ counties }}) %>% filter(state == {{ state }})
  
  df2 <- data.frame()
  plot_list <- list()
  for(county in counties) {
    # calculate moving average
    temp <- df %>% 
      filter(county == {{ county }}) %>% 
      mutate(daily = cases - lag(cases)) %>% 
      mutate(avg = rollapply(daily, lag, mean, align='right', fill = NA)) %>%
      mutate(avg = ifelse(avg >= 0, avg, 0))
    
    p <- ggplot(temp, aes(x=date, y=avg)) + 
      geom_line() +
      labs(title = paste('Cases ', lag, '-day Moving Average ', county, ' County', sep = ''),
           y = 'Cases')
    plot_list <- list.append(plot_list, p)
    
    df2 <- bind_rows(df2, temp)
  }
  
  # plot individual counties
  grid.arrange(grobs = plot_list, ncol=2, top = textGrob(Sys.Date()))
  
  # plot all counties on one plot
  p2 <- ggplot(df2) +
    geom_line(aes(x = date, y = avg, color = county)) +
    ggtitle(paste('Cases', lag, '-day Moving Average', Sys.Date()))
  print(p2)
  
  return(df2)
}