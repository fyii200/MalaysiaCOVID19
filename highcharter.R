# highchart(type = "chart") %>% 
#   hc_add_series(data$first_dose, type = "area", name = '1st Dose', color = 'lightblue') %>%
#   hc_add_series(data$second_dose, type = "area", name = '2nd Dose', color = 'darkblue') %>%
#   hc_tooltip(
#     useHTML    = TRUE,
#     crosshairs = TRUE,
#     shared     = TRUE,
#     outside    = TRUE,
#     borderWidth = 0,
#     headerFormat = '<span style="font-size:0.8em;"> <b>{point.x:%d %b %Y}<br> </span>',
#     pointFormat  = '<span style="font-size:0.8em;"> <b>{series.name}: </span> {point.y} M <br>'
#     )   %>%
#   hc_rangeSelector(enabled      = TRUE,
#                    inputEnabled = FALSE) %>%
#   hc_scrollbar(enabled = TRUE,
#                height = 5) %>%
#   hc_xAxis( labels = list(format = "{value: %d %b '%y}")
#            )                                             %>%
#   hc_yAxis(gridLineWidth = 0.5,
#            labels = list(format = "{value} M")
#            )






config_hc <- function(series, type, series_name, series_col, unit){

highchart(type = "stock") %>%
  hc_add_series(series, type = type, name = series_name, color = series_col) %>%
  
  hc_exporting(enabled = TRUE) %>%
  hc_tooltip(
    useHTML     = TRUE,
    split       = FALSE,
    shared      = TRUE,
    outside     = TRUE,
    borderWidth = 0,
    nullFormat = 'Null',
    labels = list(format = paste("{value}", unit) ),
    headerFormat    = '<span style="font-size:0.9em;"> {point.x:%d %b %Y}<br> </span>',
    pointFormat     = paste('<span style="font-size:0.9em;"> <b>{series.name}: </span> {point.y}', unit, '<br>')
              )   %>%
    
  hc_xAxis( labels  = list(format = "{value: %d %b}")
  )                                             %>%
  hc_yAxis( opposite = FALSE,
            gridLineWidth = 0.5,
            labels = list(format = paste('{value}', unit) )
            )        %>%
  hc_rangeSelector(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_legend(enabled = TRUE, 
            verticalAlign = 'top')

}

  







