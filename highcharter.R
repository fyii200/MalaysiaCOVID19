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
    
  hc_add_series(series, type = type, name = series_name, color = series_col, resetZoomButton = T) %>%
  
  hc_exporting(enabled = TRUE) %>%
    
  hc_tooltip(
    useHTML         = TRUE,
    split           = FALSE,
    shared          = TRUE,
    outside         = FALSE,
    crosshairs      = TRUE,
    shadow          = FALSE,
    borderWidth     = 0,
    nullFormat      = 'Null',
    backgroundColor = "transparent",
    hideDelay       = 1000,
    labels = list(format = paste("{value}", unit) ),
    headerFormat    = '<span style="font-size:0.9em;"> {point.x:%d %b %Y}<br> </span>',
    pointFormat     = paste('<span style="color:{series.color};"> <b>{series.name}: </span> {point.y}', unit, '<br>'),
    positioner      = JS(
                      "function () {
                      xp =  this.chart.chartWidth / 2 - this.label.width / 2
                      yp =  this.chart.chartHeight / 5

                      return { x: xp, y: yp };
                      }" )
              )   %>%
    
  hc_xAxis( labels  = list(format = "{value: %d %b}"),
            showFirstLabel = TRUE,
            showLastLabel  = TRUE
          )                                             %>%
    
  hc_yAxis( opposite = FALSE,
            gridLineWidth = 0.5,
            labels = list(format = paste('{value}', unit) )
            )        %>%
    
  hc_rangeSelector(enabled              = TRUE,
                   inputEnabled         = FALSE,
                   buttons = list(
                     list(type = 'week', count = 2, text = '2w'),
                     list(type = 'month', count = 1, text = '1m'),
                     list(type = 'month', count = 3, text = '3m'),
                     list(type = 'month', count = 6, text = '6m'),
                     list(type = 'year', count = 1, text = '1y'),
                     list(type = 'all', text = 'All')
                     )
                   ) %>%
    
    hc_plotOptions(lang = "{rangeSelectorFrom: '',
               rangeSelectorTo: '>'}"
                   )                          %>%
    
  hc_scrollbar(enabled = FALSE) %>%
    
  hc_legend(enabled = TRUE, 
            verticalAlign = 'top')
    
}

  








