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







  










