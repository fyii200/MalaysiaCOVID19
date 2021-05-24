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






# plot_map <- function(map_source, loc){
# 
# mapdata <- get_data_from_map( download_map_data(map_source) )
# 
# countries <- subset(world,
#                     date == max(world$date, na.rm = T),
#                     select = c(date, location, new_cases_per_million) )
# 
# countries$new_cases_per_million <- round(countries$new_cases_per_million, digits = 2)
# 
# names(countries)[2] <- 'name'
# 
# mapdata_countries <- subset(mapdata,
#                             select = c(name, `country-abbrev`)
#                            )
# 
# mapdata <- merge(countries, mapdata_countries)
# 
# min_cols <- brewer.pal(9, "Greens")
# max_cols <- brewer.pal(9, "Reds")
# 
# 
# plot_map <-
# hcmap(map_source,  showInLegend = FALSE,
#       data = mapdata,
#       value = 'new_cases_per_million',
#       joinBy = 'name',
#       name = 'New cases per mil pop.',
#       minSize = "5%",
#       maxSize = "20%"
#       ) %>%
# 
# hc_colorAxis(minColor = min_cols[2], maxColor = max_cols[9], min = 0, max = 500) %>%
# 
# hc_title(text = 'New cases per million population')  %>%
# 
# hc_subtitle(text = paste('How do we compare in', loc, '?') )
# 
# map_config(plot_map)
# }


# plot_map("custom/world-highres", 'globally')




