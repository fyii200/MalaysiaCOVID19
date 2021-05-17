dygraph.plot <- function(data, col, lwd=2, 
                         fill = TRUE, 
                         fillalpha = 0.2, 
                         legend.width = 250,
                         vr,                  # y axis range
                         rightgap = 4,       
                         display = 'always',
                         dates = mys$date) {                          # must provide a list of dates!
  
  gray <- brewer.pal(9 ,'Greys')
  
  dygraph(data)                         %>%
    
            dyOptions(colors=col, 
              includeZero=T,
              drawAxesAtZero=T, 
              axisLabelFontSize = 12, 
              labelsKMB='M',
              gridLineColor = gray[2],
              disableZoom = T,
              animatedZooms = T,
              fillGraph = fill,
              fillAlpha = fillalpha,
              strokeWidth = lwd,
              rightGap = rightgap,
              mobileDisableYTouch =F)               %>%
    dyAxis('x', 
           drawGrid = F, 
           axisLabelColor = gray[5], 
           axisLineColor = gray[1] )           %>%
    dyAxis('y', 
           axisLineColor = gray[1], 
           axisLabelColor = gray[5],
           valueRange = (vr))                 %>%             
    dyLegend(display, 
             width = legend.width,
             hideOnMouseOut = T,
             labelsSeparateLines = T)        %>%
    
    dyRangeSelector(dateWindow = 
                      c(min(dates), 
                        max(dates) ),
                    height = 8 )
  
}














# # configure data frame for dygraph
# data <- subset(d, select=c(date, people_vaccinated, people_fully_vaccinated) )
# data$people_vaccinated <- round( (data$people_vaccinated/32.7e6)*100, 2)
# data$people_fully_vaccinated <- round( (data$people_fully_vaccinated/32.7e6)*100, 2)
# 
# # add future dates and '0' to each row
# names(data)[c(2,3)] <- c("1st dose (%)", "2nd dose (%)")
# future <- data.frame(date=seq(max(data$date)+1, as.Date('2022-04-30'), 1), '1st' =0, '2nd' =0 )
# names(future)[c(2,3)] <- names(data)[c(2,3)]
# data <- rbind(data, future)
# data$date <- as.Date(data$date)
# 
# # convert data frame into xts format
# data <- xts(data[,2:3], order.by = data$date)
# 
# # define colours
# col<- brewer.pal(9 ,'Greens')[c(5,9)]
# phase <- brewer.pal(9 ,'Purples')[c(1,2,3)]
# lim <- brewer.pal(9 ,'PuRd')[c(5,7,9)]
# event <- brewer.pal(3, 'Greys')[2]
# 
# # Plot!
# dygraph.plot(data, col,
#              dates=c(min(d$date),
#                      as.Date('2022-04-30') ),
#              legend.width=120,
#              display='onmouseover', vr=c(0, 100) )    %>%
#   dyLimit(72.2, color = lim[3],
#           label='Phase 3 (72.2%)',
#           labelLoc='left')                            %>%
#   dyLimit(30.3, color = lim[2],
#           label='Phase 2 (30.3%)',
#           labelLoc='left')                            %>%
#   dyLimit(1.5, color = lim[1],
#           label='Phase 1 (1.5%)',
#           labelLoc='right')                           %>%
# 
#   dyShading(from=as.POSIXlt.character('2021-02-01'),
#           to=as.POSIXlt.character('2021-04-30'),
#           color=phase[3])   %>%
#   dyShading(from=as.POSIXlt.character('2021-04-30'),
#             to=as.POSIXlt.character('2021-08-31'),
#             color=phase[2])   %>%
#   dyShading(from=as.POSIXlt.character('2021-08-31'),
#             to=as.POSIXlt.character('2022-02-28'),
#             color=phase[1]) %>%
# 
#   dyEvent("2021-04-30", "Phase 1",
#           labelLoc = "top",
#           color=event, 'dotted')                      %>%
# dyEvent("2021-08-31", "Phase 2",
#         labelLoc = "top",
#         color=event, 'dotted')                        %>%
# dyEvent("2022-02-28", "Phase 3",
#         labelLoc = "top",
#         color=event, 'dotted')
# 
# 
# 
# 
# 
# 
# 
