dygraph.plot <- function(data, col, lwd=2, 
                         fill = TRUE, 
                         fillalpha = 0.2, 
                         legend.width = 250,
                         vr,                                           # y axis range
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
              fillGraph = fill,
              fillAlpha = fillalpha,
              strokeWidth = lwd,
              rightGap = rightgap,
              mobileDisableYTouch = F)               %>%
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
             labelsSeparateLines = T)         %>%
    
    dyRangeSelector(dateWindow = 
                      c(min(dates), 
                        max(dates) ),
                    height = 13 )             %>%
    
    dyCrosshair(direction = "vertical")       %>%                      # Add vertical crosshair line over th epont closest to the mouse
  
    dyUnzoom()
}





# 
# # configure data frame for dygraph
# data <- subset(full_date, select=c(date, daily1, daily2) )
# names(data)[c(2,3)] <- c("1st dose", "2nd dose")
# data$date <- as.Date(data$date)
# # convert data frame into xts format
# data <- xts(data[,2:3], order.by = data$date)         
# 
# # col [1] for 2 doses [2] for 1 dose
# col <- brewer.pal(9,'Greens')[c(5,9)]
# # Plot!
# dygraph.plot(data, col, fill=FALSE, 
#              dates=c(max(d$date)-14, max(d$date)), 
#              legend.width=120, display='always',
#              vr=c(0, max(data[,1], na.rm=T)+2e4 )) %>%
#   dyHighlight(highlightCircleSize = 5, 
#               highlightSeriesBackgroundAlpha = 0.2,
#               hideOnMouseOut = TRUE)              
# 
# 
# 
# 
