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
             labelsSeparateLines = T)        %>%
    
    dyRangeSelector(dateWindow = 
                      c(min(dates), 
                        max(dates) ),
                    height = 15 )
  
}













