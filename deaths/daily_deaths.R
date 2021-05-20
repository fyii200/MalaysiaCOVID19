# col[1] for unsmoothed deaths; [2] for smoothed auc; [3] for smoothed line
col <- brewer.pal(9,'OrRd')[c(3,7,9)]

x <- mys$date
y <- mys$new_deaths

# y.s stores smoothed deaths
y.s <- mys$new_deaths_smoothed

# compute 7-day avg (week ending latest date)
death7avg <-  round(mys[length(y),]$new_deaths_smoothed, 0)

# compute 7-day average (for the week before this week)
lastdeath7avg <- round(mys[which(mys$date == as.Date(max(x)) - 7),]$new_deaths_smoothed, 0)
lastdeath7avg_date <- format(as.Date(max(x)) - 7, '%d %b')                      

# mean change between last 7 days and last last 7 days (if + cases have gone up, vice versa if -)
weekchange <- death7avg - lastdeath7avg
                     
# define 'arrow colour' depending on whether or not weekchange is positive or negative
arrowcol <- ifelse(weekchange >= 0, 'red', 'green')

# weekchange expressed in %
weekchange_perc <- round((weekchange / lastdeath7avg)*100,1)

# print absolute change (weekchange) & percentage change
label <- paste(weekchange, ' (', weekchange_perc, '%)', sep='')

