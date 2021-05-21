# col[1] for auc; col[2] for smoothed line
death_col <- brewer.pal(9, 'Reds')[ c(3, 9) ]

# store all dates as 'x'
x <- mys$date

# store number of daily new deaths as 'new_deaths'
new_deaths <- mys$new_deaths[ length(mys$new_deaths) ]

# y.s stores smoothed deaths
y.s <- mys$new_deaths_smoothed

# compute 7-day avg (week ending latest date)
death7avg <-  round( mys[length(y),]$new_deaths_smoothed, 
                    digits = 0)

# compute 7-day average (for the week before this week)
lastdeath7avg <- round(mys[which(mys$date == as.Date(max(x)) - 7),]$new_deaths_smoothed, 0)
lastdeath7avg_date <- format(as.Date(max(x)) - 7, '%d %b')                      

# mean change between last 7 days and last last 7 days (if + cases have gone up, vice versa if -)
weekchange <- death7avg - lastdeath7avg
                     
# define 'arrow colour' depending on whether or not weekchange is positive or negative
arrowcol <- ifelse(weekchange >= 0, 'red', 'green')

# weekchange expressed in %
weekchange_perc <- round( (weekchange / lastdeath7avg) * 100, 
                         digits = 1)

# print absolute change (weekchange) & percentage change
label <- paste( weekchange, 
                ' (', 
                weekchange_perc, 
                '%)', 
                sep = '')

