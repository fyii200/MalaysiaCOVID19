# col[1] for unsmoothed deaths; [2] for smoothed auc; [3] for smoothed line
col <- brewer.pal(9,'OrRd')[c(3,7,9)]

x <- mys$date
y <- mys$new_deaths

# configure y axis 
ytlab <- seq(0, max(y, na.rm=T)+5, 5)

# configure x axis
xtlab <- seq(min(x), max(x), length.out = 6)

# plot unsmoothed deaths
plot_case(x, y, '', '', xlim=c(min(x),max(x)), auc=F,
          ylim=c(0,max(ytlab)), col[1], xtlab_format,
          xtlab.at=xtlab,  ticksize=0.8, ytlab, ytlab.at=ytlab, 
          main='')

# add H ref lines
abline(h=ytlab, col='gray88', lwd=0.5)

# y.s stores smoothed deaths
y.s <- mys$new_deaths_smoothed

# shade area under smoothed line & add smoothed line
polygon(c(min(x),x,max(x)), c(0,y.s,0), border=NA, col=col[2])
lines(x, y.s, lwd=3.5, col=col[3])

# compute 7-day avg (week ending latest date)
death7avg <-  round(mean(y[(length(y)-6) : length(y)], na.rm=T),0)

# compute 7-day average (for the week before this week)
lastdeath7avg <- round(mean(y[(length(y)-13) : (length(y)-7)], na.rm=T),0) 
lastdeath7avg_date <- format(x[length(y)-7], '%d %b')                      

# mean change between last 7 days and last last 7 days (if + cases have gone up, vice versa if -)
weekchange <- death7avg - lastdeath7avg
                     
# define 'arrow colour' depending on whether or not weekchange is positive or negative
arrowcol <- ifelse(weekchange >= 0, 'red', 'green')

# weekchange expressed in %
weekchange_perc <- round((weekchange / lastdeath7avg)*100,1)

# print absolute change (weekchange) & percentage change
label <- paste(weekchange, ' (', weekchange_perc, '%)', sep='')

