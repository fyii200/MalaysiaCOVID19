# call function to plot number of new cases
source('functions/plot_case.R')

col<-brewer.pal(9, 'Reds')[c(3,9)]

x <- mys$date
y <- mys$new_cases
xtlab <- seq(min(x), max(x), length.out=6)
ytlab <- seq(0,7000,1000)

xtlab_format <- format(seq(min(x), max(x), length.out=6), '%d %b %Y')

plot_case(x, y, '', '', xlim=c(min(x),max(x)), 
          ylim=c(0,7000), col[1], xtlab_format, 
          xtlab.at=xtlab,  ticksize=0.8, 
          format(ytlab, scientific=F, big.mark = ','), 
          ytlab.at=ytlab, main='')

# add H ref lines
abline(h=ytlab, col='gray88', lwd=0.5)

# latest number of new cases
dailynew <- format(y[length(y)], scientific=F, big.mark=',')

# a vector containing last 7 dates
last7days <- x[(length(x)-7) : length(x)]
min7day <- format(min(last7days), '%d %b')
max7day <- format(max(last7days), '%d %b')

#vector containing  daily new cases in the past 7 days
last7cases <- y[(length(y)-6) : length(y)]

# vector containing daily new cases over the past 7 days prior to the past 7 days
lastlast7cases <- y[(length(y)-13) : (length(y)-7)]
# lastlast7cases: for the week ending which date?
lastlastdate <- format(x[(length(y)-7)], '%d %b')

# daily confirmed cases (7-day avg)
weeknew <- round(mean(last7cases, na.rm=T), 0)

# daily confirmed cases (7-day avg the week before)
lastweeknew <- round(mean(lastlast7cases, na.rm=T), 0) 
                      

# mean change between last 7 days and last last 7 days (if + cases have gone up, vice versa if -)
weekchange <- weeknew - lastweeknew

# define 'arrow colour' depending on whether or not weekchange is positive or negative
arrowcol <- ifelse(weekchange >= 0, 'red', 'green')

# weekchange expressed in %
weekchange_perc <- round((weekchange / mean(lastlast7cases))*100, 1)

# print absolute change (weekchange) & percentage change
label <- paste(format(weekchange, scientific=F, big.mark=','), 
               ' (', weekchange_perc, '%)', sep='')

# add smoothed lines
lines(x, mys$new_cases_smoothed, col=col[2], lwd=4)







