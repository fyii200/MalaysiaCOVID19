# create data frame 'test' to record unsmoothed data
test <- data.frame(x=mys$date, y=mys$new_tests) 
test <- test[complete.cases(test),]
test <- test[-which(test$x == as.Date('2020-05-15')),]

# col[1] for unsmoothed data (new tests), col[2] for smoothed data
col<-brewer.pal(9, 'Blues')[c(2,9)]

xtlab <- seq(min(test$x), max(test$x), length.out=6)
ytlab <- seq(0, max(test$y)+2e4,1e4)

# define margin on b, l, t & r (leave more space at the bottom)
par(mar=c(8,5,4,2) )

# Plot unsmotthed data
plot_case(test$x, test$y, '', '', xlim=c(min(test$x),max(test$x)),
          ylim=c(0,1e5), col[1], format(xtlab, '%d %b %Y'), 
          xtlab.at=xtlab,  ticksize=0.8, format(ytlab, scientific=F, big.mark=','), 
          ytlab.at=ytlab, main='')

# add H ref lines
abline(h=ytlab, col='gray88', lwd=0.5)

# legend describes what each coloured line represents
par(xpd = TRUE)                 # set xpd to true so legend is not 
                                # constrained within the plot
legend(min(test$x), 0-2e4, bty='n', lty=1, col=c(col[2],'red'), text.col=c(col[2],'red'), 
       horiz=T, c('Tests taken', 'Total positive cases'), lwd=6, cex=1.2)

# 'test.s' stores smoothed data
test.s <- data.frame(x=mys$date, y=mys$new_tests_smoothed)
# add smoothed line to plot
lines(test.s$x, test.s$y, lwd=4, col=col[2])

# nrow corresponding to latest date 
# where latest new test no. is available 
max.row <- which(mys$date == max(test$x))

# number of positive cases reported on the same day when daily test data are last available
pos_case <- format(mys[max.row,]$new_cases, scientific=F, big.mark=',')

# shade area under line (positive cases)
polygon(c(min(mys$date), mys$date[1:max.row], mys$date[max.row]),
        c(0,mys[1:max.row,]$new_cases,0), border=NA, col='red')


# store latest date where data are available & compute total number of tests
latest_date <- test$x[nrow(test)]
latest_tests <- format(test$y[nrow(test)], scientific=F, big.mark=',')

# data frame for 7 day avg of daily tests (latest week); store dates and tests)
last7 <- data.frame(date=test[(nrow(test) - 6) : nrow(test),1],
                    test=test[(nrow(test) - 6) : nrow(test),2])
last7avg <- round(mean(last7$test),0)

# data frame for 7 day avg of daily tests (the week before latest  week); store dates and tests)
lastlast7 <- data.frame(date=test[(nrow(test) - 13) : (nrow(test)-7),1],
                        test=test[(nrow(test) - 13) : (nrow(test)-7),2])
lastlast7avg <- round(mean(lastlast7$test),0)

# compute change between two 7 day averages (+ means number has gone up)
weekchange <- last7avg - lastlast7avg

# compute percentage change
weekchange_perc <- round((weekchange / lastlast7avg)*100,1)

# define 'arrow colour' depending on whether or not weekchange is positive or negative
arrowcol <- ifelse(weekchange >= 0, 'green', 'red')

# print absolute change (weekchange) & percentage change
label <- paste(format(weekchange, scientific=F, big.mark=','), 
                      ' (', weekchange_perc, '%)', sep='')








