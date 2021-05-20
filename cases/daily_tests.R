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

# 'test.s' stores smoothed data
test.s <- data.frame(x=mys$date, y=mys$new_tests_smoothed)
# # add smoothed line to plot
# lines(test.s$x, test.s$y, lwd=4, col=col[2])

# nrow corresponding to latest date 
# where latest new test no. is available 
max.row <- which(mys$date == max(test$x))

# number of positive cases reported on the same day when daily test data are last available
pos_case <- format(mys[max.row,]$new_cases, scientific=F, big.mark=',')

# store latest date where data are available & compute total number of tests
latest_date <- test$x[nrow(test)]
latest_tests <- format(test$y[nrow(test)], scientific=F, big.mark=',')

# data frame for 7 day avg of daily tests (latest week); store dates and tests)
last7avg <- mys[max.row,]$new_tests_smoothed
last7avg_date <- format(mys$date[max.row], '%d %b')

# data frame for 7 day avg of daily tests (the week before latest  week); store dates and tests)
lastlast7avg <- mys[which(mys$date == mys$date[max.row]-7),]$new_tests_smoothed
lastlast7avg_date <- format(mys$date[max.row]-7, '%d %b')

# compute change between two 7 day averages (+ means number has gone up)
weekchange <- last7avg - lastlast7avg

# compute percentage change
weekchange_perc <- round((weekchange / lastlast7avg)*100,1)

# define 'arrow colour' depending on whether or not weekchange is positive or negative
arrowcol <- ifelse(weekchange >= 0, 'green', 'red')

# print absolute change (weekchange) & percentage change
label <- paste(format(weekchange, scientific=F, big.mark=','), 
                      ' (', weekchange_perc, '%)', sep='')








