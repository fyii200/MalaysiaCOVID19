## Define x & y
x<- mys$date
y<- mys$positive_rate*100
ytlab <- seq(0,20,2)

# pos = full data containing positive rate
pos <- mys$positive_rate*100

# pos7avg (7-day average) pos rate
pos7avg <- round(mean(pos[(length(pos)-6):length(pos)], na.rm=T),2)

# latest positivity rate (for which data are available)
latest_pr <- round(mys[which(mys$date == tests_7day_date ),]$positive_rate*100, 2)



