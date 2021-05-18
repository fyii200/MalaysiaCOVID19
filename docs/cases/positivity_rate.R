## Define x & y
x<- mys$date
y<- mys$positive_rate*100
ytlab <- seq(0,20,2)

plot_case(x, y, '', '', xlim=c(min(x),max(x)), auc=F,
          ylim=c(0,20), 'red', format(xtlab, '%d %b %Y'),
          xtlab.at=xtlab,  ticksize=0.8, lwd=4,
          paste(format(ytlab, scientific=F, big.mark=','),'%', sep=''), 
          ytlab.at=ytlab, main='')

# add H ref lines
abline(h=ytlab, col='gray88', lwd=0.5)

# pos = full data containing positive rate
pos <- mys$positive_rate*100

# pos7avg (7-day average) pos rate
pos7avg <- round(mean(pos[(length(pos)-6):length(pos)], na.rm=T),2)

# latest positivity rate (for which data are available)
latest_pr <- round(mys[which(mys$date == max(test$x) ),]$positive_rate*100, 2)



