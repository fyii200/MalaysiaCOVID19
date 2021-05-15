# col[1] for AUC, col[2] for the curve
col<-brewer.pal(9, 'Oranges')[c(3,9)]

x<- mys$date
# cumulative num of cases
y<- mys$total_cases

# configure y axis
ytlab <- seq(0,max(mys$total_cases)+2e5,1e5)

# plot and shade AUC
plot_case(x, y, '', '', xlim=c(min(x),max(x)), auc=T,
          ylim=c(0,max(ytlab)), col[1], xtlab_format, lwd=4,
          xtlab.at=xtlab,  ticksize=0.8, 
          paste(formatC(ytlab,'d',big.mark=','), sep=''), 
          ytlab.at=ytlab, 
          main=paste('How many cases have been confirmed so far?') )

# add H ref lines
abline(h=ytlab, col='gray88', lwd=0.5)                                  

# add line
lines(x,y,lwd=4,col=col[2])

max(mys$total_cases_per_million)
max(mys$total_cases)




