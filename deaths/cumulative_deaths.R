# col[1] to shade auc; [2] for line
col <- brewer.pal(9,'Greys')[c(3,7)]

x <- mys$date
# cumulative number of deaths
y <- mys$total_deaths

# configure y axis 
ytlab <- seq(0, max(y, na.rm=T)+500, 500)

# plot cumulative deeaths (shade area under the curve)
plot_case(x, y, '', '', xlim=c(min(x),max(x)), auc=T,
          ylim=c(0,max(ytlab)), col[1], xtlab_format,
          xtlab.at=xtlab,  ticksize=0.8, formatC(ytlab,'d',big.mark=','), 
          ytlab.at=ytlab, 
          main='')

# add H ref lines
abline(h=ytlab, lwd=0.5, col='gray88')

# add smoothed line
lines(x,y, lwd=4, col=col[2])

# save latest cumulative num of deaths 
cum_deaths <- format(max(y, na.rm=T), scientific=F, big.mark=',')


