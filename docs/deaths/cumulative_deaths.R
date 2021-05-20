# col[1] to shade auc; [2] for line
col <- brewer.pal(9,'Greys')[c(3,7)]

x <- mys$date
# cumulative number of deaths
y <- mys$total_deaths

# configure y axis 
ytlab <- seq(0, max(y, na.rm=T)+500, 500)

# save latest cumulative num of deaths 
cum_deaths <- format(max(y, na.rm=T), scientific=F, big.mark=',')


