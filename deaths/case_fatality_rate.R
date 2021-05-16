# col[1] to shade auc; [2] for smoothed line
col <- c('white', brewer.pal(9,'OrRd')[4])

# remove NA
data <- subset(mys, select=c(total_deaths, total_cases, date))
data <- data[complete.cases(data),]

# define x
x <- data$date

# compute daily case fatality rate and store as 'y'
y <- (data$total_deaths / data$total_cases)*100

# configure y axis 
ytlab <- seq(0, max(y, na.rm=T)+0.2, 0.2)

# plot smoothed fatality rate (shade area under the curve)
plot_case(x, y, '', '', xlim=c(min(x),max(x)), auc=T,
          ylim=c(0,max(ytlab)), col[1], xtlab_format,
          xtlab.at=xtlab,  ticksize=0.8, paste(ytlab,'%',sep=''), 
          ytlab.at=ytlab, 
          main='')

# add H ref lines
abline(h=ytlab, col='gray88', lwd=0.5)

# add smoothed lines
lines(x, y, lwd=5, col=col[2])

# compute 7-day avg
cfr <- round(y[length(y)], 2)



