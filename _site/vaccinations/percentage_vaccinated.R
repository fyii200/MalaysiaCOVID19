# source function to plot cumulative number of doses
source('functions/plot_vac.R')

# col[1] and col[2] for 1st & 2nd dose respectively
col <- brewer.pal(9,'Greens')[c(5,9,7)] 

# define 'x' (date)
x <- d$date

# percentage of population vaccinated (1st then 2nd dose)
y1 <- round( (d$people_vaccinated/32.7e6)*100, 2)
y2 <- round( (d$people_fully_vaccinated/32.7e6)*100, 2)

# configure y axis label such that '%' is shown
yaxis.lab.at <- seq(0, max(y1)+10, 5)
yaxis.lab <- paste(yaxis.lab.at, '%', sep='')
ylim <- c(0, max(yaxis.lab.at) )

# configure x axis
xaxis.lab.at <- seq(min(x), max(x), length.out=6)
xaxis.lab <- format(seq(min(x), max(x), length.out=6), '%d %b %Y')
xlim <- c(min(x), max(x))

# plot percentage of pop vaccinated
plot_vac(x, y1, y2, col=col, title='', xlim=xlim, 
  ylim=ylim, tailor.xaxis=T, xaxis.lab, xaxis.lab.at, tailor.yaxis=T,
  yaxt='n', yaxis.lab.at, yaxis.lab, refline=F, leg_ypos=min(ylim)-2)

# add lines for 1st
lines(x, y1, lwd=5, col=col[3])

# compute increase in pop vaccinated in % terms
inc1 <- round( max(y1) - y1[length(y1)-1], 2)
inc2 <- round( max(y2) - y2[length(y2)-1], 2)










