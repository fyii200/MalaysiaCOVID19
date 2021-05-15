# source function to plot cumulative number of doses
source('functions/plot_vac.R')

# col[1] and col[2] for 1 & 2nd dose respectively
col <- brewer.pal(9,'Blues')[c(3,8,5)]                    

# express y-val in terms of million
mil <- round(d$people_vaccinated/1e6, digits=2)

# configure x axis
xaxis.lab.at <- seq(min(x), max(x), length.out=6)
xaxis.lab <- format(seq(min(x), max(x), length.out=6), '%d %b %Y')

# crate sequence of y axis values for y axis configuration
yaxis.lab.at <- seq(min(mil), max(mil)+0.5, 0.5)*1e6

# append 'M' to y-val to denote 'million'
yaxis.lab <- paste(yaxis.lab.at/1e6, 'M', sep='')

# set xlim & ylim
xlim <- c(min(d$date), max(d$date))
ylim <- c(0,max(d$people_vaccinated)+5e5)

# define x, y1 & y2
x <- d$date
y1 <- d$people_vaccinated
y2 <- d$people_fully_vaccinated  

# plot cumulative number of doses given
plot_vac(x, y1, y2, refline=F,
         title=paste('How many doses have been given so far?'),
         xlim, ylim, tailor.xaxis=T, xaxis.lab, xaxis.lab.at, col,
         tailor.yaxis=T, yaxt='n', yaxis.lab.at, yaxis.lab)

# add lines for 1st dose
lines(x, y1, lwd=5, col=col[3])








