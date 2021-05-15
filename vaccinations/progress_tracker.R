# source function to plot cumulative number of doses
source('functions/plot_vac.R')

# 3 colour codes for 3 phases
phase_col <- brewer.pal(9, 'Purples')[c(4,6,9)]

# col[1,3] and col[2] for 1st (polygon and then line) & 2nd dose respectively
col <- brewer.pal(9,'Greens')[c(5,9,7)]

# set xlim, define args to configure y axis
xlim<-c(min(d$date), '2022-02-23')
yaxis.lab.at <- seq(0,100,20)
yaxis.lab <- paste(yaxis.lab.at, '%', sep='')

# define x, y1, y2
x <- d$date
y1 <- round( (d$people_vaccinated/32.7e6)*100, 2)
y2 <- round( (d$people_fully_vaccinated/32.7e6)*100, 2)

# plot!
plot_vac(x, y1, y2, col=col,
         title=paste('Is the vaccination programme on track?'),
         xlim=xlim, ylim=c(1,100), tailor.xaxis=F, xaxis.lab, xaxis.lab.at, 
         tailor.yaxis=T, yaxt='n', yaxis.lab.at, yaxis.lab, 
         refline=F, xaxt='n')

# function to add H + V reference lines for each phase
# 'txt.move' determines how much to the left the txt should be moved by
# 'phase' must be character strings; 'perc' = pop coverage in %
refline <- function(x, col, txt.size, txt.move, phase, perc){
  x <- as.Date(x)
  segments(x0=x, y0=0, x1=x, y1=perc, col=col, lty=1, lwd=1.5)
  segments(x0=0, y0=perc, x1=x, y1=perc, col=col, lty=1, lwd=1.5)
  text(x - txt.move, perc+10, labels=paste(phase, ' (', perc,'%)'), col=col,             cex=txt.size)
}

refline(x='2021-04-30', col=phase_col[1],                # phase 1
        txt.size=1.2, txt.move=20, 'Phase 1', 1.5)
refline(x='2021-08-31', col=phase_col[2],                # phase 2
        txt.size=1.4, txt.move=90, 'Phase 2', 30.3)
refline(x='2022-02-28', col=phase_col[3],                # phase 3
        txt.size=1.6, txt.move=110, 'Phase 3', 72.2)

# annotate x axis with dates when each phase ends
par(las=0)
lab <- as.Date(c('2021-04-30', '2021-08-31', '2022-02-28'))
axis(side=1, labels=format(lab,'%d %b %Y'), at=lab, lty=0, col.axis='gray56', cex.axis=1.2)

# how many daily new 1st & 2nd doses needed to hit phase 2 target?
dose.1.required <- (30.3 - max(y1))*32.7e6 / 100
dose.2.required <- (30.3 - max(y2))*32.7e6 / 100

dayleft <- as.vector(as.Date('2021-08-31') - max(x))

dailydose1 <- format(round(dose.1.required / dayleft,0), scientific=F, big.mark=',')
dailydose2 <- format(round(dose.2.required / dayleft,0), scientific=F, big.mark=',')