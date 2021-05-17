
# call function to plot daily doses administered
source('functions/plot_dailyvac.R')

col <- brewer.pal(9,'Greens')[c(4,9)]

# data frame to list dates sequentially (without breaks)
dat <- data.frame(date=seq(min(d$date), max(d$date), 1)) 

# subset of raw data                  
sub <- subset(d, select=c(date, people_vaccinated, people_fully_vaccinated))

# merge two data frames (retain all dates in 'dat')
full_date <- merge(dat, sub, all.x=TRUE)
full_date$daily1 <- 0 ; full_date$daily2 <- 0

# compute daily jabs (1st dose then 2nd dose)
for(i in 2:nrow(full_date)-1){
  full_date$daily1[i+1] <- 
    full_date$people_vaccinated[i+1] - full_date$people_vaccinated[i]
  
  full_date$daily2[i+1] <- 
    full_date$people_fully_vaccinated[i+1] - full_date$people_fully_vaccinated[i]
}

# compute daily total
full_date$total <- apply(full_date[,4:5], 1, sum)

# remove rows with NAs
full_date <- full_date[complete.cases(full_date),]
par(las=0)

# define x, y1 & y2
x <- full_date$date
y1 <- full_date$daily1
y2 <- full_date$daily2

# configure x axis
xaxis.lab.at <- seq(min(x), max(x), length.out=6)
xaxis.lab <- format(seq(min(x), max(x), length.out=6), '%d %b %Y')

# cofigure y axis
ylim <- c(0, (max(y1, y2)+1e4))
yaxis.lab <- seq(0, ylim[2], 1e4)

# plot!
plot_dailyvac(x, y1, y2, ylim, xaxis.lab, xaxis.lab.at, 
              yaxis.lab ,leg_ypos= -max(yaxis.lab)/4 )

# latest 1st dose
new.y1 <- format(y1[length(y1)], scientific=F, big.mark=',')
new.y2 <- format(y2[length(y2)], scientific=F, big.mark=',')

# 7 day avg from latest week
row7day <- (nrow(full_date)-6) : nrow(full_date)                        
week.y1 <- round(mean(y1[row7day]), 0)
week.y2 <- round(mean(y2[row7day]), 0)

# 7 day average from week before latest week
rowlast7day <- (nrow(full_date)-13) : (nrow(full_date)-7)
lastweek.y1 <- round(mean(y1[rowlast7day]), 0)
lastweek.y2 <- round(mean(y2[rowlast7day]), 0)

# diff b/w two 7 day averages for 1st & 2nd dose separately (+ means this week up from last week)
weekchange_y1 <- week.y1 - lastweek.y1
weekchange_y2 <- week.y2 - lastweek.y2

# define 'arrow colour' depending on whether or not weekchange is positive or negative
arrowcol_y1 <- ifelse(weekchange_y1 >= 0, 'green', 'red')
arrowcol_y2 <- ifelse(weekchange_y2 >= 0, 'green', 'red')

# weekchange expressed in %
weekchange_perc_y1 <- round((weekchange_y1 / mean(lastweek.y1))*100, 1)
weekchange_perc_y2 <- round((weekchange_y2 / mean(lastweek.y2))*100, 1)

# print absolute change (weekchange) & percentage change
label_y1 <- paste(format( abs(weekchange_y1), scientific=F, big.mark=','), 
               ' (', abs(weekchange_perc_y1), '%)', sep='')

label_y2 <- paste(format( abs(weekchange_y2), scientific=F, big.mark=','), 
                  ' (', abs(weekchange_perc_y2), '%)', sep='')








