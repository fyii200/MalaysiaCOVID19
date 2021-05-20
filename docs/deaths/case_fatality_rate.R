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

# compute 7-day avg
cfr <- round(y[length(y)], 2)



