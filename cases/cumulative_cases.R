# col[1] for AUC, col[2] for the curve
col<-brewer.pal(9, 'Oranges')[c(3,9)]

x<- mys$date
# cumulative num of cases
y<- mys$total_cases

# configure y axis
ytlab <- seq(0,max(mys$total_cases)+2e5,1e5)

max(mys$total_cases_per_million)
max(mys$total_cases)




