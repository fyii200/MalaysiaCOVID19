## function to update covid 19 dataset!

data_update <- function(current_data){
  
# read latest (row-shortened) data into R and named 'latest_data'
latest_data <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/latest/owid-covid-latest.csv')
latest_data <- latest_data[which(latest_data$iso_code=='MYS'),]

# 'no' = vector number where 'last_updated_date' can be found
no <- which(names(latest_data)=='last_updated_date')

# change last_updated_date to 'date' so 'mys' and latest_data
# are identical (compatible)
names(latest_data)[no] <- "date"
current_data$date <- as.Date(current_data$date) 
latest_data$date <- as.Date(latest_data$date) 

# check if current data frame (mys) is up to date; 
# if no, bind two df together, otherwise do nothing.
if(max(latest_data$date) > max(current_data$date) ){
  current_data <- rbind(current_data,latest_data)
} else{}

# safety check against duplicated rows 
current_data <- distinct(current_data)
return(current_data)
}






