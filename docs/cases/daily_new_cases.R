########################### useful functions ###########################

# function to print label describing abs change between last two weeks & % change
print_label <- function(abs_change, last_7day_avg){
               # weekly change expressed in %
               perc_change <- round( (abs_change / last_7day_avg) * 100,
                                    digits = 1)
  
  
               paste( format(
               abs_change, scientific = F, big.mark = ','), 
               ' (', 
               perc_change, 
               '%)', 
               sep = '')
               } 

########################### latest date for cases & deaths ########################### 
x <- mys$date

# latest data for cases & deaths provided on...
latest_date <- format( max(mys$date), '%d %b %Y' )


########################### computations for 'Cases' ###########################
case_col <- brewer.pal(9, 'Reds')[3]

y   <- mys$new_cases
y.s <- mys$new_cases_smoothed

# latest number of new cases
new_cases <- format( y[ length(y) ], scientific = F, big.mark = ',')

# 7-day avg of new cases (most recent week)
cases_7day_avg <- round(y.s[ length(y.s) ], digits = 0)

cases_7day_avg_formatted <- format(cases_7day_avg, scientific = F, big.mark = ',')

# date: 1 week prior to most recent week
cases_last_7day_date <- max(x) - 7

cases_last_7day_date_formatted <- format(cases_last_7day_date, '%d %b')

# 7-day avg of new cases (1 week prior to most recent week)
cases_last_7day_avg <- round( y.s[ which( x == cases_last_7day_date) ], digits = 0)

cases_last_7day_avg_formatted <- format(cases_last_7day_avg, scientific = F, big.mark = ',')
                      
# mean change in avg no. of cases between recent week and last week; 
# if + cases have gone up, vice versa if -
cases_week_change <- cases_7day_avg - cases_last_7day_avg

# define 'arrow colour' depending on whether or not weekly change is positive or negative
cases_arrowcol <- ifelse(cases_week_change >= 0, 'red', 'green')

# print label!
cases_label <- print_label(cases_week_change, cases_last_7day_avg)


########################### computations for 'Deaths' ###########################
death_col <- brewer.pal(9, 'Reds')[9]

y   <- mys$new_deaths
y.s <- mys$new_deaths_smoothed

# latest number of new cases
new_deaths <- format( y[ length(y) ], scientific = F, big.mark = ',')

# 7-day avg of new cases (most recent week)
deaths_7day_avg <- round(y.s[ length(y.s) ], digits = 0)

# date: 1 week prior to most recent week
deaths_last_7day_date <- max(x) - 7

deaths_last_7day_date_formatted <- format(deaths_last_7day_date, '%d %b')

# 7-day avg of new cases (1 week prior to most recent week)
deaths_last_7day_avg <- round( y.s[ which( x == deaths_last_7day_date) ], digits = 0)

# mean change in avg no. of cases between recent week and last week; 
# if + cases have gone up, vice versa if -
deaths_week_change <- deaths_7day_avg - deaths_last_7day_avg

# define 'arrow colour' depending on whether or not weekly change is positive or negative
deaths_arrowcol <- ifelse(deaths_week_change >= 0, 'red', 'green')

# print label!
deaths_label <- print_label(deaths_week_change, deaths_last_7day_avg)


########################### computations for 'Testing' ###########################
# col[1] for total tests; col[2] for positive tests
test_col <- brewer.pal(11 ,'RdYlBu')[ c(11, 1) ]

all_tests <- mys$new_tests_smoothed

# latest 7-day avg of total daily tests
tests_7day_avg <- tail( all_tests[ complete.cases(all_tests) ], 1)

tests_7day_avg_formatted <- format(tests_7day_avg, scientific = F, big.mark = ',')

# date when last 7-day avg testing data are available
tests_7day_date <- mys$date[ which(all_tests == tests_7day_avg) ]

tests_7day_date_formatted <- format(tests_7day_date, '%d %b')

# latest daily (not 7-day avg!) total no. of tests
latest_tests <- format( round( mys[which(mys$date == tests_7day_date), ]$new_tests, 
                        digits = 0),
                      scientific = F, big.mark = ','
                      )
                          
# date: 1 week prior to most recent date when testing data are available
tests_last_7day_date <- tests_7day_date - 7

tests_last_7day_date_formatted <- format(tests_last_7day_date, '%d %b')

# 7-day avg of new tests (1 week prior to most recent date)
tests_last_7day_avg <- round( all_tests[ which( x == tests_last_7day_date) ], 
                            digits = 0)

tests_last_7day_avg_formatted <- format(tests_last_7day_avg, scientific = F, big.mark = ',')

# mean change in avg no. of cases between recent week and last week; 
# if + cases have gone up, vice versa if -
tests_week_change <- tests_7day_avg - tests_last_7day_avg

# define 'arrow colour' depending on whether or not weekly change is positive or negative
tests_arrowcol <- ifelse(tests_week_change >= 0, 'green', 'red')

# 7-day avg of positive cases corresponding to the date when latest testing data are available
pos_tests <- mys$new_cases_smoothed
pos_tests <- pos_tests[ which(mys$date == tests_7day_date) ]

pos_tests_formatted <- format( round(pos_tests, 0), 
                              scientific = F, big.mark = ',')

# print label!
tests_label <- print_label(tests_week_change, tests_last_7day_avg)


########################### computations for 'Positivity Rate' ###########################
pos_rate_col <- brewer.pal(9 ,'Reds')[8]

# vector containing daily positivity rate %
pos_rate <- mys$positive_rate * 100

# compute 7-day average of pos_rate
pos_rate_7day <- round( mean(
                             pos_rate[ (length(pos_rate) -6) : length(pos_rate) ],
                             na.rm = T),
                        digits = 2)

# latest positivity rate (for which data are available)
latest_pos_rate <- tail(pos_rate, 1)


########################### computations for 'Cumulative Cases' ###########################
# col[1] for AUC, col[2] for the curve
cumul_cases_col <- brewer.pal(9, 'Oranges')[4]

# cumulative cases
cumul_cases <- format( max(mys$total_cases), 
                      scientific = F, big.mark = ',')

# cumulative cases per million population
cumul_cases_per_mil <- format( round( 
                                    max( mys$total_cases_per_million), 
                               digits = 0), 
                       scientific = F, big.mark = ',')


########################### computations for 'Case Fatality Ratio' ###########################
cfr_col <- c('white', brewer.pal(9, 'OrRd')[4] )

# getting raw data ready (deaths & cases): remove NAs
cfr <- subset(mys, select = c(total_deaths, total_cases, date) )
cfr <- cfr [ complete.cases(cfr), ]

# compute daily CFR and store as a vector
cfr <- (cfr$total_deaths / cfr$total_cases) * 100

# store CFRs and their corresponding dates (except the first 52 NA dates!) in a data frame
cfr <- data.frame(date = mys$date[ -c(1:52) ],
                  cfr = cfr)

latest_cfr <- round( tail(cfr$cfr, 1),
                    digits = 2)


########################### computations for 'Cumulative Deaths' ###########################
cumul_death_col <- brewer.pal(9, 'Greys')[7]

cumul_deaths <- format ( max(
                              mys$total_deaths, na.rm=T), 
                       scientific=F, big.mark=',')




