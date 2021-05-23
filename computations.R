########################### useful functions ###########################

# function to plot line and x-axis only #
plot_line <- function(x, y, col){
             par(mar = c(1, 0.3, 0, 0.3), las = 0)
             plot(x, y, bty = 'n', 
                  xaxt = 'n', 
                  yaxt = 'n', 
                  type = 'n', 
                  xlab = '', 
                  ylab = '', 
                  ylim = c(0, max(y, na.rm = T) ) 
                 )
  
             xtlab <- seq(min(x), max(x), length.out=6)
             xtlab_format <- format(xtlab, '%d %b %Y')
  
             axis(side = 1, 
                  labels = format(xtlab, '%b'), 
                  at = xtlab, tick = F, 
                  col.axis = 'gray56', 
                  cex.axis = 0.6, padj = -3)
  
             lines(x, y, col = col, lwd = 2)
             }

# function to plot dygraph interactive plot
dygraph.plot <- function(data, col, lwd=2, 
                         fill = TRUE, 
                         fillalpha = 0.2, 
                         legend.width = 250,
                         vr,                                           # y axis range
                         rightgap = 4,       
                         display = 'always',
                         dates = c(mys$date)                           # must provide a list of dates!
                         ) {                                           
  
                gray <- brewer.pal(9 ,'Greys')
                dygraph(data)                   %>%
                dyOptions(colors=col, 
                includeZero=T,
                drawAxesAtZero=T, 
                axisLabelFontSize = 12, 
                labelsKMB='M',
                gridLineColor = gray[2],
                disableZoom = T,
                fillGraph = fill,
                fillAlpha = fillalpha,
                strokeWidth = lwd,
                rightGap = rightgap,
                mobileDisableYTouch = F)        %>%
                dyAxis('x', 
                drawGrid = F, 
                axisLabelColor = gray[5], 
                axisLineColor = gray[1] )       %>%
                dyAxis('y', 
                axisLineColor = gray[1], 
                axisLabelColor = gray[5],
                valueRange = (vr))              %>%             
                dyLegend(display, 
                width = legend.width,
                hideOnMouseOut = T,
                labelsSeparateLines = T)        %>%
    
                dyRangeSelector(dateWindow = 
                   c(min(dates) , max(dates) ),
                     height = 13 )                    %>%
                dyCrosshair(direction = "vertical")        %>%           
                dyUnzoom()
                }

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


# function to compute number of new doses administered daily (1st & 2nd dose)
compute_daily_dose <- function(start_date, end_date){                       
                      dat <- data.frame(date =                               # list dates sequentially from 1st 
                                       seq (start_date, end_date, 1)         # to latest vaccination date
                                       ) 

                      sub <- subset(d, select = c(date, people_vaccinated,   # subset of raw vaccination data                  
                                                  people_fully_vaccinated) 
                                    )

                      
                      daily_vac <- merge(dat, sub, all.x = TRUE)             # merge two data frames &
                      daily_vac$dose1 <- 0 ; daily_vac$dose2 <- 0            # retain all dates in 'dat'

                      for(i in 2 : nrow(daily_vac) -1 ) {                    # compute daily jabs (1st then 2nd dose)
  
                                daily_vac$dose1 [i + 1] <-                   # first dose
                                 daily_vac$people_vaccinated [i + 1] - 
                                 daily_vac$people_vaccinated [i]
  
                                daily_vac$dose2 [i + 1] <-                   # second dose
                                 daily_vac$people_fully_vaccinated [i + 1] - 
                                 daily_vac$people_fully_vaccinated [i]
                                }
                      
                      return(daily_vac) 
                      }

########################### latest date for cases & deaths ########################### 
x <- mys$date

# latest data for cases & deaths provided on...
latest_date <- format( max(mys$date), '%d %b %Y' )


########################### computations for 'Daily Cases' ###########################
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

cases_last_7day_date_formatted <- format(cases_last_7day_date, '%d %b %Y')

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


########################### computations for 'Daily Deaths' ###########################
death_col <- brewer.pal(9, 'Reds')[9]

y   <- mys$new_deaths
y.s <- mys$new_deaths_smoothed

# latest number of new cases
new_deaths <- format( y[ length(y) ], scientific = F, big.mark = ',')

# 7-day avg of new cases (most recent week)
deaths_7day_avg <- round(y.s[ length(y.s) ], digits = 0)

# date: 1 week prior to most recent week
deaths_last_7day_date <- max(x) - 7

deaths_last_7day_date_formatted <- format(deaths_last_7day_date, '%d %b %Y')

# 7-day avg of new cases (1 week prior to most recent week)
deaths_last_7day_avg <- round( y.s[ which( x == deaths_last_7day_date) ], digits = 0)

# mean change in avg no. of cases between recent week and last week; 
# if + cases have gone up, vice versa if -
deaths_week_change <- deaths_7day_avg - deaths_last_7day_avg

# define 'arrow colour' depending on whether or not weekly change is positive or negative
deaths_arrowcol <- ifelse(deaths_week_change >= 0, 'red', 'green')

# print label!
deaths_label <- print_label(deaths_week_change, deaths_last_7day_avg)


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


########################### computations for 'Testing' ###########################
# col[1] for total tests; col[2] for positive tests
test_col <- brewer.pal(11 ,'RdYlBu')[ c(11, 1) ]

# data.frame to store daily new tests, 7-day avg & corresponding dates
all_tests <- data.frame(date = mys$date,
                        daily = mys$new_tests,
                        weekly_avg = mys$new_tests_smoothed)

# remove rows with NAs
all_tests <- all_tests[ complete.cases(all_tests), ]

# latest daily (not 7-day avg!) total no. of tests
latest_tests <- format( tail( all_tests$daily, 1),
                        scientific = F, big.mark = ','
                      )

# date when last 7-day avg testing data are available
tests_7day_date <- tail(all_tests$date, 1)

tests_7day_date_formatted <- format(tests_7day_date, '%d %b %Y')

# latest 7-day avg of total daily tests
tests_7day_avg <- tail(all_tests$weekly_avg, 1)

tests_7day_avg_formatted <- format(tests_7day_avg, scientific = F, big.mark = ',')
                          
# date: 1 week prior to most recent date when testing data are available
tests_last_7day_date <- tests_7day_date - 7

tests_last_7day_date_formatted <- format(tests_last_7day_date, '%d %b %Y')

# 7-day avg of new tests (1 week prior to most recent date)
tests_last_7day_avg <- all_tests[ which(all_tests$date == tests_last_7day_date), ]$weekly_avg 

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
pos_rate <- pos_rate[ complete.cases(pos_rate) ]                              # remove NAs

# latest positivity rate (for which data are available)
latest_pos_rate <- tail(pos_rate, 1)

# compute 7-day average of pos_rate
pos_rate_7day <- round( mean(
                             pos_rate[ (length(pos_rate) -6) : length(pos_rate) ],
                             na.rm = T),
                        digits = 1)


########################### computations for 'Vaccinations (Daily Doses)' ###########################
# col[1] for 1st dose; col[2] for 2nd dose
vac_col <- brewer.pal(9, 'Blues')[ c(5, 8) ]  

# most recent data date
latest_dose_date <- format( tail( d$date, 1), 
                          '%d %b %Y')

# y1 = 1st dose; y2 = 2nd dose
y1  <- d$people_vaccinated
y2  <- d$people_fully_vaccinated

# latest daily 1st dose & 2nd dose
daily_dose <- compute_daily_dose( min(d$date), max(d$date) )
daily_dose <- daily_dose [ complete.cases(daily_dose), ]                                            # remove NAs

latest_dose1 <- tail(daily_dose, 1 )$dose1
latest_dose1_formatted <- format(latest_dose1, scientific = F, big.mark = ',')

latest_dose2 <- tail(daily_dose, 1 )$dose2
latest_dose2_formatted <- format(latest_dose2, scientific = F, big.mark = ',')

# compute 7-day avg of new doses and add to existing data frame 'daily_dose'
daily_dose$dose1_7day_avg <- 0 ; daily_dose$dose2_7day_avg <- 0

daily_dose$dose1_7day_avg [7 : nrow(daily_dose) ] <- round(                                         # 1st dose 7-day avg
                                                           rollmean( daily_dose$dose1, k = 7), 
                                                     digits = 0)      

daily_dose$dose2_7day_avg [7 : nrow(daily_dose) ] <- round(                                         # 2nd dose 7-day avg
                                                           rollmean( daily_dose$dose2, k = 7),        
                                                     digits = 0)      

# 7-day avg of new dose 1 & 2 (most recent week)
latest_dose1_7day_avg <- tail( daily_dose$dose1_7day_avg, 1)
dose1_7day_avg_formatted <- format(latest_dose1_7day_avg, scientific = F, big.mark = ',')

latest_dose2_7day_avg <- tail( daily_dose$dose2_7day_avg, 1)
dose2_7day_avg_formatted <- format(latest_dose2_7day_avg, scientific = F, big.mark = ',')


########################### computations for 'Vaccinations (Cumulative Total)' ###########################
# compute cumulative total of 1st & 2nd doses
cumul_dose1 <- tail(d$people_vaccinated, 1)
cumul_dose1_formatted <- format(cumul_dose1, scientific = F, big.mark = ',')

cumul_dose2 <- tail(d$people_fully_vaccinated, 1)
cumul_dose2_formatted <- format(cumul_dose2, scientific = F, big.mark = ',')


########################### computations for 'Vaccinations (Percentage Vaccinated)' ###########################
# % of total population (32.6 mil) inoculated

perc_cumul_dose1 <- round( (cumul_dose1 / 32.6e6) * 100,             # % of pop receiving 1st dose
                          digits = 1)

perc_cumul_dose2 <- round( (cumul_dose2 / 32.6e6) * 100,             # % of pop receiving 2nd dose
                          digits = 1)

perc_daily_dose1 <- round( (latest_dose1 / 32.6e6) * 100,            # daily new 1st doses in terms of % of pop
                           digits = 2)

perc_daily_dose2 <- round( (latest_dose2 / 32.6e6) * 100,            # daily new 2nd doses in terms of % of pop
                           digits = 2)


########################### computations for 'Vaccinations (Progress Tracker)' ###########################
# how many daily new 1st & 2nd doses needed to hit phase 2 target?
dose1_required <- (30.3 - perc_cumul_dose1) * 32.7e6 / 100
dose2_required <- (30.3 - perc_cumul_dose2) * 32.7e6 / 100

dayleft <- as.vector( as.Date('2021-08-31') - tail(d$date, 1) )

dose1_required <- format( round ( dose1_required / dayleft,
                                  digits = 0), 
                         scientific = F, big.mark = ',')

dose2_required <- format( round ( dose2_required / dayleft,
                                  digits = 0), 
                          scientific = F, big.mark = ',')


########################### computations for 'Vaccinations (Square Plot)' ###########################
# what % of target population (23.6 mil) has received 1st dose?
unrounded_target_dose1 <- (max(y1) / 23.6e6 ) * 100
target_dose1           <- round( unrounded_target_dose1, 0)

# what % of total population (23.6 mil) has received 2nd dose?
unrounded_target_dose2 <- (max(y2) / 23.6e6 ) * 100
target_dose2           <- round( unrounded_target_dose2, 0)

# what % has not received a dose yet?
rem_target <- 100 - target_dose1

# create a data frame to store all computations above
target <- data.frame(
                     dose = c("2nd dose", "1st dose", "Remaining target pop."),
                     percentage = c( target_dose2, target_dose1 - target_dose2, rem_target ) 
                     )

# plot square chart (called 'p' so ggplot can be forced to be printed later)
p <- function(leg.position)
     ggplot(data = target, 
         aes(fill = dose, values = percentage) ) +
     geom_waffle(n_rows = 10, size = 0.5, colour = "#ffffff",  flip = TRUE) +
     scale_fill_manual(values = c(vac_col, 'gray92') ) +
     coord_equal() +
     theme_minimal() +
     theme_enhance_waffle() +
     theme(legend.title=element_blank()) +
     theme(legend.position=leg.position)











