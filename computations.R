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

# function to plot Highcharter #
config_hc <- function(series, type, series_name, series_col, unit){
  
             highchart(type = "stock") %>%
    
             hc_add_series(series, type = type, name = series_name, color = series_col) %>%
    
             hc_exporting(enabled = TRUE) %>%
    
             hc_tooltip(
                        useHTML         = TRUE,
                        split           = FALSE,
                        shared          = TRUE,
                        outside         = FALSE,
                        crosshairs      = TRUE,
                        shadow          = FALSE,
                        borderWidth     = 0,
                        nullFormat      = 'Null',
                        backgroundColor = "transparent",
                        hideDelay       = 1000,
                        labels = list(format = paste("{value}", unit) ),
                        headerFormat    = ' <b> <span style="font-size:1.1em;"> {point.x:%d %b %Y} <br>',
                        pointFormat     = paste('<span style="font-size:1em; color:{series.color};"> {series.name}: </span> {point.y}', unit, '<br>'),
                        positioner      = JS(
                                             "function () {
                                              xp =  this.chart.chartWidth / 2 - this.label.width / 2
                                              yp =  this.chart.chartHeight / 7

                                              return { x: xp, y: yp };
                                              }" )
                        )                                             %>%
    
          hc_xAxis( labels  = list(format = "{value: %d %b}"),
                    showFirstLabel = TRUE,
                    showLastLabel  = TRUE
                   )                                                  %>%
    
           hc_yAxis( opposite = FALSE,
                     gridLineWidth = 0.5,
                     labels = list(format = paste('{value}', unit) )
                    )                                                 %>%
    
           hc_rangeSelector( enabled              = TRUE,
                             inputEnabled         = FALSE,
                             buttons = list(
                                         list(type = 'week', count = 2, text = '2w'),
                                         list(type = 'month', count = 1, text = '1m'),
                                         list(type = 'month', count = 6, text = '6m'),
                                         list(type = 'year', count = 1, text = '1y'),
                                         list(type = 'all', text = 'All')
                                         )
                            )                                         %>%
    
            hc_scrollbar(enabled = FALSE)                             %>%
    
            hc_legend(enabled = TRUE, 
                      verticalAlign = 'bottom')
  
}

# function to create a data frame called 'mapdata' for hcmap (plot map) #
# data_column = give column from 'world' in which data of interest can be found
create_mapdata <- function(map_source, data_column, decimal_place){
                  
                  # get info on chosen map (https://code.highcharts.com/mapdata/)
                  mapdata <- get_data_from_map( download_map_data(map_source) )
                  
                  # 'countries' contains only latest covid 19
                  countries <- subset(world,
                               date == max(world$date, na.rm = T)
                               )
                  
                  # round covid data to what decimal place?
                  countries[, data_column] <- 
                      round(countries[, data_column], digits = decimal_place)

                  names(countries)[3] <- 'name'

                  # subset mapdata so only country names and abbreviations are shown
                  mapdata_countries <- subset(mapdata,
                                       select = c(name, `country-abbrev`)
                                        )
                  # mapdata contains country names and covid data of interest
                  mapdata <- merge(countries, mapdata_countries)
                  return(mapdata)
                  }

# function to configure hcmap (add navigation, file export, etc. functionalities) #
map_config <-   function (plot_map) {  
                plot_map %>%
                hc_mapNavigation(enabled = TRUE)           %>%
    
                hc_caption(text = paste('Data as of', 
                                           format( as.Date(mapdata$date[1]), '%d %b %Y')
                                       ) 
                           )                               %>%
    
                hc_exporting(enabled = TRUE)              
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
                                     max( mys$total_cases_per_million, na.rm = T), 
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
cumul_deaths_col <- brewer.pal(9, 'Greys')[7]

# cumulative number of deaths
cumul_deaths <- format ( max( mys$total_deaths, na.rm=T), 
                       scientific=F, big.mark=',')

# cumulative deaths per million population
cumul_deaths_per_mil <- format( round( 
                                     max( mys$total_deaths_per_million, na.rm = T), 
                                     digits = 0), 
                               scientific = F, big.mark = ',')


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

####### plot "Vaccination Progress Tracker #######

# configure data frame for highcharter
data <- subset(d, select = c(date, people_vaccinated, people_fully_vaccinated) )

data$people_vaccinated <- round( (data$people_vaccinated/32.7e6) * 100, digits = 2)
data$people_fully_vaccinated <- round( (data$people_fully_vaccinated/32.7e6) * 100, digits = 2)

# add future dates and '0' to each row
names(data)[c(2,3)] <- c("1st dose", "2nd dose")
future <- data.frame(date  = seq(max(data$date) + 1, as.Date('2022-05-31'), 1), 
                     '1st' = 0, 
                     '2nd' = 0 )

names(future)[ c(2, 3) ] <- names(data)[ c(2, 3) ]
data <- rbind(data, future)
data$date <- as.Date(data$date)

# convert data frame into xts format
data <- xts(data[, 2:3], order.by = data$date)         

# define colours
col     <- brewer.pal(9, 'Greens')[ c(5, 9) ]
phase   <- brewer.pal(9, 'Purples')[ c(2, 3, 4) ]
lim     <- brewer.pal(9, 'PuRd')[ c(6, 7, 9) ]

plot_progress_tracker <-
  
         config_hc(data$`1st dose`,
                   'area',
                   '1st dose',
                   col[1],
                   '%')                      %>%
  
          hc_add_series(data$`2nd dose`,
                        type = 'area',
                        name = '2nd dose',
                        color = col[2])      %>%
  
          hc_yAxis(max = 100)                %>%
  
          hc_xAxis(plotBands = list(
                                 list( from = datetime_to_timestamp(as.Date('2021-04-01') ), 
                                       to = datetime_to_timestamp(as.Date('2021-04-30') ),
                                       label = list( text = 'Phase 1' ,
                                                     rotation = 45,
                                                     style = list(fontWeight = 'bold') 
                                                     ),
                                        color = phase[1]
                                        ),
    
                                 list( from = datetime_to_timestamp(as.Date('2021-08-01') ), 
                                       to = datetime_to_timestamp(as.Date('2021-08-31') ),
                                       label = list( text = 'Phase 2' ,
                                                     rotation = 45,
                                                     style = list(fontWeight = 'bold') 
                                                     ),
                                        color = phase[2]
                                        ),
    
                                  list( from = datetime_to_timestamp(as.Date('2022-02-01') ), 
                                        to = datetime_to_timestamp(as.Date('2022-02-28') ),
                                        label = list( text = 'Phase 3' ,
                                                      rotation = 45,
                                                      style = list(fontWeight = 'bold') 
                                                     ),
                                        color = phase[3]
                                        )
                                 )
  
                      )                         %>%
  
        hc_yAxis(plotLines = list(
                                list( color = lim[1],
                                      width = 0.5,
                                      value = 1.5,
                                      label = list( text = '1.5%' )
                                      ),
    
                                 list( color = lim[2],
                                       width = 0.5,
                                       value = 30.3,
                                       label = list( text = '30.3% ' )
                                       ),
    
                                 list( color = lim[3],
                                       width = 0.5,
                                       value = 72.2,
                                       label = list( text = '72.2% ' )
                                       )
                                )
  
                  )            


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


############# computations for 'Comparisons (new cases per million population)' #############

# create mapdata data frame containing country names and only latest covid 19
mapdata <- create_mapdata(map_source = 'custom/world-highres',
                          data_column = which(names(world) == 'new_cases_per_million'),
                          decimal_place = 2)
  
# Plot map function
plot_map_cases_per_million <- function(dataset, mapsource){
                              min_cols <- brewer.pal(9, "Greens")
                              max_cols <- brewer.pal(9, "Reds")
  
                              plot_map <-
                              hcmap(mapsource,  showInLegend = FALSE,
                              data = dataset,
                              value = 'new_cases_per_million',
                              joinBy = 'name',
                              name = 'New cases per mil pop.',
                              minSize = "1%",
                              maxSize = "100%"
                              ) %>%
    
                              hc_colorAxis(minColor = min_cols[2], 
                                           maxColor = max_cols[9], 
                                           min = 0, max = 500)
    
                              map_config(plot_map)
                              }

## to plot bubble plot (new cases per million pop) ##

# add a 'note' column to group all other countries collectively
add_note <- function(d){
            d$note                                       <- 'Other countries'
            d[which(d$name == 'Malaysia'), ]$note        <- 'Malaysia'
            d[which(d$name == 'India'), ]$note           <- 'India'
            d[which(d$name == 'United Kingdom'), ]$note  <- 'UK'
            d[which(d$name == 'Thailand'), ]$note        <- 'Thailand'
            d[which(d$name == 'Qatar'), ]$note           <- 'Qatar'
            d[which(d$name == 'Indonesia'), ]$note       <- 'Indonesia'
            d[which(d$name == 'Singapore'), ]$note       <- 'Singapore'
            return(d)
            }
mapdata <- add_note(mapdata)

# colours for India, Indonesia, Mys, other countries, Qatar, Sg, Thai & UK
bubble_cols <- brewer.pal(12, "Set3")[ c(1:2, 4, 9, 5:8) ]

plot_bubble_cases <- hchart( mapdata,
                     type = "point",
                     hcaes(life_expectancy, 
                     new_cases_per_million, 
                     size = population, 
                     group = note)
                     )                                                 %>%
  
               hc_yAxis( title = list (
                                   text = 'New cases per million'),
                         type = 'logarithmic')                         %>%
  
               hc_xAxis( title = list( 
                                   text = 'Life expectancy') )         %>%
  
               hc_tooltip( useHTML = TRUE,
                           headerFormat = "<b>{point.key}</b><br>",
                           pointFormat  = "{point.y}"
                          )                                            %>%
  
               hc_colors(bubble_cols)                                  %>%
  
               hc_yAxis(gridLineWidth = 0.3,
                        plotLines = list(
                                       list( color = bubble_cols[3],
                                             width = 1,
                                             value = tail(mys$new_cases_per_million, 1) ,
                                             label = list( text = 'Malaysia')
                                             )
                                        ) 
                                               
                         )                                             %>%
  
                 hc_subtitle( text = paste('Data as of', 
                                        format( as.Date(mapdata$date[1]), '%d %b %Y')
                                        )  
                          )


############# computations for 'Comparisons (vaccinations per million population)' #############

mapdata <- create_mapdata(map_source = 'custom/world-highres',
                          data_column = which(names(world) == 'new_cases_per_million'),
                          decimal_place = 2)

# extract country names
unique_country <- unique(world$location)

# create data.frame to store most recent vaccination data from different countries
vac <- data.frame(name = unique_country,
                  people_vaccinated_per_hundred = 0, 
                  people_fully_vaccinated_per_hundred = 0)

# loop to extract most recent data from every country
for(i in 1:length(unique_country) ) {
  
        a <- subset(world, location == unique_country[i] )
        
  
        vac[i, 2:3] <- c( max(a$people_vaccinated_per_hundred, na.rm = T),
                          max(a$people_fully_vaccinated_per_hundred, na.rm = T)
                         )
        }

vac <- vac[which(vac$people_fully_vaccinated_per_hundred >= 0),]

# plot map
plot_map_vac_per_hundred <- function(dataset, mapsource){
                             cols <- brewer.pal(9, "Greens")
  
                             plot_map <-
                             hcmap(mapsource,  showInLegend = FALSE,
                             data = dataset,
                             value = 'people_fully_vaccinated_per_hundred',
                             joinBy = 'name',
                             name = 'Fully vaccinated (%)',
                             minSize = "1%",
                             maxSize = "100%"
                             ) %>%
    
                             hc_colorAxis(minColor = cols[2], 
                                          maxColor = cols[9], 
                                          min = 0, max = 100)
  
                             map_config(plot_map)
                             }


## to plot bubble plot (% fully vaccinated) ##

a <- subset(world, select = c(
                         location, life_expectancy, population) 
            )

names(a)[1] <- 'name'
vac_data <- merge(vac, a, by='name')
vac_data <- distinct(vac_data)

vac_data <- add_note(vac_data)
vac_data[which(vac_data$name == 'World'), ]$note <- 'Global average'

# colours for world, India, Indonesia, mys, other countries, Qatar, Sg, Thai & UK
bubble_cols <- brewer.pal(12, "Set3")[ c(1:4, 9, 5:8, 10) ]

plot_bubble_vac <- hchart( vac_data,
                           type = "point",
                           hcaes(life_expectancy, 
                                 people_fully_vaccinated_per_hundred, 
                                 size = population,
                                 group = note
                                 )
                          )                                    %>%
  
                    hc_yAxis( title = list (
                    text = 'Fully vaccinated (%)'),
                    type = 'logarithmic')                      %>%
  
                    hc_xAxis( title = list( 
                    text = 'Life expectancy') )                %>%
  
                    hc_tooltip( useHTML = TRUE,
                    headerFormat = "<b>{point.key}</b><br>",
                    pointFormat  = "{point.y}%"
                    )                                          %>%
  
                    hc_colors(bubble_cols)                     %>%
  
                    hc_yAxis(gridLineWidth = 0.3,
                             max = 100,
                             plotLines = list(
                                           list( color = bubble_cols[3],
                                                 width = 1,
                                                 value = tail(mys$people_fully_vaccinated_per_hundred, 1) ,
                                                 label = list( text = 'Malaysia')
                                                )
                                             ) 
           
                              )                                 %>%
  
                              hc_subtitle( text = paste('Data as of', 
                                                         format( as.Date(max(world$date) ), '%d %b %Y')
                                                       )  
                                         ) 









