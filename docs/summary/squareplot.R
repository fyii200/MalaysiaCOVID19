#### Plot square plot to show what % of target population has been vaccinated ###

# col[1] and col[2] for 1 & 2nd dose respectively
col <- brewer.pal(9, 'Blues')[ c(5, 8)] 

# define y1 and y2
y1 <- d$people_vaccinated
y2 <- d$people_fully_vaccinated

# what % of target population (23.6 mil) has received 1st dose?
unrounded_target_y1 <- (max(y1) / 23.6e6) * 100
target_y1 <- round(unrounded_target_y1, 0)

# what % of total population (23.6 mil) has received 2nd dose?
unrounded_target_y2 <- (max(y2) / 23.6e6) * 100
target_y2 <- round(unrounded_target_y2, 0)

# what % havs not received a dose yet?
rem_target <- 100 - target_y1

# create a data frame to store all computations above
target <- data.frame(
          dose = c("2nd dose", "1st dose", "Remaining target pop."),
          percentage = c( target_y2, target_y1-target_y2, rem_target ) 
          )

# plot square chart (called 'p' so ggplot can be forced to be printed later)
p <- function(leg.position)
              ggplot(data = target, 
              aes(fill = dose, values = percentage)) +
              geom_waffle(n_rows = 10, size = 0.5, colour = "#ffffff",  flip = TRUE) +
              scale_fill_manual(values = c(col, 'gray92') ) +
              coord_equal() +
              theme_minimal() +
              theme_enhance_waffle() +
              theme(legend.title=element_blank()) +
              theme(legend.position=leg.position)
           
           






