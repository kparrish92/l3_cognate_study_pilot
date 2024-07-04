### Simulate a visualization of a hypothesis 
library(tidyverse)

# Simulate 100 data points using rnorm
# First, generate normally distributed data with mean and standard deviation
mean <- 83  # Center point between 0.5 and 2
sd <- (135 - 54) / 6  # 99.7% of data within 0.5 to 2

data_points <- rnorm(100, mean = mean, sd = sd)

# Clip data to ensure all values fall between 0.5 and 2
x <- round(pmin(pmax(data_points, 54), 135))
error = rnorm(n = 100, mean = 0, sd = 130)
yint = 83
y = x*-3.1 + error + yint

data = data.frame(y,x)


data %>% 
  ggplot(aes(x,y)) + geom_point() + goodale_theme() + geom_smooth(method = "lm") +
  ylab("Reaction time effect") + xlab("Proficiency")




# Display the simul


#### Theme 
goodale_theme <- function() {
  theme(
    # add border 1)
    # color background 2)
    panel.background = element_rect(fill = "white"),
    
    
    #  panel.grid.major.x = element_line(colour = "black", linetype = 2, size = 0.2),
    #panel.grid.minor.x = element_blank(),
    #    panel.grid.major.y =  element_line(colour = "black", linetype = 2, size = 0.2),
    # panel.grid.minor.y = element_blank(),
    #
    # modify grid 3)
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "black", family = "Times New Roman", size = 10),
    axis.title = element_text(colour = "black", family = "Times New Roman", size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    # legend at the bottom 6)
    legend.position = "bottom",
    legend.title = element_text(colour = "black", family = "Times New Roman", size = 10),
    legend.text = element_text(colour = "black", family = "Times New Roman", size = 10)
  )
} 