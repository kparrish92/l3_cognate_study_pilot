library(ggplot2)

# Generate synthetic data
set.seed(123) # for reproducibility
group <- rep(c("L3 Speakers", "L2 Speakers"), each = 100)
CFE <- c(rnorm(100, mean = 0, sd = 25), rnorm(100, mean = -70, sd = 25))

# Create a data frame
data <- data.frame(group, CFE)

# Plot the data
ggplot(data, aes(x = group, y = CFE)) +
  geom_boxplot(fill = c("lightblue", "lightgreen")) +
  labs(title = "Hypothetical CFE for L2 and L3 Speakers", 
       x = "Group", 
       y = "Effect") +
  theme_minimal() + xlab("") 

ggsave("boxplot_es_h.png")


set.seed(123) # for reproducibility
language_proficiency <- seq(0, 100, by=1)
reaction_time <- ifelse(language_proficiency < 60, 
                        300 - 3 * language_proficiency + rnorm(101, mean = 0, sd = 10), 
                        120 + rnorm(101, mean = 0, sd = 5))

# Create a data frame
data <- data.frame(language_proficiency, reaction_time)

# Plot the data
ggplot(data, aes(x = language_proficiency, y = reaction_time)) +
  geom_point(color = 'blue') +
  geom_smooth(method = "loess", se = FALSE, color = 'red') +
  labs(title = "Hypothetical Plateau Effect", 
       x = "Language Proficiency", 
       y = "Reaction Time (ms)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Mute specific numbers on x-axis
        axis.text.y = element_blank(),  # Mute specific numbers on y-axis
        axis.ticks = element_blank())   # Remove ticks

ggsave("prof_h.png")
