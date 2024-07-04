library(lmerTest)
library(here)
library(tidyverse)
p_data_l2 = read.csv(here("data", "tidy", "tidy_ldt_l2.csv"))

p_data_l2 %>% 
  group_by(type) %>% 
  summarize(n = n())

p_data_l2 %>% 
  ggplot(aes(x = log_rt, fill = type)) + geom_histogram(color = "black") +
  facet_wrap(~type)


# Define the custom theme
goodale_theme <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "black", family = "Times New Roman", size = 10),
    axis.title = element_text(colour = "black", family = "Times New Roman", size = 12),
    legend.position = "bottom",
    legend.title = element_text(colour = "black", family = "Times New Roman", size = 10),
    legend.text = element_text(colour = "black", family = "Times New Roman", size = 10),
    plot.title = element_text(colour = "black", family = "Times New Roman", size = 14, hjust = 0.5)  # Center align the title
  )
}

# Summarize the data
summary_data <- p_data_l2 %>% 
  group_by(type) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt),
            sd_rt = sd(key_resp_lextale_trial.rt))

model = lmerTest::lmer(log_rt ~ type + (1 | word) + (type | ppt), data = p_data_l2)
summary(model)

summary_data <- p_data_l2 %>% 
  group_by(type) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt),
            sd_rt = sd(key_resp_lextale_trial.rt),
            n = n()) %>% 
  mutate(margin = qt(0.975,df=n-1)*sd_rt/sqrt(n)) %>% 
  mutate(upper_95_ci = mean_rt + margin,
         lower_95_ci = mean_rt - margin)

# Plot the data
ggplot(summary_data, aes(y = type, x = mean_rt, color = type)) + 
  geom_pointrange(aes(xmin = lower_95_ci, xmax = upper_95_ci)) + 
  scale_fill_manual(values = c("#f0ad4e", "#6c757d", "#5cb85c", "#d9534f")) + 
  xlab("Word Type") + 
  ylab("Reaction Time (ms)") +
  theme(legend.position = "none") + 
  ggtitle("Reaction times of the L3 group by word type") +
  goodale_theme()

ggsave("desc_bar_l2.png", path = here("slides", "img"))

# bonus for ind.

# bonus for ind.
df = p_data_l2 %>% 
  group_by(type, ppt) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt),
            sd_rt = sd(key_resp_lextale_trial.rt),
            n = n()) %>% 
  mutate(margin = qt(0.975,df=n-1)*sd_rt/sqrt(n)) %>% 
  mutate(upper_95_ci = mean_rt + margin,
         lower_95_ci = mean_rt - margin) %>% 
  ggplot(aes(y = type, x = mean_rt, color = type)) + 
  geom_pointrange(aes(xmin = lower_95_ci, xmax = upper_95_ci)) + 
  scale_fill_manual(values = c("#f0ad4e", "#6c757d", "#5cb85c", "#d9534f")) + 
  xlab("Word Type") + 
  ylab("Reaction Time (ms)") +
  theme(legend.position = "none") + 
  ggtitle("Reaction times of the L3 group by word type") +
  goodale_theme() + facet_wrap(~ppt)



unique(l2_df$ppt)

library(bayesplot)
## Run the model 

p_data_l2$type = as.factor(p_data_l2$type)
p_data_l2$type = relevel(p_data_l2$type, ref = "non_cognate")

model_b_prior = brms::brm(log_rt ~ type + (1 | word) + (type | ppt), data = p_data_l2)

summary(model_b_prior)


# Extract posterior samples
posterior_samples <- as.data.frame(model_b_prior)

# Calculate the percentage of negative values for each parameter
percent_negative <- function(parameter) {
  mean(posterior_samples[[parameter]] < 0) * 100
}


parameters <- c("b_typepseudoword", "b_typethree_way_cognate", "b_typetwo_way_cognate")
percentages <- sapply(parameters, percent_negative)
names(percentages) <- parameters

library(bayesplot)
mcmc_areas_plot <- mcmc_areas(
  model_b_prior,
  pars = parameters,
  prob = 0.8
) + 
  labs(
    title = "Posterior distributions for the L3 within-subjects model",
    subtitle = "with medians and 80% intervals"
  )
# Add percentage of negative values as text annotations
for (i in seq_along(parameters)) {
  mcmc_areas_plot <- mcmc_areas_plot + 
    annotate(
      "text", 
      x = Inf, 
      y = length(parameters) - i + 1, # Adjust y position to match the order
      label = paste0(round(percentages[i], 2), "% < 0"), 
      hjust = 1.1, 
      vjust = -0.5, 
      family = "Times New Roman", 
      size = 4
    )
}

summary(model_b_prior)

mcmc_areas_plot
ggsave("mcmc_l2_plot.png", path = here("slides", "img"))


p_data_l2 %>% 
  write.csv(here("data", "tidy", "l2_data_tidy.csv"))