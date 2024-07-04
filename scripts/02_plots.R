library(lmerTest)
library(here)
library(tidyverse)
p_data = read.csv(here("data", "tidy", "tidy_ldt.csv"))

mean_diff_ci = function(n1,xbar1,s1,n2,xbar2,s2)
{
  
  sp = ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
  margin <- qt(0.975,df=n1+n2-1)*sqrt(sp/n1 + sp/n2)
  
  lowerinterval <- (xbar1-xbar2) - margin
  upperinterval <- (xbar1-xbar2) + margin
  
  df=data.frame(mean_difference = xbar1-xbar2, lower = lowerinterval, upper = upperinterval)
  return(df)
}

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
summary_data <- p_data %>% 
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

ggsave("desc_bar.png", path = here("slides", "img"))


summary_data_p <- p_data %>% 
  group_by(type, ppt) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt),
            sd_rt = sd(key_resp_lextale_trial.rt),
            n = n())

nc = summary_data_p %>% filter(type == "non_cognate")
three = summary_data_p %>% filter(type == "three_way_cognate")
two = summary_data_p %>% filter(type == "two_way_cognate")

nc_l2 = mean_diff_ci(two$n,two$mean_rt,two$sd_rt,
                     nc$n,nc$mean_rt,nc$sd_rt) %>% 
  mutate(ppt = nc$ppt,
         effect = "L2_cfe")

n3_l3 = mean_diff_ci(three$n,three$mean_rt,three$sd_rt,
                     nc$n,nc$mean_rt,nc$sd_rt) %>% 
  mutate(ppt = nc$ppt,
         effect = "L3_cfe")


ci_df = rbind(nc_l2, n3_l3)


ci_df %>% 
  filter(effect == "L2_cfe") %>% 
  ggplot(aes(y = reorder(ppt, -mean_difference), x = mean_difference)) + 
  geom_pointrange(aes(xmin = lower, xmax = upper), color = "seagreen") + 
  xlab("Word Type") + 
  ylab("Reaction Time (ms)") +
  theme(legend.position = "none") + 
  ggtitle("Two-way cognates effect sizes by individual") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  goodale_theme()

ci_df %>% 
  filter(effect == "L2_cfe") %>% 
  ggplot(aes(y = reorder(ppt, -mean_difference), x = mean_difference)) + 
  geom_pointrange(aes(xmin = lower, xmax = upper), color = "darkorange") + 
  xlab("Word Type") + 
  ylab("Reaction Time (ms)") +
  theme(legend.position = "none") + 
  ggtitle("Three-way cognates effect sizes by individual") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  goodale_theme()

ci_df %>% 
  ggplot(aes(y = reorder(ppt, -mean_difference), x = mean_difference, color = effect)) + 
  geom_pointrange(aes(xmin = lower, xmax = upper), position = position_dodge(width = .4)) + 
  xlab("Word type") + 
  ylab("Reaction time effect (ms)") +
  theme(legend.position = "none") + 
  ggtitle("Cognate effect sizes per individual") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  goodale_theme() +
  labs(color="Effect") + 
  scale_color_discrete(breaks=c('Two-way cognate', 'Three-way cognate'))


ggsave("ind_eff.png", path = here("slides", "img"))


mean_difference

library(bayesplot)
## Run the model 
model_b_prior = brms::brm(log_rt ~ type + (1 | word) + (type | ppt), data = p_data)

# Extract posterior samples
posterior_samples <- as.data.frame(model_b_prior)

# Calculate the percentage of negative values for each parameter
percent_negative <- function(parameter) {
  mean(posterior_samples[[parameter]] < 0) * 100
}

parameters <- c("b_typepseudoword", "b_typethree_way_cognate", "b_typetwo_way_cognate")
percentages <- sapply(parameters, percent_negative)
names(percentages) <- parameters

mcmc_areas_plot <- bayesplot::mcmc_areas(
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

mcmc_areas_plot

ggsave("mcmc_l3_plot.png", path = here("slides", "img"))



## Check degree of overlap between the posterior for three way and two way cogates 





## Filter for participants who got more than 70 correct trials 
cdf = p_data %>% 
  group_by(ppt) %>% 
  summarize(no_correct = sum(is_correct))

e_df = p_data %>% 
  group_by(ppt, type) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt)) %>% 
  pivot_wider(names_from = type, values_from = mean_rt) %>% 
  mutate(cfe_3 = non_cognate - three_way_cognate)

prof_df = left_join(cdf, e_df, by = "ppt")

## The proficiency effect is in the opposite direction 
prof_df %>% 
  ggplot(aes(x = no_correct, y = cfe_3)) + geom_point() + geom_smooth(method = "lm")



# Plotting
cdf %>% 
  ggplot(aes(y = reorder(ppt, -no_correct), x = no_correct, fill = as.factor(ppt))) + 
  geom_col() +
  labs(x = "ppt", y = "Number Correct", fill = "ppt") +
  theme_minimal() +
  coord_flip() + ggtitle("No. total correct answers (192 possible) per participant")


## Model 
library(lmerTest)

#p_data$type = as.factor(p_data$type)

#p_data$type = relevel(p_data$type, ref = "three_way_cognate")

model = lmerTest::lmer(log_rt ~ type + (1 | word) + (type | ppt), data = p_data)
summary(model)
# extract model coefficients

