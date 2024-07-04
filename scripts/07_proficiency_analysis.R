library(tidyverse)
library(here)
# Proficiency plots and analysis
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
  
l2_df = read.csv(here("data", "tidy", "tidy_ldt_l2.csv")) %>% 
  mutate(group = "L2 Spanish")

l3_df = read.csv(here("data", "tidy", "tidy_ldt.csv")) %>% 
  mutate(group = "L3 Spanish")

comb_df = rbind(l2_df, l3_df)

## Filter for participants who got more than 70 correct trials 
cdf = comb_df %>% 
  group_by(ppt, group) %>% 
  summarize(no_correct = sum(is_correct))

e_df_3way = comb_df %>% 
  group_by(ppt, type) %>% 
  summarize(mean_rt = mean(log_rt)) %>% 
  pivot_wider(names_from = type, values_from = mean_rt) %>% 
  mutate(three_way_cognate_effect = non_cognate - three_way_cognate)

e_df_2way = comb_df %>% 
  group_by(ppt, type) %>% 
  summarize(mean_rt = mean(log_rt)) %>% 
  pivot_wider(names_from = type, values_from = mean_rt) %>% 
  mutate(two_way_cognate_effect = non_cognate - two_way_cognate)

prof_df$no_correct

prof_df = left_join(cdf, e_df_3way, by = "ppt") %>% 
  left_join(e_df_2way, by = "ppt") %>% 
  select(group, ppt, two_way_cognate_effect, three_way_cognate_effect, no_correct) %>% 
  pivot_longer(cols = c(3:4), names_to = "effect_type", values_to = "value") 


## The proficiency effect is in the opposite direction 
prof_df %>% 
  ggplot(aes(x = no_correct, y = value, color = effect_type)) + geom_point(alpha = .4) + 
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~group) + goodale_theme() +
  scale_y_reverse() + 
  ylab("Effect from non-cognate") + xlab("Answers correct")


prof_df %>% 
  ggplot(aes(x = no_correct, y = value, color = effect_type)) + geom_point(alpha = .4) + 
  geom_smooth(method = "loess", se = FALSE) + 
  facet_wrap(~group) + goodale_theme() +
  scale_y_reverse() + 
  ylab("Effect from non-cognate") + xlab("Answers correct")

ggsave("prof_plot.png", path = here("slides", "img"))


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

prof_df %>% 
  filter(group == "L2 Spanish") %>% 
  group_by(ppt, no_correct) %>% 
  summarise(value_b = mean(value)) %>% 
  ggplot(aes(x = no_correct, y = value_b)) +
  geom_point(color = 'blue') +
  scale_y_reverse() + 
  geom_smooth(method = "loess", se = FALSE, color = 'red') +
  labs(title = "Collected Data", 
       x = "Language Proficiency", 
       y = "Reaction Time") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Mute specific numbers on x-axis
        axis.text.y = element_blank(),  # Mute specific numbers on y-axis
        axis.ticks = element_blank())   # Remove ticks


ggsave("comp_to_h.png", path = here("slides", "img"))


prof_df


# Plot the data
ggplot(prof_df, aes(x = group, y = value)) +
  geom_boxplot(fill = c("lightblue", "lightgreen")) +
  labs(title = "Hypothetical CFE for L2 and L3 Speakers", 
       x = "Group", 
       y = "Reduction in RT") +
  scale_y_reverse() +
  theme_minimal() + xlab("") 

ggsave("comp_to_h_b.png", path = here("slides", "img"))


prof_df = comb_df %>% 
  left_join(cdf, by = c("ppt", "group")) %>% 
  mutate(proficiency = no_correct/192)

model = lmerTest::lmer(log_rt ~ type*group + proficiency + (1 | word) + (type | ppt), data = prof_df)
summary(model)  


model_b = brms::brm(log_rt ~ type*group + proficiency + (1 | word) + (type | ppt), data = prof_df)


# Plotting
#cdf %>% 
#  ggplot(aes(y = reorder(ppt, -no_correct), x = no_correct, fill = as.factor(ppt))) + 
#  geom_col() +
#  labs(x = "ppt", y = "Number Correct", fill = "ppt") +
#  theme_minimal() +
#  coord_flip() + ggtitle("No. total correct answers (192 possible) per participant")

