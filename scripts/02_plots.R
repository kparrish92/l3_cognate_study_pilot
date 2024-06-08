library(lmerTest)
p_data = read.csv(here("data", "tidy", "pilot_data.csv"))



p_data %>% 
  group_by(type) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt),
            sd_rt = sd(key_resp_lextale_trial.rt)) %>% 
  ggplot(aes(x = type, y = mean_rt, fill = type)) + geom_col(position = "dodge") 

p_data %>% 
  ggplot(aes(x = type, y = key_resp_lextale_trial.rt, fill = type)) + geom_boxplot() + facet_wrap(~source)


p_data %>% 
  ggplot(aes(x = key_resp_lextale_trial.rt, fill = type)) + 
  geom_density(alpha = 0.5) + 
  labs(x = "Response Time (s)", y = "Density", fill = "Type") +
  theme_minimal()


## Filter for participants who got more than 70 correct trials 
cdf = p_data %>% 
  group_by(ppt) %>% 
  summarize(no_correct = sum(is_correct)) 

unique(cdf$ppt)

# Plotting
cdf %>% 
  ggplot(aes(y = reorder(ppt, -no_correct), x = no_correct, fill = as.factor(ppt))) + 
  geom_col() +
  labs(x = "ppt", y = "Number Correct", fill = "ppt") +
  theme_minimal() +
  coord_flip() + ggtitle("No. total correct answers (96 possible) per participant")


## Model 
library(lmerTest)

model = lm(log_rt ~ type, data = p_data)

model = lmerTest::lmer(log_rt ~ type + (1 | word) + (1 | ppt), data = p_data)

summary(model) # There is an effect for two-way cognates, but not 3 

## Check random effects to see if any of the stimuli are causing issues (have high leverage)
word_cats = read.csv(here("data", "stimuli", "word_list.csv"))

res = (ranef(model)$word) %>% 
  rownames_to_column("word") %>% 
  left_join(word_cats, by = "word")


res %>% 
  filter(type == "three_way_cognate") %>% 
    ggplot(aes(x = `(Intercept)`, y = word, label = word)) + geom_text() 




eff_df = p_data %>% 
  group_by(ppt, type) %>% 
  summarize(mean_rt_ppt = mean(key_resp_lextale_trial.rt)) %>% 
  pivot_wider(names_from = type, values_from = mean_rt_ppt) %>% 
  mutate(eff_two = two_way_cognate - non_cognate,
         eff_three = three_way_cognate - non_cognate)

no_correct = p_data %>% 
  group_by(ppt) %>%
  summarize(n = n())

eff_df %>% 
  left_join(no_correct, by = "ppt") %>% 
  pivot_longer(cols = c(eff_two, eff_three), names_to = "eff", values_to = "size") %>% 
  ggplot(aes(x = n, y = size, color = eff)) + geom_point() + geom_smooth(method = "lm")


eff_df %>% 
  left_join(no_correct, by = "ppt") %>% 
  ggplot(aes(x = n, y = eff_three)) + geom_point() + geom_smooth(method = "lm")



