library(lmerTest)
p_data = read.csv(here("data", "tidy", "pilot_data.csv"))

## Filter for participants who got more than 70 correct trials 
cdf = p_data %>% 
  group_by(source) %>% 
  summarize(no_correct = sum(is_correct)) 

unique(cdf$source)

cdf %>% 
  ggplot(aes(x = source, y = no_correct, fill = source)) + geom_col()

# Plotting
cdf %>% 
  ggplot(aes(y = reorder(source, -no_correct), x = no_correct, fill = source)) + 
  geom_col() +
  labs(x = "Source", y = "Number Correct", fill = "Source") +
  theme_minimal() +
  coord_flip() + ggtitle("No. total correct answers (96 possible) per participant")

# Group_by for plotting purposes  
cdf_m = p_data %>% 
  filter(source %in% unique(cdf$source)) %>% 
  group_by(type,source) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt))

## Individual plots 
cdf_m %>% 
  filter(type == "non_cognate" | type == "two_way_cognate" | type == "three_way_cognate") %>% 
  ggplot(aes(x = type, y = mean_rt, fill = type)) + geom_col(position = "dodge") +
  facet_wrap(~source)

## Individual plots 
p_data %>% 
  group_by(type,source,word) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt)) %>% 
  filter(type == "non_cognate" | type == "two_way_cognate" | type == "three_way_cognate") %>% 
  ggplot(aes(x = word, y = mean_rt, fill = type)) + geom_col(position = "dodge") +
  facet_wrap(~source)

p_data %>% 
  group_by(type,source,word) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt)) %>% 
  filter(type == "non_cognate" | type == "two_way_cognate" | type == "three_way_cognate") %>% 
  ggplot(aes(x = word, y = mean_rt, fill = type)) + geom_col(position = "dodge") +
  facet_wrap(~source)


## Group data
cdf_m %>% 
  filter(type == "non_cognate" | type == "two_way_cognate" | type == "three_way_cognate") %>% 
  ggplot(aes(x = type, y = mean_rt, fill = type)) + geom_col(position = "dodge") 


## Model 
library(lmerTest)

model = lmerTest::lmer(log_rt ~ type + (1 | source) + (1 | word), data = p_data)

summary(model) # There is an effect for two-way cognates, but not 3 

## Check random effects to see if any of the stimuli are causing issues (have high leverage)
word_cats = read.csv(here("data", "stimuli", "word_list.csv"))

res = (ranef(model)$word) %>% 
  rownames_to_column("word") %>% 
  left_join(word_cats, by = "word")


res %>% 
  filter(type == "three_way_cognate") %>% 
  ggplot(aes(x = `(Intercept)`, y = word, label = word)) + geom_text() 

