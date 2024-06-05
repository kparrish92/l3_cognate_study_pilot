library(lmerTest)
p_data = read.csv(here("data", "tidy", "pilot_data.csv")) 

# Function to check for influential data points at the individual level per word type
# Plot returns the index (row no.) of the influence of indvidual data points on an 
# intercept only model for RT

p_data %>% 
  group_by(type) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt),
            sd_rt = sd(key_resp_lextale_trial.rt))


ppt = "5f989fa02b"
word_type = "three_way_cognate"

find_outliers = function(ppt, word_type) {  
  p_data = read.csv(here("data", "tidy", "pilot_data.csv")) %>% 
    filter(source == ppt) %>% 
    filter(type == word_type)
  model_check = lm(log_rt ~ 1, data = p_data)
  pf = cooks.distance(model_check) %>% 
    as.data.frame() %>% 
    rownames_to_column("index") 
  
  p_data$index = c(1:nrow(p_data))
  p_data$index = as.character(p_data$index)
  
  p = pf %>% 
    left_join(p_data, by = "index") %>% 
    ggplot(aes(y = ., x = as.numeric(index), label = word)) + 
    geom_text(size = 3.5) +
    theme_minimal() + 
    xlab("Index") + 
    ylab("Leverage") + 
    ggtitle("Each data point's on the intercept only model")
  return(p)
}


loop_df = crossing(ppt = unique(p_data$source),
word_type = unique(p_data$type))

find_outliers("5f989fa02b", "three_way_cognate") 




