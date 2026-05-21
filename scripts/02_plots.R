library(lmerTest)
library(tidyverse)
library(here)


#### Add lexical frequency as a predictor
#### Get proficiency data for the L2 Spanish group + more for L3 group if possible
#### Expand both data sets 






p_data = read.csv(here("data", "tidy", "pilot_data.csv")) %>% 
  filter(is_correct == 1) %>% 
  filter(!is.na(participant))



inc_data %>% 
  filter(type == "two_way_cognate" | type == "three_way_cognate")%>% 
  group_by(word, type) %>% 
  summarise(mean_rt = mean(key_resp_lextale_trial.rt)) %>% 
  ggplot(aes(y = word, color = type, x = mean_rt)) + geom_point()



totals = p_data %>% 
  group_by(type, group) %>% 
  summarise(total_n = n())


### Accuracy 

p_data %>% 
  group_by(type, group, is_correct) %>% 
  summarise(n = n()) %>% 
  left_join(totals) %>% 
  mutate(pct_correct = n/total_n) %>% 
  ggplot(aes(x = type, y = pct_correct, fill = group)) + 
  geom_col(position = "dodge", color = "black") + theme_minimal()

ggsave(here("plots", "manuscript", "accuracy_plot.png"), dpi = 600)
### Accuracy model 

### Lot rt plots (pooled and non-pooled)

### Proficiency 


### Check Bayesian model and plots and report those - consider a model with priors 






summary_data <- p_data %>% 
  group_by(type, group) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt),
            sd_rt = sd(key_resp_lextale_trial.rt),
            n = n()) %>% 
  mutate(margin = qt(0.975,df=n-1)*sd_rt/sqrt(n)) %>% 
  mutate(upper_95_ci = mean_rt + margin,
         lower_95_ci = mean_rt - margin)

summary_data_pooled <- p_data %>% 
  mutate(type = case_when(
         type == "three_way_cognate" ~ "cognate",
         type == "two_way_cognate" ~ "cognate",
         type == "pseudoword" ~ "pseudoword",
         type == "non_cognate" ~ "non_cognate")) %>% 
  group_by(type) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt),
            sd_rt = sd(key_resp_lextale_trial.rt),
            n = n()) %>% 
  mutate(margin = qt(0.975,df=n-1)*sd_rt/sqrt(n)) %>% 
  mutate(upper_95_ci = mean_rt + margin,
         lower_95_ci = mean_rt - margin)

summary_data_pooled %>%
  ggplot(aes(y = type, x = mean_rt, color = type)) + 
  geom_pointrange(aes(xmin = lower_95_ci, xmax = upper_95_ci)) + 
  scale_color_manual(values = c("#f0ad4e", "#5cb85c", "#d9534f")) + 
  xlab("Word Type") + 
  ylab("Reaction Time (ms)") +
  theme(legend.position = "none") + 
  ggtitle("Reaction times by word type") +
  goodale_theme() + theme(legend.position="none")

ggsave(here("plots", "overall_cognates.png"), dpi = 600)


summary_data %>%
  ggplot(aes(y = type, x = mean_rt, color = type, shape = group)) + 
  geom_pointrange(aes(xmin = lower_95_ci, xmax = upper_95_ci)) + 
  scale_fill_manual(values = c("#f0ad4e", "#6c757d", "#5cb85c", "#d9534f")) + 
  xlab("Word Type") + 
  ylab("Reaction Time (ms)") +
  theme(legend.position = "none") + 
  ggtitle("Reaction times of the both groups") +
  goodale_theme() + theme(legend.position="none") + facet_grid(~group)

ggsave(here("plots", "groups_cfe.png"), dpi = 600)

summary_data %>%
  filter(group == "L2_group") %>% 
  ggplot(aes(y = type, x = mean_rt, color = type)) + 
  geom_pointrange(aes(xmin = lower_95_ci, xmax = upper_95_ci)) + 
  scale_fill_manual(values = c("#f0ad4e", "#6c757d", "#5cb85c", "#d9534f")) + 
  xlab("Word Type") + 
  ylab("Reaction Time (ms)") +
  theme(legend.position = "none") + 
  ggtitle("Reaction times of the L2 group by word type") +
  goodale_theme() + theme(legend.position="none")

ggsave(here("plots", "l2_desc.png"), dpi = 600)


#model = lmerTest::lmer(log_rt ~ group*type + (1 | word) + (type | participant), data = inc_data)

#summary(model)

l2_data = p_data %>% filter(group == "L2_group") %>% 
  mutate(new_type = case_when(
    type == "two_way_cognate" ~ "cognate",
    type == "three_way_cognate" ~ "cognate",
    type == "non_cognate" ~ "non_cognate"
  )) %>% 
  filter(!is.na(new_type))
  
l2_model = lmerTest::lmer(log_rt ~ new_type + (1 | word) + (type | participant), data = l2_data)

summary(l2_model)



l3_data_p = p_data %>% filter(group == "L3_group") %>% 
  filter(type != "pseudoword")


l3_data_p$type = as.factor(l3_data_p$type)
l3_data_p$type = relevel(l3_data_p$type, ref = "two_way_cognate")


l3_model_p = lmerTest::lmer(log_rt ~ type + (1 | word) + (type | participant), data = l3_data_p)


summary(l3_model_p)
summary(l3_model_a)
summary(l2_model)

b_model = brms::brm(log_rt ~ type*group + (1 | word) + (type | participant), iter = 4000, 
                    data = inc_data, file = here("data", "models", "l3_models_u.rds"))



p_data %>% 
  filter(group == "L3_group") %>% 
  ggplot(aes(y = log_rt, x = type)) + geom_boxplot() + 
  facet_wrap(~participant)

summary(b_model)
summ = bayestestR::describe_posterior(b_model) %>% 
  as.data.frame() %>% 
  janitor::clean_names() 

conditional_effects(b_model)




library(bayestestR)
library(bayesplot)

posterior_o<- as.matrix(b_model)
summary(b_model)
plot_title <- ggtitle("Posterior distributions of effect relative to two-way cognates",
                      "with the probability of direction and ROPE")
mcmc_areas(posterior_o,
           pars = c("b_Intercept",
                    "b_typenon_cognate", 
                    "b_typethree_way_cognate",
                    "b_groupL2_group",
                    "b_typenon_cognate:groupL2_group",
                    "b_typethree_way_cognate:groupL2_group",
                    "b_typepseudoword",
                    "b_typepseudoword:groupL2_group"),
           prob = 0.8) + plot_title + theme_minimal() +
  xlim(-.75,.75) + 
  geom_text(data = mutate_if(summ, is.numeric, round, 2),
            aes(label = paste("Pd = ",pd, " - In ROPE = ", rope_percentage), x = Inf), 
            hjust = "inward", size = 3) +
  scale_y_discrete(labels=c(
    "b_typenon_cognate"="Two-way to non-cognates in L3 group",
                            "b_typethree_way_cognate"="Two-way to three-way cognate L3 group",
                            "b_groupL2_group"="L3 to L2 in two-way cognates",
                            "b_typenon_cognate:groupL2_group"="L3 to L2 in non-cognates",
                            "b_typethree_way_cognate:groupL2_group"="L3 to L2 in three-way cognates",
                            "b_typepseudoword"="Two-way cognates to pseudowords L3 group",
                            "b_typepseudoword:groupL2_group"="L3 to L2 in pseudowords",
    "b_Intercept"="Baseline: Two-way cognates L3 group"))
  

ggsave()

summary(model) # There is an effect for two-way cognates, but not 3 

## Check random effects to see if any of the stimuli are causing issues (have high leverage)
word_cats = read.csv(here("data", "stimuli", "word_list.csv"))

res = (ranef(model)$word) %>% 
  rownames_to_column("word") %>% 
  left_join(word_cats, by = "word")


res_fe = (ranef(model)$participant) %>% 
  rownames_to_column("participant")


groups = inc_data %>% 
  select(participant, group) %>% 
  unique()

res_fe %>% 
  left_join(groups, by = "participant") %>% 
  ggplot(aes(x = typenon_cognate, y = participant)) + geom_point() + facet_wrap(~group)




res %>% 
  filter(type == "three_way_cognate") %>% 
    ggplot(aes(x = `(Intercept)`, y = word, label = word)) + geom_text() + 
  theme_minimal() + xlab("Random Effect in log rt") + ylab("Three-way cognates") 

res %>% 
  filter(type == "two_way_cognate") %>% 
  ggplot(aes(x = `(Intercept)`, y = word, label = word)) + geom_text() +
  theme_minimal() + xlab("Random Effect in log rt") + ylab("Two-way cognates")


res %>% 
  filter(type == "non_cognate") %>% 
  ggplot(aes(x = `(Intercept)`, y = word, label = word)) + geom_text() +
  theme_minimal() + xlab("Random Effect in log rt") + ylab("non cognates")




eff_df = p_data %>% 
  group_by(participant, type) %>% 
  summarize(mean_rt_ppt = mean(key_resp_lextale_trial.rt)) %>% 
  pivot_wider(names_from = type, values_from = mean_rt_ppt) %>% 
  mutate(eff_two = two_way_cognate - non_cognate,
         eff_three = three_way_cognate - non_cognate)

no_correct = p_data %>% 
  group_by(participant) %>%
  summarize(n = n())

eff_df %>% 
  left_join(no_correct, by = "participant") %>% 
  pivot_longer(cols = c(eff_two, eff_three), names_to = "eff", values_to = "size") %>% 
  ggplot(aes(x = n, y = size, color = eff)) + geom_point() + geom_smooth(method = "lm") +
  theme_classic() + xlab("Number of total correct answers") + ylab("Effect compared to non-cognates") 

prof_mod_df = 
  eff_df %>% 
  left_join(no_correct, by = "ppt") %>% 
  pivot_longer(cols = c(eff_two, eff_three), names_to = "eff", values_to = "size") 

model_prof = lm(size ~ n, data = prof_mod_df)


summary(model_prof) # There is an effect for two-way cognates, but not 3 


eff_df %>% 
  left_join(no_correct, by = "ppt") %>% 
  ggplot(aes(x = n, y = eff_three)) + geom_point() + geom_smooth(method = "lm")


d = inc_data %>% 
  group_by(participant, type, group) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt)) %>% 
  filter(type != "pseudoword") %>% 
  pivot_wider(names_from = type, values_from = mean_rt) %>% 
  mutate(non_cognate_effect = non_cognate - two_way_cognate) %>% 
  mutate(triple_cognate_effect = three_way_cognate - two_way_cognate)
  ggplot(aes(y = mean_rt, x = type, fill = type)) + geom_col() + facet_wrap(~participant)
  
  
  thisppt = inc_data %>% 
    filter(participant == ppt_list[25])
  
  thisppt %>% 
    group_by(type) %>% 
    summarize(n = n())
  
  


ppt_container = list()

ppt_list = unique(inc_data$participant)

for (i in 1:length(ppt_list)) {
 
   thisppt = inc_data %>% 
    filter(participant == ppt_list[i])
  
  thismodel = glm(log_rt ~ type, data = thisppt)
  
  d = summary(thismodel)[["coefficients"]]
  
  non_cognate_sig = ifelse(d[2,4] < .05, 1, 0)
  three_way_cognate_sig = ifelse(d[4,4] < .05, 1, 0)
  
  non_cognate_is_slower = ifelse(d[2,1] > 0, 1, 0)
  three_way_cognate_faster = ifelse(d[4,1] < 0, 1, 0)
  
  ppt_container[[i]] = data.frame(ppt_list[i], non_cognate_sig, three_way_cognate_sig, non_cognate_is_slower, three_way_cognate_faster)
    
}

ind_models_df = do.call(rbind, ppt_container)


