library(lmerTest)
library(tidyverse)
library(here)

goodale_theme <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "black", family = "Times New Roman", size = 14),
    axis.title = element_text(colour = "black", family = "Times New Roman", size = 14),
    legend.position = "bottom",
    legend.title = element_text(colour = "black", family = "Times New Roman", size = 16),
    legend.text = element_text(colour = "black", family = "Times New Roman", size = 14),
    plot.title = element_text(colour = "black", family = "Times New Roman", size = 14, hjust = 0.5)  # Center align the title
  )
}


p_data = read.csv(here("data", "tidy", "pilot_data.csv")) %>% 
  filter(is_correct == 1) %>% 
  filter(!is.na(participant))

inc_data = read.csv(here("data", "tidy", "pilot_data.csv")) %>% 
  filter(!is.na(participant))


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
  scale_fill_manual(values = c("#f0ad4e", "#5cb85c", "#d9534f")) + 
  xlab("Word Type") + 
  ylab("Reaction Time (ms)") +
  theme(legend.position = "none") + 
  ggtitle("Reaction times by word type") +
  goodale_theme() + theme(legend.position="none")

ggsave(here("plots", "overall_cognates.png"), dpi = 600)


## Run a model to test this.

simple_cognate_test$type = as.factor(simple_cognate_test$type)
simple_cognate_test$type = relevel(simple_cognate_test$type, ref = "cognate")

simple_cognate_test <- p_data %>% 
  mutate(type = case_when(
    type == "three_way_cognate" ~ "cognate",
    type == "two_way_cognate" ~ "cognate",
    type == "pseudoword" ~ "pseudoword",
    type == "non_cognate" ~ "non_cognate"))


simple_model = brms::brm(log_rt ~ type + (1 | word) + (type | participant), iter = 2000, 
                    data = simple_cognate_test, file = here("data", "models", "simple_mod.rds"))

summ_simple = bayestestR::describe_posterior(simple_model) %>% 
  as.data.frame() %>% 
  janitor::clean_names() 



summary_data %>%
  ggplot(aes(y = type, x = mean_rt, color = type)) + 
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


p_data %>% 
 # filter(participant %in% el) %>% 
  group_by(type, participant) %>% 
  summarize(mean_rt = mean(key_resp_lextale_trial.rt),
            sd_rt = sd(key_resp_lextale_trial.rt)) %>% 
  ggplot(aes(x = type, y = mean_rt, fill = type)) + 
  geom_col(position = "dodge", color = "black") + theme_classic() + xlab("Word Type") + ylab("Reaction Time (ms)") +
  theme(legend.position = "none") + 
  facet_wrap(~participant)


p_data %>% 
  ggplot(aes(x = type, y = key_resp_lextale_trial.rt, fill = type)) + 
  geom_boxplot() + theme_classic() + xlab("Word Type") + ylab("Reaction Time (ms)") +
  theme(legend.position = "none") + facet_wrap(~group)

p_data %>% 
  ggplot(aes(x = type, y = key_resp_lextale_trial.rt, fill = type)) + geom_boxplot() + facet_wrap(~participant)


p_data %>% 
  ggplot(aes(x = log_rt, fill = type)) + 
  geom_density(alpha = 0.5) + 
  labs(x = "Response Time (s)", y = "Density", fill = "Type") +
  theme_minimal()


totals = inc_data %>% 
  group_by(type, group) %>% 
  summarise(total_n = n())

# Proficiency as a function of reaction time 
# Proficiency as a function of correctess 

inc_data %>% 
  group_by(type, group, is_correct) %>% 
  summarise(n = n()) %>% 
  filter(is_correct == 1) %>% 
  left_join(totals) %>% 
  mutate(pct_correct = n/total_n) %>% 
  ggplot(aes(x = type, y = pct_correct, fill = group)) + 
  geom_col(position = "dodge", color = "black") + theme_minimal()

## Model 
library(lmerTest)


p_data$type = as.factor(p_data$type)
p_data$type = relevel(p_data$type, ref = "two_way_cognate")
p_data$group = as.factor(p_data$group)
p_data$group = relevel(p_data$group, ref = "L3_group")

p_data %>% group_by(group, type) %>% summarize(n = n())

p_data %>% 
  ggplot(aes(x = log_rt)) + geom_histogram() + facet_wrap(group~type, ncol = 2)


model = lmerTest::lmer(log_rt ~ group*type + (1 | word) + (type | participant), data = p_data)

summary(model)

b_model = brms::brm(log_rt ~ type*group + (1 | word) + (type | participant), iter = 4000, 
                    data = p_data, file = here("data", "models", "l3_models.rds"))

summary(b_model)
summ = bayestestR::describe_posterior(b_model) %>% 
  as.data.frame() %>% 
  janitor::clean_names() 

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



