
source(here::here("scripts", "00_libraries.R"))
source(here::here("scripts", "02_load_data.R"))


#### Accuracy

totals_n = p_data %>% # create df for overall accuracy
  group_by(type, is_correct) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = is_correct, values_from = n) %>% 
  rename("correct" = "1") %>% 
  rename("incorrect" = "0") %>% 
  mutate(total = correct + incorrect) %>% 
  mutate(pct_correct = correct/total) %>% 
  select(type, pct_correct)

ind_data = p_data %>% # create df for individual accuracy
  group_by(participant, type, is_correct) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = is_correct, values_from = n) %>% 
  rename("correct" = "1") %>% 
  rename("incorrect" = "0")


ind_data[is.na(ind_data)] <- 0 # replace NAs with 0s 

neat_ind_data = ind_data %>% # plotting df 
  mutate(total = correct + incorrect) %>% 
  mutate(pct_correct = (correct/total)*100) %>% 
  select(participant, type, pct_correct) %>% 
  pivot_wider(names_from = type, values_from = pct_correct)


# Figure 1
totals_n %>% 
  filter(type != "pseudoword") %>%
  mutate(pct_correct = 100*pct_correct) %>% 
  mutate(type = factor(
    type,
    levels = c("non_cognate", "two_way_cognate", "three_way_cognate"),
    labels = c("Non-cognate", "Two-way cognate", "Three-way cognate")
  )) %>% 
  ggplot(aes(x = type, y = pct_correct)) + theme_minimal() + geom_col(color = "black", fill = "seagreen4") + ylim(0,100) + custom_theme() + xlab("") +
  ylab("Overall percentage of correct trials")


ggsave(here("docs", "plots", "overall_acc.png"), dpi = 600)


# Figure 2
neat_ind_data %>% 
  pivot_longer(cols = `non_cognate`:`two_way_cognate`, names_to = "type", values_to = "accuracy") %>% 
  ggplot(aes(x = accuracy)) + theme_minimal() + geom_histogram(binwidth = 10, color = "black", fill = "skyblue2") + 
  xlab("Accuracy") + facet_wrap(~type) + custom_theme()

ggsave(here("docs", "plots", "ind_acc.png"), dpi = 600)


# Accuracy Model 
p_data$type = as.factor(p_data$type)
p_data$type = relevel(p_data$type, ref = "two_way_cognate")

accuracy_mod_b = brms::brm(is_correct ~ type + frequency_z + proficiency_z + (type | participant) + (1 | word), family =
                             bernoulli(),
                           iter = 4000, data = p_data, file = here("data", "models", "accuracy_mod_z.rds"))


# Figure 3
posterior <- as.matrix(accuracy_mod_b)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(accuracy_mod_b, 
           pars = c("b_typethree_way_cognate", "b_typenon_cognate", "Intercept", "b_frequency_z", "b_proficiency_z"), 
           prob = 0.8) + plot_title

describe_posterior(accuracy_mod_b)

ggsave(here("docs", "plots", "acc_mod.png"), dpi = 600)

# RT Model 


l3_model_rt = brms::brm(log_rt ~ type + frequency_z + proficiency_z + (type | participant) + (1 | word),
                           iter = 4000, data = rt_trials, file = here("data", "models", "rt_mod_z.rds"))


mcmc_areas(l3_model_rt, 
           pars = c("b_typethree_way_cognate", "b_typenon_cognate", "Intercept", "b_frequency_z", "b_proficiency_z"), 
           prob = 0.8) + ggtitle("Posterior distributions",
                                 "with medians and 80% intervals")

describe_posterior(l3_model_rt, rope_range = c(-0.04, 0.04))


conditional_effects(l3_model_rt)



