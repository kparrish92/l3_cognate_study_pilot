p_data = read.csv(here("data", "tidy", "tidy_data.csv")) %>% 
  filter(!is.na(participant))

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

ind_data = p_data %>% # create df for indivudual accuracy
  group_by(participant, type, is_correct) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = is_correct, values_from = n) %>% 
  rename("correct" = "1") %>% 
  rename("incorrect" = "0")


ind_data[is.na(ind_data)] <- 0

neat_ind_data = ind_data %>% 
  mutate(total = correct + incorrect) %>% 
  mutate(pct_correct = (correct/total)*100) %>% 
  select(participant, type, pct_correct) %>% 
  pivot_wider(names_from = type, values_from = pct_correct)


neat_ind_data %>% 
  filter(non_cognate > two_way_cognate & two_way_cognate > three_way_cognate)


rt_trials = p_data %>% 
  filter(key_resp_lextale_trial.rt < 2) %>% 
  filter(key_resp_lextale_trial.rt > .5)

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

neat_ind_data %>% 
  pivot_longer(cols = `non_cognate`:`two_way_cognate`, names_to = "type", values_to = "accuracy") %>% 
  ggplot(aes(x = accuracy)) + theme_minimal() + geom_histogram(binwidth = 10, color = "black", fill = "skyblue2") + 
  xlab("Accuracy") + facet_wrap(~type) + custom_theme()

ggsave(here("docs", "plots", "ind_acc.png"), dpi = 600)

accuracy_mod_b = brms::brm(is_correct ~ type + (type | participant) + (1 | word), family =
                             bernoulli(),
                           iter = 4000, data = p_data, file = here("data", "models", "accuracy_mod.rds"))

posterior <- as.matrix(accuracy_mod_b)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(accuracy_mod_b, 
           pars = c("b_typepseudoword", "b_typethree_way_cognate", "b_typetwo_way_cognate", "Intercept"), 
           prob = 0.8) + plot_title

ggsave(here("docs", "plots", "acc_mod.png"), dpi = 600)

