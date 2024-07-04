### Make df to compare groups in their effects? 

#### Four factors: 
##### L2 grp L2 cog

l2_df = read.csv(here("data", "tidy", "tidy_ldt_l2.csv")) %>% 
  mutate(group = "L2 Spanish")

l3_df = read.csv(here("data", "tidy", "tidy_ldt.csv")) %>% 
  mutate(group = "L3 Spanish")

comb_df = rbind(l2_df, l3_df)

combined_mod = brms::brm(log_rt ~ type*group + (1 | word) + (type | ppt), data = comb_df)


mod_f = lme4::lmer(log_rt ~ type + (1 | word) + (type | ppt), data = l2_df)


mod_f = lme4::lmer(log_rt ~ type*group + (1 | word) + (type | ppt), data = comb_df)
summary(mod_f)


comb_df$key_resp_lextale_trial.rt
w = comb_df %>% 
  group_by(group, type) %>% 
  summarise(mean_rt = mean(key_resp_lextale_trial.rt), sd_rt = sd(key_resp_lextale_trial.rt))


d = brms::conditional_effects(combined_mod)
