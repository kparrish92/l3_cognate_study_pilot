
source(here::here("scripts", "00_libraries.R"))
source(here::here("scripts", "02_load_data.R"))


# gender demographics 
gender_pct = survey_data %>% 
  group_by(gender) %>% 
  summarise(n = n())

pct_female = round(gender_pct$n[1]/sum(gender_pct$n)*100)
pct_male = round(gender_pct$n[2]/sum(gender_pct$n)*100)

# age range
lowage = round(range(survey_data$age), digits = 1)[1]
oldage = round(range(survey_data$age), digits = 1)[2]

# English age of onset + acquisition
eng_aoo = round(mean(survey_data$english_aoo), digits = 1)
eng_aoo_sd = round(sd(survey_data$english_aoo), digits = 1)

eng_aoa = round(mean(survey_data$english_aoa), digits = 1)
eng_aoa_sd = round(sd(survey_data$english_aoa), digits = 1)

# Spanish age of onset + acquisition
sp_aoo = round(mean(as.numeric(survey_data$spanish_aoo)), digits = 1)
sp_aoo_sd = round(sd(as.numeric(survey_data$spanish_aoo)), digits = 1)

sp_aoa = round(mean(c(15,18,20,22,24,26,30)), digits = 1)
sp_aoa_sd = round(sd(c(15,18,20,22,24,26,30)), digits = 1)

# Self-rated proficiency in German (L1)
de_prof_c = round(mean(as.numeric(survey_data$german_perception_level)), digits = 2)
de_prof_c_sd = round(sd(as.numeric(survey_data$german_perception_level)), digits = 2)

de_prof_s = round(mean(as.numeric(survey_data$german_speaking_level)), digits = 2)
de_prof_s_sd = round(sd(as.numeric(survey_data$german_speaking_level)), digits = 2)

# Self-rated proficiency in English (L2)
en_prof_c = round(mean(as.numeric(survey_data$english_comprehension)), digits = 2)
en_prof_c_sd = round(sd(as.numeric(survey_data$english_comprehension)), digits = 2)

en_prof_s = round(mean(as.numeric(survey_data$english_speaking_level)), digits = 2)
en_prof_s_sd = round(sd(as.numeric(survey_data$english_speaking_level)), digits = 2)

# Self-rated proficiency in Spanish (L3)
sp_prof_c = round(mean(as.numeric(survey_data$spanish_comprehension_level)), digits = 2)
sp_prof_c_sd = round(sd(as.numeric(survey_data$spanish_comprehension_level)), digits = 2)

sp_prof_s = round(mean(as.numeric(survey_data$spanish_speaking_level)), digits = 2)
sp_prof_s_sd = round(sd(as.numeric(survey_data$spanish_speaking_level)), digits = 2)


# Range of the adapted Lextale scores 
prof_mean = round(mean(rt_trials$ps_lextale_score), digits = 1)
prof_sd = round(sd(rt_trials$ps_lextale_score), digits = 1)
prof_lo = round(range(rt_trials$ps_lextale_score), digits = 1)[1]
prof_hi = round(range(rt_trials$ps_lextale_score), digits = 1)[2]


# Figure 1: Histogram of adapted lextale scores
rt_trials %>% 
  group_by(participant) %>% 
  summarise(mean_lextale = mean(ps_lextale_score)) %>% 
  ggplot(aes(x = mean_lextale)) + 
  geom_histogram(binwidth = 8, color = "black", fill = "orange3") + custom_theme() +
  xlab("Mean adapted LexTALE")

ggsave(here("docs", "plots", "apapted_lextale_hist.png"), dpi = 600)

# Figure 2: Overall accuracy by word type

totals_n = p_data %>% # create df for overall accuracy
  group_by(type, is_correct) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = is_correct, values_from = n) %>% 
  rename("correct" = "1") %>% 
  rename("incorrect" = "0") %>% 
  mutate(total = correct + incorrect) %>% 
  mutate(pct_correct = correct/total) %>% 
  select(type, pct_correct)

totals_n %>% 
  filter(type != "pseudoword") %>%
  mutate(pct_correct = 100*pct_correct) %>% 
  mutate(type = factor(
    type,
    levels = c("non_cognate", "two_way_cognate", "three_way_cognate"),
    labels = c("Non-cognate", "Two-way cognate", "Three-way cognate")
  )) %>% 
  ggplot(aes(x = type, y = pct_correct)) + geom_col(color = "black", fill = "seagreen4") + ylim(0,100) + custom_theme() + xlab("") +
  ylab("Overall percentage of correct trials")


ggsave(here("docs", "plots", "overall_acc.png"), dpi = 600)

# Figure 3: Individual accuracy by word type

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

neat_ind_data %>% 
  pivot_longer(cols = `non_cognate`:`two_way_cognate`, names_to = "type", values_to = "accuracy") %>% 
  ggplot(aes(x = accuracy)) + theme_minimal() + geom_histogram(binwidth = 10, color = "black", fill = "skyblue2") + 
  xlab("Accuracy") + facet_wrap(~type) + custom_theme()

ggsave(here("docs", "plots", "ind_acc.png"), dpi = 600)


# Fit accuracy model 
p_data$type = as.factor(p_data$type)
p_data$type = relevel(p_data$type, ref = "two_way_cognate") # change reference level to two-way cognates

accuracy_mod_b = brms::brm(is_correct ~ type + frequency_z + proficiency_z + (type | participant) + (1 | word), family =
                             bernoulli(),
                           iter = 4000, data = p_data, file = here("data", "models", "accuracy_mod_z.rds"))


# Figure 4: Posterior distribution of the accuracy model
posterior <- as.matrix(accuracy_mod_b)

mcmc_areas(accuracy_mod_b, 
           pars = c("b_typethree_way_cognate", "b_typenon_cognate", "Intercept", "b_frequency_z", "b_proficiency_z"), 
           prob = 0.8) + custom_theme()

describe_posterior(accuracy_mod_b)

ggsave(here("docs", "plots", "acc_mod.png"), dpi = 600)

# Fit reaction time model
l3_model_rt = brms::brm(log_rt ~ type + frequency_z + proficiency_z + (type | participant) + (1 | word),
                           iter = 4000, data = rt_trials, file = here("data", "models", "rt_mod_z.rds"))



rt_trials %>% 
  group_by(type) %>% 
  summarise(mean_rt = mean(key_resp_lextale_trial.rt), 
            sd_log_rt = sd(key_resp_lextale_trial.rt),
            n = n()) %>% 
  mutate(me = 1.96*sd_log_rt/sqrt(n)) %>% 
  mutate(type = factor(
    type,
    levels = c("non_cognate", "two_way_cognate", "three_way_cognate"),
    labels = c("Non-cognate", "Two-way cognate", "Three-way cognate")
  )) %>% 
  ggplot(aes(x = type, y = mean_rt, ymax = mean_rt + me, ymin = mean_rt - me)) +
  geom_col(color = "black", fill = "steelblue2") +
  geom_pointrange() + custom_theme() + ylim(0,1)

ggsave(here("docs", "plots", "over_rt.png"), dpi = 600)


# Calculate individual desc effects 
library(effsize)

ppts = unique(rt_trials$participant)
df_c = list()

for (i in 1:length(ppts)) {
this_df = rt_trials %>% 
  filter(participant == ppts[i]) %>% 
  select(participant, type, key_resp_lextale_trial.rt)
  
  ncdf = this_df %>% filter(type == "non_cognate")
  dobuledf = this_df %>% filter(type == "two_way_cognate")
  tripledf =  this_df %>% filter(type == "three_way_cognate")
  
  nc_to_dub = as.numeric(cohen.d(dobuledf$key_resp_lextale_trial.rt, ncdf$key_resp_lextale_trial.rt)[["estimate"]]) # going from nc to double  
  nc_to_trip = as.numeric(cohen.d(tripledf$key_resp_lextale_trial.rt, ncdf$key_resp_lextale_trial.rt)[["estimate"]]) # going from nc to triple
  cumulative_d = as.numeric(cohen.d(tripledf$key_resp_lextale_trial.rt, dobuledf$key_resp_lextale_trial.rt)[["estimate"]]) # going from double to triple
  
  df_c[[i]] = data.frame(nc_to_dub = nc_to_dub, nc_to_trip = nc_to_trip, cumulative_d = cumulative_d, ppt = ppts[i])
  
}

df_comb = do.call(rbind, df_c) %>% 
  pivot_longer(cols = c(1:3), names_to = "comparison", values_to = "d") %>% 
  mutate(is_pos = ifelse(d > 0, "ps","ng"))


df_comb %>% 
  ggplot(aes(x = d, fill = is_pos, group = is_pos)) + geom_histogram(binwidth = 0.1, breaks = seq(from = -1, to = 1, by = 0.1),
                                                                     color = "black", center = 0) +
  facet_wrap(
    ~comparison,
    ncol = 1,
    labeller = labeller(
      comparison = c(
        "cumulative_d" = "Double cognates to triple cognates",
        "nc_to_dub" = "Non-cognate to double cognates",
        "nc_to_trip" = "Non-cognate to triple cognates"
      )
    )
  ) +
  scale_fill_discrete(palette  = c("seagreen3", "purple4")) +
  scale_x_continuous(breaks = seq(from = -1, to = 1, by = 0.1)) +
  custom_theme()+ theme(legend.position = "none") + theme(axis.text.x = element_text(colour = "black", size = 8))

ggsave(here("docs", "plots", "ind_rt.png"), dpi = 600)

# Figure 6: Posterior distribution of the RT model
mcmc_areas(l3_model_rt, 
           pars = c("b_typethree_way_cognate", "b_typenon_cognate", "Intercept", "b_frequency_z", "b_proficiency_z"), 
           prob = 0.8) + custom_theme()

describe_posterior(l3_model_rt, rope_range = c(-0.0065, 0.0065)) # ~ +/- 5ms

describe_posterior(l3_model_rt, rope_range = c(-0.013, 0.013)) # ~ +/- 10ms

ggsave(here("docs", "plots", "rt_mod.png"), dpi = 600)





