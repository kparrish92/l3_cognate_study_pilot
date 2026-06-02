source(here::here("scripts", "02_load_data.R"))

cohens_d <- function(mean1, sd1, n1,
                     mean2, sd2, n2,
                     hedges_correction = FALSE) {
  
  # pooled SD
  pooled_sd <- sqrt(
    (((n1 - 1) * sd1^2) + ((n2 - 1) * sd2^2)) /
      (n1 + n2 - 2)
  )
  
  # Cohen's d
  d <- (mean1 - mean2) / pooled_sd
  
  # Optional Hedges' g correction for small samples
  if (hedges_correction) {
    J <- 1 - (3 / (4 * (n1 + n2) - 9))
    d <- d * J
  }
  
  return(d)
}



generate_ind_estimates_rt = function(ppt)
  
{
ind_df = rt_trials %>% 
  filter(participant == ppt) %>% 
  filter(type != "pseudoword")


mod = lm(log_rt ~ type + frequency, data = ind_df)

res = summary(mod)


# For H0: beta >= 0    
#     H1: beta < 0
three_way_faster = ifelse(pt(coef(res)[, 3], mod$df, lower = TRUE)[3] < .05, 1, 0)

# For H0: beta <= 0
#     H1: beta > 0
non_cog_slower = ifelse(pt(coef(res)[, 3], mod$df, lower = FALSE)[2] < .05, 1, 0)

lf_eff = ifelse(pt(coef(res)[, 3], mod$df, lower = TRUE)[4] < .05, 1, 0)


effs_d = ind_df %>% group_by(type) %>% summarise(mean_rt = mean(key_resp_lextale_trial.rt),
                                                 sd_rt = sd(key_resp_lextale_trial.rt), 
                                                 n = n())

three_way_effect = cohens_d(effs_d$mean_rt[3], effs_d$sd_rt[3], effs_d$n[3],
                            effs_d$mean_rt[1], effs_d$sd_rt[1], effs_d$n[1])

non_cog_effect = cohens_d(effs_d$mean_rt[2], effs_d$sd_rt[2], effs_d$n[2],
                            effs_d$mean_rt[1], effs_d$sd_rt[1], effs_d$n[1])

ind_report = ind_df %>% group_by(type) %>% summarise(mean_rt = mean(key_resp_lextale_trial.rt)) %>% 
  pivot_wider(names_from = type, values_from = mean_rt) %>% mutate(participant = ppt) %>% 
  mutate(three_way_faster) %>% mutate(non_cog_slower) %>% mutate(three_way_effect) %>% 
  mutate(non_cog_effect) %>% 
  mutate(lf_eff) %>% 
  mutate(lf_direction = coef(mod)[4],
         total_data_points = nrow(ind_df))


return(ind_report)
}

ppt_list = unique(rt_trials$participant)
output_list_rt = list()

for (i in 1:length(ppt_list)) {
output_list_rt[[i]] = generate_ind_estimates_rt(ppt_list[i])
}

output_df_rt = do.call(rbind, output_list_rt) %>% 
  mutate(ppt_type = case_when(
    non_cognate > two_way_cognate & two_way_cognate > three_way_cognate ~ "cumulative",
    non_cognate > two_way_cognate & non_cognate > three_way_cognate & three_way_cognate > two_way_cognate ~ "general",
    three_way_cognate > non_cognate & two_way_cognate > non_cognate & three_way_cognate > two_way_cognate ~ "slow-down",
    three_way_cognate > non_cognate & non_cognate > two_way_cognate ~ "slow-three",
    two_way_cognate > non_cognate & non_cognate > three_way_cognate ~ "slow-two",
    ))




slow2df = output_df_rt %>% 
  filter(ppt_type == "slow-two")

words = rt_trials %>% filter(participant %in% slow2df$participant) %>% 
  group_by(word,type) %>% 
  summarise(mean_rt = mean(key_resp_lextale_trial.rt))







