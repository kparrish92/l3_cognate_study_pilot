
l2_scores = read.csv(here("data", "l2_lex", "cognate_eng_lextale.csv"))
l3_scores = read.csv(here("data", "tidy", "l3_span.csv"))

summary_rts = read.csv(here("data", "tidy", "pilot_data.csv")) %>% 
  filter(is_correct == 1) %>% 
  filter(!is.na(participant)) %>%
  select(participant, type, log_rt, group) %>% 
  group_by(type, participant, group) %>% 
  summarize(mean_rt = mean(log_rt)) %>% 
  pivot_wider(names_from = type, values_from = mean_rt) %>% 
  rename(id = "participant") %>% 
  left_join(l2_scores) %>% 
  left_join(l3_scores) %>% 
  mutate(l3_effect = non_cognate - three_way_cognate) %>% 
  mutate(l2_effect = non_cognate - two_way_cognate)
  

summary_rts %>% 
  filter(!is.na(lextale_avg)) %>%
  ggplot(aes(x = l3_effect, y = lextale_avg)) + geom_point(alpha = .6) + 
  geom_smooth(method = "lm", color = "seagreen3") + theme(legend.position="none") +
  goodale_theme() + theme(legend.position="none") + ylab("Spanish LexTALE scores") +
  xlab("Advantage") + ggtitle("Spanish Proficiency and cognate faciliation")

ggsave(here("plots", "prof_cognates.png"), dpi = 600)

summary_rts %>% 
  filter(!is.na(lextale_avg)) %>%
  ggplot(aes(x = l2_effect, y = lextale_avg)) + geom_point() + 
  geom_smooth(method = "lm") 

model_df_l3 = summary_rts %>% 
  filter(!is.na(lextale_avg)) %>% 
  select(id, l3_effect, lextale_avg) 

model = brms::brm(l3_effect ~ lextale_avg, data = model_df_l3)

summary(model)  

summ_p = bayestestR::describe_posterior(model) %>% 
  as.data.frame() %>% 
  janitor::clean_names() 


### Check phonological overlap 