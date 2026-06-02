source(here::here("scripts", "00_libraries.R"))


ppts_remove = c("5bbbdca68f3bd70001e713ef",
                "6789000b08f2703ba0b959b1",
                "63b87d2c9edd47ad702413a1",
                "5ed2edb5b7f800107d47c777",
                "6046ac1161b60d0fc94b0000",
                "6789000b08f2703ba0b959b1",
                "603a80b01bb75f608c3dbb0d",
                "627bfd52e3e0bfd01cd0cad5")

words_remove = c("pelvis",
                 "listo",
                 "cuenco")

p_data = read.csv(here("data", "tidy", "tidy_data.csv")) %>% 
  filter(!is.na(participant)) %>% 
  filter(!participant %in% ppts_remove) %>% 
  filter(!word %in% words_remove) %>% 
  mutate(frequency_z = scale(frequency)[,1]) %>% 
  mutate(proficiency_z = scale(ps_lextale_score)[,1])


rt_trials = p_data %>% 
  filter(key_resp_lextale_trial.rt < 2) %>% 
  filter(key_resp_lextale_trial.rt > .5) %>%
  filter(is_correct == 1) %>%
  filter(!participant %in% ppts_remove) %>% 
  filter(!word %in% words_remove) %>% 
  mutate(frequency_z = scale(frequency)[,1]) %>% 
  mutate(proficiency_z = scale(ps_lextale_score)[,1])


rt_trials$type = as.factor(rt_trials$type)
rt_trials$type = relevel(rt_trials$type, ref = "two_way_cognate")

