library(tidyverse)
library(here)
grps = c("Kyra", "Giulia", "Robo")


# Step 1: Read in the data
tidy_data <- fs::dir_ls(here("data", "raw"),
                        regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  mutate(source = str_remove(source, "/Users/kyleparrish/Documents/GitHub/l3_cognate_study_official/data/raw/")) %>% 
  separate(source, into = c("ppt", "t"), sep = "_") %>% 
  filter(!ppt %in% grps) %>% 
  select(ppt, word, type, correct_reponse, key_resp_lextale_trial.keys, key_resp_lextale_trial.corr, key_resp_lextale_trial.rt) %>% 
  filter(!is.na(key_resp_lextale_trial.keys)) %>% 
  mutate(is_correct =
           ifelse(
             correct_reponse == 1 & key_resp_lextale_trial.keys == 1 | 
               correct_reponse == 0 & key_resp_lextale_trial.keys == 0, 1, 0)) %>% 
  mutate(ppt = case_when(
    ppt == "Christina" ~ "sl3_01",
    ppt == "Greblicki" ~ "sl3_02",
    ppt == "Hagen" ~ "sl3_03",
    ppt == "Marco" ~ "sl3_04",
    ppt == "Mika" ~ "sl3_05",
    ppt == "Pascal" ~ "sl3_06",
    ppt == "anisa" ~ "sl3_07",
    ppt == "anna" ~ "sl3_08",
    ppt == "mattea" ~ "sl3_09",
    ppt == "mp2" ~ "sl3_10"
  ))

tidy_data$key_resp_lextale_trial.rt = as.numeric(tidy_data$key_resp_lextale_trial.rt)
tidy_data$log_rt = log(tidy_data$key_resp_lextale_trial.rt)


tidy_data %>% 
  group_by(ppt) %>% 
  summarize(n=n())


# Overall accuracy 

sum(tidy_data$is_correct)/nrow(tidy_data)

# Mean RT 

mean(tidy_data$key_resp_lextale_trial.rt)

sd_df = tidy_df_filtered = tidy_data %>%
  filter(key_resp_lextale_trial.rt < 3)

sd(sd_df$key_resp_lextale_trial.rt)

# Accuracy rate per word 
totals = tidy_data %>% 
  group_by(word) %>% 
  summarize(total = n())

correct = tidy_data %>% 
  group_by(word) %>% 
  summarize(correct = sum(is_correct)) %>% 
  left_join(totals, by = "word") %>% 
  mutate(correct_pct = correct/total)

# remove words less than 50% correct 

remove = correct %>%
  filter(correct_pct < .25)


tidy_df_filtered = tidy_data %>% 
  filter(key_resp_lextale_trial.rt < 3)  %>%  # no trials longer than 2s or less than 300ms
  filter(is_correct == 1) # filter for only correct answers %>% 

removal_df = tidy_df_filtered %>% # a dataframe that takes the mean of each ppt +/- 2 sds 
  group_by(ppt) %>% 
  summarise(mean_rt = mean(key_resp_lextale_trial.rt),
            sd_rt = sd(key_resp_lextale_trial.rt)) %>% 
  mutate(remove_upper = mean_rt + 2*sd_rt) %>% 
  mutate(remove_lower = mean_rt - 2*sd_rt)

ppt_list = list()

for (i in 1:nrow(removal_df)) {
this_df = tidy_df_filtered %>% 
  filter(ppt == removal_df$ppt[i]) %>% 
  filter(key_resp_lextale_trial.rt > removal_df$remove_lower[i] & key_resp_lextale_trial.rt < removal_df$remove_upper[i])
ppt_list[[i]] = this_df  
}
df_after_outliers = do.call(rbind, ppt_list)

removed_low_corr_rate = nrow(df_after_outliers) - nrow(df_after_outliers %>% 
       filter(!word %in% remove$word))

df_after_outliers %>% 
  filter(!word %in% remove$word) %>% 
  write.csv(here("data", "tidy", "tidy_ldt.csv"))


