library(tidyverse)
library(fs)
library(here)

# ((n_corr_real / n_real_words * 100) + (n_corr_nonse / n_nonse_words * 100)) / 2
score_lextale <- function(
    n_real = NULL, 
    n_nonse = NULL, 
    n_real_correct = NULL, 
    n_nonse_correct = NULL, 
    n_nonse_incorrect = NULL) {
  
  if (is.null(n_nonse_incorrect)) {
    avg_real <-  (n_real_correct / n_real * 100)
    avg_nonse <- (n_nonse_correct / n_nonse * 100)
    val <- (avg_real + avg_nonse) / 2
  } else {
    val <- n_real_correct - (2 * n_nonse_incorrect)
  }
  return(val)
}


lextale_sp_l3 = dir_ls(
  path = here("data", "l3_lextale_sp"), 
  regexp = "\\.csv$"
) %>% 
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp_lextale_trial.keys)) %>% 
  mutate(
    response = `key_resp_lextale_trial.keys`, 
    is_correct = key_resp_lextale_trial.corr, 
    is_incorrect = if_else(is_correct == 0, 1, 0), 
    is_real = case_when(
      response == 1 & is_correct == 1 ~ "real", 
      response == 1 & is_correct == 0 ~ "nonse", 
      response == 0 & is_correct == 1 ~ "nonse", 
      response == 0 & is_correct == 0 ~ "real"),  
    real_correct    = if_else(is_real == "real"  & is_correct == 1, 1, 0), 
    real_incorrect  = if_else(is_real == "real"  & is_correct == 0, 1, 0), 
    nonse_correct   = if_else(is_real == "nonse" & is_correct == 1, 1, 0), 
    nonse_incorrect = if_else(is_real == "nonse" & is_correct == 0, 1, 0)
  ) %>% 
  group_by(id) %>% 
  summarize(totals = n(), 
            real_correct = sum(real_correct), 
            real_incorrect = sum(real_incorrect), 
            nonse_correct = sum(nonse_correct), 
            nonse_incorrect = sum(nonse_incorrect), .groups = "drop") %>% 
  mutate(
    n_real = real_correct + real_incorrect, 
    n_nonse = nonse_correct + nonse_incorrect, 
    n = n_real + n_nonse, 
    lextale_avg = score_lextale(
      n_real = n_real, 
      n_nonse = n_nonse,
      n_real_correct = real_correct, 
      n_nonse_correct = nonse_correct), 
    lextale_tra = score_lextale(
      n_real_correct = real_correct,
      n_nonse_incorrect = nonse_incorrect
    )) %>% 
  select(id, lextale_avg)

lextale_sp_l3 %>% write.csv(here("data", "tidy", "l3_span.csv"))
