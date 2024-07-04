p_data = read.csv(here("data", "tidy", "tidy_ldt.csv"))

d_eff_df

d_eff_df = p_data %>% 
  group_by(type, ppt) %>% 
  summarise(mean_rt = mean(key_resp_lextale_trial.rt)) %>% 
  filter(type == "non_cognate" | type == "three_way_cognate") %>% 
  pivot_wider(names_from = type, values_from = mean_rt) %>% 
  mutate(desc_eff = three_way_cognate - non_cognate) %>% 
  mutate(eff_direction = ifelse(desc_eff > 0, "positive", "negative"))
  
d_plot = p_data %>% 
  left_join(d_eff_df, by = "ppt") %>% 
  filter(type == "non_cognate" | type == "three_way_cognate") %>% 
  ggplot(aes(x = type, y = key_resp_lextale_trial.rt, color = eff_direction)) + geom_boxplot() +
  facet_wrap(~ppt)

mean(d_eff_df$non_cognate)
sd(d_eff_df$non_cognate)

mean(d_eff_df$sd_rt)


d_eff_df %>% 
  filter(desc_eff < 0) %>% 
  summarise(m = mean(desc_eff)*1000, sd = sd(desc_eff)*1000)


# Plot descriptives for direction.
# Show LMs: no pooling and partial pooling  
# Simulate as if the descriptives are true and determine which model makes more sense. 


### What is the probability that there was a sign error if three participants show a true effect of 0,
### and the rest show an average effect of d = .7

model = lmerTest::lmer(log_rt ~ type + (1 | word) + (type | ppt), data = p_data)

summary(model)
model_b = brms::brm(log_rt ~ type + (1 | word) + (type | ppt), data = p_data)

summary(model_b)

p_data$key_resp_lextale_trial.rt = p_data$key_resp_lextale_trial.rt*1000

re_df <- lmList(key_resp_lextale_trial.rt ~ type | ppt, p_data) %>% 
  coef() %>% 
  # Subject IDs are stored as row-names. Make them an explicit column
  rownames_to_column("ppt") %>% 
  rename(Intercept = `(Intercept)`) %>% 
  mutate(slope_direction = ifelse(typethree_way_cognate > 0, "positive", "negative"))

# Join the raw data so we can use plot the points and the lines.
df_models <- re_df %>% 
  left_join(p_data, by = "ppt")

np_plot = df_models %>% 
  filter(type == "non_cognate" | type == "three_way_cognate") %>% 
  ggplot(aes(x = type, y = key_resp_lextale_trial.rt)) + 
  geom_boxplot(alpha = .02) +
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(
    aes(intercept = Intercept, slope = typethree_way_cognate, color = slope_direction),
    size = .75) + 
  facet_wrap("ppt") 


model = lmerTest::lmer(log_rt ~ type + (1 | word) + (type | ppt), data = p_data)


exp(.039)



df_partial_pooling <- coef(model)[["ppt"]] %>% 
  rownames_to_column("ppt") %>% 
  as_tibble() %>% 
  rename(Intercept = `(Intercept)`) %>% 
  mutate(slope_direction = ifelse(typethree_way_cognate > 0, "positive", "negative")) %>% 
  left_join(p_data, by = "ppt")


pp_plot = df_partial_pooling %>% 
  filter(type == "non_cognate" | type == "three_way_cognate") %>% 
  ggplot(aes(x = type, y = key_resp_lextale_trial.rt)) + 
  geom_boxplot(alpha = .02) +
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(
    aes(intercept = Intercept, slope = typethree_way_cognate, color = slope_direction),
    size = .75) + 
  facet_wrap("ppt") 

d_plot
np_plot
pp_plot


coef(model_b)[["ppt"]]

fixef(model_b)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

bayesplot::mcmc_areas(model_b,
           pars = c("b_typepseudoword", "b_typethree_way_cognate", "b_typetwo_way_cognate"),
           prob = 0.8) + plot_title



