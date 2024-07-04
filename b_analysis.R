# Bayesian Analysis 

library(brms)
library(rstan)
library(ggplot2)
library(dplyr)


priors <- c(
  set_prior("normal(-30, 35)", class = "b", coef = "typetwo_way_cognate"),
  set_prior("normal(-15, 35)", class = "b", coef = "typethree_way_cognate"),
#  set_prior("normal(0, 5)", class = "b"),  # Weakly informative prior for other coefficients
  #  set_prior("normal(60, 10)", class = "Intercept"),  # Prior for the intercept
  set_prior("student_t(3, 0, 10)", class = "sigma")  # Prior for the residual standard deviation
)


unique(p_data$type)
model_b_prior = brms::brm(key_resp_lextale_trial.rt ~ type + (1 | word) + (type | ppt), prior = priors, data = p_data)

summary(model_b_prior)

bayesplot::mcmc_areas(model_b_prior,
                      pars = c("b_typepseudoword", "b_typethree_way_cognate", "b_typetwo_way_cognate"),
                      prob = 0.95) 