# load libraries ---------------------------

library("tidyverse")
library("papaja")
library("knitr")
library("here")
library("fs")
library("purrr")
library("dplyr")
library("tidyr")
library("forcats")
library("readr")
library("readxl")
library("writexl")
library("glue")
library("ggplot2")
#library("ganttrify")
#library("contributoR")
library("magick")
library("webshot")
library("kableExtra")
library("hrbrthemes")
library("gridExtra")
library("sjPlot")
library("sjlabelled")
library("sjmisc")
library("lme4")
library("lmerTest")
library("brms")
library("emmeans")
library("tidyr")
library("dplyr")
library("stringr")
library("glue")
library("purrr")
library("forcats")
library("fs")
library("ggplot2")
library("bayesplot")
library("tidybayes")
library("ggbeeswarm")
library("patchwork")
library("ggdag")
library("readr")
library("here")
library("broom")
library("brms")
library("future")
library("bayestestR")
library("stringr")
library("flextable")
#library("cmdstanr")


## Functions 

### rounding function

round2 <- function(x) sprintf("%.2f", x)

### proficiency scoring function

p_lexTALE_score <- function(words_correct, nonwords_correct) {
  ((words_correct / 48 * 100) +
     (nonwords_correct / 96 * 100)) / 2
}

### plotting function

custom_theme <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "black", size = 10),
    axis.title = element_text(colour = "black", size = 10),
    strip.text.x = element_text(size = 16),
    strip.text.y = element_text(size = 16),
    strip.background = element_rect(fill = "white", color = "black"),
    legend.position = "bottom",
    legend.title = element_text(colour = "black", size = 12),
    legend.text = element_text(colour = "black", size = 10),
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.4
    ),
    panel.grid.minor = element_line(
      colour = "grey92",
      linewidth = 0.2),
    plot.title = element_text(colour = "black", size = 12, hjust = 0.5)  # Center align the title
  )
}





# ------------------------------------------