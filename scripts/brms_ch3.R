#--------------------------------Chapter 3 analysis---------------------------
#-Author: A. Rose Benton---------------------------Created:  February, 7 2024-#
#-R Version: 4.3.1---------------------------------Revised:  February, 12 2024-#

#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

pacman::p_load(
  "tidyverse",
  "brms",
  "sf",
  "cmdstanr",
  "patchwork", # Combining multiple plots into one
  "ggdist", # Visualizations of Distributions and Uncertainty
  install = FALSE
)



# Set Session Options
options(digits = 6,
        # Significant figures output
        scipen = 999,
        # Disable scientific notation
        repos = getOption("repos")["CRAN"])


source("../../helpers/plot-themes.R")
source("../../helpers/brms-tidy-method.R")
source("../../helpers/brms-glance-method.R")

#----------------------------------data-----------------------------------------

fa_df <- read_csv("data/Chapter 3/final_df.csv")


mod_form <- bf(
  classlink ~ factor1_1*union + job_loss + respondent_class +  agecat + 
    male + bachelor + (1 + union | region),
  decomp = "QR",
  family = categorical(link = "logit", ref = "Not at all")
)

get_prior(mod_form, data = fa_df)

## Specify some weakly informative priors for the model parameters
mlogit_priors <- 
  prior(normal(0, 1), class = "b", dpar = "muNotverymuch") +
  prior(student_t(15, 0, 1), class = "Intercept", dpar = "muNotverymuch") +
  prior(exponential(1.5), class = "sd", dpar = "muNotverymuch") +
  prior(normal(0, 1), class = "b", dpar = "muSome") +
  prior(student_t(15, 0, 1), class = "Intercept", dpar = "muSome") +
  prior(exponential(1.5), class = "sd", dpar = "muSome") +
  prior(normal(0, 1), class = "b", dpar = "muAlot") +
  prior(student_t(15, 0, 1), class = "Intercept", dpar= "muAlot") +
  prior(exponential(1.5), class = "sd", dpar = "muAlot") +
  prior(lkj(4), class = cor)

## Fit the model
bayes_mlogit <- brm(
  formula = mod_form,
  prior = mlogit_priors,
  data = fa_df,
  cores = 6,
  chains = 6,
  iter = 2000,
  warmup = 1000,
  silent = 0,
  seed = 12345,
  #control = list(adapt_delta = 0.95,
  #               max_treedepth  = 13),
  save_pars = save_pars(all = TRUE),
  stan_model_args = list(stanc_options = list("O1")),
  backend = "cmdstanr",
  file = "output/fits/bayes_class_lfate_ch31-2"
)

# 
df_y0 <- bayes_mlogit$data %>% 
  mutate(region = as.character(round(runif(n(), 100, 200))))

df_y1 <- df_y0 %>% 
  mutate(factor1_1 = factor1_1 + 1)

hi_yes <- posterior_epred(
  bayes_mlogit, 
  newdata = df_y1 %>% filter(union == "Yes"),
  allow_new_levels = TRUE,
  sample_new_levels = "gaussian"
)

lo_yes <- posterior_epred(
  bayes_mlogit, 
  newdata = df_y0 %>% filter(union == "Yes"),
  allow_new_levels = TRUE,
  sample_new_levels = "gaussian"
)

model_ames_yes <- tibble(
  union = "Yes",
  ame_1 = rowMeans(hi_yes[, , 1] - lo_yes[, , 1]),
  ame_2 = rowMeans(hi_yes[, , 2] - lo_yes[, , 2]),
  ame_3 = rowMeans(hi_yes[, , 3] - lo_yes[, , 3]),
  ame_4 = rowMeans(hi_yes[, , 4] - lo_yes[, , 4])
)

hi_no <- posterior_epred(
  bayes_mlogit, 
  newdata = df_y1 %>% filter(union == "No"),
  allow_new_levels = TRUE,
  sample_new_levels = "gaussian"
)

lo_no <- posterior_epred(
  bayes_mlogit, 
  newdata = df_y0 %>% filter(union == "No"),
  allow_new_levels = TRUE,
  sample_new_levels = "gaussian"
)

model_ames_no <- tibble(
  union = "No",
  ame_1 = rowMeans(hi_no[, , 1] - lo_no[, , 1]),
  ame_2 = rowMeans(hi_no[, , 2] - lo_no[, , 2]),
  ame_3 = rowMeans(hi_no[, , 3] - lo_no[, , 3]),
  ame_4 = rowMeans(hi_no[, , 4] - lo_no[, , 4])
)

model_ames <- bind_rows(model_ames_yes, model_ames_no)

# write_rds(model_ames, "output/predictions/model_ames_ch3.rds")
# 
# write_rds(model_ames_no, "output/predictions/model_ames_nounion_ch3.rds")
# 
# write_rds(model_ames_yes, "output/predictions/model_ames_yesunion_ch3.rds")

model_ames <- readRDS("output/predictions/model_ames_ch3.rds")

model_ames_yes <- readRDS("output/predictions/model_ames_yesunion_ch3.rds")

model_ames_no <- readRDS("output/predictions/model_ames_nounion_ch3.rds")

##------------------------------plots-------------------------------------------

#------------------------------full sample--------------------------------------
ame_1_plot <- ggplot(model_ames, aes(x = ame_1, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.05, .075))+
  labs(x = "Not at all")


ame_2_plot <- ggplot(model_ames, aes(x = ame_2, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.05, .075))+
  labs(x = "Not very much")


ame_3_plot <- ggplot(model_ames, aes(x = ame_3, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.05, .075)) +
  labs(x = "Some")


ame_4_plot <- ggplot(model_ames, aes(x = ame_4, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.05, .075))+
  labs(x = "A lot")


ame_plot <- ame_1_plot  + ame_2_plot + ame_3_plot + ame_4_plot +
  plot_layout(ncol = 1) +
  plot_annotation(title = "Figure 1: Average Marginal Effects",
                  caption = "Plots represent the change in probabilty of linked-fate responses after manipulation
                  of environmental gedredation. Estimates represent 95% Bayesian credible intervals.") & 
  theme_bw(base_size = 10)

ggsave(
  filename = "ame_plot_full_ch3.jpg",
  plot = ame_plot,
  device = "jpeg",
  width = 8,
  dpi = "retina",
  type = "cairo"
)

#------------------------------------1 & 4-------------------------------------

ame_plot_1_4 <- ame_1_plot  + ame_4_plot +
  plot_layout(ncol = 1) +
  plot_annotation(title = "Figure X: Average Marginal Effects",
                  caption = "Plots represent the change in probabilty of linked-fate responses after joint manipulation
                  of both environmental gedredation and union membership. Estimates represent 95% Bayesian credible intervals.") & 
  theme_bw(base_size = 10)

ggsave(
  filename = "ame_plot_1_4_ch3.jpg",
  plot = ame_plot_1_4,
  device = "jpeg",
  width = 8,
  dpi = "retina",
  type = "cairo"
)

#------------------------------------Union------------------------------------

ame_1_yes_plot <- ggplot(model_ames_yes, aes(x = ame_1, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.1, .075))+
  labs(x = "Not at all")


ame_2_yes_plot <- ggplot(model_ames_yes, aes(x = ame_2, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.1, .075))+
  labs(x = "Not very much")


ame_3_yes_plot <- ggplot(model_ames_yes, aes(x = ame_3, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.1, .075)) +
  labs(x = "Some")


ame_4_yes_plot <- ggplot(model_ames_yes, aes(x = ame_4, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.1, .075))+
  labs(x = "A lot")


ame_plot_full_yes <- ame_1_yes_plot  + ame_2_yes_plot + ame_3_yes_plot + ame_4_yes_plot +
  plot_layout(ncol = 1)+
  plot_annotation(title = "Figure 2: Average Marginal Effects for Union Members") & 
  theme_bw(base_size = 10)

ggsave(
  filename = "ame_plot_full_yes_ch3.jpg",
  plot = ame_plot_full_yes,
  device = "jpeg",
  width = 5,
  dpi = "retina",
  type = "cairo"
)

#---------------------------------no union-------------------------------------

ame_1_no_plot <- ggplot(model_ames_no, aes(x = ame_1, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.05, .075))+
  labs(x = "Not at all")


ame_2_no_plot <- ggplot(model_ames_no, aes(x = ame_2, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.05, .075))+
  labs(x = "Not very much")


ame_3_no_plot <- ggplot(model_ames_no, aes(x = ame_3, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.05, .075)) +
  labs(x = "Some")


ame_4_no_plot <- ggplot(model_ames_no, aes(x = ame_4, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.05, .075))+
  labs(x = "A lot")


ame_plot_full_no <- ame_1_no_plot  + ame_2_no_plot + ame_3_no_plot + ame_4_no_plot +
  plot_layout(ncol = 1)+
  plot_annotation(title = "Figure 3: Average Marginal Effects for Non-Union Members") & 
  theme_bw(base_size = 10)

ggsave(
  filename = "ame_plot_full_no_ch3.jpg",
  plot = ame_plot_full_no,
  device = "jpeg",
  width = 5,
  dpi = "retina",
  type = "cairo"
)

