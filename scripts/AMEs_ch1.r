#--------------Chapter 2 Average Model Effects Calculation-------------
#-Author: A. Rose Benton--------------------------------Created: December 11, 2022-#
#-R Version: 4.1.2--------------------------------Last Modified: December 11, 2022-#

## Load the necessary libraries
pacman::p_load(
  "tidyverse", # Suite of packages for data management 
  "brms", # Bayesian regression models with Stan
  "tidybayes", # Functions for wrangling posteriors tidy-style\
  "patchwork", # Combining multiple plots into one
  "ggdist", # Visualizations of Distributions and Uncertainty
  install = FALSE
)



set.seed(1234)


df_y00 <- bayes_mlogit_lfate_3$data %>% 
  mutate(
    state = as.numeric(state) + round(runif(n(), 100, 200)),
    district = district + round(runif(n(), 100, 200)),
    `state:district` = paste(state, district, sep = "_"),
    time = 0
    )

df_y10 <- df_y00 %>% 
  mutate(log_km_from_center_mean = log_km_from_center_mean + 1)

df_y11 <- df_y10 %>% 
  mutate(time = 1)

df_y01 <- df_y00 %>% 
  mutate(time = 1)

hi <- posterior_epred(
  bayes_mlogit_lfate_3, 
  newdata = df_y11,
  allow_new_levels = TRUE,
  sample_new_levels = "gaussian"
  )

 lo <- posterior_epred(
  bayes_mlogit_lfate_3, 
  newdata = df_y00,
  allow_new_levels = TRUE,
  sample_new_levels = "gaussian"
  )

model_ames <- tibble(
  ame_1 = rowMeans(hi[, , 1] - lo[, , 1]),
  ame_2 = rowMeans(hi[, , 2] - lo[, , 2]),
  ame_3 = rowMeans(hi[, , 3] - lo[, , 3])
  )

write_rds(model_ames, "model_ames.rds")

model_ames <- read_rds("model_ames.rds")

hist(model_ames$ame_1)
hist(model_ames$ame_2)
hist(model_ames$ame_3)

ame_1_plot <- ggplot(model_ames, aes(x = ame_1, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.25, .3))+
  labs(x = "Not a lot",
       y = "")


ame_3_plot <- ggplot(model_ames, aes(x = ame_3, fill = stat(x > 0))) +
  stat_halfeye(
    aes(slab_alpha = stat(f)),
    point_interval = median_qi,
    fill_type = "gradient",
    show.legend = FALSE
  )+
  scale_x_continuous(limits = c(-.25, .3))+
  labs(x = "A lot",
       y = "")


ame_plot <- ame_1_plot + ame_3_plot+
  plot_layout(ncol = 1) +
  plot_annotation(title = "Figure 4: Average Marginal Effects",
                  caption = "Plots represent the change in probabilty of linked-fate responses after joint manipulation
                  of both time and proximity. Estimates represent 95% Bayesian credible intervals.") & 
  theme_bw(base_size = 10)

ggsave(
  filename = "ame_plot_mod_3.jpg",
  plot = ame_plot,
  device = "jpeg",
  width = 5,
  dpi = "retina",
  type = "cairo"
)
