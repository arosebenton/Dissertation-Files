#-------------------------------Who Fuckin Knows TBH---------------------------
#-Author: A. Rose Benton---------------------------Created: September, 26, 2022-#
#-R Version: 4.1.3---------------------------------Revised:  November, 27 , 2022-#

pacman::p_load(
  "tidyverse",
  "brms",
  "sf",
  "cmdstanr",
  "ggeffects",
  "ggplot2",
  "marginaleffects",
  "patchwork",
  "brmsmargins",
  "texreg",
  "modelsummary",
  "kableExtra"
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


##----------------------------------------------------------------------------##
#---------------------- ------Model Data Whatever-----------------------------
##----------------------------------------------------------------------------##
# 
# ## Read in the processed data
# anes_temp <- read_rds("data/anes_npl_full.rds") %>%
#   as_tibble() %>%
#   mutate(
#     female = sex,
#     age_sc = (age - 40) / 10,
#     income = as.integer(income),
#     educ = fct_recode(
#       educ,
#       "High School or Less" = "Less than High School",
#       "High School or Less" = "High School Graduate",
#       "Some College" = "Some College",
#       "Some College" = "Vocational Degree",
#       "Some College" = "Associate Degree",
#       "Bachelor's Degree or Higher" = "Bachelor's Degree",
#       "Bachelor's Degree or Higher" = "Master's Degree",
#       "Bachelor's Degree or Higher" = "Doctoral Degree",
#       "Bachelor's Degree or Higher" = "Professional/Doctoral Degree"
#     ),
#     party = as.integer(pre_partisan) - 3,
#     state_fips = as.numeric(state_fips),
#     district = as.numeric(district),
#     lfate = case_when(
#       lfate %in% c("Not at all", "Not very  much") ~ 0,
#       lfate == "Some" ~ 1,
#       lfate == "A lot" ~ 2
#     ),
#     time = case_when(year == 2016 ~ 0,
#                      year == 2020 ~ 1),
#     log_km_from_center_mean = log(km_from_center_mean),
#     state = state_fips
#   ) %>% 
#   mutate(
#     state = case_when(
#       state == "1" ~ "01",
#       state == "2" ~ "02",
#       state == "3" ~ '03', 
#       state == "4" ~ "04",
#       state == "5" ~ "05", 
#       state == "6" ~ "06",
#       state == "7" ~ "07",
#       state == "8" ~ "08",
#       state == "9" ~ "09",
#       TRUE ~ as.character(state)
#     )
#   )%>%
#   droplevels()
# 
# #add regional control
# states <- tigris::states() %>% 
#   transmute(
#     region = REGION,
#     state = as.character(STATEFP)
#   ) 
# 
# anes <- left_join(anes_temp, states, by = "state")


# # ## Apply sum contrast coding to gender and citizen
# anes <- within(anes, {
#   contrasts(race) <- contr.sum(levels(race))
#   contrasts(female) <- contr.sum(levels(female))
# })

#write_rds(anes, "data/anes_model_version.rds")

anes <- read_rds("data/anes_model_version.rds")

#------------------------------------------------------------------------------#
#-----------------------------Model Specification-------------------------------
#------------------------------------------------------------------------------#

# # # Approximate model formula is
# # 
# library(lme4)
# x <-
#   lmer(
#     as.integer(lfate) ~ time + in_district + avg_hrs + time * log_km_from_center_mean + race + female +
#       educ + age_sc + party + income + region + (1 | state:district) +
#       (1 + time | state),
#     data = anes
#   )
# 
# equatiomatic::extract_eq(x, wrap = 5
#                          )
# 
# mod_form <- bf(
#   lfate ~ time + in_district + log(km_from_center_mean) + race + female + educ + age_sc +
#     party + income + (1 | state / district),
#   decomp = "QR",
#   family = cumulative(link = "logit", threshold = "flexible")
# )
# 
# #get_prior(mod_form, data = anes)
# 
# ## Specify some weakly informative priors for the model parameters
# mlogit_priors <-
#   prior(normal(0, 1), class = "b") +
#   prior(student_t(10, 0, 1), class = "Intercept") +
#   prior(exponential(1), class = "sd")
# 
# ## Fit the model
# bayes_mlog_lfate <- brm(
#   formula = mod_form,
#   prior = mlogit_priors,
#   family = cumulative(link = "logit", threshold = "flexible"),
#   data = anes,
#   cores = 6,
#   chains = 6,
#   iter = 4500,
#   warmup = 3000,
#   refresh = 10,
#   seed = 12345,
#   control = list(
#     adapt_delta = 0.99,
#     max_treedepth  = 13,
#     step_size = 0.02
#   ),
#   save_pars = save_pars(all = TRUE),
#   stan_model_args = list(stanc_options = list("O1")),
#   backend = "cmdstanr",
#   file = "bayes_mlog_lfate_time2"
# )


mod_form_2 <- bf(
  lfate ~ time + in_district + time * log_km_from_center_mean +  race + female +
    educ + age_sc + party + income + region + (1 | state:district) +
    (1 + time | state),
  decomp = "QR",
  family = categorical(link = "logit", ref = 1)
)

#get_prior(mod_form_2, data = anes)

## Specify some weakly informative priors for the model parameters
mlogit_priors_2 <-
  prior(normal(0, 1), class = "b", dpar = mu0) +
  prior(student_t(15, 0, 1), class = "Intercept", dpar = mu0) +
  prior(exponential(1.5), class = "sd", dpar = mu0) +
  prior(normal(0, 1), class = "b", dpar = mu2) +
  prior(student_t(15, 0, 1), class = "Intercept", dpar = mu2) +
  prior(exponential(1.5), class = "sd", dpar = mu2)

## Fit the model
bayes_mlogit_lfate_2 <- brm(
  formula = mod_form_2,
  prior = mlogit_priors_2,
  data = anes,
  cores = 6,
  chains = 6,
  iter = 5000,
  warmup = 3000,
  refresh = 10,
  seed = 12345,
  control = list(adapt_delta = 0.95,
                 max_treedepth  = 13),
  save_pars = save_pars(all = TRUE),
  stan_model_args = list(stanc_options = list("O1")),
  backend = "cmdstanr",
  file = "output/fits/bayes_creg_lfate_time.html"
)


mod_form_3  <- bf(
  lfate ~ time + in_district + avg_hrs + time * log_km_from_center_mean + race + female +
    educ + age_sc + party + income + region + (1 | state:district) +
    (1 + time | state),
  decomp = "QR",
  family = categorical(link = "logit", ref = 1)
)

#get_prior(mod_form_3, data = anes)

## Specify some weakly informative priors for the model parameters
mlogit_priors_3 <-
  prior(normal(0, 1), class = "b", dpar = mu0) +
  prior(student_t(15, 0, 1), class = "Intercept", dpar = mu0) +
  prior(exponential(1.5), class = "sd", dpar = mu0) +
  prior(normal(0, 1), class = "b", dpar = mu2) +
  prior(student_t(15, 0, 1), class = "Intercept", dpar = mu2) +
  prior(exponential(1.5), class = "sd", dpar = mu2)

## Fit the model
bayes_mlogit_lfate_3 <- brm(
  formula = mod_form_3,
  prior = mlogit_priors_3,
  data = anes,
  cores = 6,
  chains = 6,
  iter = 5000,
  warmup = 3000,
  refresh = 10,
  seed = 12345,
  control = list(adapt_delta = 0.95,
                 max_treedepth  = 13),
  save_pars = save_pars(all = TRUE),
  stan_model_args = list(stanc_options = list("O1")),
  backend = "cmdstanr",
  file = "output/fits/bayes_creg_lfate_time_hrs.html"
)


#------------------------------------------------------------------------------#
#------------------------------------Tables-------------------------------------
#------------------------------------------------------------------------------#

#regression table

## Extract the information for the multinomial logit model
mlogit_table_ls <- modelsummary(
  bayes_mlogit_lfate_3,
  output = "modelsummary_list",
  fmt = 3,
  estimate = "{estimate}",
  statistic = "conf.int",
  conf_level = 0.95,
  metrics = "none"
)

options(modelsummary_get = "broom")

options(knitr.kable.NA = '')

## Data frame for the mlogit model
mlogit_table_df <- modelsummary(
  mlogit_table_ls,
  output = "data.frame",
  fmt = 3,
  estimate = "{estimate}",
  statistic = "conf.int",
  conf_level = 0.95,
  metrics = "none",
  shape = term + statistic ~ response,
  coef_map = c(
    "b_Intercept" = "Intercept",
    "b_time" = "Time",
    "b_in_district" = "In District",
    "b_avg_hrs" = "Average HRS Score",
    "b_log_km_from_center_mean" = "Proximity",
    "b_race1" = "Asian",
    "b_race2" = "Hispanic",
    "b_female1" = "Female",
    "b_educSomeCollege" = "Some College",
    "b_educBachelorsDegreeorHigher" = "Bachelors Degree or Higher",
    "b_age_sc" = "Age",
    "b_party" = "Republican",
    "b_income" = "Income",
    "b_time:log_km_from_center_mean" = "Log Proximity:Time",
    "sd_state__Intercept" = "State Intercept SD",
    "sd_state__time" = "State-Year SD",
    "sd_state:district__Intercept" = "State-District Intercept SD",
    "cor_state__Intercept__time" = "State Intercept, Time Slope Correlation"
  ),
  gof_map = tribble(
    ~ raw,
    ~ clean,
    ~ fmt,
    "n",
    "N",
    0,
    "state",
    "States",
    0,
    "state:district",
    "Districts",
    0
  )
) %>%
  ## Tweaking term names
  mutate(term = case_when(statistic == "conf.int" ~ NA_character_,
                          TRUE ~ term)) %>%
  ## Select a subset of the columns
  select(-c(part, statistic))

mlogit_table_df[33:34, 2:3] = mlogit_table_df[33:34, 4:5]

## Render the table in text
tbl <- mlogit_table_df[, 1:3] %>%
  kbl(
    .,
    format = "html",
    row.names = F,
    col.names = c("", "Not at All", "A Lot"),
    caption = "Table 1. Multilevel Logistic Analysis of Linked Fate",
    align = "lcc",
    booktabs = T,
    escape = FALSE,
    linesep = ""
  ) %>%
  # Set the header to repeat at the top of each page
  kable_classic(html_font = "serif") %>%
  ## Add section labels
  group_rows(
    index = c(
      "Population-Level Effects" = 26,
      "Variance Components" = 8
    ),
    indent = FALSE,
    label_row_css = "border-bottom: 1px solid black;"
  )

save_kable(tbl, file = "output/tables/mlog_time_table.html")



#Descriptives Tables
vtable::st(
  anes,
  vars = c("race",
           "female",
           "educ",
           "age_cat",
           "pre_partisan"),
  labels = c("Race",
             "Sex",
             "Education Level",
             "Age",
             "Party"),
  out = "return",
  file = "output/tables/dem_disc.html"
)


vtable::st(
  anes,
  vars = c("km_from_center_mean",
           "in_district",
           "avg_hrs"),
  labels = c("Proximity (log(Km))",
             "In District",
             "Average HRS Score"),
  out = "return",
  file = "output/tables/iv_disc.html"
)

barplot <- barplot(table(anes$lfate))
#------------------------------------------------------------------------------#
#------------------------------------Plots--------------------------------------
#------------------------------------------------------------------------------#
#posterior predictive check
pp_check_dens_mod_2 <-
  pp_check(bayes_mlogit_lfate_3, type = "bars") +
  theme_bw(base_size = 16) +
  labs(title = "Figure 3: Posterior Predictive Check") +
  scale_x_continuous(breaks = seq(1, 3, 1))


ggsave(
  filename = "output/figures/pp_check_dens_mod_2.jpg",
  plot = pp_check_dens_mod_2,
  units = "in",
  dpi = "retina"
)


#probability of direction plot
pd_plot <- bayestestR::pd(bayes_mlogit_lfate_3) %>% plot() +
  scale_y_discrete(
    labels = c(
      "b_Intercept[1]" = "Intercept (Not Very Much)",
      "b_Intercept[2]" = "Intercept (Some)",
      "b_Intercept[3]" = "Intercept (A Lot)",
      "b_time" = "Time",
      "b_in_district" = "In District",
      "b_avg_hrs" = "Average HRS Score",
      "b_log_km_from_center_mean" = "Proximity",
      "b_race1" = "Asian or Pacific Islander",
      "b_race2" = "Hispanic",
      "b_female1" = "Female",
      "b_educSomeCollege" = "Some College",
      "b_educBachelorsDegreeorHigher" = "Bachelors Degree or Higher",
      "b_age_sc" = "Age",
      "b_party" = "Partisanship (Dem-Rep)",
      "b_income" = "Income",
      "b_time:log_km_from_center_mean" = "Proximity*Time"
    )
  ) +
  labs(title = "Figure 3: Probabilty of Direction") +
  theme(
    legend.text = element_text(family = "serif", size = 20),
    legend.title = element_text(
      family = "serif",
      size = 15,
      face = "bold"
    ),
    plot.title = element_text(size = 30),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    text = element_text(size = 20)
  )


ggsave(
  filename = "otput/figures/pd_plot.jpeg",
  plot = pd_plot,
  width = 20,
  height = 15
)


prox_dist <- ggplot(anes, aes(x = log_km_from_center_mean)) +
  geom_histogram(fill = "seagreen") +
  scale_x_continuous(limits = c(2, 7.5))+
  labs(title = "Figure 2: Distribution of Proximity to NPL Sites",
       x = "Proximity (log(km))",
       y = "")
  

ggsave(
  filename = "output/figures/prox_dist.jpg",
  plot = prox_dist,
  dpi = "retina",
  type = "cairo"
)
