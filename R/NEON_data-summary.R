library(tidyverse)
library(brms)
theme_set(theme_minimal())
rstan::rstan_options(auto_write = TRUE)
### Working with NEON metabolism estimates
bayesMetFile_est = read.csv(here::here("ignore/NEON Metabolism Data/8_NEON_metab_estimates.csv"))

metFull <- bayesMetFile_est %>%
  select(site, date, GPP, GPP.lower, GPP.upper, ER, ER.lower, ER.upper, discharge) %>%
  group_by(site,date) %>%
  summarise(across(c(GPP, GPP.lower, GPP.upper, ER, ER.lower, ER.upper, discharge), \(x) mean(x, na.rm = TRUE))) %>%
  mutate(GPP = if_else(GPP <= 0, 0.001, GPP),
         date = as.Date(date),
         doy = yday(date),
         mean_doy = mean(doy),
         doy_c_100 = (doy - mean_doy)/100,
         year = year(date),
         year_f = as.character(year)) %>%
  ungroup

gpp_c_split = split(metFull %>% select(site, doy_c_100, GPP, year_f), f = metFull$site)
brm_GPP_gam = brm_multiple(GPP ~ s(doy_c_100) + (1|year_f),
                            family = Gamma(link = 'log'),
                            data = gpp_c_split,
                            prior =
                              c(prior(normal(0, 2),
                                      class = "Intercept"),
                                prior(normal(0, 1),
                                      class = "b"),
                                prior(normal(0, 1),
                                      class = "sds"),
                                prior(exponential(1),
                                      class = "sd")),
                            iter = 1500, chains = 4, combine = TRUE,
                            backend = 'rstan')

saveRDS(brm_GPP_gam,here::here("data/derived-data/models/site_gpp_gamm_full.rds") )
brm_GPP_gam = readRDS(here::here("data/derived-data/models/site_gpp_gamm_full"))

gpp_conditionals <- NULL
brm_GPP_gam <- setNames(brm_GPP_gam, nm = names(gpp_c_split))
for(i in 1:length(brm_GPP_gam)){
  # brm_gpp_fit[[i]]$data %>%
  # distinct(tibble) %>%
  tibble(doy = 1:365) %>%
    dplyr::mutate(mean_doy = mean(doy),
                  doy_c_100 = (doy - mean_doy)/100) %>%
    tidybayes::add_epred_draws(brm_GPP_gam[[i]],
                               re_formula = NA,
                               ndraws = 500) %>%
    mutate(model = names(brm_GPP_gam)[i]) %>%
    bind_rows() -> posts_tidy
  gpp_conditionals <- rbind(gpp_conditionals, posts_tidy)

}
for(i in seq_along(names(brm_GPP_gam))){
  conditional_effects(brm_GPP_gam[[i]]) %>%
    pluck(1) %>%
    data.frame %>%
    ggplot()+
    geom_ribbon(aes(x = doy_c_100, ymin = lower__, ymax = upper__), fill ='lightgrey',alpha = 0.8)+
    geom_line(aes(x = doy_c_100, y = estimate__), linewidth = 1.2)+
    geom_point(data = lapply(conditional_effects(brm_GPP_gam[[i]]), attributes) %>%
                 pluck('doy_c_100') %>%
                 pluck('points'), aes(x = doy_c_100, y = resp__))+
    scale_y_continuous(name = expression("GPP (g "*O[2]~m^-2~d^-1~")"))+
    scale_x_continuous(name = "Day of Year")+
    ggtitle(names(brm_GPP_gam)[[i]])+
    theme(axis.text.x = element_blank())
}
plot(conditional_effects(brm_GPP_gam[['SYCA']]), points = TRUE)



gpp_means = gpp_conditionals %>%
  dplyr::rename(site = 'model') %>%
  dplyr::mutate(.epred = .epred) %>%
  left_join(metFull %>% group_by(site) %>%
              dplyr::mutate(day_min = min(doy_c_100),
                            day_max = max(doy_c_100)) %>%
              distinct(site, day_min, day_max)) %>%
  dplyr::mutate(.epred = case_when(doy_c_100 < day_min ~ 0,
                                   doy_c_100 > day_max ~ 0,
                                   TRUE ~ .epred)) %>%
  group_by(site, .draw) %>%
  dplyr::summarize(total_gpp = sum(.epred)) %>%
  dplyr::summarize(mean = mean(total_gpp),
                   sd = sd(total_gpp))

saveRDS(gpp_means, file = "./data/derived_data/gpp_means.rds")

gpp_means %>%
  dplyr::mutate(cv = sd/mean) %>% print(n = 24)
