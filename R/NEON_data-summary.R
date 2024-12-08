library(tidyverse)
library(brms)
library(tidybayes)
library(units)
units_options(negative_power = TRUE)
theme_set(theme_minimal())
rstan::rstan_options(auto_write = TRUE)
fill_dates = function(siteCode = NULL, ...){
  siteDates = metFull %>%
    filter(site == siteCode) %>%
    select(date) %>%
    reduce(c) %>%
    range

  fullDates = data.frame(date = seq.Date(as.Date(siteDates[1]), as.Date(siteDates[2]), by = 1) )

  siteDf =  metFull %>%
    filter(site == siteCode)

  newDf = merge(fullDates, siteDf, by = 'date', all = TRUE) %>%
    fill(site) %>%
    mutate(doy = yday(date),
           doy_c_100 = (doy-183)/100,
           year_f = as.character(year(date)))

  #upload metabolism models
  site_GPP_model = brm_GPP_gam[[siteCode]]
  gpp_conditionals = newDf %>%
    select(site, date, doy, doy_c_100, year_f) %>%
    add_epred_draws(site_GPP_model, ndraws = 100, allow_new_levels = TRUE) %>%
    ungroup %>%
    summarise(GPP_est = mean(.epred, na.rm = TRUE),
              GPP_est_l = quantile(.epred, 0.025),
              GPP_est_u = quantile(.epred, 0.975),
              .by = c('site','date'))
  site_ER_model = brm_ER_gam[[siteCode]]
  er_conditionals = newDf %>%
    select(site, date, doy, doy_c_100, year_f) %>%
    add_epred_draws(site_ER_model, ndraws = 100, allow_new_levels = TRUE) %>%
    mutate(.epred = .epred*-1) %>%
    ungroup %>%
    summarise(ER_est = mean(.epred, na.rm = TRUE),
              ER_est_l = quantile(.epred, 0.025),
              ER_est_u = quantile(.epred, 0.975),
              .by =  c('site','date'))
  fullDf = newDf %>%
    left_join(gpp_conditionals, by = c('site','date')) %>%
    left_join(er_conditionals, by = c('site','date')) %>%
    mutate( discharge_mn = median(discharge, na.rm = TRUE),
            discharge = zoo::na.spline(discharge, maxgap = 5)) %>%
    rowwise() %>%
    mutate(GPP = coalesce(GPP, GPP_est),
           GPP.lower = coalesce(GPP.lower, GPP_est_l),
           GPP.upper = coalesce(GPP.upper, GPP_est_u),
           ER = coalesce(ER, ER_est),
           ER.lower = coalesce(ER.lower, ER_est_l),
           ER.upper = coalesce(ER.upper, ER_est_u),
           discharge = coalesce(discharge, discharge_mn)) %>%
    mutate(across(matches('^GPP'), \(x) if_else(x < 0, 0.001, x)),
           across(matches('^ER'), \(x) if_else(x > 0, -0.001, x))) %>%
    select(site, date, GPP, GPP.lower, GPP.upper, ER, ER.lower, ER.upper, discharge)


  return(fullDf)
}
rerun = FALSE

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
sites = unique(metFull$site)
if(rerun){
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
} else{
brm_GPP_gam = readRDS(here::here("data/derived-data/models/site_gpp_gamm_full.rds"))
}
if(rerun){
ER_c_split = split(metFull %>% select(site, doy_c_100, ER, year_f) %>% mutate(ER = if_else(ER>=0,0.001,-ER)), f = metFull$site)
brm_ER_gam = brm_multiple(ER ~ s(doy_c_100) + (1|year_f),
                           family = Gamma(link = 'log'),
                           data = ER_c_split,
                           prior =
                             c(prior(normal(0, 2),
                                     class = "Intercept"),
                               prior(normal(0, 1),
                                     class = "b"),
                               prior(normal(0, 1),
                                     class = "sds"),
                               prior(exponential(1),
                                     class = "sd")),
                           iter = 2000, chains = 4, combine = FALSE,
                           backend = 'rstan')
brm_ER_gam = setNames(brm_ER_gam, sites)
saveRDS(brm_ER_gam,here::here("data/derived-data/models/site_ER_gamm_full.rds") )
} else{
  brm_ER_gam = readRDS(here::here("data/derived-data/models/site_ER_gamm_full.rds"))
}

site_metab_list = sites %>% map(\(x) fill_dates(x)) %>% setNames(., sites)
if(rerun){
site_metab_list %>% pluck("BLDE") %>%
  data.frame %>%
  ggplot()+
  geom_ribbon(aes(x = date, ymin = GPP.lower, ymax = GPP.upper), fill = 'grey', alpha = 0.5)+
  geom_ribbon(aes(x = date, ymin = ER.lower, ymax = ER.upper), fill = 'grey', alpha = 0.5)+
  geom_line(aes(x = date, y =ER), color = 'brown', linewidth = 1.3)+
  geom_line(aes(x = date, y =GPP), color = 'green', linewidth = 1.3)

site_metab_df = site_metab_list%>%
  bind_rows(.id = 'site') %>%
  mutate(year = year(date))

site_metab_annual_summ = site_metab_df %>%
  ungroup %>%
  summarise(gpp_sum = sum(GPP, na.rm = TRUE),
            gpp_sum_l = sum(GPP.lower, na.rm = TRUE),
            gpp_sum_u = sum(GPP.upper, na.rm = TRUE),
            gpp_median = median(GPP, na.rm = TRUE),
            er_sum = sum(ER, na.rm = TRUE),
            er_sum_l = sum(ER.lower, na.rm = TRUE),
            er_sum_u = sum(ER.upper, na.rm = TRUE),
            er_median = median(ER, na.rm = TRUE),
            discharge_mn = mean(discharge, na.rm = TRUE),
            .by = c('site','year')) %>%
  left_join(metFull %>% summarise(ndays = n(), .by = c('site', 'year')), by = c('site','year')) %>%
  data.frame

comment(site_metab_annual_summ) =
  "'site' represents the 4-digit site code.
'year' represents the 4-digit year code.
'gpp_sum' represents daily GPP values (both measured and estimated) summed across the year.
'gpp_sum_l' represents lower estimates of daily GPP values (both measured and estimated) summed across the year.
'gpp_sum_u' represents upper estimates of daily GPP values (both measured and estimated) summed across the year.
'gpp_median' represents the median daily GPP values (both measured and estimated) across the year.
'er_sum' represents daily ER values (both measured and estimated) summed across the year.
'er_sum_l' represents lower estimates of daily ER (both measured and estimated) summed across the year.
'er_sum_u' represents upper estimates of daily ER (both measured and estimated) summed across the year.
'er_median' represents the median daily ER values (both measured and estimated) across a year.
'ndays' the number of days GPP or ER were measured directly each year.
'discharge' is the mean discharge measured over the year. "

gpp_er_units = as_units("g")*as_units("m-2")*as_units("yr-1")
for(i in 3:10){
  units(site_metab_annual_summ[,i]) <- gpp_er_units
}
units(site_metab_annual_summ[,11]) <- as_units("d")
# units(site_metab_annual_summ[,12])

saveRDS(site_metab_annual_summ, file = here::here("data/derived-data/site_metab_annual_summ.rds"))
} else{site_metab_annual_summ = readRDS(file = here::here("data/derived-data/site_metab_annual_summ.rds"))}
#summarise by Junes

site_metab_JuneDf = site_metab_list%>%
  bind_rows(.id = 'site') %>%
  mutate(sample_yr = case_when(month(date) < 6 ~ (year(date) - 1),
                               .default = year(date))) %>%
  mutate(sample_yr = str_c(sample_yr,"-06-01")) %>%
  ungroup %>%
  summarise(across(c(GPP, GPP.lower, GPP.upper, ER, ER.lower, ER.upper), \(x) sum(x, na.rm = TRUE), .names = "{.col}_sum"),
            across(discharge, \(x) mean(x, na.rm = TRUE), .names = "{.col}_mean"),
            ndays = n(), .by = c('site','sample_yr'))

comment(site_metab_JuneDf) =
  "'site' represents the 4-digit site code.
'sample_yr' represents the starting month of the sample year.
'GPP_sum' represents daily GPP values (both measured and estimated) summed across the year.
'GPP.lower_sum' represents lower estimates of daily GPP values (both measured and estimated) summed across the year.
'GPP.upper_sum' represents upper estimates of daily GPP values (both measured and estimated) summed across the year.
'ER_sum' represents daily ER values (both measured and estimated) summed across the year.
'ER.lower_sum' represents lower estimates of daily ER (both measured and estimated) summed across the year.
'ER.upper_sum' represents upper estimates of daily ER (both measured and estimated) summed across the year.
'discharge' is the mean discharge measured over the year.
'ndays' the number of days GPP or ER were measured directly each year."

saveRDS(site_metab_JuneDf, file = here::here("data/derived-data/site_metab_June_summ.rds"))


# gpp_conditionals <- NULL
# brm_GPP_gam <- setNames(brm_GPP_gam, nm = names(gpp_c_split))
# for(i in 1:length(brm_GPP_gam)){
#   # brm_gpp_fit[[i]]$data %>%
#   # distinct(tibble) %>%
#   tibble(doy = 1:365) %>%
#     dplyr::mutate(mean_doy = mean(doy),
#                   doy_c_100 = (doy - mean_doy)/100) %>%
#     tidybayes::add_epred_draws(brm_GPP_gam[[i]],
#                                re_formula = NA,
#                                ndraws = 500) %>%
#     mutate(model = names(brm_GPP_gam)[i]) %>%
#     bind_rows() -> posts_tidy
#   gpp_conditionals <- rbind(gpp_conditionals, posts_tidy)
#
# }
# for(i in seq_along(names(brm_GPP_gam))){
#   conditional_effects(brm_GPP_gam[[i]]) %>%
#     pluck(1) %>%
#     data.frame %>%
#     ggplot()+
#     geom_ribbon(aes(x = doy_c_100, ymin = lower__, ymax = upper__), fill ='lightgrey',alpha = 0.8)+
#     geom_line(aes(x = doy_c_100, y = estimate__), linewidth = 1.2)+
#     geom_point(data = lapply(conditional_effects(brm_GPP_gam[[i]]), attributes) %>%
#                  pluck('doy_c_100') %>%
#                  pluck('points'), aes(x = doy_c_100, y = resp__))+
#     scale_y_continuous(name = expression("GPP (g "*O[2]~m^-2~d^-1~")"))+
#     scale_x_continuous(name = "Day of Year")+
#     ggtitle(names(brm_GPP_gam)[[i]])+
#     theme(axis.text.x = element_blank())
# }
# plot(conditional_effects(brm_GPP_gam[['SYCA']]), points = TRUE)
# # debugonce(fill_dates)


# data summary
site_date_files = list.files(here::here("data/derived-data/"), pattern = "site_date_.*.rds", full.names = TRUE)
site_date_names = lapply(site_date_files, function(x) gsub("(site_date_.*).rds$","\\1",sapply(strsplit(x,"/"),"[",10)))
site_date_list = site_date_files %>% purrr::map(readRDS) %>% setNames(site_date_names)

site_date_annual_summaries = site_date_list %>%
  rlist::list.subset(!grepl("riparian", names(.))) %>%
  map(\(x) x %>% ungroup %>%
        rename(site = 'siteID', date = 'collectDate') %>%
        mutate(year = year(as.Date(date))) %>%
        select(-date) %>%
        summarise(across(everything(), \(x) mean(x, na.rm = TRUE)),
                  n = n(),.by = c('site','year')))

saveRDS(site_date_annual_summaries, here::here("data/derived-data/site_date_annual_summaries.rds"))


site_date_june_summaries =  site_date_list %>%
  rlist::list.subset(!grepl("riparian", names(.))) %>%
  map(\(x) x %>% ungroup %>%
        rename(site = 'siteID', date = 'collectDate') %>%
        mutate(sample_yr = case_when(month(as.Date(date)) < 6 ~ (year(as.Date(date)-1)),
                                     .default = year(as.Date(date)))) %>%
        mutate(sample_yr = str_c(sample_yr,"-06-01")) %>%
        select(-date) %>%
        summarise(across(everything(), \(x) mean(x, na.rm = TRUE)),
                  n = n(),.by = c('site','sample_yr')))
saveRDS(site_date_june_summaries, here::here("data/derived-data/site_date_june_summaries.rds"))

site_date_riparian_num_summaries = site_date_list %>%
  pluck("site_date_riparian") %>%
  rename(site = 'siteID',date = 'startDate') %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),.by = c('site','year'))

saveRDS(site_date_riparian_num_summaries, here::here("data/derived-data/site_date_riparian_num_summary.rds"))
site_date_riparian_fac_summaries = site_date_list %>%
  pluck("site_date_riparian") %>%
  rename(site = 'siteID',date = 'startDate') %>%
  select(-namedLocation, -where(is.numeric),site, date) %>%
  mutate(date = as.Date(date)) %>%
  group_by(site, date) %>%
  pivot_longer(cols = -c(site,date),
               names_to = 'variable',
               values_to = 'value') %>%
  mutate(count = 1) %>%
  ungroup %>%
  pivot_wider(names_from = c(variable, value),
              values_from = count,
              values_fill = 0,
              values_fn = sum) %>%
  pivot_longer(cols = -c(site,date),
               names_to = 'variable',
               values_to = 'count') %>%
  mutate(year = year(date)) %>%
  summarise(count = sum(count), .by = c(site,year,variable)) %>%
  separate(col = variable, c("variable","value"), convert = TRUE, sep = "_", remove = FALSE)

saveRDS(site_date_riparian_fac_summaries, here::here("data/derived-data/site_date_riparian_fac_summary.rds"))


site_date_riparian_comp_summaries = site_date_list %>%
  pluck("site_date_riparian_comp") %>%
  rename(site = 'siteID',date = 'startDate') %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  summarise(`canopyCover%_mean` = mean(canopyCoverPercent, na.rm = TRUE),
            `canopyCover%_sd` = sd(canopyCoverPercent, na.rm = TRUE), .by = c(site, year))

saveRDS(site_date_riparian_comp_summaries,
        here::here("data/derived-data/site_date_riparian_cover_summary.rds"))
