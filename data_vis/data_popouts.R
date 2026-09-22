# clear environment
#rm(list = ls())
# Load packages.
library(here)
source(here('src', 'setup.R'))

metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear_nTest.rds'))

#ER_CPR1
#MarshallGulch
#EB
#w3

target_site = 'EB'
site_dat <- metrics %>%
    filter(site_code == target_site,
           agg_code == 'annual',
           water_year < 2022) %>%
    select(site_code, water_year, q_mean, q_cv, gpp_global_500m_median,  temp_mean, precip_mean) %>%
    pivot_longer(names_to = 'var',
                 values_to = 'val',
                 cols = -c('site_code', 'water_year'))

site_dat$var <- factor(site_dat$var, levels = c('temp_mean', 'precip_mean', 'gpp_global_500m_median', 'q_cv', 'q_mean'),
                   labels = c('MAT (degrees C)', 'MAP (mm)', 'GPP (kgC/m^2/year)', 'Q CV', 'MAQ (mm)'))

ggplot(site_dat, aes(x = water_year, y = val))+
    geom_point()+
    geom_line()+
    facet_wrap(~var, scales = 'free', ncol = 1)+
    theme_few(base_size = 20)+
    labs(x = 'Water Year',
         y = '',
         title = target_site)


target_site = 'w3'
site_dat <- metrics %>%
    filter(site_code == target_site,
           agg_code == 'annual',
           water_year < 2022) %>%
    select(site_code, water_year, q_mean, q_cv, gpp_global_500m_median,  temp_mean, precip_mean) %>%
    pivot_longer(names_to = 'var',
                 values_to = 'val',
                 cols = -c('site_code', 'water_year'))

site_dat$var <- factor(site_dat$var, levels = c('temp_mean', 'precip_mean', 'gpp_global_500m_median', 'q_cv', 'q_mean'),
                       labels = c('MAT (degrees C)', 'MAP (mm)', 'GPP (kgC/m^2/year)', 'Q CV', 'MAQ (mm)'))

ggplot(site_dat, aes(x = water_year, y = val))+
    geom_point()+
    geom_line()+
    facet_wrap(~var, scales = 'free', ncol = 1)+
    theme_few(base_size = 20)+
    labs(x = 'Water Year',
         y = '',
         title = target_site)


target_site = 'MarshallGulch'
site_dat <- metrics %>%
    filter(site_code == target_site,
           agg_code == 'annual',
           water_year < 2022) %>%
    select(site_code, water_year, q_mean, q_cv, gpp_global_500m_median,  temp_mean, precip_mean) %>%
    pivot_longer(names_to = 'var',
                 values_to = 'val',
                 cols = -c('site_code', 'water_year'))

site_dat$var <- factor(site_dat$var, levels = c('temp_mean', 'precip_mean', 'gpp_global_500m_median', 'q_cv', 'q_mean'),
                       labels = c('MAT (degrees C)', 'MAP (mm)', 'GPP (kgC/m^2/year)', 'Q CV', 'MAQ (mm)'))

ggplot(site_dat, aes(x = water_year, y = val))+
    geom_point()+
    geom_line()+
    facet_wrap(~var, scales = 'free', ncol = 1)+
    theme_few(base_size = 20)+
    labs(x = 'Water Year',
         y = '',
         title = target_site)

target_site = 'ER_CPR1'
site_dat <- metrics %>%
    filter(site_code == target_site,
           agg_code == 'annual',
           water_year < 2022) %>%
    select(site_code, water_year, q_mean, q_cv, gpp_global_500m_median,  temp_mean, precip_mean) %>%
    pivot_longer(names_to = 'var',
                 values_to = 'val',
                 cols = -c('site_code', 'water_year'))

site_dat$var <- factor(site_dat$var, levels = c('temp_mean', 'precip_mean', 'gpp_global_500m_median', 'q_cv', 'q_mean'),
                       labels = c('MAT (degrees C)', 'MAP (mm)', 'GPP (kgC/m^2/year)', 'Q CV', 'MAQ (mm)'))

ggplot(site_dat, aes(x = water_year, y = val))+
    geom_point()+
    geom_line()+
    facet_wrap(~var, scales = 'free', ncol = 1)+
    theme_few(base_size = 20)+
    labs(x = 'Water Year',
         y = '',
         title = target_site)
