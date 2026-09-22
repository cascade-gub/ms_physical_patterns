# handle setup
library(here)
source(here('src', 'setup.R'))

# read in full q_metrics.R output
#source(here('src', 'q_metrics.R'))
metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear_nTest.rds')) %>%
    distinct() %>%
    tibble()


full <- metrics %>%
    filter(site_code == 'MarshallGulch',
           agg_code == 'annual',
           water_year < 2022) %>%
    select(site_code, water_year, q_mean, gpp_global_500m_median,  temp_mean, precip_mean) %>%
    pivot_longer(names_to = 'var',
                 values_to = 'val',
                 cols = -c('site_code', 'water_year'))

full$var <- factor(full$var, levels = c('temp_mean', 'precip_mean', 'gpp_global_500m_median', 'q_mean'),
                   labels = c('MAT (degrees C)', 'MAP (mm)', 'GPP (kgC/m^2/year)', 'MAQ (mm)'))


reduced <- full %>%
    reduce_to_best_range(., metric = 'MAQ (mm)')

min <- min(reduced$water_year)
max <- max(reduced$water_year)

ggplot(full, aes(x = water_year, y = val))+
    geom_point()+
    geom_line()+
    geom_vline(xintercept = max)+
    geom_vline(xintercept = min)+
    #geom_smooth(method = 'lm', se = F)+
    facet_wrap(~var, scales = 'free', ncol = 1)+
    theme_few(base_size = 20)+
    labs(x = 'Water Year',
         y = '')

ggplot(reduced, aes(x = water_year, y = val))+
    geom_point()+
    geom_line()+
    geom_smooth(method = 'lm', se = F)+
    scale_x_continuous(limits = c(analysis_start_year, analysis_end_year))+
    facet_wrap(~var, scales = 'free', ncol = 1)+
    theme_few(base_size = 20)+
    labs(x = 'Water Year',
         y = '')
