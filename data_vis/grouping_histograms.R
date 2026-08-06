library(here)
source(here('src', 'setup.R'))

ms_trends_grouped <- read_csv(here('data_working', 'site_groupings_by_prsim_trend.csv'))


daymet <- ms_load_product(here('data_raw', 'ms'),
                          prodname = 'ws_attr_CAMELS_Daymet_forcings',
                          warn = FALSE)

metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear_nTest.RDS')) %>%
    distinct()

p <- ms_load_product(my_ms_dir,
                     prodname = 'ws_attr_timeseries:climate',
                     filter_vars = 'precip_median',
                     warn = FALSE) %>%
    select(-var, -year, -pctCellErr, precip_median = val)

# aET data
et <- read_csv(here('data_raw', 'ms_add_ons', 'macrosheds_et2.csv'))
et_obs <- metrics %>%
    filter(agg_code == 'annual') %>%
    select(site_code, wy = water_year, precip_total, q_totsum) %>%
    mutate(ei_obs = (precip_total - q_totsum)/precip_total)

# indices ####
d <- inner_join(p, daymet, by = c('network', 'domain', 'site_code', 'date')) %>%
    mutate(year = as.integer(as.character(water_year(date, origin = 'usgs')))) %>%
    group_by(year, site_code) %>%
    summarize(aridity_index = sum(`pet(mm)`)/sum(`prcp(mm/day)`),
              precip = sum(`prcp(mm/day)`))  %>%
    full_join(., et, by = c('site_code', 'year')) %>%
    mutate(evaporative_index = val/(precip*10)) %>%
    full_join(., et_obs, by = c('site_code', 'year' = 'wy'))


aridity <- d %>%
    select(site_code, aridity_index) %>%
    group_by(site_code) %>%
    summarize(mean_ai = mean(aridity_index, na.rm = T)) %>%
    full_join(ms_trends_grouped, by = 'site_code') %>%
    filter(ws_status == 'non-experimental')
aridity$q_flag[is.na(aridity$q_flag)] <- "data limited"
aridity$q_flag <- fct_relevel(aridity$q_flag, 'increasing', 'decreasing', 'non-significant', 'data limited')


# set axes the same
ylims = c(0, 17)
xlims = c(0,6)

#hotter wetter
aridity %>%
    filter(trend_precip_mean > 0,
           trend_temp_mean > 0) %>%
    nrow()

aridity %>%
    filter(trend_precip_mean > 0,
           trend_temp_mean > 0) %>%
    ggplot(aes(x = mean_ai, fill = q_flag))+
        #geom_density(lwd = 2)+
    geom_histogram()+
    scale_fill_manual(values = c('blue','red', 'grey', 'black'))+
    scale_x_continuous(limits = xlims)+
    scale_y_continuous(limits = ylims)+
    labs(x = 'Aridity Index (mean, 1980-2020)',
         y = 'n',
         fill = 'Q trend',
         title = 'Hotter and wetter')+
    geom_vline(xintercept = 1, color = 'orange', lwd = 2, lty = 'longdash')+
    theme_few(base_size = 20)


#Hotter drier
aridity %>%
    filter(trend_precip_mean < 0,
           trend_temp_mean > 0) %>%
    nrow()

aridity %>%
    filter(trend_precip_mean < 0,
           trend_temp_mean > 0) %>%
    ggplot(aes(x = mean_ai, fill = q_flag))+
    #geom_density(lwd = 2)+
    geom_histogram()+
    scale_fill_manual(values = c('red', 'grey', 'black'))+
    scale_x_continuous(limits = xlims)+
    scale_y_continuous(limits = ylims)+
    labs(x = 'Aridity Index (mean, 1980-2020)',
         y = 'n',
         fill = 'Q trend',
         title = 'Hotter and drier')+
    geom_vline(xintercept = 1, color = 'orange', lwd = 2, lty = 'longdash')+
    theme_few(base_size = 20)

#cooler drier
aridity %>%
    filter(trend_precip_mean < 0,
           trend_temp_mean < 0, q_trend > 0) %>%
    nrow()


aridity %>%
    filter(trend_precip_mean < 0,
           trend_temp_mean < 0) %>%
    ggplot(aes(x = mean_ai, fill = q_flag))+
    geom_histogram()+
    #geom_density(lwd = 2)+
    #geom_histogram()+
    scale_fill_manual(values = c('black'))+
    scale_x_continuous(limits = xlims)+
    scale_y_continuous(limits = ylims)+
    labs(x = 'Aridity Index (mean, 1980-2020)',
         y = 'n',
         color = 'Q trend',
         title = 'Cooler and drier')+
    geom_vline(xintercept = 1, color = 'orange', lwd = 2, lty = 'longdash')+
    theme_few(base_size = 20)

#cooler wetter
aridity %>%
    filter(trend_precip_mean > 0,
           trend_temp_mean < 0) %>%
    nrow()

aridity %>%
    filter(trend_precip_mean > 0,
           trend_temp_mean < 0) %>%
    ggplot(aes(x = mean_ai, fill = q_flag))+
    geom_histogram()+
    geom_density(lwd = 2)+
    scale_fill_manual(values = c('black'), drop=FALSE)+
    guides(fill = guide_legend(override.aes = list(alpha=1))) +
    scale_x_continuous(limits = xlims)+
    scale_y_continuous(limits = ylims)+
    labs(x = 'Aridity Index (mean, 1980-2020)',
         y = 'n',
         color = 'Q trend',
         title = 'Cooler and wetter')+
    geom_vline(xintercept = 1, color = 'orange', lwd = 2, lty = 'longdash')+
    theme_few(base_size = 20)
