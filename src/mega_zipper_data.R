# handle setup
library(here)
source(here('src', 'setup.R'))

# read in full q_metrics.R output
#source(here('src', 'q_metrics.R'))



#metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear.rds')) %>%
metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear_nTest.rds')) %>%
    distinct()

# headwater filter: adjusted stream order <= max_stream_order (see setup.R)
hw_sites <- headwater_sites()
metrics <- metrics %>%
    filter(site_code %in% hw_sites)
message('headwater sites in metrics: ', n_distinct(metrics$site_code))

# run full climate trends first
clim_trends <- metrics %>%
    select(-contains('date'), -source) %>% # dates breaking math
    pivot_longer(cols = -c('site_code', 'water_year', 'agg_code'),
                 names_to = 'var',
                 values_to = 'val') %>%
    filter(var %in% c('temp_mean', 'precip_mean', 'gpp_global_500m_median'),
           agg_code == 'annual',
           water_year >= analysis_start_year,
           water_year <= analysis_end_year) %>%
    distinct() %>%
    reduce_to_longest_site_runs(., metric = 'temp_mean') %>%
    detect_trends(.)

write_csv(clim_trends, here('data_working', 'trends', 'full_prisim_climate.csv'))

# check nas from climate run
clim_trends %>%
    filter(is.na(trend)) %>%
    select(site_code) %>%
    distinct()

# create longest run w/ prism data ####
# make frame of all data during prism

prism_site_run_trends_data <- metrics %>%
    select(-contains('date')) %>% # dates breaking math
    filter(agg_code == 'annual',
           n > full_year_min_obs_per_year) %>%
    select(site_code, water_year, agg_code,
           # climate
           temp_mean,
           precip_mean,
           p_q50_dowy_exceed = p50_dowy_exceed,
           p_n_days = ppt_days,
           ppt_intensity_ratio,
           # hydro
           q_mean, q_ar1, q_rbi,
           q_q50_dowy_exceed = q50_exceed,
           runoff_ratio,
           # in-stream
           stream_temp_mean,
           # productivity
           gpp_modis = gpp_global_500m_median) %>%
    #drop_na(temp_mean_ann, precip_mean_ann, q_mean) %>%
    pivot_longer(cols = -c('site_code', 'water_year', 'agg_code'),
                 names_to = 'var',
                 values_to = 'val') %>%
           filter(water_year >= analysis_start_year,
                  water_year <= analysis_end_year) %>%
    reduce_to_best_range(., metric = 'q_mean')

# trend detection ####
prism_site_run_trends <- detect_trends(prism_site_run_trends_data) %>%
    mutate(q_window = 'water_year')

# seasonal-site path ####
# Sites that cannot reach a usable q_mean trend through the standard gate
# (frequency_check in metrics.R, then >= 10 years with <= 40% missing) are
# evaluated on mean discharge over a fixed summer window instead
# (q_summer_siteyear.rds from metrics.R; summer_months and
# seasonal_min_summer_days in setup.R). A site enters best_run_prisim.csv
# through exactly one path; q_window records which.
if (seasonal_summer_path) {
    good_q_sites <- prism_site_run_trends %>%
        filter(var == 'q_mean', code == 'good') %>%
        pull(site_code) %>%
        unique()

    q_summer <- readRDS(here('data_working', 'q_summer_siteyear.rds')) %>%
        filter(site_code %in% hw_sites,
               !site_code %in% good_q_sites,
               water_year >= analysis_start_year,
               water_year <= analysis_end_year)

    summer_run_data <- metrics %>%
        filter(agg_code == 'annual') %>%
        select(site_code, water_year, agg_code,
               temp_mean, precip_mean,
               gpp_modis = gpp_global_500m_median) %>%
        distinct() %>%
        inner_join(select(q_summer, site_code, water_year, q_mean = q_mean_summer),
                   by = c('site_code', 'water_year')) %>%
        pivot_longer(cols = -c('site_code', 'water_year', 'agg_code'),
                     names_to = 'var',
                     values_to = 'val') %>%
        reduce_to_best_range(., metric = 'q_mean')

    summer_run_trends <- detect_trends(summer_run_data) %>%
        mutate(q_window = 'JJA')

    summer_good_sites <- summer_run_trends %>%
        filter(var == 'q_mean', code == 'good') %>%
        pull(site_code) %>%
        unique()

    summer_path_sites <- tibble(site_code = unique(q_summer$site_code)) %>%
        left_join(summer_run_trends %>%
                      filter(var == 'q_mean') %>%
                      add_flags() %>%
                      select(site_code, start, end, n, trend, trend_lower, trend_upper, code, flag),
                  by = 'site_code') %>%
        mutate(code = replace_na(code, 'under_10'),
               entered_via_summer_window = site_code %in% summer_good_sites)
    write_csv(summer_path_sites, here('data_working', 'seasonal_path_sites.csv'))
    message('summer-window path: ', length(summer_good_sites), ' of ',
            n_distinct(q_summer$site_code),
            ' sites without a standard q_mean trend gained one on the summer window: ',
            paste(summer_good_sites, collapse = ', '))

    prism_site_run_trends <- prism_site_run_trends %>%
        filter(!site_code %in% summer_good_sites) %>%
        bind_rows(filter(summer_run_trends, site_code %in% summer_good_sites))
}

# export
#write_csv(prism_site_run_trends , here('data_working', 'trends', 'longest_site_run_prisim.csv'))
write_csv(prism_site_run_trends , here('data_working', 'trends', 'best_run_prisim.csv'))

