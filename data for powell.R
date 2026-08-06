test <- ms_var_catalog %>%
    #filter(variable_code %in% c('NO3_N', 'temp'),
    #                      chem_category %in% c('stream_conc', NA)) %>%
    filter(variable_code %in% 'discharge') %>%
    mutate(length = as.integer(last_record-first_record),
           obs_rate = (observations/2)/(as.integer(length)), #duplicated q values removed
           obs_rate = round(obs_rate, digits = 2)) %>%
    #filter( mean_obs_per_day > 0, length > 365) %>%
    unique() %>%
    select(site_code, domain, network, variable_code, observations, first_record, last_record, length, mean_obs_per_day, first_record, last_record, obs_rate) %>%
    pivot_wider(id_cols = c('site_code','domain', 'network'),
                names_from = 'variable_code',
                values_from = c('length', 'observations', 'first_record', 'last_record', 'obs_rate')) %>%
    na.omit() %>%
    filter( length_discharge > (365*20),
           obs_rate_discharge >= .75) %>%
    mutate(length_discharge_years = length_discharge/365) %>%
    left_join(., ms_site_data, by = 'site_code')

write_csv(test, file = '20_yr_discharge_coverage_ms.csv')

View(test)

#discharge, et, precip, for all sites above + RI from luqillo


sites <- c(test$site_code, 'RI')

q <- ms_load_product(my_ms_dir, site_codes = sites,
                     prodname = 'discharge') %>%
    select(date, site_code, var, val)

et <- ms_load_product(my_ms_dir, site_codes = sites,
                      prodname = 'ws_attr_timeseries:climate') %>%
    select(date, site_code, var, val)

p <- ms_load_product(my_ms_dir, site_codes = sites,
                     prodname = 'precipitation') %>%
    select(date, site_code, var, val)

out <- bind_rows(q, p, et)
out
