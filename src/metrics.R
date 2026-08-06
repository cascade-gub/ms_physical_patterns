# READ ME ####
# The following script will calculate the "magnificent 7"
# discharge metrics (Archfield et al., 2014) as well as
# the Richards-Baker Flashiness Index (Baker et al., 2004)
# and some other annual temperature/climate/chemistry metrics
# for all MacroSheds sites.
library(here)
source(here('src', 'setup.R'))

# set logging
set_logger()
# Q ####
q_freq_check = F
## read data ####
log_info('load data')

q_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "discharge",
    warn = F
)

log_info({nrow(q_data)}, ' rows of discharge data')

## Tidy ####
# Filter out repeat measures detected.
q_data_nodup <- dplyr::distinct(q_data, site_code,
                                date, .keep_all = TRUE)

log_info({nrow(q_data) - nrow(q_data_nodup)}, ' rows of discharge data removed during duplicate check')

# And filter out sites that were interpolated.
q_data_nointerp <- q_data_nodup %>%
  filter(ms_interp == 0)

log_info({nrow(q_data_nodup) - nrow(q_data_nointerp)},
         ' rows of discharge data removed during interp check')

sites_lost <- q_data_nodup %>%
    select(site_code) %>%
    distinct() %>%
    dplyr::filter(!site_code %in% unique(q_data_nointerp$site_code))

if(length(sites_lost > 0)){
log_warn({nrow(sites_lost)}, ' sites lost')
}

# Only ~ 2% of records were marked as "1" or "questionable"
# in the ms_status column, so we've left that as is.
log_info('normalize q to watershed area')
# Normalize by watershed area.
area <- ms_site_data %>%
  select(site_code, ws_area_ha)

# And convert to mm/d.
# --- Conversion equation ---
# (L/s) * (86,400 s/d) * (0.001 m^3/L) * (1 / watershed area in ha) *
# (1 ha/10,000 m^2) * (1,000 mm/1 m) = mm/d
q_data_nointerp_scaled <- left_join(q_data_nointerp, area,
                                    by = c("site_code")) %>%
  mutate(val_mmd = (val*86400)/(ws_area_ha*10000))

## Water Years ####

# Assign a consistent water year designation to all data,
# from October 1 of the previous year to September 30 of
# the following (i.e., WY2022 is 10/01/2021 - 09/30/2022).
log_info('assigning water years')

q_data_scaled <- q_data_nointerp_scaled %>%
  mutate(month = month(date),
         year = year(date)) %>%
  mutate(water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                TRUE ~ year))

if(q_freq_check == T){
## q freq check ####
log_info('performing freq check')
freq_check <- frequency_check(q_data_scaled)

write_csv(freq_check, here('data_working', 'all_possible_good_siteyears.csv'))
log_info('all_possible_good_siteyears.csv has been created')

q_data_good <- q_data_scaled %>%
    right_join(., freq_check, by = c('site_code', 'water_year'))

log_info({nrow(q_data_scaled) - nrow(q_data_good)},
         ' rows of discharge data removed during freq check')

sites_lost <- q_data_scaled %>%
    select(site_code) %>%
    distinct() %>%
    dplyr::filter(!site_code %in% unique(q_data_good$site_code))

if(length(sites_lost > 0)){
    log_warn({nrow(sites_lost)}, ' sites lost in q freq check')
}
}else{q_data_good <- q_data_scaled}
## AR(1) ####

# A few additional calculations are required for the
# auto-regressive (AR(1)) correlation calculation.
log_info('seasonal extraction')
# Calculate long-term monthly means.
q_data_monthly <- q_data_good %>%
  group_by(site_code, month) %>%
  summarize(lt_meanQ = mean(val_mmd, na.rm = TRUE)) %>%
  ungroup()

# And use those values to calculate deseasonalized Q.
q_data_good <- q_data_good %>%
  plyr::join(q_data_monthly, by = c("site_code", "month")) %>%
  mutate(deseasonQ = val_mmd - lt_meanQ)

# And scale those values to use in the AR(1) calculations.
q_data_good$scaleQds = (q_data_good$deseasonQ - mean(q_data_good$deseasonQ,
                                                       na.rm = TRUE))/
  sd(q_data_good$deseasonQ, na.rm = TRUE)

# Function to pull out AR(1) correlation coefficient
ar1_print <- function(x) {
  a1 <- ar(x, aic = FALSE, order.max = 1, na.action = na.pass)
  a1$ar
}

## Amplitude/Phase #####
log_info('amplitude and phase extraction')
# Also, need to solve for streamflow signal at each site
# using scaled (but not deseasonalized) discharge data
# as well as decimal year.
q_data_good <- q_data_good %>%
  mutate(scaleQ = (q_data_good$val_mmd -
                     mean(q_data_good$val_mmd, na.rm = TRUE))/
           sd(q_data_good$val_mmd, na.rm = TRUE),
         decimalY = decimal_date(date),
# Add covariates for streamflow signal regression.
        sin_2pi_year = sin(2*pi*decimalY),
         cos_2pi_year = cos(2*pi*decimalY))

## RBI #####

# Function to calculate the Richard-Baker flashiness index.
rbi_print <- function(x) {
    # Checked to be sure that when grouped, the "diff" function
    # omits the first record for each site-water year.
    d <- diff(x)
    RBI <- sum(abs(d))/sum(x[2:length(x)])
    return(RBI)
}

## Cumulative Q #####
log_info('median cumulative q')
# Calculate the days of year when the x-tile
# cumulative discharge is reached.
q_data_doy <- q_data_good %>%
    # first calculate quantiles and running sums
    group_by(site_code, water_year) %>%
        mutate(q_totsum = sum(val_mmd),
               q01_sum = 0.01*sum(val_mmd),
               q05_sum = 0.05*sum(val_mmd),
               q25_sum = 0.25*sum(val_mmd),
               q50_sum = 0.5*sum(val_mmd),
               q75_sum = 0.75*sum(val_mmd),
               q95_sum = 0.95*sum(val_mmd),
               q99_sum = 0.99*sum(val_mmd),
               q_sum = cumsum(val_mmd),
               n = n()) %>%
    # and then add in demarcation of when cumulative q
    # thresholds are surpassed
        mutate(q01_exceed = case_when(q_sum > q01_sum ~ 1,
                                      q_sum <= q01_sum ~ 0),
               q05_exceed = case_when(q_sum > q05_sum ~ 1,
                                     q_sum <= q05_sum ~ 0),
               q25_exceed = case_when(q_sum > q25_sum ~ 1,
                                     q_sum <= q25_sum ~ 0),
               q50_exceed = case_when(q_sum > q50_sum ~ 1,
                                     q_sum <= q50_sum ~ 0),
               q75_exceed = case_when(q_sum > q75_sum ~ 1,
                                     q_sum <= q75_sum ~ 0),
               q95_exceed = case_when(q_sum > q95_sum ~ 1,
                                     q_sum <= q95_sum ~ 0),
               q99_exceed = case_when(q_sum > q99_sum ~ 1,
                                     q_sum <= q99_sum ~ 0)) %>%
    ungroup() %>%
    # pivot this so all exceedances are in a named column
    pivot_longer(cols = q01_exceed:q99_exceed,
                 names_to = "quantile_exceeded",
                 values_to = "exceed") %>%
    filter(exceed == 1) %>%
    group_by(site_code, water_year, quantile_exceeded) %>%
    slice_head() %>%
    # reformat date to be days into the WY - VERY IMPORTANT
    mutate(dowy_exceed = as.numeric(difftime(date,
                                             as_date(paste(water_year-1, "10", "01")),
                                             units = "days"))) %>%
    # and keep only columns of interest for later joining
    select(site_code, water_year, q_totsum:q99_sum, quantile_exceeded, dowy_exceed, n) %>%
    # and finally pivot for easier viewing
    pivot_wider(names_from = quantile_exceeded, values_from = dowy_exceed) %>%
    mutate(agg_code = 'annual')

## monthly flow summaries #####
log_info('month q summaries')

q_data_month_summaries <- q_data_good %>%
    group_by(site_code, month, water_year) %>%
    summarize(q_mean = mean(val_mmd, na.rm = TRUE), # mean
              q_q01 = quantile(val_mmd, probs = 0.01, na.rm = TRUE), # 1st percentile Q
              q_q05 = quantile(val_mmd, probs = 0.05, na.rm = TRUE), # 5th percentile Q
              q_q25 = quantile(val_mmd, probs = 0.25, na.rm = TRUE), # 25th percentile Q
              q_q50 = quantile(val_mmd, probs = 0.50, na.rm = TRUE), # median Q
              q_q75 = quantile(val_mmd, probs = 0.75, na.rm = TRUE), # 75th percentile Q
              q_q95 = quantile(val_mmd, probs = 0.95, na.rm = TRUE), # 95th percentile Q
              q_q99 = quantile(val_mmd, probs = 0.99, na.rm = TRUE), # 99th percentile Q
              q_cv = (sd(val_mmd, na.rm = TRUE)/
                          mean(val_mmd, na.rm = TRUE)), # coefficient of variation
              q_skew = skewness(val_mmd, na.rm = TRUE), # skewness
              q_kurt = kurtosis(val_mmd, na.rm = TRUE), # kurtosis
              q_rbi = rbi_print(val_mmd), # Richards-Baker flashiness index
              n = n()) %>%
    rename(agg_code = month) %>%
    mutate(agg_code = as.character(agg_code))

## seasonal flow summaries ####
q_data_season_summaries <- q_data_good %>%
    mutate(season = case_when(month %in% c(6,7,8) ~ "Summer",
                              month %in% c(12,1,2) ~ "Winter",
                              month %in% c(3,4,5) ~ "Spring",
                              month %in% c(9,10,11) ~ "Fall")) %>%
    group_by(site_code, season, water_year) %>%
    summarize(q_mean = mean(val_mmd, na.rm = TRUE), # mean
              q_q01 = quantile(val_mmd, probs = 0.01, na.rm = TRUE), # 1st percentile Q
              q_q05 = quantile(val_mmd, probs = 0.05, na.rm = TRUE), # 5th percentile Q
              q_q25 = quantile(val_mmd, probs = 0.25, na.rm = TRUE), # 25th percentile Q
              q_q50 = quantile(val_mmd, probs = 0.50, na.rm = TRUE), # median Q
              q_q75 = quantile(val_mmd, probs = 0.75, na.rm = TRUE), # 75th percentile Q
              q_q95 = quantile(val_mmd, probs = 0.95, na.rm = TRUE), # 95th percentile Q
              q_q99 = quantile(val_mmd, probs = 0.99, na.rm = TRUE), # 99th percentile Q
              q_cv = (sd(val_mmd, na.rm = TRUE)/
                          mean(val_mmd, na.rm = TRUE)), # coefficient of variation
              q_skew = skewness(val_mmd, na.rm = TRUE), # skewness
              q_kurt = kurtosis(val_mmd, na.rm = TRUE), # kurtosis
              q_rbi = rbi_print(val_mmd), # Richards-Baker flashiness index
              n = n()) %>%
    rename(agg_code = season)


## High flow days #####
log_info('high flow days')
# Calculating the number of days per year that fall above the mean +1SD
# of flows.
q_data_summary <- q_data_good %>%
    group_by(site_code, water_year) %>%
    summarize(q_mean = mean(val_mmd, na.rm = TRUE),
              q_sd = sd(val_mmd, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(q_mean_plus_sd = q_mean + q_sd) %>%
    select(site_code, water_year, q_mean_plus_sd)

q_high_flows <- q_data_good %>%
    full_join(q_data_summary) %>%
    mutate(high_flow = case_when(val_mmd > q_mean_plus_sd ~ "YES",
                                 TRUE ~ "NO")) %>%
    filter(high_flow == "YES") %>%
    count(site_code, water_year) %>%
    rename(high_flow_days = n) %>%
    mutate(agg_code = 'annual')

# Calculate number of records for each site-water year,
# since those with too few records will break the
# regressions below.
log_info('generating watershed site_year counts')
q_wy_counts <- q_data_good %>%
    drop_na(val_mmd) %>%
    mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
    count(site_wy) %>%
    ungroup() %>%
    mutate(use = case_when(n > 3 ~ 1,
                           n <= 3 ~ 0)) %>%
    select(site_wy, use)

# Also notate sites for which the site-water year mean
# discharge is zero, which will also not work with the
# summary calculations below.
q_wy_mean <- q_data_good %>%
    drop_na(val_mmd) %>%
    mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
    group_by(site_wy) %>%
    summarize(mean = mean(val_mmd, na.rm = TRUE),
              n = n()) %>%
    ungroup() %>%
    mutate(use2 = case_when(mean > 0 ~ 1,
                            mean <= 0 ~ 0)) %>%
    select(site_wy, use2)

log_info('make q metrics output frame')
# Create summarized dataset with all 8 metrics by site-water year.
q_metrics_siteyear <- q_data_good %>%
  # drop all NA discharge values
  # otherwise the lm() with full missing WYs below will throw
  # an error message and not output the summarized data
  drop_na(val_mmd) %>%
  mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
  # also dropping site-water years that broke the regressions' code
  full_join(q_wy_counts) %>%
  full_join(q_wy_mean) %>%
  filter(use == 1) %>%
  filter(use2 == 1) %>%
  # finally, calculate the discharge metrics
  group_by(site_code, water_year) %>%
  summarize(q_mean = mean(val_mmd, na.rm = TRUE), # mean
            q_q01 = quantile(val_mmd, probs = 0.01, na.rm = TRUE), # 1st percentile Q
            q_q05 = quantile(val_mmd, probs = 0.05, na.rm = TRUE), # 5th percentile Q
            q_q25 = quantile(val_mmd, probs = 0.25, na.rm = TRUE), # 25th percentile Q
            q_q50 = quantile(val_mmd, probs = 0.50, na.rm = TRUE), # median Q
            q_q75 = quantile(val_mmd, probs = 0.75, na.rm = TRUE), # 75th percentile Q
            q_q95 = quantile(val_mmd, probs = 0.95, na.rm = TRUE), # 95th percentile Q
            q_q99 = quantile(val_mmd, probs = 0.99, na.rm = TRUE), # 99th percentile Q
            q_cv = (sd(val_mmd, na.rm = TRUE)/
                        mean(val_mmd, na.rm = TRUE)), # coefficient of variation
            q_skew = skewness(val_mmd, na.rm = TRUE), # skewness
            q_kurt = kurtosis(val_mmd, na.rm = TRUE), # kurtosis
            q_ar1 = ar1_print(scaleQds), # AR(1) coefficient
            q_rbi = rbi_print(val_mmd), # Richards-Baker flashiness index
            # seasonal flow signal metrics a + b
            # per p. 1169 in Archfield et al., 2014
            a_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
                            na.action = na.omit)$coefficients["sin_2pi_year"],
            b_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
                            na.action = na.omit)$coefficients["cos_2pi_year"],
            n = n()) %>%
  ungroup() %>%
  mutate(q_amp = sqrt((a_flow_sig)^2 + (b_flow_sig)^2), # amplitude
         q_phi = atan(-a_flow_sig/b_flow_sig), # phase shift
         agg_code = 'annual')

sites_lost <- q_data_good%>%
    select(site_code) %>%
    distinct() %>%
    dplyr::filter(!site_code %in% unique(q_metrics_siteyear$site_code))

if(length(sites_lost > 0)){
    log_warn({nrow(sites_lost)}, ' sites lost in metric computation')
}else{log_info('no sites lost in q metric computation')}

# Join with all other discharge metrics created.
q_metrics_out <- q_metrics_siteyear %>%
    left_join(., q_data_doy, by = c('site_code', 'water_year', 'agg_code', 'n')) %>%
    left_join(., q_high_flows, by = c('site_code', 'water_year', 'agg_code')) %>%
    full_join(., q_data_month_summaries) %>%
    full_join(., q_data_season_summaries)


# CLIMATE #####
# read in climate data
log_info('read climate data')
clim <- read_feather(here('data_raw', 'ms', 'v2', 'spatial_timeseries_climate.feather')) %>%
  mutate(year = year(date),
         month = month(date),
        water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                TRUE ~ year)) %>%
    select(site_code, date, water_year, var, val) %>%
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    mutate(month = month(date)) %>%
    mutate(season = case_when(month %in% c(6,7,8) ~ "Summer",
                              month %in% c(12,1,2) ~ "Winter",
                              month %in% c(3,4,5) ~ "Spring",
                              month %in% c(9,10,11) ~ "Fall"))

## monthly clim summaries #####
log_info('monthly climate summaries')
clim_month_summaries <- clim %>%
    group_by(site_code, water_year, month) %>%
    summarize(precip_mean = mean(precip_median, na.rm = T),
              precip_total = sum(precip_median, na.rm = T),
              temp_mean = mean(temp_median, na.rm = T),
              temp_q25 = quantile(temp_median, probs = 0.25, na.rm = T),
              temp_q75 = quantile(temp_median, probs = 0.75, na.rm = T)) %>%
    mutate(agg_code = as.character(as.integer(month)))

## seasonal  clim summaries #####
log_info('seasonal climate summaries')
clim_season_summaries <- clim %>%
    group_by(site_code, water_year, season) %>%
    summarize(precip_mean = mean(precip_median, na.rm = T),
              precip_total = sum(precip_median, na.rm = T),
              temp_mean = mean(temp_median, na.rm = T),
              temp_q25 = quantile(temp_median, probs = 0.25, na.rm = T),
              temp_q75 = quantile(temp_median, probs = 0.75, na.rm = T)) %>%
    rename(agg_code = season)

## Med. Cumulative P #####
# Calculate the day of year when the median
# precip is reached.
log_info('calculate median cum p')
clim_50_doy <- clim %>%
    group_by(site_code, water_year) %>%
    mutate(p50_sum = 0.5*sum(precip_median, na.rm = T),
           p_sum = cumsum(precip_median)) %>%
    mutate(p50_exceed = case_when(p_sum > p50_sum ~ 1,
                                  p_sum <= p50_sum ~ 0)) %>%
    ungroup() %>%
    filter(p50_exceed == 1) %>%
    group_by(site_code, water_year) %>%
    slice_head() %>%
    # reformat date to be dys into the WY
    mutate(p50_dowy_exceed = as.numeric(difftime(date,
                                                 as_date(paste(water_year-1, "10", "01")),
                                                 units = "days"))) %>%
    # and keep only columns of interest for later joining
    rename(p50_date_exceed = date) %>%
    select(site_code, water_year, p50_sum, p50_date_exceed, p50_dowy_exceed) %>%
    mutate(agg_code = 'annual')

## P Event Days per Year #####
log_info('calculate days of p per year')
# Essentially normalize annual p by number of non-zero
# precipitation days per year.
ppt_days_yr <- clim %>%
    # remove days on which there is no precip data
    drop_na(precip_median) %>%
    # also remove days on which precip is negligible
    # using USGS defined cutoff of 1mm/day
    # https://earlywarning.usgs.gov/usraindry/rdreadme.php
    filter(precip_median > 5) %>%
    count(site_code, water_year) %>%
    ungroup() %>%
    rename(ppt_days = n) %>%
    mutate(agg_code = 'annual')

# join together
clim_metrics_out <- clim %>%
    group_by(site_code, water_year) %>%
    summarize(precip_mean = mean(precip_median, na.rm = T),
              precip_total = sum(precip_median, na.rm = T),
              temp_mean = mean(temp_median, na.rm = T)) %>%
    mutate(agg_code = 'annual') %>%
    left_join(., clim_50_doy) %>%
    left_join(., ppt_days_yr) %>%
    mutate(ppt_intensity_ratio = precip_total/ppt_days) %>%
    full_join(., clim_season_summaries) %>%
    full_join(., clim_month_summaries)


# STREAM TEMP ####
## read data #####
t_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "stream_chemistry",
    filter_vars = 'temp',
    warn = F) %>%
    mutate(month = month(date),
           year = year(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year))%>%
    mutate(season = case_when(month %in% c(6,7,8) ~ "Summer",
                              month %in% c(12,1,2) ~ "Winter",
                              month %in% c(3,4,5) ~ "Spring",
                              month %in% c(9,10,11) ~ "Fall"))


log_info({nrow(t_data)}, ' rows of stream temp data')
# want at least monthly sampling for most of the year for now
# 51/52 weeks of the year
log_info('performing freq check on stream temp')

freq_check <- t_data %>%
            filter(ms_interp == 0) %>%
            mutate(month_year = paste0(month(date), '_', water_year)) %>%
    group_by(site_code, month_year) %>%
    summarize(water_year = max(water_year),
              n = n()) %>%
    filter(n >= 1) %>%
    group_by(site_code, water_year) %>%
    summarize(n = n()) %>%
    filter(n >= 10)

t_good <- t_data %>%
    filter(ms_interp == 0) %>%
    right_join(., freq_check, by = c('site_code', 'water_year')) %>%
    select(site_code, water_year, season, var, val) %>%
    na.omit()


log_info({nrow(t_data) - nrow(t_good)}, ' rows of stream temp data removed during freq/interp check')

sites_lost <- t_data %>%
    select(site_code) %>%
    distinct() %>%
    dplyr::filter(!site_code %in% unique(t_good$site_code))

if(length(sites_lost > 0)){
    log_warn({nrow(sites_lost)}, ' sites lost in stream temp freq/interp check')
}

## annual stream temps ####
t_ann <- t_good %>%
    group_by(site_code, water_year) %>%
    summarize(stream_temp_mean = mean(val, na.rm = T),
              stream_temp_q05 = quantile(val, probs = 0.05, na.rm = T),
              stream_temp_q25 = quantile(val, probs = 0.25, na.rm = T),
              stream_temp_q75 = quantile(val, probs = 0.75, na.rm = T),
              stream_temp_q95 = quantile(val, probs = 0.95, na.rm = T)) %>%
    mutate(agg_code = 'annual')

## seasonal stream temps #####
t_season <- t_data %>%
    group_by(site_code, water_year, season) %>%
    summarize(stream_temp_mean = mean(val, na.rm = T),
              stream_temp_q05 = quantile(val, probs = 0.05, na.rm = T),
              stream_temp_q25 = quantile(val, probs = 0.25, na.rm = T),
              stream_temp_q75 = quantile(val, probs = 0.75, na.rm = T),
              stream_temp_q95 = quantile(val, probs = 0.95, na.rm = T)) %>%
    rename(agg_code = season)

## monthly stream temps #####
t_month <- t_data %>%
    group_by(site_code, water_year, month) %>%
    summarize(stream_temp_mean = mean(val, na.rm = T),
              stream_temp_q05 = quantile(val, probs = 0.05, na.rm = T),
              stream_temp_q25 = quantile(val, probs = 0.25, na.rm = T),
              stream_temp_q75 = quantile(val, probs = 0.75, na.rm = T),
              stream_temp_q95 = quantile(val, probs = 0.95, na.rm = T)) %>%
    mutate(agg_code = as.character(as.integer(month)))


t_out <- t_ann %>%
    full_join(t_month) %>%
    full_join(t_season) %>%
    select(-month)

# PRECIP CHEMISTRY ####
precip_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "precip_chemistry", warn = F)

# STREAM CHEMISTRY ####

# First, going to read in core data for all chemistry.

##### read data #####
chem_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "stream_chemistry", warn = F) %>%
    # remove interpolated values
    filter(ms_interp == 0) %>%
    # remove missing values
    drop_na(val)

# note - ms_calc_vwc has been deprecated from the most recent
# version of the macrosheds package
# so, hard-coding the calculation of volume-weighted conc below

# Now, the discharge (q) data.
# Leaving in Liters per second per hectare since many chem data
# values are expressed in milligrams per Liter.

q_trim <- q_data_scaled %>%
    select(date, month, year, water_year,
           site_code, ws_area_ha, val) # val in L/second

#### Nitrogen ####

# Much of the below code was initially from the
# 'nitrogen_vwm.R' file where Heili initially began
# N data exploration.

# Filter out only N analytes of interest.
n_data <- chem_data %>%
    # select for variables of interest
    # focusing on nitrate, ammonium, and total N
    filter(var %in% c("NO3_NO2_N", "NO3_N", "NO2_N",
                      "NH3_NH4_N", "NH4_N", "NH3_N",
                      "TDN", "TPN", "TN", "TIN",
                      "TDKN", "TKN", "N2O")) # skipping "d15N_NO3"

# Make uniform names for analytes
n_data <- n_data %>%
    mutate(analyte_N = case_when(var %in% c("NH4_N",
                                            "NH3_N",
                                            "NH3_NH4_N") ~ "NH3_N",
                                 var %in% c("NO3_N",
                                            "NO3_NO2_N") ~ "NO3_N",
                                 var %in% c("TDKN",
                                            "TKN") ~ "TKN",
                                 TRUE ~ var))

# Calculate daily means
n_daily <- n_data %>%
    group_by(site_code, analyte_N, date) %>%
    summarize(val_dailymean = mean(val, na.rm = TRUE)) %>%
    ungroup()

# And create a new DIN category that sums together
# if there is BOTH a NH4/NH3 record and a NO3 record
n_daily_wide <- n_daily %>%
    pivot_wider(names_from = analyte_N, values_from = val_dailymean) %>%
    mutate(DIN = case_when(is.na(NH3_N) == FALSE &
                               is.na(NO3_N) == FALSE ~ NH3_N + NO3_N,
                           is.na(NH3_N) == TRUE |
                               is.na(NO3_N) == TRUE ~ NA))

n_daily_long <- n_daily_wide %>%
    pivot_longer(cols = NH3_N:DIN) %>%
    rename(analyte_N = name,
           val_dailymean = value)

# And join with daily discharge data
n_q_data <- left_join(n_daily_long, q_trim)

# Since the fall season (Sep-Nov) overlaps the WY
# designation, we've decided to roll back a month
# so first need to delineate those seasons and then
# include September in the following WY's fall season.
n_q_data <- n_q_data %>%
    mutate(season = case_when(month(date) %in% c(9,10,11) ~ "Fall",
                              month(date) %in% c(12,1,2) ~ "Winter",
                              month(date) %in% c(3,4,5) ~ "Spring",
                              month(date) %in% c(6,7,8) ~ "Summer")) %>%
    mutate(season_year = case_when(month(date) == 9 ~ water_year + 1,
                                   TRUE ~ water_year))

# Note, the value in the water_year column will be missing
# if there's no discharge, but that's ok, since those values
# won't be included in CQ calculations anyway.

###### Monthly means #####

# And convert to volume-weighted mean concentrations,
# on a monthly basis.
n_q_vwm_month <- n_q_data %>%
    # Calculate instantaneous C*Q.
    mutate(c_q_instant = val_dailymean*val) %>%
    # And drop rows that yield NA values.
    drop_na(c_q_instant) %>%
    # Now impose groupings.
    group_by(site_code, ws_area_ha, analyte_N, water_year, month) %>%
    # Calculate mean seasonal volume weighted concentrations.
    summarize(monthly_vwm_mgL = (sum(c_q_instant,
                                      na.rm = TRUE))/(sum(val,
                                                          na.rm = TRUE)),
              n_of_obs_chem = n()) %>%
    ungroup()

log_info({nrow(n_q_vwm_month)}, ' rows of monthly vwm N chemistry data')
# want at least biweekly sampling for 10 months of the year

###### Seasonal means #####

# And convert to volume-weighted mean concentrations,
# on a seasonal basis.
n_q_vwm_seas <- n_q_data %>%
    # Calculate instantaneous C*Q.
    mutate(c_q_instant = val_dailymean*val) %>%
    # And drop rows that yield NA values.
    drop_na(c_q_instant) %>%
    # Now impose groupings.
    group_by(site_code, ws_area_ha, analyte_N, season_year, season) %>%
    # Calculate mean seasonal volume weighted concentrations.
    summarize(seasonal_vwm_mgL = (sum(c_q_instant,
                                      na.rm = TRUE))/(sum(val,
                                                          na.rm = TRUE)),
              n_of_obs_chem = n()) %>%
    ungroup()

log_info({nrow(n_q_vwm_seas)}, ' rows of seasonal vwm N chemistry data')
# ideally want at least two samples per season (i.e., every 3 months)

###### Annual means #####
log_info('also append annual means for trend analysis')

# And convert to volume-weighted mean concentrations,
# on an annual basis.
n_q_vwm_ann <- n_q_data %>%
    # Calculate instantaneous C*Q.
    mutate(c_q_instant = val_dailymean*val) %>%
    # And drop rows that yield NA values.
    drop_na(c_q_instant) %>%
    # Now impose groupings.
    group_by(site_code, ws_area_ha, analyte_N, water_year) %>%
    # Calculate mean annual volume weighted concentrations.
    summarize(annual_vwm_mgL = (sum(c_q_instant,
                                    na.rm = TRUE))/(sum(val,
                                                        na.rm = TRUE)),
              n_of_obs_chem = n()) %>%
    ungroup()

log_info({nrow(n_q_vwm_ann)}, ' rows of annual vwm N chemistry data')
# ideally want 10 obs per year, with a min. of 8 months represented

###### CQ slopes & ints ######

# Create dataset of seasonal CQ slopes.
n_q_cq_seasonal <- n_q_data %>%
    # Remove missing data.
    drop_na(val_dailymean) %>%
    drop_na(val) %>%
    filter(val_dailymean > 0) %>%
    filter(val > 0) %>%
    # Group by season.
    group_by(site_code, analyte_N, season) %>%
    # Calculate CQ slope.
    summarize(cq_slope = coef(lm(log(val_dailymean) ~ log(val)))[2],
              cq_int = coef(lm(log(val_dailymean) ~ log(val)))[1],
              n_of_obs = n()) %>%
    ungroup()

# And another that includes only data from the past decade.
n_q_cq_seasonal20 <- n_q_data %>%
    # Remove missing data.
    drop_na(val_dailymean) %>%
    drop_na(val) %>%
    filter(val_dailymean > 0) %>%
    filter(val > 0) %>%
    # Trim down to necessary years.
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    # Group by season.
    group_by(site_code, analyte_N, season) %>%
    # Calculate CQ slope.
    summarize(cq_slope = coef(lm(log(val_dailymean) ~ log(val)))[2],
              cq_int = coef(lm(log(val_dailymean) ~ log(val)))[1],
              n_of_obs = n()) %>%
    ungroup()

# Create dataset of decadal CQ slopes.
n_q_cq_decadal <- n_q_data %>%
    # Add decadal column.
    mutate(decade = case_when(water_year > 1959 & water_year <= 1969 ~ 1960,
                              water_year > 1969 & water_year <= 1979 ~ 1970,
                              water_year > 1979 & water_year <= 1989 ~ 1980,
                              water_year > 1989 & water_year <= 1999 ~ 1990,
                              water_year > 1999 & water_year <= 2009 ~ 2000,
                              water_year > 2009 & water_year <= 2019 ~ 2010,
                              water_year > 2019 & water_year <= 2029 ~ 2020)) %>%
    # Remove missing data.
    drop_na(val_dailymean) %>%
    drop_na(val) %>%
    filter(val_dailymean > 0) %>%
    filter(val > 0) %>%
    # Group by season.
    group_by(site_code, analyte_N, decade) %>%
    # Calculate CQ slope.
    summarize(cq_slope = coef(lm(log(val_dailymean) ~ log(val)))[2],
              cq_int = coef(lm(log(val_dailymean) ~ log(val)))[1],
              n_of_obs = n()) %>%
    ungroup()

##### Export VWM N data #####

saveRDS(n_q_vwm_month, "data_working/N_VWM_monthly.rds")
saveRDS(n_q_vwm_seas, "data_working/N_VWM_seasonal.rds")
saveRDS(n_q_vwm_ann, "data_working/N_VWM_annual.rds")
saveRDS(n_q_cq_seasonal, "data_working/N_CQ_seasonal.rds")
saveRDS(n_q_cq_seasonal20, "data_working/N_CQ_seasonal20.rds")
saveRDS(n_q_cq_decadal, "data_working/N_CQ_decadal.rds")

# #### DOC ####
#
# ##### Monthly means #####
#
# doc_monthly_vwmeans <- chem_q_good %>%
#     select(site_code, var, water_year, month, monthly_vwm_mgL) %>%
#     pivot_wider(
#         names_from = c(var, month),
#         values_from = monthly_vwm_mgL)
#
# #out_path <- here("data_working", "doc_monthly_VWM.rds")
# #saveRDS(doc_monthly_vwmeans, out_path)
#
# pH_monthly_vwmeans <- chem_q_good %>%
#     select(site_code, var, water_year, month, monthly_vwm_mgL) %>%
#     pivot_wider(
#         names_from = c(var, month),
#         values_from = monthly_vwm_mgL)
#
# #out_path <- here("data_working", "pH_monthly_VWM.rds")
# #saveRDS(pH_monthly_vwmeans, out_path)
#
# ##### Annual means #####
#
# doc_annual_vwmeans <- chem_q_good %>%
#     group_by(site_code, var, water_year) %>%
#     summarize(Annual = mean(monthly_vwm_mgL, na.rm = TRUE)) %>%
#     ungroup() %>%
#     pivot_wider(
#         names_from = var,
#         values_from = Annual) %>%
#     rename(DOC_Annual = DOC)
#
# doc_vwmeans <- full_join(doc_monthly_vwmeans, doc_annual_vwmeans)
#
# #out_path <- here("data_working", "doc_annual_VWM.rds")
# #saveRDS(doc_vwmeans, out_path)
#
# pH_annual_vwmeans <- chem_q_good %>%
#     group_by(site_code, var, water_year) %>%
#     summarize(Annual = mean(monthly_vwm_mgL, na.rm = TRUE)) %>%
#     ungroup() %>%
#     pivot_wider(
#         names_from = var,
#         values_from = Annual) %>%
#     rename(pH_Annual = pH)
#
# pH_vwmeans <- full_join(pH_monthly_vwmeans, pH_annual_vwmeans)

#out_path <- here("data_working", "pH_annual_VWM.rds")
#saveRDS(pH_vwmeans, out_path)

##### Season means #####

# Check to see how things look: DOC
# ggplot(chem_q_data %>%
#            mutate(year = year(date),
#                   month = month(date),
#                   day = day(date)) %>%
#            mutate(season = case_when(month %in% c(6,7,8) ~ "Summer",
#                                      month %in% c(12,1,2) ~ "Winter",
#                                      month %in% c(3,4,5) ~ "Spring",
#                                      month %in% c(9,10,11) ~ "Fall",
#                                      TRUE ~ NA)) %>%
#            mutate(water_year = factor(case_when(month %in% c(10, 11, 12) ~ year+1,
#                                          TRUE ~ year))) %>%
#            filter(site_code == "w9" & var == "DOC"),
#        aes(x = log(q_Lsecha),
#            y = log(val),
#            color = water_year)) +
#     geom_point() +
#     geom_smooth(method = "lm", se = F) +
#     theme_bw() +
#     facet_wrap(.~season, scales = "free")

# PRODUCTIVITY ####
## read data ####
p_data <- read_feather(here('data_raw', 'ms', 'v2', 'spatial_timeseries_vegetation.feather')) %>%
    mutate(month = month(date),
           year = year(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    mutate(season = case_when(month %in% c(6,7,8) ~ "Summer",
                              month %in% c(12,1,2) ~ "Winter",
                              month %in% c(3,4,5) ~ "Spring",
                              month %in% c(9,10,11) ~ "Fall"))#,
                              #TRUE ~ NA))

log_info({nrow(p_data)}, ' rows of productivity data')

## annual prod ####
p_ann <- p_data %>%
    distinct() %>%
    group_by(site_code, water_year, var) %>%
    summarize(val = mean(val, na.rm = T)) %>%
    pivot_wider(id_cols = c('site_code', 'water_year'),
                names_from = 'var', values_from = 'val') %>%
    mutate(agg_code = 'annual')

## seasonal prod ####
p_season <- p_data %>%
    distinct() %>%
    group_by(site_code, water_year, season, var) %>%
    summarize(val = mean(val, na.rm = T)) %>%
    pivot_wider(id_cols = c('site_code', 'water_year', 'season'),
                names_from = 'var', values_from = 'val') %>%
    rename(agg_code = season)

## monthly prod ####
p_month <- p_data %>%
    distinct() %>%
    group_by(site_code, water_year, month, var) %>%
    summarize(val = mean(val, na.rm = T)) %>%
    pivot_wider(id_cols = c('site_code', 'water_year', 'month'),
                names_from = 'var', values_from = 'val') %>%
    mutate(agg_code = as.character(month))

p_out <- p_ann %>%
    full_join(p_season) %>%
    full_join(p_month) %>%
    select(-month)

# EXPORT ####
## climate ####
out_path <- here('data_working', 'clim_summaries.rds')
saveRDS(clim_metrics_out, file = out_path)
log_info('file saved to ', out_path)

## site_year level ####
log_info('saving out')
q_data_out <- q_metrics_out %>%
    full_join(., readRDS(here('data_working', 'clim_summaries.rds')),
              by = c('site_code', 'water_year', 'agg_code')) %>%
    mutate(runoff_ratio = q_mean/precip_mean) %>%
    # adding column to denote source
    mutate(source = "MS") %>%
    full_join(., t_out, by = c('site_code', 'water_year', 'agg_code')) %>%
    full_join(., p_out, by = c('site_code', 'water_year', 'agg_code')) %>%
    #full_join(., n_vwmeans, by = c('site_code', 'water_year')) %>%
    #full_join(., chem_seas_cq, by = c('site_code', 'water_year')) %>%
    distinct() %>%
    drop_na(agg_code)

out_path <- here('data_working', 'discharge_metrics_siteyear_nTest.rds')
saveRDS(q_data_out, out_path)
log_info('file saved to ', out_path)

# commening out as no analyses use this now
## site level Q ####
# Create summarized dataset with all 8 metrics for full time series at each site.
# log_info('calculate site level metrics')
# q_metrics_site <- q_data_good %>%
#   group_by(site_code) %>%
#   # drop all NA discharge values
#   # otherwise the lm() will throw an error message
#   drop_na(val_mmd) %>%
#   # also dropping site-water years that broke the regressions' code previously
#   mutate(site_wy = paste(site_code,water_year, sep = "_")) %>%
#   summarize(q_mean = mean(val_mmd, na.rm = TRUE), # mean
#             q_q1 = quantile(val_mmd, probs = 0.01, na.rm = TRUE), # 1st percentile Q
#             q_q5 = quantile(val_mmd, probs = 0.05, na.rm = TRUE), # 5th percentile Q
#             q_q25 = quantile(val_mmd, probs = 0.25, na.rm = TRUE), # 25th percentile Q
#             q_q50 = quantile(val_mmd, probs = 0.50, na.rm = TRUE), # median Q
#             q_q75 = quantile(val_mmd, probs = 0.75, na.rm = TRUE), # 75th percentile Q
#             q_q95 = quantile(val_mmd, probs = 0.95, na.rm = TRUE), # 95th percentile Q
#             q_q99 = quantile(val_mmd, probs = 0.99, na.rm = TRUE), # 99th percentile Q
#             q_cv = (sd(val_mmd, na.rm = TRUE)/
#                         mean(val_mmd, na.rm = TRUE)), # coefficient of variation
#             q_skew = skewness(val_mmd, na.rm = TRUE), # skewness
#             q_kurt = kurtosis(val_mmd, na.rm = TRUE), # kurtosis
#             q_ar1 = ar1_print(scaleQds), # AR(1) coefficient
#             q_rbi = rbi_print(val_mmd), # Richards-Baker flashiness index
#             a_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
#                             na.action = na.omit)$coefficients["sin_2pi_year"],
#             b_flow_sig = lm(scaleQ ~ sin_2pi_year + cos_2pi_year,
#                             na.action = na.omit)$coefficients["cos_2pi_year"]) %>% # flow signal metrics
#   ungroup() %>%
#   mutate(q_amp = sqrt((a_flow_sig)^2 + (b_flow_sig)^2), # amplitude
#          q_phi = atan(-a_flow_sig/b_flow_sig)) # phase shift
#
# out_path <- here("data_working", "discharge_metrics_site.rds")
# saveRDS(q_metrics_site, out_path)
# log_info('file saved to ', out_path)
# # End of script.
