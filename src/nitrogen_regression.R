# Nitrogen regression script

#### README ####
# The following script will build a regression
# model to examine how watershed characteristics
# influence relative magnitude of N exports.

#### Load packages ####
library(here)
library(GGally)
library(nlme)
source(here('src', 'setup.R'))

#### Load data ####

# Annual VWM for Nitrogen
N_VWM_annual <- readRDS("data_working/N_VWM_annual.rds")

# Site characteristics added in using setup script loaded above.

#### Tidy ####

# This is so our data match the time frame 2010-2020.
mean_N_VWM_annual20 <- N_VWM_annual %>%
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    group_by(site_code, analyte_N) %>%
    summarize(mean_annual_VWM_mgL = mean(annual_vwm_mgL,
                                         na.rm = TRUE),
              mean_annual_obs = mean(n_of_obs_chem,
                                     na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup()

# Join with site info so that we can filter out experimental sites.
mean_N_VWM_annual20 <- left_join(mean_N_VWM_annual20, ms_site_data)

mean_N_VWM_annual20_nonexp <- mean_N_VWM_annual20 %>%
    filter(ws_status == "non-experimental")

# And add LEVEL I ecoregion column.
# Referenced: https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/cec_na/NA_LEVEL_I.pdf
mean_N_VWM_annual20_nonexp <- mean_N_VWM_annual20_nonexp %>%
    mutate(ecoregion = case_when(domain == "santa_barbara" ~ "MediterraneanCalifornia",
                                 domain %in% c("niwot",
                                               "boulder",
                                               "catalina_jemez",
                                               "east_river",
                                               "hjandrews",
                                               "krew",
                                               "loch_vale") |
                                 site_code %in% c("BIGC",
                                                  "BLDE",
                                                  "COMO",
                                                  "MART",
                                                  "MCRA",
                                                  "REDB",
                                                  "TECR",
                                                  "WLOU") ~ "NWForestedMountain",
                                 domain %in% c("baltimore",
                                               "bear",
                                               "shale_hills",
                                               "fernow",
                                               "santee",
                                               "usgs",
                                               "plum",
                                               "walker_branch",
                                               "panola",
                                               "hbef") |
                                 site_code %in% c("BLWA",
                                                  "FLNT",
                                                  "MAYF",
                                                  "POSE",
                                                  "TOMB",
                                                  "WALK") ~ "ETemperateForest",
                                 domain == "bonanza" |
                                 site_code == "CARI" ~ "Taiga",
                                 domain == "luquillo" |
                                 site_code %in% c("CUPE",
                                                  "GUIL") ~ "TropicalWetForest",
                                 domain == "arctic" |
                                 site_code == "OKSR" ~ "Tundra",
                                 domain %in% c("krycklan",
                                               "sleepers",
                                               "trout_lake") ~ "NorthernForest",
                                 domain == "mcmurdo" ~ "Antarctic",
                                 site_code %in% c("ARIK",
                                                  "PRIN") ~ "GreatPlains"))

#### NO3 Regression ####

# Filter down to analyte of interest.
no3_data <- mean_N_VWM_annual20_nonexp %>%
    filter(analyte_N == "NO3_N") %>%
    # and join with watershed characteristics
    left_join(ms_ws_attr) %>%
    mutate(nlcd_wetland = nlcd_wetland_herb + nlcd_wetland_wood,
           nlcd_dev = nlcd_dev_hi + nlcd_dev_med + nlcd_dev_low + nlcd_dev_open)

# Examine variables.
hist(log(no3_data$mean_annual_VWM_mgL)) # needs a log transformation
hist(log(no3_data$ws_area_ha)) # needs a log transformation
hist(no3_data$slope_mean) # ok
hist(log(no3_data$nlcd_wetland)) # needs a log transformation
hist(log(no3_data$nlcd_dev)) # needs a log transformation
hist(no3_data$temp_mean) # ok
hist(no3_data$precip_mean) # ok
hist(no3_data$gpp_CONUS_30m_mean) # ok
hist(no3_data$N_flux_mean) # ok
ggplot(no3_data, aes(x = ecoregion,
                     y = log(mean_annual_VWM_mgL))) +
    geom_boxplot() +
    geom_jitter() +
    theme_bw() # ecoregion appears an appropriate random intercept

# And examine for possible correlations.
no3_select <- no3_data %>%
    # Need to take care of 0s in the following columns
    mutate(nlcd_wetland_ed = case_when(nlcd_wetland == 0 ~ 0.0001,
                                       TRUE ~ nlcd_wetland),
           nlcd_dev_ed = case_when(nlcd_dev == 0 ~ 0.0001,
                                   TRUE ~ nlcd_dev)) %>%
    mutate(log_NO3 = log(mean_annual_VWM_mgL),
           log_area = log(ws_area_ha),
           log_wetland = log(nlcd_wetland_ed),
           log_dev = log(nlcd_dev_ed)) %>%
    select(log_NO3, log_area, slope_mean, log_wetland,
           log_dev, temp_mean, precip_mean, gpp_CONUS_30m_mean,
           N_flux_mean, ecoregion)

ggpairs(no3_select) # GPP and Temperature appear strongly correlated (0.84).
# So proceeding with only temperature in the model for now.

# Scale all variables so effect sizes are comparable.
no3_scaled <- no3_select %>%
    mutate(scale_NO3 = scale(log_NO3),
           scale_area = scale(log_area),
           scale_slope = scale(slope_mean),
           scale_wetland = scale(log_wetland),
           scale_dev = scale(log_dev),
           scale_temp = scale(temp_mean),
           scale_precip = scale(precip_mean),
           scale_gpp = scale(gpp_CONUS_30m_mean),
           scale_dep = scale(N_flux_mean)) %>%
    select(scale_NO3, scale_area, scale_slope,
           scale_wetland, scale_dev, scale_temp,
           scale_precip, scale_gpp, scale_dep, ecoregion)

# First, building simple linear regression.
lm1 <- lm(scale_NO3 ~ scale_area + scale_slope + scale_wetland +
              scale_temp + scale_precip + scale_dep,
          data = no3_scaled)
summary(lm1) # summary of results
plot(lm1) # examine residuals - look good!

# Refit with GLS to better compare with LMEM
# And need to remove all instances of missingness
lm2 <- gls(scale_NO3 ~ scale_area + scale_slope + scale_wetland +
           scale_temp + scale_precip + scale_dep,
           data = drop_na(no3_scaled))

# Fit multi-level model to account for variation w/i ecoregion
lmm <- lme(scale_NO3 ~ scale_area + scale_slope + scale_wetland +
           scale_temp + scale_precip + scale_dep,
           random = ~1|ecoregion,
           data = drop_na(no3_scaled))

# Compare the model structures
AIC(lm2, lmm) # LMEM is better

# Examine residuals and results
plot(lmm)
qqnorm(lmm) # look alright!
summary(lmm)

# Examine multi-level model with gpp in place of temperature
lmm_gpp <- lme(scale_NO3 ~ scale_area + scale_slope + scale_wetland +
               scale_gpp + scale_precip + scale_dep,
           random = ~1|ecoregion,
           data = drop_na(no3_scaled))
summary(lmm_gpp) # same outcome, gpp not sig.

#### NH3 Regression ####

# Filter down to analyte of interest.
nh3_data <- mean_N_VWM_annual20_nonexp %>%
    filter(analyte_N == "NH3_N") %>%
    # and join with watershed characteristics
    left_join(ms_ws_attr) %>%
    mutate(nlcd_wetland = nlcd_wetland_herb + nlcd_wetland_wood,
           nlcd_dev = nlcd_dev_hi + nlcd_dev_med + nlcd_dev_low + nlcd_dev_open)

# Examine variables.
hist(log(nh3_data$mean_annual_VWM_mgL)) # needs a log transformation
hist(log(nh3_data$ws_area_ha)) # needs a log transformation
hist(nh3_data$slope_mean) # ok
hist(log(nh3_data$nlcd_wetland)) # needs a log transformation
hist(log(nh3_data$nlcd_dev)) # needs a log transformation
hist(nh3_data$temp_mean) # ok
hist(nh3_data$precip_mean) # ok
hist(nh3_data$gpp_CONUS_30m_mean) # ok
hist(nh3_data$N_flux_mean) # ok
ggplot(nh3_data, aes(x = ecoregion,
                     y = log(mean_annual_VWM_mgL))) +
    geom_boxplot() +
    geom_jitter() +
    theme_bw() # ecoregion still appears appropriate

# And examine for possible correlations.
nh3_select <- nh3_data %>%
    # Need to take care of 0s in the following columns
    mutate(nlcd_wetland_ed = case_when(nlcd_wetland == 0 ~ 0.0001,
                                       TRUE ~ nlcd_wetland),
           nlcd_dev_ed = case_when(nlcd_dev == 0 ~ 0.0001,
                                   TRUE ~ nlcd_dev),
           mean_annual_VWM_mgL_ed = case_when(mean_annual_VWM_mgL == 0 ~ 0.000000001,
                                              TRUE ~ mean_annual_VWM_mgL)) %>%
    mutate(log_NH3 = log(mean_annual_VWM_mgL_ed),
           log_area = log(ws_area_ha),
           log_wetland = log(nlcd_wetland_ed),
           log_dev = log(nlcd_dev_ed)) %>%
    select(log_NH3, log_area, slope_mean, log_wetland,
           log_dev, temp_mean, precip_mean, gpp_CONUS_30m_mean,
           N_flux_mean, ecoregion)

ggpairs(nh3_select) # GPP and Temperature again strongly correlated (0.81).
# So proceeding with only temperature in the model for now.

# Scale all variables so effect sizes are comparable.
nh3_scaled <- nh3_select %>%
    mutate(scale_NH3 = scale(log_NH3),
           scale_area = scale(log_area),
           scale_slope = scale(slope_mean),
           scale_wetland = scale(log_wetland),
           scale_dev = scale(log_dev),
           scale_temp = scale(temp_mean),
           scale_precip = scale(precip_mean),
           scale_gpp = scale(gpp_CONUS_30m_mean),
           scale_dep = scale(N_flux_mean)) %>%
    select(scale_NH3, scale_area, scale_slope,
           scale_wetland, scale_dev, scale_temp,
           scale_precip, scale_gpp, scale_dep, ecoregion)

# First, building simple linear regression.
lm1.2 <- lm(scale_NH3 ~ scale_area + scale_slope + scale_wetland +
            scale_temp + scale_precip + scale_dep,
          data = nh3_scaled)
summary(lm1.2) # summary of results
plot(lm1.2) # examine residuals - need to remove that ONO2 site that had
# a zero value, it's a clear outlier
lm1.2.2 <- lm(scale_NH3 ~ scale_area + scale_slope + scale_wetland +
              scale_temp + scale_precip + scale_dep,
            data = nh3_scaled[-45,])
summary(lm1.2.2) # summary of results
plot(lm1.2.2) # examine residuals - MUCH better fit

# Refit with GLS to better compare with LMEM
# And need to remove all instances of missingness
lm2.2 <- gls(scale_NH3 ~ scale_area + scale_slope + scale_wetland +
             scale_temp + scale_precip + scale_dep,
           data = drop_na(nh3_scaled[-45,]))

# Fit multi-level model to account for variation w/i ecoregion
lmm.2 <- lme(scale_NH3 ~ scale_area + scale_slope + scale_wetland +
             scale_temp + scale_precip + scale_dep,
           random = ~1|ecoregion,
           data = drop_na(nh3_scaled[-45,]))

# Compare the model structures
AIC(lm2.2, lmm.2) # LMEM is better

# Examine residuals and results
plot(lmm.2)
qqnorm(lmm.2) # looks good again!
summary(lmm.2)

# Examine multi-level model with gpp in place of temperature
lmm.2_gpp <- lme(scale_NH3 ~ scale_area + scale_slope + scale_wetland +
                 scale_gpp + scale_precip + scale_dep,
               random = ~1|ecoregion,
               data = drop_na(nh3_scaled[-45,]))
summary(lmm.2_gpp) # temp appears to be a stronger predictor.

#### TDN Regression ####

# Filter down to analyte of interest.
tdn_data <- mean_N_VWM_annual20_nonexp %>%
    filter(analyte_N == "TDN") %>%
    # and join with watershed characteristics
    left_join(ms_ws_attr) %>%
    mutate(nlcd_wetland = nlcd_wetland_herb + nlcd_wetland_wood,
           nlcd_dev = nlcd_dev_hi + nlcd_dev_med + nlcd_dev_low + nlcd_dev_open)

# Examine variables.
hist(log(tdn_data$mean_annual_VWM_mgL)) # needs a log transformation
hist(log(tdn_data$ws_area_ha)) # needs a log transformation
hist(tdn_data$slope_mean) # ok
hist(log(tdn_data$nlcd_wetland)) # needs a log transformation
hist(log(tdn_data$nlcd_dev)) # needs a log transformation
hist(tdn_data$temp_mean) # ok
hist(tdn_data$precip_mean) # ok
hist(tdn_data$gpp_CONUS_30m_mean) # ok
hist(tdn_data$N_flux_mean) # ok
ggplot(tdn_data, aes(x = ecoregion,
                     y = log(mean_annual_VWM_mgL))) +
    geom_boxplot() +
    geom_jitter() +
    theme_bw() # ecoregion still appropriate

# And examine for possible correlations.
tdn_select <- tdn_data %>%
    # Need to take care of 0s in the following columns
    mutate(nlcd_wetland_ed = case_when(nlcd_wetland == 0 ~ 0.0001,
                                       TRUE ~ nlcd_wetland),
           nlcd_dev_ed = case_when(nlcd_dev == 0 ~ 0.0001,
                                   TRUE ~ nlcd_dev),
           mean_annual_VWM_mgL_ed = case_when(mean_annual_VWM_mgL == 0 ~ 0.000000001,
                                              TRUE ~ mean_annual_VWM_mgL)) %>%
    mutate(log_TDN = log(mean_annual_VWM_mgL_ed),
           log_area = log(ws_area_ha),
           log_wetland = log(nlcd_wetland_ed),
           log_dev = log(nlcd_dev_ed)) %>%
    select(log_TDN, log_area, slope_mean, log_wetland,
           log_dev, temp_mean, precip_mean, gpp_CONUS_30m_mean,
           N_flux_mean, ecoregion)

ggpairs(tdn_select) # GPP and Temperature again strongly correlated (0.84).
# So proceeding with only temperature in the model for now.

# Scale all variables so effect sizes are comparable.
tdn_scaled <- tdn_select %>%
    mutate(scale_TDN = scale(log_TDN),
           scale_area = scale(log_area),
           scale_slope = scale(slope_mean),
           scale_wetland = scale(log_wetland),
           scale_dev = scale(log_dev),
           scale_temp = scale(temp_mean),
           scale_precip = scale(precip_mean),
           scale_gpp = scale(gpp_CONUS_30m_mean),
           scale_dep = scale(N_flux_mean)) %>%
    select(scale_TDN, scale_area, scale_slope,
           scale_wetland, scale_dev, scale_temp,
           scale_precip, scale_gpp, scale_dep, ecoregion)

# First, building simple linear regression.
lm1.3 <- lm(scale_TDN ~ scale_area + scale_slope + scale_wetland +
            scale_temp + scale_precip + scale_dep,
            data = tdn_scaled)
summary(lm1.3) # summary of results
plot(lm1.3) # examine residuals - look good!

# Refit with GLS to better compare with LMEM
# And need to remove all instances of missingness
lm2.3 <- gls(scale_TDN ~ scale_area + scale_slope + scale_wetland +
             scale_temp + scale_precip + scale_dep,
             data = drop_na(tdn_scaled))

# Fit multi-level model to account for variation w/i ecoregion
lmm.3 <- lme(scale_TDN ~ scale_area + scale_slope + scale_wetland +
             scale_temp + scale_precip + scale_dep,
             random = ~1|ecoregion,
             data = drop_na(tdn_scaled))

# Compare the model structures
AIC(lm2.3, lmm.3) # LMEM is better yet again

# Examine residuals and results
plot(lmm.3)
qqnorm(lmm.3) # looking good!
summary(lmm.3)

# Examine multi-level model with gpp in place of temperature
lmm.3_gpp <- lme(scale_TDN ~ scale_area + scale_slope + scale_wetland +
                 scale_gpp + scale_precip + scale_dep,
                 random = ~1|ecoregion,
                 data = drop_na(tdn_scaled))
summary(lmm.3_gpp) # same outcome, gpp not sig.

# End of script.
