# Nitrogen data viz script

#### README ####
# The following script will create sequential
# figures for use in Heili's 2024 AGU talk to
# discuss MacroSheds as a project as well as
# annual & monthly nitrogen trends.

# It will also create figures for Heili's poster
# at the 2025 Gordon Research Conference and,
# ultimately, the N manuscript.

#### Load packages ####
library(here)
library(ggridges)
source(here('src', 'setup.R'))

#### Load data ####

# Datasets for AGU figures:
# Annual
# All sites for which we could calculate annual VWM
# n_annual_vwm <- readRDS("data_working/nitrogen_annual_VWM.rds")

# Sites that were actually used in annual-scale trend analyses
# n_annual_vwm_filtered <- readRDS("data_working/nitrogen_annual_VWM_good.rds")

# And annual trends that were calculated.
# n_annual_trends <- readRDS("data_working/nitrogen_annual_trends.rds")

# Including monthly data.
# n_monthly_vwm <- readRDS("data_working/nitrogen_monthly_VWM.rds")

# n_monthly_vwm_filtered <- readRDS("data_working/nitrogen_monthly_VWM_good.rds")

# As well as monthly trends.
# n_monthly_trends <- readRDS("data_working/nitrogen_monthly_trends.rds")

# Datasets for MANUSCRIPT figures (from `metrics.R`):
# Annual
# All sites and analytes for which we could calculate annual VWMs
N_VWM_annual <- readRDS("data_working/N_VWM_annual.rds")

# All sites and analytes for which we could calculate monthly VWMs
N_VWM_monthly <- readRDS("data_working/N_VWM_monthly.rds")

# All sites and analytes for which we could calculate seasonal VWMs
N_VWM_seasonal <- readRDS("data_working/N_VWM_seasonal.rds")

# All sites and analytes for which we could fit annual CQ models.
N_CQ_annual <- readRDS("data_working/N_CQ_annual.rds")

# And seasonal CQ models.
N_CQ_seasonal <- readRDS("data_working/N_CQ_seasonal.rds")

# And seasonal CQ models for the most recent decade.
N_CQ_seasonal20 <- readRDS("data_working/N_CQ_seasonal20.rds")

# And decadal CQ models.
N_CQ_decadal <- readRDS("data_working/N_CQ_decadal.rds")

# NO3 annual trends
no3_trends_ann <- readRDS("data_working/no3_trends_annual.rds")

# data used to estimate NO3 annual trends
no3_data_for_trends_ann <- readRDS("data_working/no3_data_for_trends_annual.rds")

# NH3 annual trends
nh3_trends_ann <- readRDS("data_working/nh3_trends_annual.rds")

# data used to estimate NH3 annual trends
nh3_data_for_trends_ann <- readRDS("data_working/nh3_data_for_trends_annual.rds")

# TDN annual trends
tdn_trends_ann <- readRDS("data_working/tdn_trends_annual.rds")

# data used to estimate TDN annual trends
tdn_data_for_trends_ann <- readRDS("data_working/tdn_data_for_trends_annual.rds")

# N Deposition annual trends
ndep_trends_ann <- readRDS("data_working/ndep_trends_annual.rds")

# data used to estimate N deposition annual trends
ndep_data_for_trends_ann <- readRDS("data_working/ndep_data_for_trends_annual.rds")

# Greenness annual trends
green_trends_ann <- readRDS("data_working/green_trends_annual.rds")

# data used to estimate greennness annual trends
green_data_for_trends_ann <- readRDS("data_working/green_data_for_trends_annual.rds")

# Climate trends (from mega_zipper_data.R script)
# note, this was updated/re-run 10/8/26 to use Nic's new dataset
clim_trends <- read_csv("data_working/trends/full_prisim_climate.csv")

# Discharge metrics
q_metrics <- readRDS("data_working/discharge_metrics_siteyear.rds")

#### AGU Figures ####

##### HB only #####

# This is to illustrate the W6 record at Hubbard Brook, which
# many will be familiar with.
(fig1 <- ggplot(n_annual_vwm %>%
                    # create new column to delineate W6 @ HB
                    mutate(select = factor(case_when(site_code == "w6" ~ "YES",
                                                     TRUE ~ "NO"),
                                           levels = c("YES", "NO"))) %>%
                    filter(analyte == "nitrate_N") %>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    color = select)) +
     geom_line(linewidth = 1) +
     scale_color_manual(values = c("black", "transparent")) +
     labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
     theme_bw() +
     theme(legend.position = "none",
           axis.title.y = element_markdown(),
           text = element_text(size = 20)))

# ggsave(fig1,
#        filename = "figures/agu_fig1.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### All sites #####

site_count <- n_annual_vwm %>%
    filter(analyte == "nitrate_N") %>%
    select(site_code) %>%
    distinct()

# generating additional base year dataset
# so as to remove the connecting lines on the plot
years <- seq(from = 1964, to = 2024, by = 1)
yrs_rep <- rep(years, times = 183)
site_rep <- rep(site_count$site_code, each = 61)
full_site_years <- as.data.frame(cbind(yrs_rep, site_rep))

full_site_years <- full_site_years %>%
    rename(water_year = yrs_rep,
           site_code = site_rep) %>%
    mutate(water_year = as.numeric(water_year)) %>%
    mutate(analyte = "nitrate_N")

# This is the base plot that all others should be built
# around since it includes all possible sites (n = 183).
(fig2 <- ggplot(n_annual_vwm %>%
                    filter(analyte == "nitrate_N") %>%
                    # removing 4 outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                    # add full site-years in to help with
                    # plotting lines properly
                    dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL)) +
     geom_line(aes(group = site_code),
               linewidth = 1, color = "black") +
     labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
     theme_bw() +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20)))

# ggsave(fig2,
#        filename = "figures/agu_fig2.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### Remove exp. sites #####

n_annual_vwm <- n_annual_vwm %>%
    left_join(., ms_site_data)

site_counts_experimental <- n_annual_vwm %>%
    filter(analyte == "nitrate_N") %>%
    select(site_code, ws_status) %>%
    distinct() %>%
    count(ws_status) %>%
    ungroup()

# Color by experimental (n = 36) and
# non-experimental (n = 145) sites.
(fig3 <- ggplot(n_annual_vwm %>%
                    # removes Bonanza Creek sites only
                    drop_na(ws_status) %>%
                    filter(analyte == "nitrate_N") %>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                    # add full site-years in to help with
                    # plotting lines properly
                    dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    group = site_code,
                    color = ws_status)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("transparent", "black")) +
        labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig3,
#        filename = "figures/agu_fig3.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### Trends #####

# This will include only sites with sufficient
# data for trends (i.e., min. 10 mos per year
# and 10 years within a 20 year timeframe, n = 26).

# First, to keep the same base plot we need to create a
# set of site-wys that passed our filters that we'll use
# to create a new column.
passed <- paste(n_annual_vwm_filtered$site_code,
                n_annual_vwm_filtered$water_year,
                sep = "_")

# Joining this with the site metadata df too to combine
# information about experimental/non-experimental sites
# and those that passed the filters in the same column.
n_annual_vwm <- n_annual_vwm %>%
    mutate(site_code_wy = paste(site_code, water_year, sep = "_")) %>%
    mutate(keep = factor(case_when(site_code_wy %in% passed ~ "YES",
                                    TRUE ~ "NO"),
                         levels = c("YES", "NO"))) %>%
    # also need to combine with experimental watershed filter
    mutate(keepkeep = factor(case_when(keep == "YES" &
                                       ws_status == "non-experimental" ~ "YES",
                                       TRUE ~ "NO"),
                             levels = c("YES", "NO")))

site_counts_sufficientdata <- n_annual_vwm %>%
    select(site_code, keepkeep) %>%
    distinct() %>%
    count(keepkeep) %>%
    ungroup()

(fig4 <- ggplot(n_annual_vwm %>%
                    filter(analyte == "nitrate_N")%>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                    # add full site-years in to help with
                    # plotting lines properly
                    dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    color = keepkeep,
                    group = site_code)) +
     geom_line(linewidth = 1) +
     scale_color_manual(values = c("black", "transparent")) +
     labs(x = "Water Year",
          y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
     theme_bw() +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20),
           legend.position = "none"))

# ggsave(fig4,
#        filename = "figures/agu_fig4.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# And now to actually color said trends.
# Pull out trends for NO3 at sites.
sites_trends <- n_annual_trends %>%
    filter(var == "nitrate_N") %>%
    select(site_code, group)

n_annual_vwm <- n_annual_vwm %>%
    left_join(., sites_trends) %>%
    # adding yet another column that combines the filters
    # of interest - passed/exp/sig
    mutate(keepkeepkeep = factor(case_when(keepkeep == "YES" &
                                               group == "sig. increasing" ~ "increasing",
                                           keepkeep == "YES" &
                                               group == "sig. decreasing" ~ "decreasing",
                                           keepkeep == "YES" &
                                               group == "no trend" ~ "no trend",
                                           TRUE ~ "remove"),
                                 levels = c("increasing",
                                            "decreasing",
                                            "no trend",
                                            "remove")))

site_counts_trends <- n_annual_vwm %>%
    select(site_code, keepkeepkeep) %>%
    distinct() %>%
    count(keepkeepkeep) %>%
    ungroup()

(fig5a <- ggplot(n_annual_vwm %>%
                    filter(analyte == "nitrate_N")%>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                     # add full site-years in to help with
                     # plotting lines properly
                     dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    group = factor(site_code),
                    color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                     "transparent")) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a,
#        filename = "figures/agu_fig5a.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

sen <- function(..., weights = NULL) {
    mblm::mblm(...)
}

# Highlighting positive trends at BES sites.
(fig5a1 <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years) %>%
                      mutate(width = case_when(site_code %in% c("BARN",
                                                                "MCDN") ~ "BES",
                                               TRUE ~ "Other")),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code),
                      color = keepkeepkeep,
                      linewidth = width)) +
        geom_line() +
        scale_linewidth_manual(values = c(3, 1)) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        # Add increasing trends at BES
        # geom_smooth(data = n_annual_vwm %>%
        # # create additional column to designate which lms show
        # mutate(model = factor(case_when(site_code %in% c("BARN", "MCDN") ~ "YES",
        #                                                 TRUE ~ "NO"),
        #                                       levels = c("YES", "NO"))) %>%
        #                 filter(model == "YES"),
        #             aes(x = water_year,
        #                 y = annual_vwm_mgL,
        #                 group = site_code),
        #             method = sen,
        #             se = F,
        #             color = "#F48849FF",
        #             linewidth = 2) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a1,
#        filename = "figures/agu_fig5a1.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# Now, creating zoomed in version of only decreasing sites.
list_keep <- sites_trends %>%
    filter(group == "sig. decreasing")

list_keep <- list_remove$site_code

(fig5a_zoom <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      filter(keepkeepkeep == "decreasing") %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code))) +
        xlim(c(1964, 2024)) +
        geom_line(linewidth = 1, color = "#0D0887FF") +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a_zoom,
#        filename = "figures/agu_fig5a_zoom.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# Highlighting negative trends at LUQ sites.
# And removing positive/no trend sites to help with interpretation.
(fig5a2 <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      filter(keepkeepkeep == "decreasing") %>%
                      mutate(coloring = case_when(site_code %in% c("Q1","Q2","Q3") ~ "LUQ",
                                                  TRUE ~ "Other")) %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code),
                      alpha = coloring,
                      linewidth = coloring)) +
        xlim(c(1964, 2024)) +
        geom_line(color = "#0D0887FF") +
        scale_alpha_manual(values = c(1, 0.2)) +
        scale_linewidth_manual(values = c(3, 1)) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a2,
#        filename = "figures/agu_fig5a2.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# Highlighting negative trends at HB sites.
(fig5a3 <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      filter(keepkeepkeep == "decreasing") %>%
                  mutate(coloring = case_when(site_code %in% c("w3","w6","w7", "w8", "w9") ~ "HB",
                                              TRUE ~ "Other")) %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code),
                      alpha = coloring,
                      linewidth = coloring)) +
        xlim(c(1964, 2024)) +
        geom_line(color = "#0D0887FF") +
        scale_alpha_manual(values = c(1, 0.2)) +
        scale_linewidth_manual(values = c(3, 1)) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a3,
#        filename = "figures/agu_fig5a3.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

#### Monthly #####

site_count_monthly <- n_monthly_vwm %>%
    filter(analyte == "nitrate_N") %>%
    select(site_code) %>%
    distinct()

# generating additional base year dataset
# so as to remove the connecting lines on the plot
years <- seq(from = 1964, to = 2024, by = 1)
yrs_rep <- rep(years, times = 183)
site_rep <- rep(site_count_monthly$site_code, each = 61)
full_site_years <- as.data.frame(cbind(yrs_rep, site_rep))

full_site_years <- full_site_years %>%
    rename(year = yrs_rep,
           site_code = site_rep) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(analyte = "nitrate_N")

# Also want to filter out unused data right off the bat.
n_monthly_vwm <- n_monthly_vwm %>%
    left_join(., ms_site_data)

# Create set of site-wys that passed our filters that we'll use
# to create a new column.
passed_m <- paste(n_monthly_vwm_filtered$site_code,
                n_monthly_vwm_filtered$year,
                sep = "_")

# Joining this with the site metadata df too to combine
# information about experimental/non-experimental sites
# and those that passed the filters in the same column.
n_monthly_vwm <- n_monthly_vwm %>%
    mutate(site_code_y = paste(site_code, year, sep = "_")) %>%
    mutate(keep = factor(case_when(site_code_y %in% passed_m ~ "YES",
                                   TRUE ~ "NO"),
                         levels = c("YES", "NO"))) %>%
    # also need to combine with experimental watershed filter
    mutate(keepkeep = factor(case_when(keep == "YES" &
                                           ws_status == "non-experimental" ~ "YES",
                                       TRUE ~ "NO"),
                             levels = c("YES", "NO")))

# Pull out trends for NO3 at sites.
sites_trends_m <- n_monthly_trends %>%
    filter(var == "nitrate_N") %>%
    select(site_code, group)

n_monthly_vwm <- n_monthly_vwm %>%
    left_join(., sites_trends_m) %>%
    # adding yet another column that combines the filters
    # of interest - passed/exp/sig
    mutate(keepkeepkeep = factor(case_when(keepkeep == "YES" &
                                               group == "sig. increasing" ~ "increasing",
                                           keepkeep == "YES" &
                                               group == "sig. decreasing" ~ "decreasing",
                                           keepkeep == "YES" &
                                               group %in% c("ns. increasing",
                                                            "ns. decreasing",
                                                            "no trend")~ "no trend",
                                           TRUE ~ "remove"),
                                 levels = c("increasing",
                                            "decreasing",
                                            "no trend",
                                            "remove")))

# This is the base plot that all others should be built
# around since it includes all possible sites (n = 185).
(fig_monthly <- ggplot(n_monthly_vwm %>%
                     filter(analyte == "nitrate_N")%>%
                     filter(month == 6) %>%
                     # removing outliers for plotting ease
                     filter(monthly_vwm_mgL > 0.000001),
                 aes(x = year,
                     y = monthly_vwm_mgL,
                     group = factor(site_code),
                     color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        labs(x = "Water Year",
             y = "Mean June VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig_monthly,
#        filename = "figures/agu_figmonthly_A.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# And alternatively adding all linear models to the figure
# for those with significant trend.
(fig5b <- ggplot(n_annual_vwm %>%
                     filter(analyte == "nitrate_N")%>%
                     # removing outliers for plotting ease
                     filter(annual_vwm_mgL > 0.000001) %>%
                     filter(annual_vwm_mgL < 40),
                 aes(x = water_year,
                     y = annual_vwm_mgL,
                     group = factor(site_code),
                     color = keepkeepkeep)) +
        geom_point(size = 2, shape = 21) +
        scale_fill_manual(values = c("#55C667FF",
                                     "#404788FF",
                                     "transparent",
                                     "transparent")) +
        geom_smooth(data = n_annual_vwm %>%
                        filter(keepkeepkeep %in% c("increasing",
                                                   "decreasing")),
                    aes(x = water_year,
                        y = annual_vwm_mgL,
                        group = site_code,
                        fill = keepkeepkeep,
                        color = keepkeepkeep),
                    method = "lm",
                    se = F) +
        scale_color_manual(values = c("#55C667FF",
                                      "#404788FF",
                                      "grey70",
                                      "transparent")) +
        labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5b,
#        filename = "figures/agu_fig5b.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### HB Specific #####

# Creating a historical monthly dataset for HB data (1965-1975)
(fig_hb_past <- ggplot(n_monthly_vwm %>%
                           filter(analyte == "nitrate_N") %>%
                           filter(site_code == "w6") %>%
                           filter(year >= 1965) %>%
                           filter(year < 1975),
                       aes(x = factor(month),
                           y = monthly_vwm_mgL)) +
        geom_boxplot(fill = "#0D0887FF",
                     color = "#0D0887FF",
                     alpha = 0.2) +
        ylim(0, 1.25) +
        labs(x = "Month",
             y = "Monthly VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     theme_bw() +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20)))

(fig_hb_present <- ggplot(n_monthly_vwm %>%
                           filter(analyte == "nitrate_N") %>%
                           filter(site_code == "w6") %>%
                           filter(year >= 2010) %>%
                           filter(year < 2020),
                       aes(x = factor(month),
                           y = monthly_vwm_mgL)) +
        geom_boxplot(fill = "#0D0887FF",
                     color = "#0D0887FF",
                     alpha = 0.2) +
        ylim(0, 1.25) +
        labs(x = "Month",
             y = "Monthly VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20)))

(fig_hb <- fig_hb_past + fig_hb_present)

# ggsave(fig_hb,
#        filename = "figures/agu_figHB.jpeg",
#        height = 16,
#        width = 40,
#        units = "cm")

# Making additional W2 figure.
(fig_hb_present_w2 <- ggplot(n_monthly_vwm %>%
                              filter(analyte == "nitrate_N") %>%
                              filter(site_code == "w2") %>%
                              filter(year >= 2010) %>%
                              filter(year < 2020),
                          aes(x = factor(month),
                              y = monthly_vwm_mgL)) +
        geom_boxplot(fill = "#F2C66B",
                     color = "#B0854E",
                     alpha = 0.4) +
        #ylim(0, 1.25) +
        labs(x = "Month",
             y = "Monthly VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20)))

ggsave(fig_hb_present_w2,
       filename = "figures/sfs_figHB.jpeg",
       height = 10,
       width = 20,
       units = "cm")

#### Drivers ####

##### Temperature #####

# Load in climate data.
clim_raw <- read_feather(here('data_raw',
                          'spatial_timeseries_climate.feather'))
clim <- clim_raw %>%
    mutate(year = year(date),
           month = month(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    select(site_code, date, water_year, var, val) %>%
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    mutate(month = month(date))

# Create new columns to add to 41 sites for plotting.
sites_to_plot <- n_annual_vwm %>%
    filter(analyte == "nitrate_N") %>%
    filter(keepkeepkeep %in% c("decreasing",
                              "increasing",
                              "no trend")) %>%
    select(site_code, keepkeepkeep) %>%
    unique()

my41sites <- sites_to_plot$site_code

# Filter climate dataset for sites of interest
clim_trim_annual <- clim %>%
    filter(site_code %in% my41sites) %>%
    group_by(site_code, water_year) %>%
    summarize(mean_ann_temp = mean(temp_median, na.rm = TRUE),
              sum_N_dep = sum(N_flux_mean, na.rm = TRUE)) %>%
    ungroup() %>%
    full_join(sites_to_plot)

(fig6 <- ggplot(clim_trim_annual %>%
                    # filter out strange final year
                    filter(water_year < 2023) %>%
                    mutate(keepkeepkeep = factor(keepkeepkeep)),
                 aes(x = water_year,
                     y = mean_ann_temp,
                     group = factor(site_code),
                     color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        labs(x = "Water Year",
             y = "Mean Annual Temperature (C)") +
        theme_bw() +
        facet_grid(keepkeepkeep~.) +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none",
              strip.background = element_blank(),
              strip.text.y = element_blank()))

# ggsave(fig6,
#        filename = "figures/agu_fig6.jpeg",
#        height = 25,
#        width = 15,
#        units = "cm")

##### Deposition #####

# Need to make new dataset for deposition since it's
# already annualized.
clim_dep <- clim_raw %>%
    filter(var == "N_flux_mean") %>%
    select(site_code, year, var, val) %>%
    pivot_wider(id_cols = c(site_code, year),
                names_from = var, values_from = val, values_fn = mean) %>%
    filter(site_code %in% my41sites) %>%
    full_join(sites_to_plot)

(fig7 <- ggplot(clim_dep %>%
                    mutate(keepkeepkeep = factor(keepkeepkeep)),
                aes(x = year,
                    y = N_flux_mean,
                    group = factor(site_code),
                    color = keepkeepkeep)) +
     geom_line(linewidth = 1) +
     scale_color_manual(values = c("#F48849FF",
                                   "#0D0887FF",
                                   "grey80",
                                   "transparent")) +
     labs(x = "Year",
          y = "Cumulative Annual N Deposition (kg/ha)") +
     theme_bw() +
     facet_grid(keepkeepkeep~.) +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20),
           legend.position = "none",
           strip.background = element_blank(),
           strip.text.y = element_blank()))

# ggsave(fig7,
#        filename = "figures/agu_fig7.jpeg",
#        height = 25,
#        width = 15,
#        units = "cm")

##### Productivity #####

prod_raw <- read_feather(here('data_raw',
                              'spatial_timeseries_vegetation.feather'))

# Filter productivity dataset for sites of interest
prod <- prod_raw %>%
    mutate(year = year(date),
           month = month(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    select(site_code, date, water_year, var, val) %>%
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    mutate(month = month(date))

prod_trim_annual <- prod %>%
    filter(site_code %in% my41sites) %>%
    group_by(site_code, water_year) %>%
    summarize(sum_ann_prod = sum(gpp_CONUS_30m_median, na.rm = TRUE)) %>%
    ungroup() %>%
    full_join(sites_to_plot)

(fig8 <- ggplot(prod_trim_annual %>%
                    # remove weird final years
                    filter(water_year < 2021) %>%
                    # and need to remove LUQ sites
                    # where we don't have GPP
                    filter(sum_ann_prod > 0) %>%
                    mutate(keepkeepkeep = factor(keepkeepkeep)),
                aes(x = water_year,
                    y = sum_ann_prod,
                    group = factor(site_code),
                    color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        labs(x = "Water Year",
             y = "Cumulative Annual GPP (kg C/m<sup>2</sup>)") +
        theme_bw() +
        facet_grid(keepkeepkeep~.) +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none",
              strip.background = element_blank(),
              strip.text.y = element_blank()))

# ggsave(fig8,
#        filename = "figures/agu_fig8.jpeg",
#        height = 25,
#        width = 15,
#        units = "cm")

#### Manuscript Figures ####

##### Data Coverage #####

# Below, we need to figure out a way to display the
# relative coverage of the different analytes, their
# annual VWM concentrations, and the relative certainty
# of these estimates (i.e., no. of observations per site).

# Calculate means of VWM concentrations & number of obs.
mean_N_VWM_annual <- N_VWM_annual %>%
    group_by(site_code, analyte_N) %>%
    summarize(mean_annual_VWM_mgL = mean(annual_vwm_mgL,
                                           na.rm = TRUE),
              mean_annual_obs = mean(n_of_obs_chem,
                                     na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup()

# Set color legend break points.
my_breaks <- c(1, 10, 50)

# This is an initial version of summary figure in the manuscript.
(summaryfig1 <- ggplot(mean_N_VWM_annual,
                      aes(x = mean_annual_VWM_mgL,
                          y = analyte_N,
                          fill = mean_annual_obs,
                          size = total_years)) +
    geom_jitter(width = 0.05, alpha = 0.9, shape = 21) +
    scale_fill_gradientn(colors = c("white", "#FAB455",
                                    "#69B9FA", "#59A3F8",
                                    "#4B9FF7", "#045CB4", "black"),
                          trans = "log",
                         breaks = my_breaks, labels = my_breaks) +
    scale_size_continuous(breaks = my_breaks) +
    scale_x_log10() +
    facet_grid(analyte_N~., scales = "free") +
    labs(x = "Mean Annual Concentration (mg/L)",
         y = "Analyte",
         fill = "Mean No. of Annual Observations",
         size = "Record Length (yrs)") +
    theme_bw() +
    theme(strip.text.y = element_blank(),
          legend.position = "top"))

# ggsave(summaryfig1,
#        filename = "figures/summaryfig_allN.jpeg",
#        height = 20,
#        width = 20,
#        units = "cm")

# Also, wanted to create a data coverage figure for reference.
site_counts <- mean_N_VWM_annual %>%
    count(analyte_N)

(summaryfig2 <- ggplot(N_VWM_annual,
                       aes(x = water_year,
                           y = site_code)) +
        geom_line() +
        facet_grid(.~analyte_N, scales = "free") +
        labs(x = "Year",
             y = "Site") +
        theme_bw() +
        theme(axis.text.y = element_blank()))

# This figure gives a better idea as to data density
# within a given time frame.
ts_counts <- N_VWM_annual %>%
    count(analyte_N, water_year)

(summaryfig3 <- ggplot(ts_counts,
                       aes(x = water_year,
                           y = n)) +
        geom_line() +
        facet_grid(.~analyte_N) +
        labs(x = "Year",
             y = "Site No.") +
        theme_bw())

# ggsave(summaryfig3,
#        filename = "figures/summaryfig_ts_sitecounts.jpeg",
#        height = 10,
#        width = 30,
#        units = "cm")

# Calculate means of VWM concentrations & number of obs.
# BUT FILTER DOWN TO 2010-2020 [INCLUSIVE]
# AND REMOVE EXPERIMENTAL SITES
# for better comparison in the summary figure.
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
    filter(ws_status == "non-experimental") %>%
    # re-order analytes for density
    mutate(analyte_N_f = factor(analyte_N,
                              levels = c("NO3_N", "NH3_N", "DIN",
                                         "TDN", "TN", "NO2_N",
                                         "TPN", "N2O", "TIN",
                                         "TKN")))

# Set color legend break points.
color_breaks <- c(1, 10, 50)
size_breaks <- c(1,5,10)

# Count number of sites per analyte
site_counts20 <- mean_N_VWM_annual20_nonexp %>%
    count(analyte_N)

# And create list with which to add in count annotations
dat_text <- data.frame(
    label = c("n = 142", "n = 116", "n = 116", "n = 84", "n = 60",
              "n = 46", "n = 31", "n = 20", "n = 10", "n = 1"),
    analyte_N = c("NO3_N", "NH3_N", "DIN", "TDN", "TN",
                  "NO2_N", "TPN", "N2O", "TIN", "TKN")) %>%
    mutate(analyte_N_f = factor(analyte_N,
                                levels = c("NO3_N", "NH3_N", "DIN", "TDN", "TN",
                                           "NO2_N", "TPN", "N2O", "TIN", "TKN")))

# Note, the log transformation on the x-axis only removes
# one site's data for one analyte - ONO2 for NH3_N.
(summaryfig4 <- ggplot(mean_N_VWM_annual20_nonexp) +
        geom_jitter(aes(x = mean_annual_VWM_mgL,
                        y = analyte_N_f,
                        fill = mean_annual_obs,
                        size = total_years),
                    width = 0.05, alpha = 0.9, shape = 21) +
        scale_fill_gradientn(colors = c("white", "#FAB455",
                                        "#69B9FA", "#59A3F8",
                                        "#4B9FF7", "#045CB4", "black"),
                             trans = "log",
                             breaks = color_breaks,
                             labels = color_breaks) +
        scale_size_continuous(breaks = size_breaks) +
        scale_x_log10(labels = scales::comma,
                      breaks = c(0.001, 0.01, 0.1, 1, 10)) +
        facet_grid(analyte_N_f~., scales = "free") +
        scale_y_discrete(drop = TRUE) +
        labs(x = "Mean Annual Concentration (mg/L)",
             y = "Analyte",
             fill = "Mean No. of Annual Observations",
             size = "Record Length (yrs)") +
        geom_text(data = dat_text,
                  mapping = aes(x = 20,
                                y = analyte_N_f,
                                label = label)) +
        theme_bw() +
        theme(strip.text.y = element_blank(),
              legend.position = "top"))

# ggsave(summaryfig4,
#        filename = "figures/summaryfig_N_2010_to_2020.jpeg",
#        height = 20,
#        width = 24,
#        units = "cm")

# Now, I also want to create a summary figure of sorts for
# seasonal data, so will do so using a similar style as above.

# Calculate means of VWM concentrations & number of obs.
# BUT FILTER DOWN TO 2010-2020 [INCLUSIVE]
# AND REMOVE EXPERIMENTAL SITES
# for better comparison in the summary figure.
mean_N_VWM_seasonal20 <- N_VWM_seasonal %>%
    filter(season_year > 2009) %>%
    filter(season_year < 2021) %>%
    group_by(site_code, season, analyte_N) %>%
    summarize(mean_seasonal_VWM_mgL = mean(seasonal_vwm_mgL,
                                         na.rm = TRUE),
              mean_seasonal_obs = mean(n_of_obs_chem,
                                     na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup() %>%
    mutate(season = factor(season, levels = c("Spring",
                                              "Summer",
                                              "Fall",
                                              "Winter")))

# Join with site info so that we can filter out experimental sites.
mean_N_VWM_seasonal20 <- left_join(mean_N_VWM_seasonal20, ms_site_data)

mean_N_VWM_seasonal20_nonexp <- mean_N_VWM_seasonal20 %>%
    filter(ws_status == "non-experimental")

# Test figure.
# Set color legend break points.
color_breaks <- c(1, 3, 12)
size_breaks <- c(1,5,10)

# Count number of sites per analyte
site_counts20_seas <- mean_N_VWM_seasonal20_nonexp %>%
    count(analyte_N, season)

# And create list with which to add in count annotations
dat_text_seas <- data.frame(
    position = c(10, 10, 10, 10,
                 0.0002, 0.0002, 0.0002, 0.0002,
                 0.0002, 0.0002, 0.0002, 0.0002),
    label = c("n = 87", "n = 85", "n = 97", "n = 90",
              "n = 113", "n = 115", "n = 129", "n = 119",
              "n = 73", "n = 73", "n = 82", "n = 64"),
    season = c("Spring", "Summer", "Fall", "Winter",
               "Spring", "Summer", "Fall", "Winter",
               "Spring", "Summer", "Fall", "Winter"),
    analyte_N = c("NH3_N", "NH3_N", "NH3_N", "NH3_N",
                  "NO3_N", "NO3_N", "NO3_N", "NO3_N",
                  "TDN", "TDN", "TDN", "TDN"))

# Note, the log transformation on the x-axis only removes
# one site's data for one analyte - ONO2 for NH3_N.
(summaryfig5 <- ggplot(mean_N_VWM_seasonal20_nonexp %>%
                           filter(analyte_N %in% c("NO3_N",
                                                   "NH3_N",
                                                   "TDN")) %>%
                           # remove 3 outliers that caused axis stretch
                           filter(mean_seasonal_VWM_mgL > 0.0000001)) +
        geom_jitter(aes(x = mean_seasonal_VWM_mgL,
                        y = season,
                        fill = mean_seasonal_obs,
                        size = total_years),
                    height = 0.1, alpha = 0.9, shape = 21) +
        scale_fill_gradientn(colors = c("white", "#FAB455",
                                        "#69B9FA", #"#59A3F8",
                                        "#4B9FF7", "#045CB4", "black"),
                             trans = "log",
                             breaks = color_breaks,
                             labels = color_breaks) +
        scale_size_continuous(breaks = size_breaks) +
        scale_x_log10(labels = scales::comma,
                      breaks = c(0.001, 0.01, 0.1, 1, 10)) +
        facet_grid(analyte_N~., scales = "free") +
        labs(x = "Mean Seasonal Concentration (mg/L)",
             y = "Season",
             fill = "Mean No. of Seasonal Observations",
             size = "Record Length (yrs)") +
        geom_text(data = dat_text_seas,
                  mapping = aes(x = position, y = season, label = label)) +
        theme_bw() +
        theme(strip.background = element_rect(colour="NA", fill="NA"),
              legend.position = "top"))

# ggsave(summaryfig5,
#        filename = "figures/summaryfig_N_season_2010_to_2020.jpeg",
#        height = 20,
#        width = 20,
#        units = "cm")

# Since the above figures collapse a bit too much of the variability,
# here is an alternate figure with more of it retained.

###### New Fig 1 ######

# First, join the raw data with site info.
N_VWM_annual_sites <- left_join(N_VWM_annual, ms_site_data)

# And edit for plotting
N_VWM_annual_sites_nonexp <- N_VWM_annual_sites %>%
    # remove experimental sites
    filter(ws_status == "non-experimental") %>%
    # and analytes that are not included
    filter(analyte_N %in% c("NO3_N", "TDN", "NH3_N")) %>%
    mutate(analyte_N = factor(analyte_N,
                              levels = c("NO3_N", "NH3_N", "TDN"))) %>%
    # and add a column with median values (to sort by)
    group_by(site_code, analyte_N) %>%
    mutate(median_N = median(annual_vwm_mgL)) %>%
    ungroup() %>%
    mutate(site_code = factor(site_code))

# Make a separate dataset for preserving the ordering by NO3 values.
NO3_sites_nonexp <- N_VWM_annual_sites_nonexp %>%
    filter(analyte_N == "NO3_N") %>%
    mutate(site_code = factor(site_code)) %>%
    mutate(site_code_NO3 = fct_reorder(site_code,
                                   annual_vwm_mgL,
                                   .fun = median)) %>%
    select(site_code, site_code_NO3) %>%
    unique()

# And join this back on.
N_VWM_annual_sites_nonexp <- left_join(N_VWM_annual_sites_nonexp, NO3_sites_nonexp)

# And finally make a dataset for record length
no_years <- N_VWM_annual_sites_nonexp %>%
    count(site_code, analyte_N)

# And join this back on
N_VWM_annual_sites_nonexp <- left_join(N_VWM_annual_sites_nonexp,
                                       no_years) %>%
    mutate(n = as.numeric(n))

# Attempting another version using interval strips.
(fig_N_3panel_int <- ggplot(N_VWM_annual_sites_nonexp %>%
                                mutate(analyte_label = factor(case_when(analyte_N == "NO3_N" ~ "NO[3]-N",
                                                                 analyte_N == "NH3_N" ~ "NH[3]-N",
                                                                 analyte_N == "TDN" ~ "TDN"),
                                                              levels = c("NO[3]-N",
                                                                         "NH[3]-N",
                                                                         "TDN"))) %>%
                                filter(annual_vwm_mgL > 0),
                            # else the log scale won't work
                        aes(x = annual_vwm_mgL,
                            y = site_code_NO3,
                            color = n)) +
        ggdist::stat_interval(size = 0.5, .width = c(0.95)) +
        scale_color_gradient(low = "gray90", high = "black") +
        #scale_color_brewer(palette = "Greys") +
        stat_summary(geom = "point", fun = median, color = "black",
                     size = 0.5, shape = 16) +
        scale_x_continuous(trans = "log10",
                           labels = scales::label_comma(),
                           limits = c(0.0001, 100)) +
        labs(x = "Annual Volume Weighted Mean\nConcentration (mg/L)",
             y = "Sites",
             color = "Record\nLength (yrs)") +
        theme_classic() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        facet_grid(analyte_label~.,
                   labeller = label_parsed))

ggsave(fig_N_3panel_int,
       filename = "figures/N3panel_intfig.jpeg",
       height = 15,
       width = 10,
       units = "cm")

# calculating stats for manuscript.
counts <- N_VWM_annual_sites_nonexp %>%
    select(site_code, analyte_N) %>%
    unique() %>%
    group_by(analyte_N) %>%
    count() %>%
    ungroup()

summ_stats <- N_VWM_annual_sites_nonexp %>%
    group_by(site_code, analyte_N) %>%
    summarize(median = median(annual_vwm_mgL, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(analyte_N) %>%
    summarize(mean_median = mean(median, na.rm = TRUE),
              sd_median = sd(median, na.rm =TRUE)) %>%
    ungroup()

summ_stats_yrs <- no_years %>%
    group_by(analyte_N) %>%
    summarize(mean_yrs = mean(n, na.rm = TRUE),
              sd_yrs = sd(n, na.rm = TRUE)) %>%
    ungroup()

##### Peak Months #####

# Using a similar workflow as above, I am going to create a paneled
# figure showing peak months for Q and N analytes.

# First, I need to calculate monthly averages
# NOTE- IVE EDITED THIS CODE TO INCLUDE ALL POSSIBLE DATA
# TO SHOW AS AN ALTERNATIVE TO ONLY 2010-2020.
mean_N_VWM_monthly <- N_VWM_monthly %>%
    #filter(water_year > 2009) %>%
    #filter(water_year < 2021) %>%
    group_by(site_code, analyte_N, month) %>%
    summarize(mean_monthly_VWM_mgL = mean(monthly_vwm_mgL,
                                         na.rm = TRUE),
              mean_monthly_obs = mean(n_of_obs_chem,
                                     na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup()

# Join with site info so that we can filter out experimental sites.
mean_N_VWM_monthly <- left_join(mean_N_VWM_monthly, ms_site_data)

mean_N_VWM_monthly_nonexp <- mean_N_VWM_monthly %>%
    filter(ws_status == "non-experimental")

# Also, to impose at least a minimal filter on these data I am going to
# make a list of sites with a minimum of 3 months of data for 6 years in
# this time frame.

# Taking the average of months represented and years in records.
# NOTE NEW FILTER IS MIN. 3 MONTHS AND 3 YEARS
mean_times <- mean_N_VWM_monthly_nonexp %>%
    filter(analyte_N %in% c("NO3_N", "NH3_N", "TDN")) %>%
    group_by(site_code, analyte_N) %>%
    summarize(total_months = n(),
              avg_years = mean(total_years)) %>%
    ungroup()

keep_sites <- mean_times %>%
    mutate(keep = case_when(total_months > 2 &
                                avg_years >= 3 ~ "YES",
                            TRUE ~ "NO")) %>%
    filter(keep == "YES") %>%
    select(site_code, analyte_N)

# And trim down original monthly VWM data to match the above list.
mean_N_VWM_monthly_nonexp_keep <- left_join(keep_sites,
                                            mean_N_VWM_monthly_nonexp)

# Also, need to bring in Q data to do the same.
q_monthly <- q_metrics %>%
    filter(agg_code %in% c("1", "2", "3", "4", "5", "6",
                           "7", "8", "9", "10", "11", "12"))

# calculate monthly averages
# First, I need to calculate monthly averages
# NOTE MAKING SAME ADJUSTMENTS HERE AS ABOVE
mean_q_monthly <- q_monthly %>%
    #filter(water_year > 2009) %>%
    #filter(water_year < 2021) %>%
    group_by(site_code, month) %>%
    summarize(mean_monthly_q = mean(q_mean, na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup() %>%
    filter(total_years >= 3) %>%
    filter(!is.nan(mean_monthly_q)) %>%
    filter(!is.na(month))

# And find peak discharge months.
peak_q_months <- mean_q_monthly %>%
    group_by(site_code) %>%
    slice_max(mean_monthly_q) %>%
    mutate(analyte_N = "Q")

# Calculate peak months.
peak_months <- mean_N_VWM_monthly_nonexp_keep %>%
    group_by(site_code, analyte_N) %>%
    slice_max(mean_monthly_VWM_mgL) %>%
    # and join with Q data above
    full_join(peak_q_months) %>%
    # reorder analytes %>%
    mutate(analyte = factor(analyte_N,
                            levels = c("Q",
                                       "NO3_N",
                                       "NH3_N",
                                       "TDN"))) %>%
    ungroup()

# Peak months figure
(q_peaks <- ggplot(peak_months %>%
                       filter(analyte == "Q"), aes(x = month)) +
    geom_bar(stat = "count", fill = "#4B8FF7") +
    labs(x = "Month", y = "Count", title = "Q") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       limits = c(0,13)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)))

(no3_peaks <- ggplot(peak_months %>%
                       filter(analyte == "NO3_N"), aes(x = month)) +
        geom_bar(stat = "count", fill = "#E7A655") +
        labs(x = "Month", y = "Count", title = "Nitrate") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           limits = c(0,13)) +
        # scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10),
        #                    limits = c(0,10)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)))

(nh3_peaks <- ggplot(peak_months %>%
                         filter(analyte == "NH3_N"), aes(x = month)) +
        geom_bar(stat = "count", fill = "#E38377") +
        labs(x = "Month", y = "Count", title = "Ammonium") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           limits = c(0,13)) +
        # scale_y_continuous(breaks = c(1,2,3,4,5,6),
        #                    limits = c(0,6)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)))

(tdn_peaks <- ggplot(peak_months %>%
                         filter(analyte == "TDN"), aes(x = month)) +
        geom_bar(stat = "count", fill = "#6D4847") +
        labs(x = "Month", y = "Count", title = "Total Dissolved Nitrogen") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           limits = c(0,13)) +
        # scale_y_continuous(breaks = c(1,2,3,4,5,6),
        #                    limits = c(0,6)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)))

# And combine them into a single figure.
(peaks_all <- (q_peaks / no3_peaks / nh3_peaks / tdn_peaks))

# And export figure.
# ggsave(peaks_all,
#        filename = "figures/peak_months_N.jpeg",
#        height = 20,
#        width = 10,
#        units = "cm")

###### New SI Fig ######

# First, I need to calculate monthly averages
# NOTE- I've edited this code again to mirror the Fig 2 (CQ) filters.
N_VWM_monthly20 <- N_VWM_monthly %>%
    # trim to 2010-2020
    filter(water_year > 2009) %>%
    filter(water_year < 2021)

# And see which site-years have min. 8 months represented.
n_month_counts <- N_VWM_monthly20 %>%
    select(site_code, analyte_N, water_year, month, n_of_obs_chem) %>%
    pivot_wider(values_from = n_of_obs_chem,
                names_from = month) %>%
    # reordering just for ease of viewing
    select(site_code, analyte_N, water_year, `1`, `2`, `3`, `4`,
           `5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`) %>%
    # now to add appropriate flags
    mutate(flag_10 = case_when(`10` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_11 = case_when(`11` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_12 = case_when(`12` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_1 = case_when(`1` >= 1 ~ 1,
                              TRUE ~ 0),
           flag_2 = case_when(`2` >= 1 ~ 1,
                              TRUE ~ 0),
           flag_3 = case_when(`3` >= 1 ~ 1,
                              TRUE ~ 0),
           flag_4 = case_when(`4` >= 1 ~ 1,
                              TRUE ~ 0),
           flag_5 = case_when(`5` >= 1 ~ 1,
                              TRUE ~ 0),
           flag_6 = case_when(`6` >= 1 ~ 1,
                              TRUE ~ 0),
           flag_7 = case_when(`7` >= 1 ~ 1,
                              TRUE ~ 0),
           flag_8 = case_when(`8` >= 1 ~ 1,
                              TRUE ~ 0),
           flag_9 = case_when(`9` >= 1 ~ 1,
                              TRUE ~ 0)) %>%
    mutate(flag_months = flag_10 + flag_11 + flag_12 +
               flag_1 + flag_2 + flag_3 +
               flag_4 + flag_5 + flag_6 +
               flag_7 + flag_8 + flag_9)

# And only include sites with an average of 8 months of observations.
n_mos_sites_trim <- n_month_counts %>%
    group_by(site_code, analyte_N) %>%
    summarize(mean_months = mean(flag_months)) %>%
    ungroup() %>%
    filter(mean_months >= 8)

# And use the dataset above to trim down the included sites.

N_VWM_monthly20_8mo <- right_join(N_VWM_monthly20, n_mos_sites_trim)


mean_N_VWM_monthly <- N_VWM_monthly20_8mo%>%
    group_by(site_code, analyte_N, month) %>%
    summarize(mean_monthly_VWM_mgL = mean(monthly_vwm_mgL,
                                          na.rm = TRUE),
              mean_monthly_obs = mean(n_of_obs_chem,
                                      na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup()

# Join with site info so that we can filter out experimental sites.
mean_N_VWM_monthly <- left_join(mean_N_VWM_monthly, ms_site_data)

mean_N_VWM_monthly_nonexp <- mean_N_VWM_monthly %>%
    filter(ws_status == "non-experimental")

# Also, need to bring in Q data to do the same.
q_monthly <- q_metrics %>%
    filter(agg_code %in% c("1", "2", "3", "4", "5", "6",
                           "7", "8", "9", "10", "11", "12"))

# calculate monthly averages
# First, I need to calculate monthly averages
# NOTE MAKING SAME ADJUSTMENTS HERE AS ABOVE
mean_q_monthly <- q_monthly %>%
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    group_by(site_code, month) %>%
    summarize(mean_monthly_q = mean(q_mean, na.rm = TRUE),
              total_years = as.numeric(n())) %>%
    ungroup() %>%
    filter(!is.nan(mean_monthly_q)) %>%
    filter(!is.na(month))

# And find peak discharge months.
peak_q_months <- mean_q_monthly %>%
    group_by(site_code) %>%
    slice_max(mean_monthly_q) %>%
    mutate(analyte_N = "Q")

# Calculate peak months.
peak_months <- mean_N_VWM_monthly_nonexp %>%
    filter(analyte_N %in% c("NO3_N", "NH3_N", "TDN")) %>%
    group_by(site_code, analyte_N) %>%
    slice_max(mean_monthly_VWM_mgL) %>%
    # and join with Q data above
    full_join(peak_q_months) %>%
    # reorder analytes %>%
    mutate(analyte = factor(analyte_N,
                            levels = c("Q",
                                       "NO3_N",
                                       "NH3_N",
                                       "TDN"))) %>%
    ungroup()

# Peak months figure
(q_peaks <- ggplot(peak_months %>%
                       filter(analyte == "Q"), aes(x = month)) +
        geom_bar(stat = "count", color = "black", fill = "white",
                 alpha = 0.9) +
        geom_text(x = 10, y = 20, label = "Q") +
        geom_text(x = 10, y = 15, label = "n = 121") +
        labs(x = "Peak Q Month", y = "Site Count") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           limits = c(0,13)) +
        scale_y_continuous(breaks = c(0,5,10,15,20),
                           limits = c(0,23)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank()))

(no3_peaks <- ggplot(peak_months %>%
                         filter(analyte == "NO3_N"), aes(x = month)) +
        geom_bar(stat = "count", color = "black", fill = "white") +
        geom_text(x = 10, y = 20, label = expression(paste(NO[3], "-N"))) +
        geom_text(x = 10, y = 15, label = "n = 72") +
        labs(x = expression(paste("Peak ", NO[3], "-N Month")),
             y = "Site Count") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           limits = c(0,13)) +
        scale_y_continuous(breaks = c(0,5,10,15,20),
                           limits = c(0,23)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank()))

(nh3_peaks <- ggplot(peak_months %>%
                         filter(analyte == "NH3_N"), aes(x = month)) +
        geom_bar(stat = "count", color = "black", fill = "white") +
        geom_text(x = 10, y = 20, label = expression(paste(NH[3], "-N"))) +
        geom_text(x = 10, y = 15, label = "n = 50") +
        labs(x = expression(paste("Peak ", NH[3], "-N Month")),
             y = "Site Count") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           limits = c(0,13)) +
        scale_y_continuous(breaks = c(0,5,10,15,20),
                           limits = c(0,23)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank()))

(tdn_peaks <- ggplot(peak_months %>%
                         filter(analyte == "TDN"), aes(x = month)) +
        geom_bar(stat = "count", color = "black", fill = "white") +
        geom_text(x = 10, y = 20, label = "TDN") +
        geom_text(x = 10, y = 15, label = "n = 35") +
        labs(x = "Peak Month", y = "Site Count") +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           limits = c(0,13)) +
        scale_y_continuous(breaks = c(0,5,10,15,20),
                           limits = c(0,23)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()))

# And combine them into a single figure.
(peaks_all_bw <- (q_peaks / no3_peaks / nh3_peaks / tdn_peaks))

# And export figure.
ggsave(peaks_all_bw,
       filename = "figures/peak_months_N_bw.jpeg",
       height = 15,
       width = 7,
       units = "cm")

# Peak ridgeline plot
(fig_ridge <- ggplot(peak_months %>%
                         mutate(analyte_f = factor(analyte_N,
                                                   levels = c("TDN",
                                                              "NH3_N",
                                                              "NO3_N",
                                                              "Q"))), aes(x = month,
                                      y = analyte_f)) +
    ggridges::geom_density_ridges(alpha = 0.5, fill = "grey50",
                                  jittered_points = TRUE,
                                  point_size = 1, point_fill = "white") +
    ggridges::theme_ridges() +
    scale_x_continuous(limits = c(1,12),
                       breaks = c(2,4,6,8,10)) +
    theme(legend.position = "none"))

##### Site Attributes #####

# Also going to make plots to investigate watershed and climate
# characteristics with magnitude of nitrogen at each site.
# Focusing in on 2010-2020 data to keep things comparable.

# First need to make a guiding site list.
# List of sites included in the 2010-2020 dataset
annual_sites20 <- unique(mean_N_VWM_annual20_nonexp$site_code)

mean_N_VWM_annual <- left_join(mean_N_VWM_annual, ms_site_data)

mean_N_VWM_annual_nonexp <- mean_N_VWM_annual %>%
    filter(ws_status == "non-experimental")

# And a list of sites included in the *full* dataset
annual_sites <- unique(mean_N_VWM_annual_nonexp$site_code)

all_sites <- as.data.frame(annual_sites) %>%
    mutate(timeframe = case_when(annual_sites %in% annual_sites20 ~ "recent",
                                 TRUE ~ "full"))

# Load in raw climate data.
clim_raw <- read_feather(here('data_raw',
                              'ms',
                              'v2',
                              'spatial_timeseries_climate.feather'))
# This takes a bit, so be patient.

clim <- clim_raw %>%
    # adds water year column
    mutate(year = year(date),
           month = month(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    # trims columns of interest
    select(site_code, date, water_year, var, val) %>%
    # pivots and aggregates daily data
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    # adds month column
    mutate(month = month(date))

# Aggregate climate data annually.
# Full dataset
clim_annual <- clim %>%
    # select only for sites of interest
    filter(site_code %in% annual_sites) %>%
    # calculate annual metrics
    group_by(site_code, water_year) %>%
    summarize(mean_ann_temp = mean(temp_median, na.rm = TRUE),
              sum_ann_ppt = sum(precip_median, na.rm = TRUE)) %>%
    ungroup()

# Only 2010-2020
clim_annual20 <- clim %>%
    # select only for sites of interest
    filter(site_code %in% annual_sites20) %>%
    # select for only timeframe of interest (2010-2020)
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    # calculate annual metrics
    group_by(site_code, water_year) %>%
    summarize(mean_ann_temp20 = mean(temp_median, na.rm = TRUE),
              sum_ann_ppt20 = sum(precip_median, na.rm = TRUE)) %>%
    ungroup()

# Further aggregates climate data to site-level.
# Full dataset
clim_decadal <- clim_annual %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_temp = mean(mean_ann_temp, na.rm = TRUE),
              mean_sum_ann_ppt = mean(sum_ann_ppt, na.rm = TRUE)) %>%
    ungroup()

# Only 2010-2020
clim_decadal20 <- clim_annual20 %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_temp20 = mean(mean_ann_temp20, na.rm = TRUE),
              mean_sum_ann_ppt20 = mean(sum_ann_ppt20, na.rm = TRUE)) %>%
    ungroup()

# And join with N data.
N_clim_data <- full_join(mean_N_VWM_annual_nonexp, clim_decadal)
N_clim_data20 <- full_join(mean_N_VWM_annual20_nonexp, clim_decadal20)

# N deposition
# New dataset since deposition is already annualized
# Full dataset
clim_deponly <- clim_raw %>%
    filter(var == "N_flux_mean") %>%
    select(site_code, year, var, val) %>%
    pivot_wider(id_cols = c(site_code, year),
                names_from = var, values_from = val, values_fn = mean) %>%
    filter(site_code %in% annual_sites)

# Only 2010-2020
clim_deponly20 <- clim_raw %>%
    filter(var == "N_flux_mean") %>%
    select(site_code, year, var, val) %>%
    pivot_wider(id_cols = c(site_code, year),
                names_from = var, values_from = val, values_fn = mean) %>%
    filter(site_code %in% annual_sites20) %>%
    filter(year > 2009) %>%
    filter(year < 2021)

# Further aggregates deposition data at site-level.
# Full dataset
dep_decadal <- clim_deponly %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_Ndep = mean(N_flux_mean, na.rm = TRUE)) %>%
    ungroup()

# Only 2010-2020
dep_decadal20 <- clim_deponly20 %>%
    group_by(site_code) %>%
    summarize(mean_mean_ann_Ndep20 = mean(N_flux_mean, na.rm = TRUE)) %>%
    ungroup()

# And join with N data.
N_dep_data <- full_join(mean_N_VWM_annual_nonexp, dep_decadal)
N_dep_data20 <- full_join(mean_N_VWM_annual20_nonexp, dep_decadal20)

# Adding GPP to the list of available WS attributes
prod_raw <- read_feather(here('data_raw',
                              'ms',
                              'v2',
                              'spatial_timeseries_vegetation.feather'))

# Filter productivity dataset for sites of interest
prod <- prod_raw %>%
    mutate(year = year(date),
           month = month(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    select(site_code, date, water_year, var, val) %>%
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    mutate(month = month(date))

# Annually aggregate productivity data.
# Full dataset
prod_annual <- prod %>%
    filter(site_code %in% annual_sites) %>%
    group_by(site_code, water_year) %>%
    summarize(sum_ann_prod = sum(gpp_CONUS_30m_median, na.rm = TRUE)) %>%
    ungroup()

# Only 2010-2020
prod_annual20 <- prod %>%
    filter(site_code %in% annual_sites20) %>%
    filter(water_year > 2009) %>%
    filter(water_year < 2021) %>%
    group_by(site_code, water_year) %>%
    summarize(sum_ann_prod20 = sum(gpp_CONUS_30m_median, na.rm = TRUE)) %>%
    ungroup()

# Further aggregates productivity data at the site-level
# Full dataset
prod_decadal <- prod_annual %>%
    group_by(site_code) %>%
    summarize(mean_sum_ann_prod = mean(sum_ann_prod, na.rm = TRUE)) %>%
    ungroup()

# Only 2010-2020
prod_decadal20 <- prod_annual20 %>%
    group_by(site_code) %>%
    summarize(mean_sum_ann_prod20 = mean(sum_ann_prod20, na.rm = TRUE)) %>%
    ungroup()

# And join with N data.
N_gpp_data <- full_join(mean_N_VWM_annual, prod_decadal)
N_gpp_data20 <- full_join(mean_N_VWM_annual20, prod_decadal20)

# Overall WS attributes

# Calculated total wetland cover.
ms_ws_attr <- ms_ws_attr %>%
    mutate(nlcd_wetland = nlcd_water + nlcd_wetland_herb + nlcd_wetland_wood,
           nlcd_dev = nlcd_dev_hi + nlcd_dev_med + nlcd_dev_low + nlcd_dev_open)

ms_ws_select <- ms_ws_attr %>%
    select(network, domain, site_code, area, slope_mean, elev_mean,
           nlcd_dev, nlcd_wetland)

# Realizing it likely makes more sense to plot overall distribution
# of attributes rather than by analyte, so going to work on that below.

# Aggregate all datasets together.
# Need to start with the most complete dataset - ws attributes.
N_data <- ms_ws_select %>% filter(site_code %in% annual_sites)
N_data <- left_join(N_data, dep_decadal)
N_data <- left_join(N_data, prod_decadal)
N_data <- left_join(N_data, clim_decadal)

N_data20 <- ms_ws_select %>% filter(site_code %in% annual_sites20)
N_data20 <- left_join(N_data20, dep_decadal20)
N_data20 <- left_join(N_data20, prod_decadal20)
N_data20 <- left_join(N_data20, clim_decadal20)

(deppanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = mean_mean_ann_Ndep),
                     color = "NA",
                     fill = "#E29244", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = mean_mean_ann_Ndep20),
                     color = "#E29244", linewidth = 2) +
        labs(x = "Cumulative Annual N Deposition (kg/ha)",
             y = "Density") +
        theme_bw())

(temppanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = mean_mean_ann_temp),
                     color = "NA",
                     fill = "#D46F10", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = mean_mean_ann_temp20),
                     color = "#D46F10", linewidth = 2) +
        labs(x = "Mean Annual Temperature (C)") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(gpppanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = mean_sum_ann_prod),
                     color = "NA",
                     fill = "#4CA49E", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = mean_sum_ann_prod20),
                     color = "#4CA49E", linewidth = 2) +
        labs(x = "Cumulative Annual GPP (kg C/m<sup>2</sup>)",
             y = "Density") +
        theme_bw() +
        theme(axis.title.x = element_markdown()))

(pptpanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = mean_sum_ann_ppt),
                     color = "NA",
                     fill =  "#69B9FA", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = mean_sum_ann_ppt20),
                     color = "#69B9FA", linewidth = 2) +
        labs(x = "Cumulative Annual Precipitation (mm)") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(sizepanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = area),
                     color = "NA",
                     fill =  "#59A3F8", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = area),
                     color = "#59A3F8", linewidth = 2) +
        scale_x_log10() +
        labs(x = "Watershed Area (ha)",
             y = "Density") +
        theme_bw())

(slopepanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = slope_mean),
                     color = "NA",
                     fill =  "#4B8FF7", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = slope_mean),
                     color = "#4B8FF7", linewidth = 2) +
        labs(x = "Watershed Slope (degrees)") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(elevpanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = elev_mean),
                     na.rm = TRUE,
                     color = "NA",
                     fill = "#5A7ECB", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = elev_mean),
                     color = "#5A7ECB", linewidth = 2) +
        labs(x = "Watershed Elevation (m)") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(wetlandpanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = nlcd_wetland),
                     color = "NA",
                     fill =  "#6B6D9F", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = nlcd_wetland),
                     color = "#6B6D9F", linewidth = 2) +
        scale_x_log10() +
        labs(x = "% Wetland Land Cover") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(devpanel <- ggplot(N_data) +
        # historical histogram
        geom_density(aes(x = nlcd_dev),
                     color = "NA",
                     fill =  "#1E2F46", alpha = 0.5) +
        # 2010-2020 histogram
        geom_density(data = N_data20,
                     mapping = aes(x = nlcd_dev),
                     color = "#1E2F46", linewidth = 2) +
        scale_x_log10() +
        labs(x = "% Developed Land Cover") +
        theme_bw() +
        theme(axis.title.y=element_blank()))

(ws_attributes <- (deppanel + temppanel + pptpanel)/
        (gpppanel + wetlandpanel + devpanel)/
        (sizepanel + slopepanel + elevpanel))

# ggsave(ws_attributes,
#        filename = "figures/summaryfig_attributes_dens_hist_v_recent.jpeg",
#        height = 16,
#        width = 24,
#        units = "cm")

# Making a larger correlation plot that is more inclusive of
# site-level attributes and comparing to annual N concentrations.

# First, need to create the base dataset using 2010-2020 data only
# and excluding experimental sites.
# Combine N dataset with calculated attributes & climate indices.
N_corr_dat <- left_join(mean_N_VWM_annual20_nonexp, ms_ws_attr)
N_corr_dat <- left_join(N_corr_dat, dep_decadal20)
N_corr_dat <- left_join(N_corr_dat, prod_decadal20)
N_corr_dat <- left_join(N_corr_dat, clim_decadal20)

# And we'll trim this down to the three most common analytes
# as well as the columns of interest.
N_corr_dat_trim <- N_corr_dat %>%
    filter(analyte_N %in% c("NO3_N", "NH3_N", "TDN")) %>%
    select(site_code, analyte_N, mean_annual_VWM_mgL, mean_annual_obs,
           latitude, longitude, ws_area_ha, slope_mean, elev_mean,
           aspect_mean, nlcd_wetland, nlcd_dev, mean_mean_ann_Ndep20,
           mean_sum_ann_prod20, mean_mean_ann_temp20, mean_sum_ann_ppt20,
           first_record, last_record)

# And then pivot it for easy plot creation.
N_corr_dat_trim_long <- N_corr_dat_trim %>%
    select(-first_record,-last_record) %>%
    pivot_longer(cols = mean_annual_obs:mean_sum_ann_ppt20,
                 names_to = "var", values_to = "val")

# Now, to make a plot.
(corr <- ggplot(N_corr_dat_trim_long,
                 aes(x = val,
                     y = mean_annual_VWM_mgL)) +
        geom_point() +
        labs(y = "VWM (mg/L)") +
        facet_grid(var~analyte_N, scales = "free") +
        theme_bw()) # well, that didn't work, so individual plots it is...

# First, I'm going to calculate correlations between all of these
# data.
corr_values <- N_corr_dat_trim %>%
    group_by(analyte_N) %>%
    summarize(corr_obs = cor(mean_annual_obs, mean_annual_VWM_mgL),
              corr_lat = cor(latitude, mean_annual_VWM_mgL),
              corr_lon = cor(longitude, mean_annual_VWM_mgL),
              corr_area = cor(ws_area_ha, mean_annual_VWM_mgL,
                              use = "pairwise.complete.obs"),
              corr_slope = cor(slope_mean, mean_annual_VWM_mgL,
                               use = "pairwise.complete.obs"),
              corr_elev = cor(elev_mean, mean_annual_VWM_mgL,
                              use = "pairwise.complete.obs"),
              corr_aspect = cor(aspect_mean, mean_annual_VWM_mgL,
                                use = "pairwise.complete.obs"),
              corr_wet = cor(nlcd_wetland, mean_annual_VWM_mgL,
                             use = "pairwise.complete.obs"),
              corr_dev = cor(nlcd_dev, mean_annual_VWM_mgL,
                             use = "pairwise.complete.obs"),
              corr_temp = cor(mean_mean_ann_temp20, mean_annual_VWM_mgL,
                              use = "pairwise.complete.obs"),
              corr_ppt = cor(mean_sum_ann_ppt20, mean_annual_VWM_mgL,
                             use = "pairwise.complete.obs"),
              corr_gpp = cor(mean_sum_ann_prod20, mean_annual_VWM_mgL,
                             use = "pairwise.complete.obs"),
              corr_dep = cor(mean_mean_ann_Ndep20, mean_annual_VWM_mgL,
                             use = "pairwise.complete.obs")) %>%
    ungroup()
# Will include corr values on plots below if >0.3.

# Building individual plots to combine later.
(corr1 <- ggplot(N_corr_dat_trim,
                 aes(x = first_record,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Date of 1st Record", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw())

(corr2 <- ggplot(N_corr_dat_trim,
                 aes(x = last_record,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Date of Last Record", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank()))

(corr3 <- ggplot(N_corr_dat_trim,
                 aes(x = mean_annual_obs,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Mean Annual Obs.", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank()))

(corr4 <- ggplot(N_corr_dat_trim,
                 aes(x = latitude,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Latitude", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank()))

(corr5 <- ggplot(N_corr_dat_trim,
                 aes(x = longitude,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Longitude", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank()))

(corr6 <- ggplot(N_corr_dat_trim,
                 aes(x = ws_area_ha,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        scale_x_log10() +
        labs(x = "Area (ha)", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(axis.title.y = element_blank()))

(corr7 <- ggplot(N_corr_dat_trim,
                 aes(x = slope_mean,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Mean Slope", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

(corr8 <- ggplot(N_corr_dat_trim,
                 aes(x = elev_mean,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Mean Elevation", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

(corr9 <- ggplot(N_corr_dat_trim,
                 aes(x = aspect_mean,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "Mean Aspect", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

(corr10 <- ggplot(N_corr_dat_trim,
                 aes(x = nlcd_wetland,
                     y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        labs(x = "% Wetland", y = "VWM (mg/L)") +
        scale_x_log10() +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

# List with which to add in correlation annotations
dat_corr_dev <- data.frame(
    xposition = c(25, 10, 10),
    yposition = c(0.18, 5, 5),
    label = c("0.33", " ", " "),
    analyte_N = c("NH3_N", "NO3_N", "TDN"))

(corr11 <- ggplot(N_corr_dat_trim,
                  aes(x = nlcd_dev,
                      y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        geom_text(data = dat_corr_dev,
                  mapping = aes(x = xposition,
                                y = yposition,
                                label = label),
                  color = "red", fontface = "bold") +
        labs(x = "% Developed", y = "VWM (mg/L)") +
        scale_x_log10() +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(axis.title.y = element_blank()))

dat_corr_temp <- data.frame(
    xposition = c(15, 12.5, 12.5),
    yposition = c(0.175, 5, 4.75),
    label = c(" ", "0.32", "0.41"),
    analyte_N = c("NH3_N", "NO3_N", "TDN"))

(corr12 <- ggplot(N_corr_dat_trim,
                  aes(x = mean_mean_ann_temp20,
                      y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        geom_text(data = dat_corr_temp,
                  mapping = aes(x = xposition,
                                y = yposition,
                                label = label),
                  color = "red", fontface = "bold") +
        labs(x = "Mean Annual Temp. (°C)", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

dat_corr_ppt <- data.frame(
    xposition = c(2000, 2000, 2000),
    yposition = c(0.175, 5, 4.75),
    label = c(" ", " ", "-0.42"),
    analyte_N = c("NH3_N", "NO3_N", "TDN"))

(corr13 <- ggplot(N_corr_dat_trim,
                  aes(x = mean_sum_ann_ppt20,
                      y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        geom_text(data = dat_corr_ppt,
                  mapping = aes(x = xposition,
                                y = yposition,
                                label = label),
                  color = "blue", fontface = "bold") +
        labs(x = "Mean Annual Ppt. (mm)", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        xlim(0, 3000) +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

dat_corr_gpp <- data.frame(
    xposition = c(1.75, 1.75, 1.75),
    yposition = c(0.18, 5, 4.75),
    label = c("0.34", " ", " "),
    analyte_N = c("NH3_N", "NO3_N", "TDN"))

(corr14 <- ggplot(N_corr_dat_trim,
                  aes(x = mean_sum_ann_prod20,
                      y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        geom_text(data = dat_corr_gpp,
                  mapping = aes(x = xposition,
                                y = yposition,
                                label = label),
                  color = "red", fontface = "bold") +
        labs(x = "Mean Annual Prod. (kg C/m^2)", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank()))

dat_corr_dep <- data.frame(
    xposition = c(5, 5, 5),
    yposition = c(0.18, 5, 4.75),
    label = c("0.34", " ", "-0.40"),
    analyte_N = c("NH3_N", "NO3_N", "TDN"),
    sign = c("positive", NA, "negative"))

(corr15 <- ggplot(N_corr_dat_trim,
                  aes(x = mean_mean_ann_Ndep20,
                      y = mean_annual_VWM_mgL)) +
        geom_point(shape = 21) +
        geom_text(data = dat_corr_dep,
                  mapping = aes(x = xposition,
                                y = yposition,
                                label = label,
                                color = sign),
                  fontface = "bold") +
        scale_color_manual(values = c("blue", "red")) +
        labs(x = "Mean Annual N Dep. (kg/ha)", y = "VWM (mg/L)") +
        facet_wrap(.~analyte_N, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "none"))

# And combine them into a single figure.
(corr_all <- ((corr1 / corr2 / corr3 / corr4 / corr5) |
                 (corr6 / corr7 / corr8 / corr9 / corr10) |
                    (corr11 / corr12 / corr13 / corr14 / corr15)))

# And export figure.
# ggsave(corr_all,
#        filename = "figures/correlationfig_N_2010_to_2020.jpeg",
#        height = 25,
#        width = 40,
#        units = "cm")

##### CQ #####

###### Seasonal (SI Fig) #######

# Filter down to desired analytes and minimum of 10 observations,
# using data from the past 10 years.
N_CQ_trim <- N_CQ_seasonal20 %>%
    filter(analyte_N %in% c("NO3_N", "NH3_N", "TDN")) %>%
    filter(n_of_obs > 9)

# And join with site data to filter out experimental sites.
N_CQ_trim <- left_join(N_CQ_trim, ms_site_data)

N_CQ_trim_nonexp <- N_CQ_trim %>%
    filter(ws_status == "non-experimental") %>%
    mutate(analyte_label = factor(case_when(analyte_N == "NO3_N" ~ "NO[3]-N",
                                            analyte_N == "NH3_N" ~ "NH[3]-N",
                                            analyte_N == "TDN" ~ "TDN"),
                                  levels = c("NO[3]-N",
                                             "NH[3]-N",
                                             "TDN"))) %>%
    # Renaming seasons
    mutate(season_ed = factor(case_when(season == "Fall" ~ "Autumn",
                                 TRUE ~ season),
                              levels = c("Spring", "Summer",
                                         "Autumn", "Winter")))

(n_cq_seas <- ggplot(N_CQ_trim_nonexp, aes(x = cq_slope)) +
        geom_vline(xintercept = 0.2, linetype = "dashed") +
        geom_vline(xintercept = -0.2, linetype = "dashed") +
        geom_histogram(fill = "white", color = "black",
                       bins = 30, alpha = 0.9) +
        annotate("rect", xmin = -0.2, xmax = 0.2, ymin = 0, ymax = 40,
                 alpha = 0.35, fill = "#FFAA00") +
        scale_x_continuous(trans = pseudo_log_trans(base = 10),
                           breaks = c(-5, -2, -1, 0, 1, 2, 5)) +
        scale_y_continuous(trans = pseudo_log_trans(base = 10),
                           breaks = c(1, 4, 10, 40)) +
        facet_grid(analyte_label~season_ed,
                   label = label_parsed) +
        labs(x = "Seasonal C-Q Slope", y = "Site Count") +
        theme_bw() +
        theme(strip.background = element_rect(colour="NA", fill="NA"),
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()))

# ggsave(n_cq_seas,
#        filename = "figures/seasonal_cq_slope_2010_to_2020.jpeg",
#        height = 16,
#        width = 20,
#        units = "cm")

# Examining outlier sites
flushing <- N_CQ_trim_nonexp %>%
    mutate(flushing = case_when(cq_slope > 0.2 ~ 1,
                                TRUE ~ 0)) %>%
    group_by(site_code, analyte_N) %>%
    summarize(sum_flushing = sum(flushing)) %>%
    ungroup() %>%
    filter(sum_flushing >= 3) %>%
    left_join(ms_site_data)

diluting <- N_CQ_trim_nonexp %>%
    mutate(diluting = case_when(cq_slope < -0.2 ~ 1,
                                TRUE ~ 0)) %>%
    group_by(site_code, analyte_N) %>%
    summarize(sum_diluting = sum(diluting)) %>%
    ungroup() %>%
    filter(sum_diluting >= 3) %>%
    left_join(ms_site_data)


(n_int_seas <- ggplot(N_CQ_trim_nonexp) +
        geom_histogram(aes(x = cq_int,
                           fill = ..x..),
                       color = "black", bins = 30) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_fill_gradient2(low = "#045CB4",
                             mid = "white",
                             high = "#FAB455",
                             midpoint = 0) +
        facet_grid(analyte_N~season, scales = "free") +
        labs(x = "Seasonal C-Q Intercept", y = "Site Count") +
        theme_bw() +
        theme(strip.background = element_rect(colour="NA", fill="NA"),
              legend.position = "none"))

# ggsave(n_int_seas,
#        filename = "figures/seasonal_cq_int_2010_to_2020.jpeg",
#        height = 16,
#        width = 16,
#        units = "cm")

# Examining percent of diluting (slope = -1),
# chemostatic (slope = 0), and flushing (slope = 1)
regimes <- N_CQ_trim_nonexp %>%
    mutate(group = case_when(cq_slope <= -1 ~ "diluting",
                             cq_slope >-1 &
                                 cq_slope < 1 ~ "chemostatic",
                             cq_slope >=1 ~ "flushing")) %>%
    group_by(analyte_N, season, group) %>%
    summarize(count = n()) %>%
    ungroup()

mean_slopes <- N_CQ_trim_nonexp %>%
    mutate(group = case_when(cq_slope <= -1 ~ "diluting",
                             cq_slope >-1 &
                                 cq_slope < 1 ~ "chemostatic",
                             cq_slope >=1 ~ "flushing")) %>%
    group_by(analyte_N, season) %>%
    summarize(mean_slope = mean(cq_slope)) %>%
    ungroup()

# And examine if CQ slopes are at all related to
# annual or seasonal VWM values.

# First, need to join annual VWMs with CQ values above.
# Both of these datasets include only 2010-2020.
cq_seas_and_annual_vwm <- left_join(N_CQ_trim_nonexp,
                                    mean_N_VWM_annual20_nonexp)

(fig_cq_v_vwm <- ggplot(cq_seas_and_annual_vwm) +
    geom_point(aes(x = mean_annual_VWM_mgL,
                   y = cq_slope)) +
    scale_x_log10() +
    facet_grid(analyte_label~season,
               label = label_parsed) +
    labs(x = "Annual VWM (mg/L)", y = "Seasonal C-Q Slope") +
    theme_bw() +
    theme(strip.background = element_rect(colour="NA", fill="NA"),
          legend.position = "none")) # no apparent relationship

cq_seas_and_seas_vwm <- left_join(N_CQ_trim_nonexp,
                                  mean_N_VWM_seasonal20_nonexp)

(fig_cq_v_vwm_seas <- ggplot(cq_seas_and_seas_vwm) +
        geom_point(aes(x = mean_seasonal_VWM_mgL,
                       y = cq_slope)) +
        scale_x_log10() +
        facet_grid(analyte_label~season,
                   label = label_parsed) +
        labs(x = "Seasonal VWM (mg/L)",
             y = "Seasonal C-Q Slope") +
        theme_bw() +
        theme(strip.background = element_rect(colour="NA", fill="NA"),
              legend.position = "none")) # again no relationship

###### Decadal #######

# Filter down to desired analytes and minimum of 10 observations.
N_CQ_dec_trim <- N_CQ_decadal %>%
    filter(analyte_N %in% c("NO3_N", "NH3_N", "TDN")) %>%
    filter(n_of_obs > 9)

# And join with site data to filter out experimental sites.
N_CQ_dec_trim <- left_join(N_CQ_dec_trim, ms_site_data)

N_CQ_dec_trim_nonexp <- N_CQ_dec_trim %>%
    filter(ws_status == "non-experimental")

(n_cq_dec <- ggplot(N_CQ_dec_trim_nonexp) +
        geom_histogram(aes(x = cq_slope,
                           fill = ..x..),
                       color = "black", bins = 30) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        # this omits an outlier, but doing this for consistent viewing
        xlim(-2, 2) +
        scale_fill_gradient2(low = "#045CB4",
                             mid = "white",
                             high = "#FAB455",
                             midpoint = 0) +
        facet_grid(analyte_N~decade, scales = "free") +
        labs(x = "Decadal C-Q Slope", y = "Site Count") +
        theme_bw() +
        theme(strip.background = element_rect(colour="NA", fill="NA"),
              legend.position = "none"))

# ggsave(n_cq_dec,
#        filename = "figures/decadal_cq_slope.jpeg",
#        height = 16,
#        width = 32,
#        units = "cm")

(n_int_dec <- ggplot(N_CQ_dec_trim_nonexp) +
        geom_histogram(aes(x = cq_int,
                           fill = ..x..),
                       color = "black", bins = 30) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        # this omits an outlier, but doing this for consistent viewing
        xlim(-15, 5) +
        scale_fill_gradient2(low = "#045CB4",
                             mid = "white",
                             high = "#FAB455",
                             midpoint = 0) +
        facet_grid(analyte_N~decade, scales = "free") +
        labs(x = "Decadal C-Q Intercept", y = "Site Count") +
        theme_bw() +
        theme(strip.background = element_rect(colour="NA", fill="NA"),
              legend.position = "none"))

# ggsave(n_int_dec,
#        filename = "figures/decadal_cq_intercept.jpeg",
#        height = 16,
#        width = 32,
#        units = "cm")

##### Trends #####

###### New Fig 2 ######
# Plotting actual data used to estimate trends so as not
# to over-represent the data.

# join together the annual VWM and trend data for NO3
no3_all <- no3_data_for_trends_ann %>%
    left_join(ms_site_data) %>%
    left_join(no3_trends_ann) %>%
    arrange(flag)

(fig4a1 <- ggplot(no3_all %>%
                    filter(ws_status == "non-experimental") %>%
                        drop_na(flag),
                aes(x = water_year,
                    y = val,
                    color = flag,
                    group = site_code,
                    alpha = flag)) +
     geom_line(linewidth = 0.5) +
     scale_color_manual(values = c("blue", "gray56")) +
     scale_alpha_manual(values = c(1, 0.5)) +
     labs(x = "Water Year",
          y = "NO<sub>3</sub><sup>-</sup>-N (mg/L)",
          color = "Trend") +
     scale_y_log10(labels = label_comma(accuracy = 0.0001),
                   limits = c(0.00001, 10)) +
     theme_bw() +
     theme(axis.title.y = element_markdown(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title.x = element_blank(),
           axis.text.x = element_blank(),
           legend.position = "none"))

no3_counts <- no3_all %>%
    filter(ws_status == "non-experimental") %>%
    drop_na(flag) %>%
    group_by(site_code) %>%
    slice_head() %>%
    ungroup() %>%
    count(flag) %>%
    mutate(flag = factor(flag, levels = c("non-significant",
                          "decreasing"))) %>%
    mutate(analyte = "NO3_N")

(fig4a2 <- ggplot(no3_counts, aes(fill = flag,
                                 y = n, x = analyte)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual(values = c("gray56", "blue")) +
    geom_text(y = 10, label = "n = 21", color = "white") +
    geom_text(y = 35, label = "n = 29", color = "white") +
    labs(y = "Site Count") +
    theme_bw() +
    theme(legend.position = "none",
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))

(fig4a <- (fig4a1 | fig4a2) +
        plot_layout(widths = c(5, 1)))

# join together the annual VWM and trend data for NH3
nh3_all <- nh3_data_for_trends_ann %>%
    left_join(ms_site_data) %>%
    left_join(nh3_trends_ann) %>%
    arrange(flag)

(fig4b1 <- ggplot(nh3_all %>%
                        filter(ws_status == "non-experimental") %>%
                        drop_na(flag) %>%
                     # replacing 2 HO00 years with small, non-zero
                     # values so the log scale works
                        mutate(val_ed = case_when(val == 0 ~ 0.00001,
                                                  TRUE ~ val)),
                    aes(x = water_year,
                        y = val_ed,
                        color = flag,
                        group = site_code,
                        alpha = flag)) +
        geom_line(linewidth = 0.5) +
        scale_color_manual(values = c("blue", "gray56")) +
        scale_alpha_manual(values = c(1, 0.5)) +
        labs(x = "Water Year",
             y = "NH<sub>3</sub>-N (mg/L)",
             color = "Trend") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001),
                      limits = c(0.00001, 10)) +
                     # breaks = c(0.00001, 0.001, 0.1)) +
        #scale_y_continuous(trans = pseudo_log_trans(base = 10)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              legend.position = "none"))

nh3_counts <- nh3_all %>%
    filter(ws_status == "non-experimental") %>%
    drop_na(flag) %>%
    group_by(site_code) %>%
    slice_head() %>%
    ungroup() %>%
    count(flag) %>%
    mutate(flag = factor(flag, levels = c("non-significant",
                                          "decreasing"))) %>%
    mutate(analyte = "NH3_N")

(fig4b2 <- ggplot(nh3_counts, aes(fill = flag,
                                  y = n, x = analyte)) +
        geom_bar(position = "stack", stat = "identity") +
        scale_fill_manual(values = c("gray56", "blue")) +
        geom_text(y = 5, label = "n = 11", color = "white") +
        geom_text(y = 23, label = "n = 23", color = "white") +
        labs(y = "Site Count") +
        theme_bw() +
        theme(legend.position = "none",
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()))

(fig4b <- (fig4b1 | fig4b2) +
        plot_layout(widths = c(5, 1)))

# join together the annual VWM and trend data for TDN
tdn_all <- tdn_data_for_trends_ann %>%
    left_join(ms_site_data) %>%
    left_join(tdn_trends_ann) %>%
    arrange(flag)

(fig4c1 <- ggplot(tdn_all %>%
                     filter(ws_status == "non-experimental") %>%
                     drop_na(flag),
                 aes(x = water_year,
                     y = val,
                     color = flag,
                     group = site_code,
                     alpha = flag)) +
        geom_line(linewidth = 0.5) +
        scale_color_manual(values = c("blue",
                                      "darkorange",
                                      "gray56")) +
        scale_alpha_manual(values = c(1, 1, 0.5)) +
        labs(x = "Water Year",
             y = "TDN (mg/L)",
             color = "Trend",
             alpha = "Trend") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001),
                      limits = c(0.00001, 10)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "bottom"))

tdn_counts <- tdn_all %>%
    filter(ws_status == "non-experimental") %>%
    drop_na(flag) %>%
    group_by(site_code) %>%
    slice_head() %>%
    ungroup() %>%
    count(flag) %>%
    mutate(flag = factor(flag, levels = c("non-significant",
                                          "increasing",
                                          "decreasing"))) %>%
    mutate(analyte = "TDN")

(fig4c2 <- ggplot(tdn_counts, aes(fill = flag,
                                  y = n, x = analyte)) +
        geom_bar(position = "stack", stat = "identity") +
        scale_fill_manual(values = c("gray56", "darkorange", "blue")) +
        geom_text(y = 5, label = "n = 9", color = "white") +
        geom_text(y = 16, label = "n = 13", color = "white") +
        geom_text(y = 9.5, label = "n = 1", color = "white") +
        labs(y = "Site Count") +
        theme_bw() +
        theme(legend.position = "none",
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()))

(fig4c <- (fig4c1 | fig4c2) +
        plot_layout(widths = c(5, 1)))

(fig4_new <- fig4a / fig4b / fig4c)

ggsave(fig4_new,
       filename = "figures/trends_no3_nh3_tdn_1980onwards.jpeg",
       height = 16,
       width = 16,
       units = "cm")

# count of sites with all three analytes present
no3_sites <- no3_trends_ann %>%
    left_join(ms_site_data) %>%
    filter(ws_status == "non-experimental") %>%
    drop_na(trend) %>%
    select(site_code) %>%
    mutate(no3 = 1)

nh3_sites <- nh3_trends_ann %>%
    left_join(ms_site_data) %>%
    filter(ws_status == "non-experimental") %>%
    drop_na(trend) %>%
    select(site_code) %>%
    mutate(nh3 = 1)

tdn_sites <- tdn_trends_ann %>%
    left_join(ms_site_data) %>%
    filter(ws_status == "non-experimental") %>%
    drop_na(trend) %>%
    select(site_code) %>%
    mutate(tdn = 1)

all_sites_w_trends <- full_join(no3_sites, nh3_sites)
all_sites_w_trends <- full_join(all_sites_w_trends, tdn_sites) %>%
    mutate(all = no3 + nh3 + tdn)

# also report median, max, and min declining trends
no3_declines <- no3_trends_ann %>%
    left_join(ms_site_data) %>%
    filter(ws_status == "non-experimental") %>%
    filter(flag == "decreasing")
nh3_declines <- nh3_trends_ann %>%
    left_join(ms_site_data) %>%
    filter(ws_status == "non-experimental") %>%
    filter(flag == "decreasing")
tdn_declines <- tdn_trends_ann %>%
    left_join(ms_site_data) %>%
    filter(ws_status == "non-experimental") %>%
    filter(flag == "decreasing")

# And summarize which sites had declines across analytes
no3_declines_sites <- no3_declines %>%
    select(site_code) %>%
    mutate(no3 = 1)
nh3_declines_sites <- nh3_declines %>%
    select(site_code) %>%
    mutate(nh3 = 1)
tdn_declines_sites <- tdn_declines %>%
    select(site_code) %>%
    mutate(tdn = 1)
all_sites_decreasing <- full_join(no3_declines_sites,
                                  nh3_declines_sites)
all_sites_decreasing <- full_join(all_sites_decreasing,
                                  tdn_declines_sites) %>%
    mutate(across(everything(), \(x) replace_na(x, 0))) %>%
    mutate(all = no3 + nh3 + tdn) %>%
    left_join(ms_site_data)
min_two_decreasing <- all_sites_decreasing %>%
    filter(all >=2)

###### New Fig 3 ######

# compare seasonal changes over the decades
# across a few sites with longer records

# Hubbard Brook
(fig_HB <- ggplot(N_VWM_monthly %>%
                    filter(analyte_N == "NO3_N") %>%
                    filter(site_code == "w6") %>%
                    filter(water_year >= 1965 &
                            water_year < 1975 |
                           water_year >= 2010 &
                             water_year < 2020) %>%
                    mutate(decade = factor(case_when(
                              water_year < 2000 ~ "1965-1975",
                              TRUE ~ "2010-2020"),
                              levels = c("1965-1975", "2010-2020"))),
                       aes(x = factor(month),
                           y = monthly_vwm_mgL,
                           color = decade,
                           fill = decade)) +
     geom_boxplot(alpha = 0.2) +
     scale_color_manual(values = c("black", "blue")) +
     scale_fill_manual(values = c("black", "blue")) +
     ylim(0, 1.25) +
     labs(title = "Hubbard Brook Exp. Forest (NH)",
          x = "Month",
          y = "Monthly VWM NO<sub>3</sub><sup>-</sup> (mg/L)",
          fill = "Decade",
          color = "Decade") +
     theme_bw() +
     theme(axis.title.y = element_markdown(),
           legend.position = "inside",
           legend.position.inside = c(0.8,0.8),
           legend.background = element_rect(fill = NA, color = NA)))

# H.J. Andrews
(fig_HJA <- ggplot(N_VWM_monthly %>%
                      filter(analyte_N == "NO3_N") %>%
                      filter(site_code == "GSWS08") %>%
                      filter(water_year >= 1975 &
                                 water_year < 1985 |
                                 water_year >= 2005 &
                                 water_year < 2015) %>%
                      mutate(decade = factor(case_when(
                          water_year < 2000 ~ "1975-1985",
                          TRUE ~ "2005-2015"),
                          levels = c("1975-1985", "2005-2015"))),
                  aes(x = factor(month),
                      y = monthly_vwm_mgL,
                      color = decade,
                      fill = decade)) +
        geom_boxplot(alpha = 0.2) +
        scale_color_manual(values = c("black", "blue")) +
        scale_fill_manual(values = c("black", "blue")) +
        ylim(0, 0.02) + # removes two outliers that squish everything
        labs(title = "H.J. Andrews Exp. Forest (OR)",
             x = "Month",
             y = "Monthly VWM NO<sub>3</sub><sup>-</sup> (mg/L)",
             fill = "Decade",
             color = "Decade") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              legend.position = "inside",
              legend.position.inside = c(0.8,0.8),
              legend.background = element_rect(fill = NA, color = NA)))

# Luquillo
(fig_LUQ <- ggplot(N_VWM_monthly %>%
                       filter(analyte_N == "NO3_N") %>%
                       filter(site_code == "MPR") %>%
                       filter(water_year >= 1990 &
                                  water_year < 2000 |
                                  water_year >= 2010 &
                                  water_year < 2020) %>%
                       mutate(decade = factor(case_when(
                           water_year < 2005 ~ "1990-2000",
                           TRUE ~ "2010-2020"),
                           levels = c("1990-2000", "2010-2020"))),
                   aes(x = factor(month),
                       y = monthly_vwm_mgL,
                       color = decade,
                       fill = decade)) +
        geom_boxplot(alpha = 0.2) +
        scale_color_manual(values = c("black", "blue")) +
        scale_fill_manual(values = c("black", "blue")) +
        labs(title = "Luquillo Exp. Forest (PR)",
             x = "Month",
             y = "Monthly VWM NO<sub>3</sub><sup>-</sup> (mg/L)",
             fill = "Decade",
             color = "Decade") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              legend.position = "inside",
              legend.position.inside = c(0.8,0.8),
              legend.background = element_rect(fill = NA, color = NA)))

# Baltimore
(fig_BES <- ggplot(N_VWM_monthly %>%
                       filter(analyte_N == "NO3_N") %>%
                       filter(site_code == "GFVN") %>%
                       filter(water_year >= 2000 &
                                  water_year < 2010 |
                                  water_year >= 2010 &
                                  water_year < 2020) %>%
                       mutate(decade = factor(case_when(
                           water_year < 2010 ~ "2000-2010",
                           TRUE ~ "2010-2020"),
                           levels = c("2000-2010", "2010-2020"))),
                   aes(x = factor(month),
                       y = monthly_vwm_mgL,
                       color = decade,
                       fill = decade)) +
        geom_boxplot(alpha = 0.2) +
        scale_color_manual(values = c("black", "blue")) +
        scale_fill_manual(values = c("black", "blue")) +
        labs(title = "Baltimore Ecosystem Study (MD)",
             x = "Month",
             y = "Monthly VWM NO<sub>3</sub><sup>-</sup> (mg/L)",
             fill = "Decade",
             color = "Decade") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              legend.position = "inside",
              legend.position.inside = c(0.15,0.15),
              legend.background = element_rect(fill = NA, color = NA)))

# and knit together
(fig5_new <- (fig_HB + fig_HJA) / (fig_LUQ + fig_BES) +
    plot_annotation(tag_levels = "A"))

ggsave(fig5_new,
       filename = "figures/monthly_vwm_old_v_new_decade.jpeg",
       height = 20,
       width = 24,
       units = "cm")

###### New add SI Fig ######

# also want to plot progression of N Deposition versus NO3
# for a few key sites - here's a few to look through

# Using full annual VWM data now and joining to deposition data.
no3_stream_and_dep <- N_VWM_annual %>%
    filter(analyte_N == "NO3_N") %>%
    rename(val = annual_vwm_mgL,
           var = analyte_N) %>%
    select(-c(ws_area_ha, n_of_obs_chem)) %>%
    full_join(ndep_data_for_trends_ann) %>%
    select(-c(network, domain, pctCellErr, timestep)) %>%
    pivot_wider(names_from = var, values_from = val) %>%
    filter(water_year > 1984)

(figW6 <- ggplot(no3_stream_and_dep %>%
                     filter(site_code == "w6"),
                aes(x = N_flux_mean,
                    y = NO3_N,
                    fill = water_year)) +
    geom_point(shape = 21, size = 4) +
    labs(title = "Hubbard Brook Exp. Forest (NH)",
         x = "Atmospheric N Deposition (kg/ha)",
         y = "Annual VWM NO<sub>3</sub><sup>-</sup>-N (mg/L)") +
    scale_y_log10() +
        scale_fill_gradient2(low = "black",
                             mid = "blue",
                             high = "#9999FF",
                             midpoint = 2005) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_markdown(),
          legend.position = "none")) # textbook

(figAB <- ggplot(no3_stream_and_dep %>%
                     filter(site_code == "AB00"),
                 aes(x = N_flux_mean,
                     y = NO3_N,
                     fill = water_year)) +
    geom_point(shape = 21, size = 2) +
    labs(title = "AB00 - Santa Barbara") +
    scale_y_log10() +
    xlim(0,3) +
    scale_fill_gradient2(low = "black",
                         mid = "blue",
                         high = "#9999FF",
                        midpoint = 2005) +
    theme_bw() +
    theme(legend.position = "none")) # not so many points to be compelling

(figGSW <- ggplot(no3_stream_and_dep %>%
                     filter(site_code == "GSWS08"),
                 aes(x = N_flux_mean,
                     y = NO3_N,
                     fill = water_year)) +
        geom_point(shape = 21, size = 4) +
        labs(title = "H.J. Andrews Exp. Forest (OR)",
             x = "Atmospheric N Deposition (kg/ha)",
             y = "Annual VWM NO<sub>3</sub><sup>-</sup>-N (mg/L)") +
        scale_y_log10() +
        scale_fill_gradient2(low = "black",
                             mid = "blue",
                             high = "#9999FF",
                             midpoint = 2005) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              legend.position = "none")) # not responsive in the same way

(figGFGL <- ggplot(no3_stream_and_dep %>%
                      filter(site_code == "GFGL"),
                  aes(x = N_flux_mean,
                      y = NO3_N,
                      fill = water_year)) +
        geom_point(shape = 21, size = 2) +
        labs(title = "GFGL - Baltimore") +
        scale_y_log10() +
        scale_fill_viridis(option = "rocket") +
        theme_bw() +
        theme(legend.position = "none")) # not really enough points

(figEB <- ggplot(no3_stream_and_dep %>%
                       filter(site_code == "EB"),
                   aes(x = N_flux_mean,
                       y = NO3_N,
                       fill = water_year)) +
        geom_point(shape = 21, size = 2) +
        labs(title = "EB - Bear Brook") +
        scale_y_log10() +
        scale_fill_viridis(option = "rocket") +
        theme_bw() +
        theme(legend.position = "right")) # nice long time series

(figW9 <- ggplot(no3_stream_and_dep %>%
                       filter(site_code == "W-9"),
                   aes(x = N_flux_mean,
                       y = NO3_N,
                       fill = water_year)) +
        geom_point(shape = 21, size = 4) +
        labs(title = "Sleepers River Res. Watershed (VT)",
             fill = "Water Year",
             x = "Atmospheric N Deposition (kg/ha)",
             y = "Annual VWM NO<sub>3</sub><sup>-</sup>-N (mg/L)") +
        scale_y_log10() +
        scale_fill_gradient2(low = "black",
                             mid = "blue",
                             high = "#9999FF",
                             midpoint = 2005) +
        theme_bw() +
        guides(fill = guide_colorbar(reverse = TRUE)) +
        theme(axis.title.y = element_blank(),
              legend.position = "right")) # huh a funny funnel shape

(figW4 <- ggplot(no3_stream_and_dep %>%
                     filter(site_code == "WS-4"),
                 aes(x = N_flux_mean,
                     y = NO3_N,
                     fill = water_year)) +
        geom_point(shape = 21, size = 4) +
        labs(title = "Fernow Exp. Forest (WV)",
             x = "Atmospheric N Deposition (kg/ha)",
             y = "Annual VWM NO<sub>3</sub><sup>-</sup>-N (mg/L)") +
        scale_y_log10() +
        scale_fill_gradient2(low = "black",
                             mid = "blue",
                             high = "#9999FF",
                             midpoint = 2005) +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "none")) # also textbook

# Ok, also making boxplots to add to either "axis" on the
# figure so I can better contextualize these sites.

# First, I need to aggregate annual dep and annual VWM values
# by site (including all years)
no3_stream_and_dep_site <- no3_stream_and_dep %>%
    group_by(site_code) %>%
    summarize(mean_NO3_N = mean(NO3_N, na.rm = TRUE),
              mean_N_flux = mean(N_flux_mean, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(group = 1) %>%
    # add column to demarcate sites of interest
    mutate(my_sites = case_when(site_code %in% c("w6",
                                                 "GSWS08",
                                                 "W-9",
                                                 "WS-4") ~ "yes",
                                TRUE ~ "no"))

# mini tibble for labels
no3_mini <- no3_stream_and_dep_site %>%
    filter(site_code %in% c("w6",
                            "GSWS08",
                            "W-9",
                            "WS-4"))

# and a simple boxplot for deposition (horizontal)
(fig_box_dep <- ggplot(no3_stream_and_dep_site,
                      aes(x = mean_N_flux,
                          y = group)) +
    geom_boxplot(color = "grey70", alpha = 0.5) +
    geom_jitter(aes(shape = my_sites, alpha = my_sites,
                    size = my_sites),
                height = 0.25) +
    geom_text(x = 0.98, y = 1.3, label = "HJA") +
    geom_text(x = 4.7, y = 1.3, label = "SLE") +
    geom_text(x = 5.7, y = 1.3, label = "FER") +
    geom_text(x = 5.0, y = 1.1, label = "HBR") +
    scale_shape_manual(values = c(1, 19)) +
    scale_alpha_manual(values = c(0.4, 1)) +
    scale_size_manual(values = c(1, 3)) +
    scale_color_manual(values = c(NA, "black")) +
    labs(x = "Atmospheric N Deposition (kg/ha)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none"))

# and a simple boxplot for deposition (horizontal)
(fig_box_vwm <- ggplot(no3_stream_and_dep_site,
                       aes(y = mean_NO3_N,
                           x = group)) +
        geom_boxplot(color = "grey70", alpha = 0.5) +
        geom_jitter(aes(shape = my_sites, alpha = my_sites,
                        size = my_sites),
                    width = 0.25) +
        scale_y_log10() +
        scale_shape_manual(values = c(1, 19)) +
        scale_alpha_manual(values = c(0.4, 1)) +
        scale_size_manual(values = c(1, 3)) +
        scale_color_manual(values = c(NA, "black")) +
        geom_text(y = log10(0.002), x = 1.3, label = "HJA") +
        geom_text(y = log10(0.22), x = 1.3, label = "SLE") +
        geom_text(y = log10(0.73), x = 1.3, label = "FER") +
        geom_text(y = log10(0.11), x = 1.3, label = "HBR") +
        labs(y = "Annual VWM NO<sub>3</sub><sup>-</sup>-N (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),,
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              legend.position = "none"))

# Lets join HB & Fernow (as textbook examples), and contrast
# with HJA and Sleepers (showing other trends).

layout <- "
ABBBBCCCC
ABBBBCCCC
ABBBBCCCC
ABBBBCCCC
ADDDDEEEE
ADDDDEEEE
ADDDDEEEE
ADDDDEEEE
#FFFFFFFF
"

(fig6_new <- fig_box_vwm + figW6 + figW4 +
    figGSW + figW9 + fig_box_dep +
    plot_layout(design = layout) +
    plot_annotation(tag_levels = "A"))

# ggsave(fig6_new,
#        filename = "figures/annual_vwm_v_dep_1985onwards.jpeg",
#        height = 24,
#        width = 28,
#        units = "cm")

##### N vs. Climate (Fig 4) #####

# Join climate and deposition trends
dep_clim_trends_ann <- rbind(ndep_trends_ann, clim_trends)

# And join greenness trends as well
green_dep_clim_trends_ann <- rbind(dep_clim_trends_ann, green_trends_ann)

###### NO3 ######

# Join NO3 and climate trends
no3_clim_trends_ann <- full_join(no3_trends_ann, green_dep_clim_trends_ann)

# Quick check of overlaps
no3_sites <- no3_trends_ann %>% select(site_code) %>% unique()
clim_sites <- dep_clim_trends_ann %>% select(site_code) %>% unique()
inner <- inner_join(no3_sites, clim_sites) # 148 sites max overlap
only_no3 <- no3_sites %>%
    filter(!site_code %in% inner$site_code) # 53 sites with only NO3 data
# i.e., the PR, Krycklan, McMurdo, Toolik, etc. sites (outside CONUS)
only_clim <- clim_sites %>%
    filter(!site_code %in% inner$site_code) # 18 sites with only climate data
# i.e., sites part of other regions but missing N data

# Trim to variables of interest
no3_clim_trends_ann_wide <- no3_clim_trends_ann %>%
    select(site_code, var, trend) %>%
    pivot_wider(names_from = var,
                values_from = trend)

# And join with trend flags and number of obs. data
no3_confidence <- no3_trends_ann %>%
    select(site_code, flag, mean_ann_records)

no3_clim_trends_ann_wide <- full_join(no3_clim_trends_ann_wide,
                                      no3_confidence) %>%
    mutate(group = factor(case_when(flag %in% c("increasing", "decreasing",
                                         "non-significant") ~ flag,
                             TRUE ~ "insufficient data"),
                          levels = c("decreasing",
                                     "increasing",
                                     "non-significant",
                                     "insufficient data")))

# Join with site level data.
no3_clim_trends_ann_wide <- left_join(no3_clim_trends_ann_wide,
                                      ms_site_data) %>%
    mutate(group2 = factor(case_when(flag %in% c("increasing", "decreasing",
                                                 "non-significant") &
                                         ws_status == "non-experimental" ~ flag,
                                     flag %in% c("increasing") &
                                         ws_status == "experimental" ~ "increasing exp",
                                     flag %in% c("decreasing") &
                                         ws_status == "experimental" ~ "decreasing exp",
                                     flag %in% c("non-significant") &
                                         ws_status == "experimental" ~ "non-significant exp",
                                     TRUE ~ "insufficient data"),
                           levels = c("decreasing",
                                      "decreasing exp",
                                      "increasing",
                                      "increasing exp",
                                      "non-significant",
                                      "non-significant exp",
                                      "insufficient data")))

# Need to order dataset properly before plotting.
# This helps points appear better.
no3_clim_trends_ann_wide_ed <- no3_clim_trends_ann_wide %>%
    filter(ws_status == "non-experimental") %>%
    arrange(NO3_N) %>%
    arrange(desc(group))

# This helps points appear better.
no3_clim_trends_ann_wide <- no3_clim_trends_ann_wide %>%
    arrange(NO3_N) %>%
    arrange(desc(group))

# PRECIP V TEMP
(figNO3_ppt_temp <- ggplot(no3_clim_trends_ann_wide_ed,
                          aes(x = temp_mean,
                              y = precip_mean)) +
    geom_hline(yintercept = 0, color = "gray20") +
    geom_vline(xintercept = 0, color = "gray20") +
    geom_point(alpha = 0.7,
               aes(shape = group,
                   color = group,
                   size = group)) +
    xlim(-0.06, 0.06) +
    ylim(-0.04, 0.04) +
    annotate("text", x = -0.042, y = 0.04, label = "Cooling, Wetting") +
    annotate("text", x = -0.042, y = -0.04, label = "Cooling, Drying") +
    annotate("text", x = 0.042, y = 0.04, label = "Warming, Wetting") +
    annotate("text", x = 0.042, y = -0.04, label = "Warming, Drying") +
    scale_shape_manual(values = c(20, 20, 4)) +
    scale_size_manual(values = c(6, 4, 2)) +
    scale_color_manual(values = c("blue",
                                  "gray56",
                                  "gray76")) +
    labs(x = "Mean Annual Temperature Trend",
         y = "Mean Annual Precipitation Trend",
         color = expression(paste(NO[3], "-N Trend")),
         shape = expression(paste(NO[3], "-N Trend")),
         size = expression(paste(NO[3], "-N Trend"))) +
    theme_bw() +
    theme(legend.position = "right"))

# PRECIP V TEMP - including exp sites
(figNO3_ppt_temp2 <- ggplot(no3_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = precip_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group2,
                       color = group2,
                       size = group2)) +
        xlim(-0.06, 0.06) +
        ylim(-0.04, 0.04) +
        annotate("text", x = -0.042, y = 0.04, label = "Cooling, Wetting") +
        annotate("text", x = -0.042, y = -0.04, label = "Cooling, Drying") +
        annotate("text", x = 0.042, y = 0.04, label = "Warming, Wetting") +
        annotate("text", x = 0.042, y = -0.04, label = "Warming, Drying") +
        scale_shape_manual(values = c(20, 15, 15, 20, 15, 4),
                           guide = "none") +
        scale_size_manual(values = c(6, 4, 4, 4, 3, 2),
                          guide = "none") +
        scale_color_manual(values = c("blue",
                                      "blue",
                                      "orange",
                                      "gray56",
                                      "gray56",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual Precipitation Trend",
             color = "Trend & Sampling Frequency") +
        theme_bw())

# Greenness V TEMP
(figNO3_green_temp <- ggplot(no3_clim_trends_ann_wide_ed,
                           aes(x = temp_mean,
                               y = nirv)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group,
                       color = group,
                       size = group)) +
        xlim(-0.06, 0.06) +
        ylim(-0.0015, 0.0015) +
        annotate("text", x = -0.04, y = 0.001475, label = "Cooling, Greening") +
        annotate("text",x = -0.04, y = -0.001475, label = "Cooling, Browning") +
        annotate("text",x = 0.04, y = 0.001475, label = "Warming, Greening") +
        annotate("text",x = 0.04, y = -0.001475, label = "Warming, Browning") +
        scale_shape_manual(values = c(20, 20, 4)) +
        scale_size_manual(values = c(6, 4, 2)) +
        scale_color_manual(values = c("blue",
                                      "gray56",
                                      "gray76")) +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual Greenness Trend") +
        theme_bw() +
        theme(legend.position = "none"))

# GPP V TEMP - including exp sites
(figNO3_gpp_temp2 <- ggplot(no3_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = gpp_CONUS_30m_median)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group2,
                       color = group2,
                       size = group2)) +
        xlim(-0.06, 0.06) +
        ylim(-0.001, 0.001) +
        annotate("text", x = -0.04, y = 0.001, label = "Cooling, Greening") +
        annotate("text",x = -0.04, y = -0.001, label = "Cooling, Browning") +
        annotate("text",x = 0.04, y = 0.001, label = "Warming, Greening") +
        annotate("text",x = 0.04, y = -0.001, label = "Warming, Browning") +
        scale_shape_manual(values = c(20, 15, 15, 20, 15, 4)) +
        scale_size_manual(values = c(6, 4, 4, 4, 3, 2)) +
        scale_color_manual(values = c("blue",
                                      "blue",
                                      "orange",
                                      "gray56",
                                      "gray56",
                                      "gray76")) +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual Productivity Trend",
             color = expression(paste(NO[3], "-N Trend")),
             shape = expression(paste(NO[3], "-N Trend")),
             size = expression(paste(NO[3], "-N Trend"))) +
        theme_bw() +
        theme(legend.position = "right"))

# DEP V TEMP
(figNO3_dep_temp <- ggplot(no3_clim_trends_ann_wide_ed,
                           aes(x = temp_mean,
                               y = N_flux_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group,
                       color = group,
                       size = group)) +
        xlim(-0.06, 0.06) +
        ylim(-0.12, 0.12) +
        annotate("text", x = -0.035, y = 0.12, label = "Cooling, Eutrophying") +
        annotate("text",x = -0.035, y = -0.12, label = "Cooling, Oligotrophying") +
        annotate("text",x = 0.035, y = 0.12, label = "Warming, Eutrophying") +
        annotate("text",x = 0.035, y = -0.12, label = "Warming, Oligotrophying") +
        scale_shape_manual(values = c(20, 20, 4)) +
        scale_size_manual(values = c(6, 4, 2)) +
        scale_color_manual(values = c("blue",
                                      "gray56",
                                      "gray76")) +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual N Deposition Trend") +
        theme_bw() +
        theme(legend.position = "none"))

# DEP V TEMP - including exp sites
(figNO3_dep_temp2 <- ggplot(no3_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = N_flux_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group2,
                       color = group2,
                       size = group2)) +
        xlim(-0.06, 0.06) +
        ylim(-0.12, 0.12) +
        annotate("text", x = -0.035, y = 0.12, label = "Cooling, Eutrophying") +
        annotate("text",x = -0.035, y = -0.12, label = "Cooling, Oligotrophying") +
        annotate("text",x = 0.035, y = 0.12, label = "Warming, Eutrophying") +
        annotate("text",x = 0.035, y = -0.12, label = "Warming, Oligotrophying") +
        scale_shape_manual(values = c(20, 15, 15, 20, 15, 4), guide = "none") +
        scale_size_manual(values = c(6, 4, 4, 4, 3, 2), guide = "none") +
        scale_color_manual(values = c("blue",
                                      "blue",
                                      "orange",
                                      "gray56",
                                      "gray56",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual N Deposition Trend") +
        theme_bw())

# Additional bar chart showing sum total trends
all_trends_ed <- rbind(clim_trends, ndep_trends_ann,
                       green_trends_ann) %>%
    left_join(ms_site_data) %>%
    filter(ws_status == "non-experimental") %>%
    mutate(flag = case_when(sign(trend_upper)!= sign(trend_lower) ~ 'non-significant',
                            sign(trend_upper) == sign(trend_lower) & sign(trend) > 0 ~ 'increasing',
                            sign(trend_upper) == sign(trend_lower) & sign(trend) < 0 ~ 'decreasing',
                            sign(trend_upper)!= sign(trend_lower) & trend == 0 ~ 'flat',
                            trend == NA ~ 'insufficient data'))

all_trends_counts <- all_trends_ed %>%
    group_by(var) %>%
    count(flag) %>%
    ungroup()

(figNO3_bar <- ggplot(all_trends_counts %>%
                          filter(var != "gpp_CONUS_30m_median") %>%
                          mutate(flag = factor(flag,
                                               exclude = NULL)) %>%
                          mutate(var = factor(var,
                                              levels = c("temp_mean",
                                                         "N_flux_mean",
                                                         "precip_mean",
                                                         "nirv"))), aes(fill = flag,
                                  y = n, x = var)) +
        geom_bar(position = "stack", stat = "identity") +
        scale_fill_manual(values = c("blue",
                                     "darkorange",
                                     "gray56"),
                          na.value = "black") +
        scale_x_discrete(labels = c("temp_mean" = "Temperature",
                                    "N_flux_mean" = "N Deposition",
                                    "precip_mean" = "Precipitation",
                                    "nirv" = "Greenness")) +
        labs(y = "Site Count",
             fill = "Trend") +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title.x = element_blank()))

(figNO3_all4 <- figNO3_dep_temp + figNO3_ppt_temp +
        figNO3_green_temp + figNO3_bar +
        plot_annotation(tag_levels = "A"))

# ggsave(figNO3_all4,
#        filename = "figures/panel4fig_no3_dep_and_clim_trends.jpeg",
#        height = 22,
#        width = 28,
#        units = "cm")

# Also investigating relative proportion of significant trends
# for each variable of interest.

# Widen and rename columns for joining.
all_trends_ed_wide <- all_trends_ed %>%
    select(site_code, var, flag) %>%
    pivot_wider(names_from = var,
                values_from = flag) %>%
    rename(precip_group = precip_mean,
           temp_group = temp_mean,
           gpp_group = gpp_CONUS_30m_median,
           ndep_group = N_flux_mean,
           green_group = nirv)

# Remember, this will show 126 sites, which is how many we have
# climate data for, whereas the full N trend dataset will show 173
# which includes sites outside of CONUS.

# Now, to join with n trends dataset.
no3_clim_trends_ann_wide_groups <- left_join(no3_clim_trends_ann_wide_ed,
                                             all_trends_ed_wide)

# Create core dataset for investigating proportions of trends
# for sites with certain NO3 trends.
no3_clim_trends_trim <- no3_clim_trends_ann_wide_groups %>%
    filter(group2 %in% c("decreasing", "increasing", "non-significant")) %>%
    # and drop sites not on plot (aka not with climate data)
    # checked to be sure this one column aligned with others' missingness
    drop_na(precip_mean)

# Filter just for sorting by decreasing trend according to
# climate variables
no3_clim_trends_teeny <- no3_clim_trends_trim %>%
    filter(group2 == "decreasing")

# And create a temperature-specific dataset
no3_temp_trends <- no3_clim_trends_ann_wide_groups %>%
    drop_na(temp_mean) %>%
    count(temp_group)

# And create a deposition-specific dataset
no3_dep_trends <- no3_clim_trends_ann_wide_groups %>%
    drop_na(N_flux_mean) %>%
    count(ndep_group)

# And create a precipitation-specific dataset
no3_ppt_trends <- no3_clim_trends_ann_wide_groups %>%
    drop_na(precip_mean) %>%
    count(precip_group)

# And create a greenness-specific dataset
no3_green_trends <- no3_clim_trends_ann_wide_groups %>%
    drop_na(nirv) %>%
    count(nirv_group)

# and export version including experimental sites

(figNO3_both2 <- figNO3_ppt_temp2 + figNO3_gpp_temp2 +
        plot_annotation(tag_levels = "a"))

# ggsave(figNO3_both2,
#        filename = "figures/panelfig_no3_clim_trends_exp.jpeg",
#        height = 12,
#        width = 28,
#        units = "cm")

###### NH3 ######

# Join NH3 and climate trends
nh3_clim_trends_ann <- full_join(nh3_trends_ann, dep_clim_trends_ann)

# Quick check of overlaps
nh3_sites <- nh3_trends_ann %>% select(site_code) %>% unique()
clim_sites <- dep_clim_trends_ann %>% select(site_code) %>% unique()
inner_nh3 <- inner_join(nh3_sites, clim_sites) # 71 sites max overlap
only_nh3 <- nh3_sites %>%
    filter(!site_code %in% inner_nh3$site_code) # 22 sites only with NH3 data
only_clim <- clim_sites %>%
    filter(!site_code %in% inner_nh3$site_code) # 95 sites with only climate data

# Trim to variables of interest
nh3_clim_trends_ann_wide <- nh3_clim_trends_ann %>%
    select(site_code, var, trend) %>%
    pivot_wider(names_from = var,
                values_from = trend)

# And join with trend flags and number of obs. data
nh3_confidence <- nh3_trends_ann %>%
    select(site_code, flag, mean_ann_records)

nh3_clim_trends_ann_wide <- full_join(nh3_clim_trends_ann_wide,
                                      nh3_confidence) %>%
    mutate(group = factor(case_when(flag %in% c("increasing", "decreasing",
                                                "non-significant") ~ flag,
                                    TRUE ~ "insufficient data"),
                          levels = c("decreasing",
                                     "increasing",
                                     "non-significant",
                                     "insufficient data")))

# Join with site level data.
nh3_clim_trends_ann_wide <- left_join(nh3_clim_trends_ann_wide,
                                      ms_site_data) %>%
    mutate(group2 = factor(case_when(flag %in% c("increasing", "decreasing",
                                                 "non-significant") &
                                         ws_status == "non-experimental" ~ flag,
                                     flag %in% c("increasing") &
                                         ws_status == "experimental" ~ "increasing exp",
                                     flag %in% c("decreasing") &
                                         ws_status == "experimental" ~ "decreasing exp",
                                     flag %in% c("non-significant") &
                                         ws_status == "experimental" ~ "non-significant exp",
                                     TRUE ~ "insufficient data"),
                           levels = c("decreasing",
                                      "decreasing exp",
                                      "increasing",
                                      "increasing exp",
                                      "non-significant",
                                      "non-significant exp",
                                      "insufficient data")))

# Need to order dataset properly before plotting.
# This helps points appear better.
nh3_clim_trends_ann_wide_ed <- nh3_clim_trends_ann_wide %>%
    filter(ws_status == "non-experimental") %>%
    arrange(NH3_N) %>%
    arrange(desc(group))

nh3_clim_trends_ann_wide <- nh3_clim_trends_ann_wide %>%
    arrange(NH3_N) %>%
    arrange(desc(group))

# PRECIP V TEMP
(figNH3_ppt_temp <- ggplot(nh3_clim_trends_ann_wide_ed,
                           aes(x = temp_mean,
                               y = precip_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group,
                       color = group,
                       size = group)) +
        xlim(-0.06, 0.06) +
        ylim(-0.04, 0.04) +
        annotate("text", x = -0.042, y = 0.04, label = "Cooling, Wetting") +
        annotate("text", x = -0.042, y = -0.04, label = "Cooling, Drying") +
        annotate("text", x = 0.042, y = 0.04, label = "Warming, Wetting") +
        annotate("text", x = 0.042, y = -0.04, label = "Warming, Drying") +
        scale_shape_manual(values = c(20, 20, 4),
                           guide = "none") +
        scale_size_manual(values = c(6, 4, 2),
                          guide = "none") +
        scale_color_manual(values = c("purple",
                                      "gray56",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual Precipitation Trend",
             color = "Trend & Sampling Frequency") +
        theme_bw())

# PRECIP V TEMP - including exp sites
(figNH3_ppt_temp2 <- ggplot(nh3_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = precip_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group2,
                       color = group2,
                       size = group2)) +
        xlim(-0.06, 0.06) +
        ylim(-0.04, 0.04) +
        annotate("text", x = -0.042, y = 0.04, label = "Cooling, Wetting") +
        annotate("text", x = -0.042, y = -0.04, label = "Cooling, Drying") +
        annotate("text", x = 0.042, y = 0.04, label = "Warming, Wetting") +
        annotate("text", x = 0.042, y = -0.04, label = "Warming, Drying") +
        scale_shape_manual(values = c(20, 15, 20, 15, 4),
                           guide = "none") +
        scale_size_manual(values = c(6, 4, 4, 3, 2),
                          guide = "none") +
        scale_color_manual(values = c("purple",
                                      "purple",
                                      "gray56",
                                      "gray56",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual Precipitation Trend",
             color = "Trend & Sampling Frequency") +
        theme_bw())

# GPP V TEMP
(figNH3_gpp_temp <- ggplot(nh3_clim_trends_ann_wide_ed,
                           aes(x = temp_mean,
                               y = gpp_CONUS_30m_median)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group,
                       color = group,
                       size = group)) +
        xlim(-0.06, 0.06) +
        ylim(-0.001, 0.001) +
        annotate("text", x = -0.04, y = 0.001, label = "Cooling, Greening") +
        annotate("text",x = -0.04, y = -0.001, label = "Cooling, Browning") +
        annotate("text",x = 0.04, y = 0.001, label = "Warming, Greening") +
        annotate("text",x = 0.04, y = -0.001, label = "Warming, Browning") +
        scale_shape_manual(values = c(20, 20, 4)) +
        scale_size_manual(values = c(6, 4, 2)) +
        scale_color_manual(values = c("purple",
                                      "gray56",
                                      "gray76")) +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual Productivity Trend",
             color = expression(paste(NH[3], "-N Trend")),
             shape = expression(paste(NH[3], "-N Trend")),
             size = expression(paste(NH[3], "-N Trend"))) +
        theme_bw() +
        theme(legend.position = "right"))

# GPP V TEMP - including exp sites
(figNH3_gpp_temp2 <- ggplot(nh3_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = gpp_CONUS_30m_median)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group2,
                       color = group2,
                       size = group2)) +
        xlim(-0.06, 0.06) +
        ylim(-0.001, 0.001) +
        annotate("text", x = -0.04, y = 0.001, label = "Cooling, Greening") +
        annotate("text",x = -0.04, y = -0.001, label = "Cooling, Browning") +
        annotate("text",x = 0.04, y = 0.001, label = "Warming, Greening") +
        annotate("text",x = 0.04, y = -0.001, label = "Warming, Browning") +
        scale_shape_manual(values = c(20, 15, 20, 15, 4)) +
        scale_size_manual(values = c(6, 4, 4, 3, 2)) +
        scale_color_manual(values = c("purple",
                                      "purple",
                                      "gray56",
                                      "gray56",
                                      "gray76")) +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual Productivity Trend",
             color = expression(paste(NH[3], "-N Trend")),
             shape = expression(paste(NH[3], "-N Trend")),
             size = expression(paste(NH[3], "-N Trend"))) +
        theme_bw() +
        theme(legend.position = "right"))

# DEP V TEMP
(figNH3_dep_temp <- ggplot(nh3_clim_trends_ann_wide_ed,
                           aes(x = temp_mean,
                               y = N_flux_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group,
                       color = group,
                       size = group)) +
        xlim(-0.06, 0.06) +
        ylim(-0.12, 0.12) +
        annotate("text", x = -0.035, y = 0.12, label = "Cooling, Eutrophying") +
        annotate("text",x = -0.035, y = -0.12, label = "Cooling, Oligotrophying") +
        annotate("text",x = 0.035, y = 0.12, label = "Warming, Eutrophying") +
        annotate("text",x = 0.035, y = -0.12, label = "Warming, Oligotrophying") +
        scale_shape_manual(values = c(20, 20, 4), guide = "none") +
        scale_size_manual(values = c(6, 4, 2), guide = "none") +
        scale_color_manual(values = c("purple",
                                      "gray56",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual N Deposition Trend") +
        theme_bw())

# DEP V TEMP - including exp sites
(figNH3_dep_temp2 <- ggplot(nh3_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = N_flux_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group2,
                       color = group2,
                       size = group2)) +
        xlim(-0.06, 0.06) +
        ylim(-0.12, 0.12) +
        annotate("text", x = -0.035, y = 0.12, label = "Cooling, Eutrophying") +
        annotate("text",x = -0.035, y = -0.12, label = "Cooling, Oligotrophying") +
        annotate("text",x = 0.035, y = 0.12, label = "Warming, Eutrophying") +
        annotate("text",x = 0.035, y = -0.12, label = "Warming, Oligotrophying") +
        scale_shape_manual(values = c(20, 15, 20, 15, 4), guide = "none") +
        scale_size_manual(values = c(6, 4, 4, 3, 2), guide = "none") +
        scale_color_manual(values = c("purple",
                                      "purple",
                                      "gray56",
                                      "gray56",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual N Deposition Trend") +
        theme_bw())

(figNH3_both <- figNH3_ppt_temp + figNH3_gpp_temp +
        plot_annotation(tag_levels = "a"))

# ggsave(figNH3_both,
#        filename = "figures/panelfig_nh3_clim_trends.jpeg",
#        height = 12,
#        width = 28,
#        units = "cm")

# and export figure with experimental sites included as well

(figNH3_both2 <- figNH3_ppt_temp2 + figNH3_gpp_temp2 +
        plot_annotation(tag_levels = "a"))

# ggsave(figNH3_both2,
#        filename = "figures/panelfig_nh3_clim_trends_exp.jpeg",
#        height = 12,
#        width = 28,
#        units = "cm")

###### TDN ######

# Join TDN and climate trends
tdn_clim_trends_ann <- full_join(tdn_trends_ann, dep_clim_trends_ann)

# Quick check of overlaps
tdn_sites <- tdn_trends_ann %>% select(site_code) %>% unique()
clim_sites <- dep_clim_trends_ann %>% select(site_code) %>% unique()
inner_tdn <- inner_join(tdn_sites, clim_sites) # 62 sites max overlap
only_tdn <- tdn_sites %>%
    filter(!site_code %in% inner_tdn$site_code) # 11 sites only with TDN data
only_clim <- clim_sites %>%
    filter(!site_code %in% inner_tdn$site_code) # 104 sites with only climate data

# Trim to variables of interest
tdn_clim_trends_ann_wide <- tdn_clim_trends_ann %>%
    select(site_code, var, trend) %>%
    pivot_wider(names_from = var,
                values_from = trend)

# And join with trend flags and number of obs. data
tdn_confidence <- tdn_trends_ann %>%
    select(site_code, flag, mean_ann_records)

tdn_clim_trends_ann_wide <- full_join(tdn_clim_trends_ann_wide,
                                      tdn_confidence) %>%
    mutate(group = factor(case_when(flag %in% c("increasing", "decreasing",
                                                "non-significant") ~ flag,
                                    TRUE ~ "insufficient data"),
                          levels = c("decreasing",
                                     "increasing",
                                     "non-significant",
                                     "insufficient data")))

# Join with site level data.
tdn_clim_trends_ann_wide <- left_join(tdn_clim_trends_ann_wide,
                                      ms_site_data) %>%
    mutate(group2 = factor(case_when(flag %in% c("increasing", "decreasing",
                                                 "non-significant") &
                                         ws_status == "non-experimental" ~ flag,
                                     flag %in% c("increasing") &
                                         ws_status == "experimental" ~ "increasing exp",
                                     flag %in% c("decreasing") &
                                         ws_status == "experimental" ~ "decreasing exp",
                                     flag %in% c("non-significant") &
                                         ws_status == "experimental" ~ "non-significant exp",
                                     TRUE ~ "insufficient data"),
                           levels = c("decreasing",
                                      "decreasing exp",
                                      "increasing",
                                      "increasing exp",
                                      "non-significant",
                                      "non-significant exp",
                                      "insufficient data")))

# Need to order dataset properly before plotting.
# This helps points appear better.
tdn_clim_trends_ann_wide_ed <- tdn_clim_trends_ann_wide %>%
    filter(ws_status == "non-experimental") %>%
    arrange(TDN) %>%
    arrange(desc(group))

tdn_clim_trends_ann_wide <- tdn_clim_trends_ann_wide %>%
    arrange(TDN) %>%
    arrange(desc(group))

# PRECIP V TEMP
(figTDN_ppt_temp <- ggplot(tdn_clim_trends_ann_wide_ed,
                           aes(x = temp_mean,
                               y = precip_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group,
                       color = group,
                       size = group)) +
        xlim(-0.06, 0.06) +
        ylim(-0.04, 0.04) +
        annotate("text", x = -0.042, y = 0.04, label = "Cooling, Wetting") +
        annotate("text", x = -0.042, y = -0.04, label = "Cooling, Drying") +
        annotate("text", x = 0.042, y = 0.04, label = "Warming, Wetting") +
        annotate("text", x = 0.042, y = -0.04, label = "Warming, Drying") +
        scale_shape_manual(values = c(20, 20, 20, 4),
                           guide = "none") +
        scale_size_manual(values = c(6, 6, 4, 2),
                          guide = "none") +
        scale_color_manual(values = c("cyan4",
                                      "darkorange",
                                      "gray56",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual Precipitation Trend",
             color = "Trend & Sampling Frequency") +
        theme_bw())

# PRECIP V TEMP - including exp sites
(figTDN_ppt_temp2 <- ggplot(tdn_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = precip_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group2,
                       color = group2,
                       size = group2)) +
        xlim(-0.06, 0.06) +
        ylim(-0.04, 0.04) +
        annotate("text", x = -0.042, y = 0.04, label = "Cooling, Wetting") +
        annotate("text", x = -0.042, y = -0.04, label = "Cooling, Drying") +
        annotate("text", x = 0.042, y = 0.04, label = "Warming, Wetting") +
        annotate("text", x = 0.042, y = -0.04, label = "Warming, Drying") +
        scale_shape_manual(values = c(20, 15, 20, 20, 15, 4),
                           guide = "none") +
        scale_size_manual(values = c(6, 4, 6, 4, 3, 2),
                          guide = "none") +
        scale_color_manual(values = c("cyan4",
                                      "cyan4",
                                      "darkorange",
                                      "gray56",
                                      "gray56",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual Precipitation Trend",
             color = "Trend & Sampling Frequency") +
        theme_bw())

# GPP V TEMP
(figTDN_gpp_temp <- ggplot(tdn_clim_trends_ann_wide_ed,
                           aes(x = temp_mean,
                               y = gpp_CONUS_30m_median)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group,
                       color = group,
                       size = group)) +
        xlim(-0.06, 0.06) +
        ylim(-0.001, 0.001) +
        annotate("text", x = -0.04, y = 0.001, label = "Cooling, Greening") +
        annotate("text",x = -0.04, y = -0.001, label = "Cooling, Browning") +
        annotate("text",x = 0.04, y = 0.001, label = "Warming, Greening") +
        annotate("text",x = 0.04, y = -0.001, label = "Warming, Browning") +
        scale_shape_manual(values = c(20, 20, 20, 4)) +
        scale_size_manual(values = c(6, 6, 4, 2)) +
        scale_color_manual(values = c("cyan4",
                                      "darkorange",
                                      "gray56",
                                      "gray76")) +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual Productivity Trend",
             color = "TDN Trend",
             shape = "TDN Trend",
             size = "TDN Trend") +
        theme_bw() +
        theme(legend.position = "right"))

# GPP V TEMP - including exp sites
(figTDN_gpp_temp2 <- ggplot(tdn_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = gpp_CONUS_30m_median)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group2,
                       color = group2,
                       size = group2)) +
        xlim(-0.06, 0.06) +
        ylim(-0.001, 0.001) +
        annotate("text", x = -0.04, y = 0.001, label = "Cooling, Greening") +
        annotate("text",x = -0.04, y = -0.001, label = "Cooling, Browning") +
        annotate("text",x = 0.04, y = 0.001, label = "Warming, Greening") +
        annotate("text",x = 0.04, y = -0.001, label = "Warming, Browning") +
        scale_shape_manual(values = c(20, 15, 20, 20, 15, 4)) +
        scale_size_manual(values = c(6, 4, 6, 4, 3, 2)) +
        scale_color_manual(values = c("cyan4",
                                      "cyan4",
                                      "darkorange",
                                      "gray56",
                                      "gray56",
                                      "gray76")) +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual Productivity Trend",
             color = "TDN Trend",
             shape = "TDN Trend",
             size = "TDN Trend") +
        theme_bw() +
        theme(legend.position = "right"))

# DEP V TEMP
(figTDN_dep_temp <- ggplot(tdn_clim_trends_ann_wide_ed,
                           aes(x = temp_mean,
                               y = N_flux_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group,
                       color = group,
                       size = group)) +
        xlim(-0.06, 0.06) +
        ylim(-0.12, 0.12) +
        annotate("text", x = -0.035, y = 0.12, label = "Cooling, Eutrophying") +
        annotate("text",x = -0.035, y = -0.12, label = "Cooling, Oligotrophying") +
        annotate("text",x = 0.035, y = 0.12, label = "Warming, Eutrophying") +
        annotate("text",x = 0.035, y = -0.12, label = "Warming, Oligotrophying") +
        scale_shape_manual(values = c(20, 20, 20, 4), guide = "none") +
        scale_size_manual(values = c(6, 6, 4, 2), guide = "none") +
        scale_color_manual(values = c("cyan4",
                                      "darkorange",
                                      "gray56",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual N Deposition Trend") +
        theme_bw())

# DEP V TEMP - including exp sites
(figTDN_dep_temp2 <- ggplot(tdn_clim_trends_ann_wide,
                           aes(x = temp_mean,
                               y = N_flux_mean)) +
        geom_hline(yintercept = 0, color = "gray20") +
        geom_vline(xintercept = 0, color = "gray20") +
        geom_point(alpha = 0.7,
                   aes(shape = group2,
                       color = group2,
                       size = group2)) +
        xlim(-0.06, 0.06) +
        ylim(-0.12, 0.12) +
        annotate("text", x = -0.035, y = 0.12, label = "Cooling, Eutrophying") +
        annotate("text",x = -0.035, y = -0.12, label = "Cooling, Oligotrophying") +
        annotate("text",x = 0.035, y = 0.12, label = "Warming, Eutrophying") +
        annotate("text",x = 0.035, y = -0.12, label = "Warming, Oligotrophying") +
        scale_shape_manual(values = c(20, 15, 20, 20, 15, 4), guide = "none") +
        scale_size_manual(values = c(6, 4, 6, 4, 3, 2), guide = "none") +
        scale_color_manual(values = c("cyan4",
                                      "cyan4",
                                      "darkorange",
                                      "gray56",
                                      "gray56",
                                      "gray76"),
                           guide = "none") +
        labs(x = "Mean Annual Temperature Trend",
             y = "Mean Annual N Deposition Trend") +
        theme_bw())

(figTDN_both <- figTDN_ppt_temp + figTDN_gpp_temp +
        plot_annotation(tag_levels = "a"))

# ggsave(figTDN_both,
#        filename = "figures/panelfig_tdn_clim_trends.jpeg",
#        height = 12,
#        width = 28,
#        units = "cm")

# and export figure including experimental sites

(figTDN_both2 <- figTDN_ppt_temp2 + figTDN_gpp_temp2 +
        plot_annotation(tag_levels = "a"))

# ggsave(figTDN_both2,
#        filename = "figures/panelfig_tdn_clim_trends_exp.jpeg",
#        height = 12,
#        width = 28,
#        units = "cm")

# Quick check of sites with trends that overlap.
no3_sig_trends <- no3_clim_trends_ann_wide_ed %>%
    filter(flag == "decreasing") %>%
    rename(flag_NO3 = flag) %>%
    select(site_code, flag_NO3)
nh3_sig_trends <- nh3_clim_trends_ann_wide_ed %>%
    filter(flag == "decreasing") %>%
    rename(flag_NH3 = flag) %>%
    select(site_code, flag_NH3)
tdn_sig_trends <- tdn_clim_trends_ann_wide_ed %>%
    filter(flag %in% c("decreasing", "increasing")) %>%
    rename(flag_TDN = flag) %>%
    select(site_code, flag_TDN)

all_sig_trends <- full_join(no3_sig_trends,
                            nh3_sig_trends)
all_sig_trends <- full_join(all_sig_trends,
                            tdn_sig_trends)

##### N VWM v. N Dep. #####

# Pull relevant deposition data (n = 166) and join with NO3 VWMs.
NO3_VWM_annual <- N_VWM_annual %>%
    filter(analyte_N == "NO3_N") %>%
    group_by(site_code) %>%
    summarize(mean_annual_vwm_mgL = mean(annual_vwm_mgL)) %>%
    ungroup()

no3_dep_trends_ann <- no3_clim_trends_ann %>%
    filter(var %in% c("NO3_N", "N_flux_mean"))

# Trim to variables of interest
no3_dep_trends_ann_wide <- no3_dep_trends_ann %>%
    select(site_code, var, trend) %>%
    pivot_wider(names_from = var,
                values_from = trend)

# And join with vwm data
no3_dep_trends_ann_wide <- full_join(no3_dep_trends_ann_wide,
                                      NO3_VWM_annual) %>%
    # and join with flags dataset created above
    full_join(no3_confidence) %>%
    mutate(group = factor(case_when(flag %in% c("increasing", "decreasing",
                                                "non-significant") ~ flag,
                                    TRUE ~ "insufficient data"),
                          levels = c("decreasing",
                                     "increasing",
                                     "non-significant",
                                     "insufficient data")))

# Join with site level data.
no3_dep_trends_ann_wide <- left_join(no3_dep_trends_ann_wide,
                                      ms_site_data) %>%
    mutate(group2 = factor(case_when(flag %in% c("increasing", "decreasing",
                                                 "non-significant") &
                                         ws_status == "non-experimental" ~ flag,
                                     flag %in% c("increasing") &
                                         ws_status == "experimental" ~ "increasing exp",
                                     flag %in% c("decreasing") &
                                         ws_status == "experimental" ~ "decreasing exp",
                                     flag %in% c("non-significant") &
                                         ws_status == "experimental" ~ "non-significant exp",
                                     TRUE ~ "insufficient data"),
                           levels = c("decreasing",
                                      "decreasing exp",
                                      "increasing",
                                      "increasing exp",
                                      "non-significant",
                                      "non-significant exp",
                                      "insufficient data")))

# Need to order dataset properly before plotting.
# This helps points appear better.
no3_dep_trends_ann_wide_ed <- no3_dep_trends_ann_wide %>%
    filter(ws_status == "non-experimental") %>%
    arrange(NO3_N) %>%
    arrange(desc(group))

# This helps points appear better.
no3_dep_trends_ann_wide <- no3_dep_trends_ann_wide %>%
    arrange(NO3_N) %>%
    arrange(desc(group))

# VWM V DEP
(figNO3_dep <- ggplot(no3_dep_trends_ann_wide_ed,
                           aes(x = N_flux_mean,
                               y = mean_annual_vwm_mgL)) +
        geom_point(alpha = 0.7,
                   aes(shape = group,
                       color = group,
                       size = group)) +
        scale_y_log10() +
        scale_shape_manual(values = c(20, 20, 4)) +
        scale_size_manual(values = c(6, 4, 2)) +
        scale_color_manual(values = c("blue",
                                      "gray56",
                                      "gray76")) +
        labs(x = "Mean Annual N Deposition Trend",
             y = expression(paste("Mean Annual ", NO[3]-N, " VWM Concentration (mg/L)")),
             color = expression(paste(NO[3], "-N Trend")),
             shape = expression(paste(NO[3], "-N Trend")),
             size = expression(paste(NO[3], "-N Trend"))) +
        theme_bw())

# ggsave(figNO3_dep,
#        filename = "figures/no3_dep_trends.jpeg",
#        height = 12,
#        width = 17,
#        units = "cm")

# VWM V DEP - including exp sites
(figNO3_dep2 <- ggplot(no3_dep_trends_ann_wide,
                            aes(x = N_flux_mean,
                                y = mean_annual_vwm_mgL)) +
        geom_point(alpha = 0.7,
                   aes(shape = group2,
                       color = group2,
                       size = group2)) +
        scale_y_log10() +
        scale_shape_manual(values = c(20, 15, 15, 20, 15, 4)) +
        scale_size_manual(values = c(6, 4, 4, 4, 3, 2)) +
        scale_color_manual(values = c("blue", "blue", "orange",
                                      "gray56", "gray56", "gray76")) +
        labs(x = "Mean Annual N Deposition Trend",
             y = expression(paste("Mean Annual ", NO[3]-N, " VWM Concentration (mg/L)")),
             color = expression(paste(NO[3], "-N Trend")),
             shape = expression(paste(NO[3], "-N Trend")),
             size = expression(paste(NO[3], "-N Trend"))) +
        theme_bw())

# ggsave(figNO3_dep2,
#        filename = "figures/no3_dep_trends_exp.jpeg",
#        height = 12,
#        width = 17,
#        units = "cm")

# End of script.
