# Time Series Figures Script
# September 11, 2024
# Heili Lowman

#### READ ME ####

# The following script will be used to generate figures
# that display time-series of climate and in-stream data
# for each site at an annual time-step (to have as a raw
# data gut-check as we perform trend analyses).

#### Setup ####

# Load packages, setup wd, and load site metadata.
library(here)
source(here('src', 'setup.R'))

#### Data ####

# Load in annual data summaries.
data <- readRDS(here('data_working', 'discharge_metrics_siteyear_nTest.rds'))

# And join with site-level data.
data <- data %>%
    left_join(ms_site_data, by = c("site_code"))

#### Plot ####
site_sub_list <- split(data, data$site_code)

plotting_covar <- function(x) {

    df <- x %>%
        select(domain, site_code, water_year,
               temp_mean_ann, gpp_global_500m_median, precip_mean_ann,
               q_mean, stream_temp_mean_ann) %>%
        pivot_longer(-c(domain, site_code, water_year),
                     names_to = "var") %>%
        mutate(var = factor(var,
                                 levels = c("temp_mean_ann", "gpp_global_500m_median", "precip_mean_ann",
                                            "q_mean", "stream_temp_mean_ann")))
    p <- ggplot(df,
                aes(x = water_year,
                    y = value,
                    color = var)) +
        geom_point(alpha = 0.9, size = 2) +
        geom_line(alpha = 0.5)+
        scale_color_manual(values = c("#D46F10", "#4CA49E", "#69B9FA",
                                      "#59A3F8", "#6B6D9F")) +
        labs(x = "Water Year (Oct 1 - Sept 30)",
             title = paste(df$domain[1],
                           df$site_code[1], sep = " ")) +
        theme_bw() +
        theme(legend.position = "none",
              axis.title.y = element_blank()) +
        facet_wrap(var~., scales = "free", ncol = 1)

    return(p)

}

lapply(site_sub_list, function(x) ggsave(plot = plotting_covar(x),
                                         filename = here('figures',
                                                         'site_covariate_plots',
                                                          paste0(x$domain[1],
                                                          x$site_code[1],
                                                          "covar.jpg")),
                                         width = 20,
                                         height = 30,
                                         units = "cm",
                                         create.dir = T))

# End of script.
