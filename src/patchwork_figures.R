library(here)
source(here('src', 'setup.R'))
library(patchwork)
library(png)
library(grid)

flag_colors <- c('increasing' = "red", 'decreasing' = 'blue',
                 'non-significant' = "grey", 'data limited' = 'black')
target_q_trend <- "q_mean"

# ---- load data ----

q_trends <- read_csv(here('data_working', 'trends', 'best_run_prisim.csv')) %>%
    add_flags() %>%
    filter(var == target_q_trend) %>%
    select(site_code, q_trend = trend, q_flag = flag)

full_prism_trends <- read_csv(here('data_working', 'trends', 'full_prisim_climate.csv')) %>%
    add_flags() %>%
    select(site_code, var, trend, flag) %>%
    pivot_wider(id_cols = site_code, values_from = c(trend, flag), names_from = var) %>%
    mutate(wetting = case_when(flag_precip_mean == 'increasing' ~ 'W',
                               flag_precip_mean == 'decreasing' ~ 'D',
                               flag_precip_mean == 'non-significant' ~ '-'),
           warming = case_when(flag_temp_mean == 'increasing' ~ 'H',
                               flag_temp_mean == 'decreasing' ~ 'C',
                               flag_temp_mean == 'non-significant' ~ '-'),
           greening = case_when(flag_gpp_CONUS_30m_median == 'increasing' ~ 'G',
                                flag_gpp_CONUS_30m_median == 'decreasing' ~ 'B',
                                flag_gpp_CONUS_30m_median == 'non-significant' ~ '-',
                                is.na(flag_gpp_CONUS_30m_median) ~ '-'),
           grouping = as.factor(paste0(warming, wetting, greening))) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    left_join(., q_trends, by = 'site_code')

# quadrant counts (non-experimental only)
ne <- full_prism_trends %>% filter(ws_status == 'non-experimental')
n_hw <- ne %>% filter(trend_precip_mean > 0, trend_temp_mean > 0) %>% nrow()
n_hd <- ne %>% filter(trend_precip_mean < 0, trend_temp_mean > 0) %>% nrow()
n_cw <- ne %>% filter(trend_precip_mean > 0, trend_temp_mean < 0) %>% nrow()
n_cd <- ne %>% filter(trend_precip_mean < 0, trend_temp_mean < 0) %>% nrow()

# ============================================================
# FIGURE 1 — maps + NWD density
# ============================================================

load_png <- function(path) {
    img <- readPNG(path)
    rasterGrob(img, interpolate = TRUE)
}

map_temp_grob  <- load_png(here('figures', 'map_temperature_trends.png'))
map_ppt_grob   <- load_png(here('figures', 'map_precip_trends.png'))
map_gpp_grob   <- load_png(here('figures', 'map_gpp_trends.png'))

# recreate density plot as ggplot object for patchwork
grid_groups <- read_csv(here('data_working', 'grid_groups.csv'))
ms_groups_data <- read_csv(here('data_working', 'site_groupings_by_prsim_trend.csv'))

ms_temp <- ms_groups_data %>%
    mutate(source = 'ms') %>%
    select(source, coarse_grouping)
grid_temp <- grid_groups %>%
    mutate(source = 'grid') %>%
    select(source, coarse_grouping)
both_groups <- rbind(ms_temp, grid_temp) %>% na.omit()

density_p <- both_groups %>%
    group_by(source, coarse_grouping) %>%
    summarize(n = n(), .groups = 'drop') %>%
    mutate(density = case_when(source == 'grid' ~ n / nrow(grid_temp),
                               source == 'ms' ~ n / nrow(ms_temp))) %>%
    ggplot(aes(y = coarse_grouping, x = density, fill = source)) +
    geom_col(position = 'dodge') +
    theme_few(base_size = 14) +
    labs(y = 'Net Water Demand Group', x = 'Density', fill = 'Dataset') +
    scale_fill_manual(labels = c('Grid', 'MacroSheds'), values = c('blue', 'black'))

fig1 <- (wrap_elements(map_temp_grob) + wrap_elements(map_gpp_grob)) /
        (wrap_elements(map_ppt_grob) + density_p)
ggsave(here('figures', 'Figure_1.png'), fig1, width = 14, height = 10, dpi = 300)

# ============================================================
# FIGURE 2 — GPP scatter + Q scatter side by side
# ============================================================

base_scatter <- function(data) {
    ggplot(data, aes(x = trend_temp_mean * 10, y = trend_precip_mean * 10)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        lims(x = c(-.6, .6), y = c(-.4, .4)) +
        theme_few(base_size = 14) +
        annotate('text', x = -0.55, y = 0.37,
                 label = paste0('n[ne]', ' == ', n_cw), parse = TRUE,
                 hjust = 0, size = 3.5, color = 'grey40') +
        annotate('text', x = 0.55, y = 0.37,
                 label = paste0('n[ne]', ' == ', n_hw), parse = TRUE,
                 hjust = 1, size = 3.5, color = 'grey40') +
        annotate('text', x = -0.55, y = -0.37,
                 label = paste0('n[ne]', ' == ', n_cd), parse = TRUE,
                 hjust = 0, size = 3.5, color = 'grey40') +
        annotate('text', x = 0.55, y = -0.37,
                 label = paste0('n[ne]', ' == ', n_hd), parse = TRUE,
                 hjust = 1, size = 3.5, color = 'grey40')
}

gpp_panel <- base_scatter(full_prism_trends) +
    geom_point(data = subset(full_prism_trends,
                             flag_gpp_CONUS_30m_median == "non-significant"),
               color = "grey", size = 2) +
    geom_point(data = subset(arrange(full_prism_trends, trend_gpp_CONUS_30m_median),
                             flag_gpp_CONUS_30m_median != "non-significant"),
               aes(color = trend_gpp_CONUS_30m_median * 10,
                   shape = ws_status), size = 4) +
    scale_color_distiller(palette = 'BrBG', direction = 1) +
    scale_shape_manual(values = c(17, 16),
                       labels = c('experimental', 'non-experimental')) +
    guides(shape = "none") +
    labs(x = 'Temperature trend\n(decade, mean annual, °C)',
         y = 'Precipitation trend\n(decade, mean annual, mm)',
         color = 'GPP trend\n(mean, kgC/m²/decade)')

limit <- max(abs(full_prism_trends$q_trend) * 10, na.rm = TRUE) * c(-1, 1)

q_plot_data <- full_prism_trends %>%
    mutate(point_type = case_when(
        is.na(q_flag) ~ 'data limited',
        TRUE ~ ws_status
    ))

q_panel <- base_scatter(q_plot_data) +
    geom_point(data = subset(q_plot_data, is.na(q_flag)),
               color = "black", size = 2, aes(shape = point_type)) +
    geom_point(data = subset(q_plot_data, q_flag == "non-significant"),
               color = "grey", size = 2, aes(shape = point_type)) +
    geom_point(data = subset(q_plot_data, q_flag %in% c("increasing", "decreasing")),
               aes(color = q_trend * 10, shape = point_type), size = 4) +
    scale_color_gradientn(
        colors = c("#2166ac", "#4393c3", "#d6604d", "#b2182b"),
        values = scales::rescale(c(min(limit), 0, max(limit))),
        limits = limit) +
    scale_shape_manual(values = c('experimental' = 17, 'non-experimental' = 16, 'data limited' = 4)) +
    labs(x = 'Temperature trend\n(decade, mean annual, °C)',
         y = 'Precipitation trend\n(decade, mean annual, mm)',
         color = 'Q trend\n(mean, mm/decade)',
         shape = 'Condition')

fig2 <- gpp_panel + q_panel +
    plot_layout(guides = 'collect') &
    theme(legend.position = 'right')
ggsave(here('figures', 'Figure_2.png'), fig2, width = 16, height = 7, dpi = 300)

# ============================================================
# FIGURE 3 — aridity histograms via facet_wrap
# ============================================================

# aridity data
metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear_nTest.rds')) %>%
    distinct()
daymet <- ms_load_product(here('data_raw', 'ms'),
                          prodname = 'ws_attr_CAMELS_Daymet_forcings',
                          warn = FALSE)
p <- ms_load_product(my_ms_dir,
                     prodname = 'ws_attr_timeseries:climate',
                     filter_vars = 'precip_median',
                     warn = FALSE) %>%
    select(-var, -year, -pctCellErr, precip_median = val)
et <- read_csv(here('data_raw', 'ms_add_ons', 'macrosheds_et2.csv'))
et_obs <- metrics %>%
    filter(agg_code == 'annual') %>%
    select(site_code, wy = water_year, precip_total, q_totsum) %>%
    mutate(ei_obs = (precip_total - q_totsum) / precip_total)

d <- inner_join(p, daymet, by = c('network', 'domain', 'site_code', 'date')) %>%
    mutate(year = as.integer(as.character(water_year(date, origin = 'usgs')))) %>%
    group_by(year, site_code) %>%
    summarize(aridity_index = sum(`pet(mm)`) / sum(`prcp(mm/day)`),
              precip = sum(`prcp(mm/day)`), .groups = 'drop') %>%
    full_join(., et, by = c('site_code', 'year')) %>%
    mutate(evaporative_index = val / (precip * 10)) %>%
    full_join(., et_obs, by = c('site_code', 'year' = 'wy'))

aridity <- d %>%
    select(site_code, aridity_index) %>%
    group_by(site_code) %>%
    summarize(mean_ai = mean(aridity_index, na.rm = TRUE)) %>%
    full_join(full_prism_trends, by = 'site_code') %>%
    filter(ws_status == 'non-experimental')
aridity$q_flag[is.na(aridity$q_flag)] <- "data limited"
aridity$q_flag <- factor(aridity$q_flag,
                         levels = c('increasing', 'decreasing',
                                    'non-significant', 'data limited'))

# assign quadrants for faceting
aridity <- aridity %>%
    mutate(quadrant = case_when(
        trend_precip_mean > 0 & trend_temp_mean < 0 ~ 'Cooler, wetter',
        trend_precip_mean > 0 & trend_temp_mean > 0 ~ 'Warmer, wetter',
        trend_precip_mean < 0 & trend_temp_mean < 0 ~ 'Cooler, drier',
        trend_precip_mean < 0 & trend_temp_mean > 0 ~ 'Hotter, drier'
    )) %>%
    filter(!is.na(quadrant)) %>%
    mutate(quadrant = factor(quadrant,
                             levels = c('Cooler, wetter', 'Warmer, wetter',
                                        'Cooler, drier', 'Hotter, drier')))

# per-quadrant annotation labels
quad_labels <- tibble(
    quadrant = factor(c('Cooler, wetter', 'Warmer, wetter',
                        'Cooler, drier', 'Hotter, drier'),
                      levels = levels(aridity$quadrant)),
    n_ne = c(n_cw, n_hw, n_cd, n_hd)
) %>%
    mutate(label = paste0('n[ne]', ' == ', n_ne))

fig3 <- ggplot(aridity, aes(x = mean_ai, fill = q_flag)) +
    geom_histogram() +
    facet_wrap(~quadrant, ncol = 2) +
    scale_fill_manual(values = flag_colors,
                      limits = c('increasing', 'decreasing',
                                 'non-significant', 'data limited'),
                      drop = FALSE) +
    scale_x_continuous(limits = c(0, 6)) +
    scale_y_continuous(limits = c(0, 17)) +
    geom_vline(xintercept = 1, color = 'orange', lwd = 1.5, lty = 'longdash') +
    geom_text(data = quad_labels, aes(x = 5.5, y = 15, label = label),
              parse = TRUE, hjust = 1, size = 4, color = 'grey30',
              inherit.aes = FALSE) +
    theme_few(base_size = 14) +
    labs(x = 'Aridity Index (mean, 1980-2020)', y = 'n', fill = 'Q trend')
ggsave(here('figures', 'Figure_3.png'), fig3, width = 10, height = 9, dpi = 300)

cat('All composite figures saved to figures/\n')
