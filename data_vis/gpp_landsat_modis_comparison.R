library(here)
source(here('src', 'setup.R'))

# Sites used in the paper analysis
paper_sites <- read_csv(here('data_working', 'trends', 'best_run_prisim.csv')) %>%
    pull(site_code) %>%
    unique()

# Load vegetation data
# gpp_CONUS_30m_median = Landsat 30m, 16-day composites (1986–2021)
# gpp_global_500m_median = MODIS 500m (MOD17A3), annual (2001–2023)
# evi_median = MODIS 250m (MOD13Q1), 16-day composites (2000–2024)
veg <- read_feather(here('data_raw', 'ms', 'v2', 'spatial_timeseries_vegetation.feather')) %>%
    filter(var %in% c('gpp_CONUS_30m_median', 'gpp_global_500m_median', 'evi_median'),
           site_code %in% paper_sites)

# --- Helper: build matched annual data for a pair of products ---
build_annual_pair <- function(veg_data, var_x, var_y) {
    # For products with dates, use water year; for annual-only (date=NA), use year
    dat <- veg_data %>%
        filter(var %in% c(var_x, var_y)) %>%
        mutate(wy = case_when(
            !is.na(date) ~ case_when(month(date) %in% c(10,11,12) ~ year + 1L,
                                     TRUE ~ year),
            TRUE ~ year
        )) %>%
        group_by(site_code, wy, var) %>%
        summarize(val = mean(val, na.rm = TRUE), .groups = 'drop') %>%
        pivot_wider(names_from = var, values_from = val)

    names(dat)[names(dat) == var_x] <- 'x_val'
    names(dat)[names(dat) == var_y] <- 'y_val'

    dat %>%
        drop_na(x_val, y_val) %>%
        rename(water_year = wy)
}

# --- Helper: compute paired trends ---
compute_paired_trends <- function(annual_data, var_x_name, var_y_name) {
    site_overlap <- annual_data %>%
        group_by(site_code) %>%
        summarize(n_years = n(),
                  start = min(water_year),
                  end = max(water_year),
                  .groups = 'drop') %>%
        filter(n_years >= 10)

    cat(nrow(site_overlap), 'of', length(paper_sites),
        'paper sites have >= 10 overlapping years\n')

    trend_long <- annual_data %>%
        filter(site_code %in% site_overlap$site_code) %>%
        pivot_longer(cols = c(x_val, y_val),
                     names_to = 'var', values_to = 'val') %>%
        mutate(var = case_when(var == 'x_val' ~ var_x_name,
                               var == 'y_val' ~ var_y_name))

    trends <- detect_trends(trend_long)

    dropped <- site_overlap$site_code[!site_overlap$site_code %in%
        (trends %>% filter(code == 'good') %>% pull(site_code) %>% unique())]
    if(length(dropped) > 0) {
        cat('Sites dropped (confint.zyp failure):', paste(dropped, collapse = ', '), '\n')
    }

    site_means <- annual_data %>%
        filter(site_code %in% site_overlap$site_code) %>%
        group_by(site_code) %>%
        summarize(mean_x = mean(x_val, na.rm = TRUE),
                  mean_y = mean(y_val, na.rm = TRUE),
                  .groups = 'drop') %>%
        filter(mean_x > 0, mean_y > 0)

    trend_wide <- trends %>%
        filter(code == 'good') %>%
        add_flags() %>%
        select(site_code, var, trend, flag) %>%
        pivot_wider(names_from = var,
                    values_from = c(trend, flag),
                    names_sep = '___') %>%
        drop_na()

    x_trend_col <- paste0('trend___', var_x_name)
    y_trend_col <- paste0('trend___', var_y_name)
    x_flag_col  <- paste0('flag___', var_x_name)
    y_flag_col  <- paste0('flag___', var_y_name)

    trend_wide <- trend_wide %>%
        left_join(site_means, by = 'site_code') %>%
        drop_na(mean_x, mean_y) %>%
        mutate(
            rel_x = (.data[[x_trend_col]] / mean_x) * 100,
            rel_y = (.data[[y_trend_col]] / mean_y) * 100,
            flag_x = .data[[x_flag_col]],
            flag_y = .data[[y_flag_col]],
            agreement = case_when(
                flag_x != 'non-significant' & flag_y != 'non-significant' &
                    flag_x == flag_y ~ 'Both sig., same sign',
                flag_x != 'non-significant' & flag_y != 'non-significant' &
                    flag_x != flag_y ~ 'Both sig., opposite sign',
                flag_x == 'non-significant' &
                    flag_y == 'non-significant' ~ 'Both non-significant',
                TRUE ~ 'One non-significant'
            )
        )

    list(data = trend_wide, overlap = site_overlap)
}

# --- Helper: make 1:1 panel ---
make_panel <- function(trend_wide, site_overlap, x_label, y_label) {
    same_sign <- sum(sign(trend_wide$rel_x) == sign(trend_wide$rel_y))
    rma_slope <- sd(trend_wide$rel_y) / sd(trend_wide$rel_x)
    rma_int   <- mean(trend_wide$rel_y) - rma_slope * mean(trend_wide$rel_x)
    r_val     <- cor(trend_wide$rel_x, trend_wide$rel_y)

    cat('\n---', y_label, '---\n')
    cat('n =', nrow(trend_wide), '| RMA slope =', round(rma_slope, 2),
        '| r =', round(r_val, 2), '| sign agree =',
        same_sign, '/', nrow(trend_wide), '\n')
    print(table(Landsat = trend_wide$flag_x, Comparison = trend_wide$flag_y))

    axis_lim <- max(abs(c(trend_wide$rel_x, trend_wide$rel_y)), na.rm = TRUE) * 1.1
    yr <- paste0(min(site_overlap$start), '–', max(site_overlap$end))

    ggplot(trend_wide, aes(x = rel_x, y = rel_y, color = agreement)) +
        geom_hline(yintercept = 0, linewidth = 0.3, color = 'grey60') +
        geom_vline(xintercept = 0, linewidth = 0.3, color = 'grey60') +
        geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'grey40') +
        geom_abline(slope = rma_slope, intercept = rma_int,
                    color = 'black', linewidth = 0.7) +
        geom_point(size = 2.5, alpha = 0.8) +
        scale_color_manual(
            values = c('Both sig., same sign' = '#2166AC',
                        'Both non-significant' = '#B0B0B0',
                        'One non-significant' = '#FDAE61',
                        'Both sig., opposite sign' = '#B2182B'),
            name = 'Trend significance',
            drop = FALSE) +
        coord_fixed(xlim = c(-axis_lim, axis_lim),
                    ylim = c(-axis_lim, axis_lim)) +
        labs(x = paste0(x_label, ' (%/yr)'),
             y = paste0(y_label, ' (%/yr)'),
             subtitle = paste0('n = ', nrow(trend_wide),
                               ' | RMA slope = ', round(rma_slope, 2),
                               ' | r = ', round(r_val, 2),
                               ' | ', same_sign, '/', nrow(trend_wide),
                               ' agree on sign')) +
        theme_few() +
        theme(plot.subtitle = element_text(size = 8),
              legend.position = 'none')
}

# ============================================================
# Panel A: Landsat GPP vs MODIS GPP (annual, 500m)
# ============================================================
cat('\n=== PANEL A: Landsat GPP vs MODIS GPP ===\n')
gpp_annual <- build_annual_pair(veg, 'gpp_CONUS_30m_median', 'gpp_global_500m_median')
gpp_res    <- compute_paired_trends(gpp_annual, 'landsat_gpp', 'modis_gpp')
panel_a    <- make_panel(gpp_res$data, gpp_res$overlap,
                          'Landsat GPP trend', 'MODIS GPP trend')

# ============================================================
# Panel B: Landsat GPP vs MODIS EVI (16-day, 250m)
# ============================================================
cat('\n=== PANEL B: Landsat GPP vs MODIS EVI ===\n')
evi_annual <- build_annual_pair(veg, 'gpp_CONUS_30m_median', 'evi_median')
evi_res    <- compute_paired_trends(evi_annual, 'landsat_gpp', 'modis_evi')
panel_b    <- make_panel(evi_res$data, evi_res$overlap,
                          'Landsat GPP trend', 'MODIS EVI trend')

# ============================================================
# Compose two-panel figure
# ============================================================
combined <- (panel_a + ggtitle('(a) MODIS GPP (500m, annual)')) +
    (panel_b + ggtitle('(b) MODIS EVI (250m, 16-day)')) +
    plot_layout(guides = 'collect') &
    theme(legend.position = 'bottom',
          plot.title = element_text(size = 10))

ggsave(here('figures', 'gpp_landsat_vs_modis_trends.png'), combined,
       width = 12, height = 6.5, dpi = 300)

cat('\nFigure saved to figures/gpp_landsat_vs_modis_trends.png\n')
