library(here)
source(here('src', 'setup.R'))

paper_sites <- read_csv(here('data_working', 'trends', 'best_run_prisim.csv')) %>%
    pull(site_code) %>%
    unique()

veg <- read_feather(here('data_raw', 'ms', 'v2', 'spatial_timeseries_vegetation.feather')) %>%
    filter(var %in% c('gpp_global_500m_median', 'evi_median'),
           site_code %in% paper_sites)

# EVI has dates (16-day), GPP is annual (date=NA) — use calendar year for both
veg_annual <- veg %>%
    group_by(site_code, year, var) %>%
    summarize(val = mean(val, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = var, values_from = val) %>%
    drop_na(gpp_global_500m_median, evi_median)

site_overlap <- veg_annual %>%
    group_by(site_code) %>%
    summarize(n_years = n(), start = min(year), end = max(year), .groups = 'drop') %>%
    filter(n_years >= 10)

cat(nrow(site_overlap), 'of', length(paper_sites), 'sites with >= 10 overlapping years\n')

trend_long <- veg_annual %>%
    filter(site_code %in% site_overlap$site_code) %>%
    rename(water_year = year) %>%
    pivot_longer(cols = c(gpp_global_500m_median, evi_median),
                 names_to = 'var', values_to = 'val')

trends <- detect_trends(trend_long)

site_means <- veg_annual %>%
    filter(site_code %in% site_overlap$site_code) %>%
    group_by(site_code) %>%
    summarize(mean_gpp = mean(gpp_global_500m_median, na.rm = TRUE),
              mean_evi = mean(evi_median, na.rm = TRUE), .groups = 'drop') %>%
    filter(mean_gpp > 0, mean_evi > 0)

trend_wide <- trends %>%
    filter(code == 'good') %>%
    add_flags() %>%
    select(site_code, var, trend, flag) %>%
    pivot_wider(names_from = var, values_from = c(trend, flag), names_sep = '___') %>%
    drop_na() %>%
    left_join(site_means, by = 'site_code') %>%
    drop_na(mean_gpp, mean_evi) %>%
    mutate(
        rel_gpp = (trend___gpp_global_500m_median / mean_gpp) * 100,
        rel_evi = (trend___evi_median / mean_evi) * 100,
        agreement = case_when(
            flag___gpp_global_500m_median != 'non-significant' &
                flag___evi_median != 'non-significant' &
                flag___gpp_global_500m_median == flag___evi_median ~ 'Both sig., same sign',
            flag___gpp_global_500m_median != 'non-significant' &
                flag___evi_median != 'non-significant' &
                flag___gpp_global_500m_median != flag___evi_median ~ 'Both sig., opposite sign',
            flag___gpp_global_500m_median == 'non-significant' &
                flag___evi_median == 'non-significant' ~ 'Both non-significant',
            TRUE ~ 'One non-significant'
        )
    )

same_sign <- sum(sign(trend_wide$rel_gpp) == sign(trend_wide$rel_evi))
rma_slope <- sd(trend_wide$rel_evi) / sd(trend_wide$rel_gpp)
rma_int   <- mean(trend_wide$rel_evi) - rma_slope * mean(trend_wide$rel_gpp)
r_val     <- cor(trend_wide$rel_gpp, trend_wide$rel_evi)

cat('n =', nrow(trend_wide), '| RMA slope =', round(rma_slope, 2),
    '| r =', round(r_val, 2), '| sign agree =', same_sign, '/', nrow(trend_wide), '\n')
print(table(GPP = trend_wide$flag___gpp_global_500m_median,
            EVI = trend_wide$flag___evi_median))

axis_lim <- max(abs(c(trend_wide$rel_gpp, trend_wide$rel_evi)), na.rm = TRUE) * 1.1

p <- ggplot(trend_wide, aes(x = rel_gpp, y = rel_evi, color = agreement)) +
    geom_hline(yintercept = 0, linewidth = 0.3, color = 'grey60') +
    geom_vline(xintercept = 0, linewidth = 0.3, color = 'grey60') +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'grey40') +
    geom_abline(slope = rma_slope, intercept = rma_int, color = 'black', linewidth = 0.7) +
    geom_point(size = 2.5, alpha = 0.8) +
    scale_color_manual(
        values = c('Both sig., same sign' = '#2166AC',
                    'Both non-significant' = '#B0B0B0',
                    'One non-significant' = '#FDAE61',
                    'Both sig., opposite sign' = '#B2182B'),
        name = 'Trend significance', drop = FALSE) +
    coord_fixed(xlim = c(-axis_lim, axis_lim), ylim = c(-axis_lim, axis_lim)) +
    labs(x = 'MODIS GPP trend (%/yr)',
         y = 'MODIS EVI trend (%/yr)',
         caption = paste0('n = ', nrow(trend_wide),
                          ' sites, matched per-site windows within ',
                          min(site_overlap$start), '–', max(site_overlap$end),
                          '\nRMA slope = ', round(rma_slope, 2),
                          ' | r = ', round(r_val, 2),
                          ' | ', same_sign, '/', nrow(trend_wide),
                          ' agree on trend sign',
                          '\nDashed = 1:1 | Solid = RMA fit')) +
    theme_few() +
    theme(plot.caption = element_text(size = 8, hjust = 0))

ggsave(here('figures', 'modis_gpp_vs_evi_trends.png'), p,
       width = 7, height = 7, dpi = 300)

cat('\nFigure saved to figures/modis_gpp_vs_evi_trends.png\n')
