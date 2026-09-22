# Supplement: separates the effect of shortening the window (Landsat 1986-2022 -> 2001-2021)
# from the effect of changing sensor (Landsat -> MODIS on the same 2001-2021 window)
# on per-site GPP trend flags.
library(here)
source(here('src', 'setup.R'))
library(patchwork)

flag_levels <- c('increasing', 'non-significant', 'decreasing', 'insufficient data')
flag_cols <- c('increasing' = 'green4', 'decreasing' = 'brown4',
               'non-significant' = 'grey', 'insufficient data' = 'black')

new_full <- read_csv(here('data_working', 'trends', 'full_prisim_climate.csv'), show_col_types = FALSE)
old_full <- read_csv(here('data_working', 'archive_landsat_1980', 'trends', 'full_prisim_climate.csv'),
                     show_col_types = FALSE)
sites <- unique(new_full$site_code)

metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear_nTest.rds')) %>%
    distinct() %>%
    filter(agg_code == 'annual', site_code %in% sites)

csv_flags <- function(d, gpp_var) {
    d %>% filter(var == gpp_var) %>% add_flags() %>% select(site_code, flag)
}

window_flags <- function(gpp_col, start, end) {
    metrics %>%
        select(site_code, water_year, val = all_of(gpp_col)) %>%
        filter(water_year >= start, water_year <= end) %>%
        drop_na(val) %>%
        mutate(var = gpp_col) %>%
        select(site_code, water_year, var, val) %>%
        detect_trends() %>%
        add_flags() %>%
        select(site_code, flag)
}

runs <- list(
    'Landsat\n1986–2022' = csv_flags(old_full, 'gpp_CONUS_30m_median'),
    'Landsat\n2001–2021' = window_flags('gpp_CONUS_30m_median', 2001, 2021),
    'MODIS\n2001–2021'   = window_flags('gpp_global_500m_median', 2001, 2021),
    'MODIS\n2001–2023'   = csv_flags(new_full, 'gpp_global_500m_median')
)

flags <- tibble(site_code = sites)
for (nm in names(runs)) {
    flags <- flags %>% left_join(rename(runs[[nm]], !!nm := flag), by = 'site_code')
}
flags <- flags %>%
    mutate(across(-site_code, ~ factor(replace_na(.x, 'insufficient data'), levels = flag_levels)))

# ---- numbers ----
l_full <- flags[['Landsat\n1986–2022']]
l_2001 <- flags[['Landsat\n2001–2021']]
m_2001 <- flags[['MODIS\n2001–2021']]
m_2023 <- flags[['MODIS\n2001–2023']]

cat('\n=== flag counts per run (n = 166 sites) ===\n')
print(sapply(flags[-1], table))

cat('\n=== Window effect: Landsat 1986-2022 (rows) -> Landsat 2001-2021 (cols) ===\n')
print(table(l_full, l_2001))
cat('\n=== Sensor effect: Landsat 2001-2021 (rows) -> MODIS 2001-2021 (cols) ===\n')
print(table(l_2001, m_2001))
cat('\n=== For reference: MODIS 2001-2021 (rows) -> MODIS 2001-2023 (cols) ===\n')
print(table(m_2001, m_2023))

inc_full <- l_full == 'increasing'
cat(sprintf('\nWindow alone: of %d Landsat-1986-2022 increasing sites, %d (%.0f%%) become non-significant and %d (%.0f%%) stay increasing under Landsat 2001-2021\n',
            sum(inc_full), sum(inc_full & l_2001 == 'non-significant'), 100 * mean(l_2001[inc_full] == 'non-significant'),
            sum(inc_full & l_2001 == 'increasing'), 100 * mean(l_2001[inc_full] == 'increasing')))
inc_2001 <- l_2001 == 'increasing'
cat(sprintf('Sensor alone: of %d Landsat-2001-2021 increasing sites, %d (%.0f%%) become non-significant and %d (%.0f%%) stay increasing under MODIS 2001-2021\n',
            sum(inc_2001), sum(inc_2001 & m_2001 == 'non-significant'), 100 * mean(m_2001[inc_2001] == 'non-significant'),
            sum(inc_2001 & m_2001 == 'increasing'), 100 * mean(m_2001[inc_2001] == 'increasing')))

sig <- c('increasing', 'decreasing')
either <- l_2001 %in% sig | m_2001 %in% sig
both <- l_2001 %in% sig & m_2001 %in% sig
cat(sprintf('Same window, sites significant in either sensor: %d; significant in both: %d; same sign in both: %d; opposite sign: %d\n',
            sum(either), sum(both), sum(both & l_2001 == m_2001), sum(both & l_2001 != m_2001)))

# ---- figure ----
counts <- flags %>%
    pivot_longer(-site_code, names_to = 'run', values_to = 'flag') %>%
    mutate(run = factor(run, levels = names(runs))) %>%
    count(run, flag)
n_lab <- counts %>% filter(flag != 'insufficient data') %>%
    group_by(run) %>% summarize(n_run = sum(n), .groups = 'drop') %>%
    mutate(lab = paste0(run, '\n(n = ', n_run, ')'))
counts <- counts %>% left_join(select(n_lab, run, lab), by = 'run') %>%
    mutate(lab = factor(lab, levels = n_lab$lab))

p_a <- ggplot(counts, aes(x = lab, y = n, fill = flag)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = flag_cols, breaks = flag_levels, name = 'GPP trend') +
    theme_few(base_size = 12) +
    labs(x = NULL, y = 'Sites', title = '(a) Trend flags by product and window')

tile_labels <- c('increasing' = 'increasing', 'non-significant' = 'non-\nsignificant',
                 'decreasing' = 'decreasing', 'insufficient data' = 'insufficient\ndata')

tile_plot <- function(from, to, xlab, ylab, title) {
    d <- as.data.frame(table(from = from, to = to)) %>%
        mutate(from = factor(from, levels = rev(flag_levels)),
               to = factor(to, levels = flag_levels))
    ggplot(d, aes(x = to, y = from, fill = Freq)) +
        geom_tile(color = 'white') +
        geom_text(aes(label = Freq, color = Freq > max(Freq) / 2), size = 4, show.legend = FALSE) +
        scale_color_manual(values = c('TRUE' = 'white', 'FALSE' = 'black')) +
        scale_fill_gradient(low = 'grey95', high = 'grey20', guide = 'none') +
        scale_x_discrete(labels = tile_labels, position = 'top') +
        scale_y_discrete(labels = tile_labels) +
        coord_fixed() +
        theme_few(base_size = 11) +
        theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8),
              panel.border = element_blank()) +
        labs(x = xlab, y = ylab, title = title)
}

p_b <- tile_plot(l_full, l_2001, 'Landsat 2001–2021', 'Landsat 1986–2022',
                 '(b) Window effect (same sensor)')
p_c <- tile_plot(l_2001, m_2001, 'MODIS 2001–2021', 'Landsat 2001–2021',
                 '(c) Sensor effect (same window)')

fig <- p_a + p_b + p_c + plot_layout(widths = c(1.4, 1, 1)) &
    theme(plot.title = element_text(size = 11))

ggsave(here('figures', 'Figure_S3.png'), fig, width = 12, height = 5, dpi = 300)
cat('\nFigure saved to figures/Figure_S3.png\n')
