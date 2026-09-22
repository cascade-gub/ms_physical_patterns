# Before/after comparison of the Landsat-1980 vs MODIS-2001 trend runs.
# Reads the archived pre-pivot outputs in data_working/archive_landsat_1980/
# and the current outputs in data_working/.
library(here)
source(here('src', 'setup.R'))

old_dir <- here('data_working', 'archive_landsat_1980')
new_dir <- here('data_working')

read_flags <- function(path, run) {
    read_csv(path, show_col_types = FALSE) %>%
        add_flags() %>%
        mutate(var = if_else(var %in% c('gpp_CONUS_30m_median', 'gpp_conus',
                                        'gpp_global_500m_median', 'gpp_modis'),
                             'gpp', var),
               run = run) %>%
        select(run, site_code, var, trend, flag, n, start, end)
}

old_full <- read_flags(file.path(old_dir, 'trends', 'full_prisim_climate.csv'), 'landsat_1980')
new_full <- read_flags(file.path(new_dir, 'trends', 'full_prisim_climate.csv'), 'modis_2001')
old_q    <- read_flags(file.path(old_dir, 'trends', 'best_run_prisim.csv'),    'landsat_1980')
new_q    <- read_flags(file.path(new_dir, 'trends', 'best_run_prisim.csv'),    'modis_2001')

flag_table <- function(d, vars) {
    d %>% filter(var %in% vars) %>%
        count(var, run, flag) %>%
        pivot_wider(names_from = run, values_from = n, values_fill = 0) %>%
        arrange(var, flag)
}

transition_tables <- function(old, new, vars) {
    tr <- full_join(select(old, site_code, var, old = flag),
                    select(new, site_code, var, new = flag),
                    by = c('site_code', 'var')) %>%
        filter(var %in% vars) %>%
        mutate(across(c(old, new), ~ replace_na(.x, 'absent')))
    for (v in vars) {
        cat('\n--', v, '(rows = Landsat/1980, cols = MODIS/2001) --\n')
        print(table(old = tr$old[tr$var == v], new = tr$new[tr$var == v]))
    }
    invisible(tr)
}

cat('\n=== Full-record climate trends: flag counts ===\n')
print(flag_table(bind_rows(old_full, new_full), c('temp_mean', 'precip_mean', 'gpp')), n = Inf)

cat('\n=== Full-record record length (median n, start, end) ===\n')
bind_rows(old_full, new_full) %>% filter(var %in% c('temp_mean', 'precip_mean', 'gpp')) %>%
    group_by(run, var) %>%
    summarize(sites = n(), med_n = median(n), start = min(start), end = max(end), .groups = 'drop') %>%
    print(n = Inf)

cat('\n=== Cut-to-Q trends: flag counts ===\n')
print(flag_table(bind_rows(old_q, new_q), c('q_mean', 'runoff_ratio', 'temp_mean', 'precip_mean', 'gpp')), n = Inf)

cat('\n=== Per-site flag transitions, full record ===\n')
transition_tables(old_full, new_full, c('gpp', 'temp_mean', 'precip_mean'))

cat('\n=== Per-site flag transitions, cut to Q ===\n')
transition_tables(old_q, new_q, c('q_mean', 'runoff_ratio', 'gpp'))

cat('\n=== NWD coarse grouping (% of sites) ===\n')
grp <- bind_rows(
    read_csv(file.path(old_dir, 'site_groupings_by_prsim_trend.csv'), show_col_types = FALSE) %>%
        transmute(source = 'ms', run = 'landsat_1980', coarse_grouping),
    read_csv(file.path(new_dir, 'site_groupings_by_prsim_trend.csv'), show_col_types = FALSE) %>%
        transmute(source = 'ms', run = 'modis_2001', coarse_grouping),
    read_csv(file.path(old_dir, 'grid_groups.csv'), show_col_types = FALSE) %>%
        transmute(source = 'grid', run = 'landsat_1980', coarse_grouping),
    read_csv(file.path(new_dir, 'grid_groups.csv'), show_col_types = FALSE) %>%
        transmute(source = 'grid', run = 'modis_2001', coarse_grouping))
grp %>% count(source, run, coarse_grouping) %>%
    group_by(source, run) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup() %>%
    select(-n) %>%
    pivot_wider(names_from = run, values_from = pct, values_fill = 0) %>%
    arrange(source, coarse_grouping) %>%
    print(n = Inf)

bind_rows(mutate(old_full, set = 'full'), mutate(new_full, set = 'full'),
          mutate(old_q, set = 'cut_to_q'), mutate(new_q, set = 'cut_to_q')) %>%
    write_csv(here('data_working', 'pivot_comparison_trends_long.csv'))
cat('\nLong table written to data_working/pivot_comparison_trends_long.csv\n')
