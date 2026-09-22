# Single source of truth for every number quoted in the manuscript.
# Writes data_working/manuscript_facts.txt. "old" = archived Landsat/1980+ run,
# "new" = current MODIS/2001-2023 run.
library(here)
source(here('src', 'setup.R'))

old_dir <- here('data_working', 'archive_landsat_1980')
new_dir <- here('data_working')
out_file <- here('data_working', 'manuscript_facts.txt')

pct <- function(n, d) sprintf('%d of %d (%.1f%%)', n, d, 100 * n / d)
hdr <- function(x) cat('\n\n==================== ', x, ' ====================\n', sep = '')
gpp_alias <- function(v) if_else(v %in% c('gpp_CONUS_30m_median', 'gpp_conus',
                                          'gpp_global_500m_median', 'gpp_modis'), 'gpp', v)

# ---- load ---------------------------------------------------------------
metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear_nTest.rds')) %>% distinct()
ann <- metrics %>% filter(agg_code == 'annual')

full_new <- read_csv(file.path(new_dir, 'trends', 'full_prisim_climate.csv'), show_col_types = FALSE)
full_old <- read_csv(file.path(old_dir, 'trends', 'full_prisim_climate.csv'), show_col_types = FALSE)
q_new    <- read_csv(file.path(new_dir, 'trends', 'best_run_prisim.csv'),    show_col_types = FALSE)
q_old    <- read_csv(file.path(old_dir, 'trends', 'best_run_prisim.csv'),    show_col_types = FALSE)

grp_new <- read_csv(file.path(new_dir, 'site_groupings_by_prsim_trend.csv'), show_col_types = FALSE)
grp_old <- read_csv(file.path(old_dir, 'site_groupings_by_prsim_trend.csv'), show_col_types = FALSE)
grid_new <- read_csv(file.path(new_dir, 'grid_groups.csv'), show_col_types = FALSE)
grid_old <- read_csv(file.path(old_dir, 'grid_groups.csv'), show_col_types = FALSE)
grid_tr  <- read_csv(file.path(new_dir, 'grid_trends.csv'), show_col_types = FALSE)

site_status <- ms_site_data %>% select(site_code, domain, ws_status) %>% distinct()

# ---- aridity index, 2001-2023, as in grouping_histograms.R ----------------
daymet <- ms_load_product(here('data_raw', 'ms'),
                          prodname = 'ws_attr_CAMELS_Daymet_forcings', warn = FALSE)
p <- ms_load_product(my_ms_dir, prodname = 'ws_attr_timeseries:climate',
                     filter_vars = 'precip_median', warn = FALSE) %>%
    select(-var, -year, -pctCellErr, precip_median = val)
ai_year <- inner_join(p, daymet, by = c('network', 'domain', 'site_code', 'date')) %>%
    mutate(year = as.integer(as.character(water_year(date, origin = 'usgs')))) %>%
    group_by(year, site_code) %>%
    summarize(aridity_index = sum(`pet(mm)`) / sum(`prcp(mm/day)`), .groups = 'drop')
ai_new <- ai_year %>% filter(year >= analysis_start_year, year <= analysis_end_year) %>%
    group_by(site_code) %>% summarize(mean_ai = mean(aridity_index, na.rm = TRUE), .groups = 'drop')
ai_old <- ai_year %>% filter(year %in% 1980:2020) %>%
    group_by(site_code) %>% summarize(mean_ai_1980_2020 = mean(aridity_index, na.rm = TRUE), .groups = 'drop')
rm(daymet, p)

sink(out_file, split = TRUE)
cat('MANUSCRIPT FACT SHEET - generated ', format(Sys.time(), '%Y-%m-%d %H:%M'),
    ' by data_vis/manuscript_facts.R\n',
    'Windows: new = ', analysis_start_year, '-', analysis_end_year,
    ' (MODIS GPP); old = archived Landsat / 1980+ run\n', sep = '')

# ---- 1. site pools --------------------------------------------------------
hdr('1. SITE POOLS')
cat('All sites in discharge_metrics_siteyear_nTest.rds:', n_distinct(metrics$site_code), '\n')
cat('Sites with any annual q_mean in RDS:', ann %>% filter(!is.na(q_mean)) %>% distinct(site_code) %>% nrow(), '\n')
fc_new <- unique(full_new$site_code); fc_old <- unique(full_old$site_code)
cat('Full-climate trend set (full_prisim_climate.csv): new', length(fc_new), '| old', length(fc_old), '\n')
st166 <- grp_new %>% count(ws_status)
cat('  new set by ws_status:', paste(st166$ws_status, st166$n, collapse = '; '), '\n')
qg_new <- q_new %>% filter(var == 'q_mean', code == 'good') %>% left_join(site_status, by = 'site_code')
qg_old <- q_old %>% filter(var == 'q_mean', code == 'good') %>% left_join(site_status, by = 'site_code')
cat('Sites with a good q_mean trend: new', nrow(qg_new), '| old', nrow(qg_old), '\n')
cat('  new by ws_status:', paste(names(table(qg_new$ws_status)), table(qg_new$ws_status), collapse = '; '), '\n')
cat('  old by ws_status:', paste(names(table(qg_old$ws_status)), table(qg_old$ws_status), collapse = '; '), '\n')
n_ne <- sum(grp_new$ws_status == 'non-experimental', na.rm = TRUE)
n_ne_q <- sum(qg_new$ws_status == 'non-experimental', na.rm = TRUE)
in166 <- qg_new %>% filter(site_code %in% fc_new)
n_ne_q166 <- sum(in166$ws_status == 'non-experimental', na.rm = TRUE)
cat('Q-trend sites that are ALSO in the 166 full-climate set: new', nrow(in166),
    '(non-exp', n_ne_q166, ') | old', sum(qg_old$site_code %in% fc_old), '\n')
cat('Q-trend sites OUTSIDE the full-climate set (no PRISM/GPP trends -> cannot be placed in Figs 3-4):\n')
print(qg_new %>% filter(!site_code %in% fc_new) %>% add_flags() %>%
      select(domain, site_code, ws_status, flag, n, start, end) %>% as.data.frame())
cat('RECOMMENDED DENOMINATORS (consistent with the 166-site pool used in every figure):\n')
cat('  Non-experimental: full-climate', n_ne, '-> with Q record', pct(n_ne_q166, n_ne),
    '  [all non-exp Q-trend sites regardless of climate data:', n_ne_q, ']\n')
cat('  All sites: full-climate', length(fc_new), '-> with Q record', pct(nrow(in166), length(fc_new)),
    '  [all Q-trend sites:', nrow(qg_new), ']\n')
cat('  Old (for reference): full-climate', length(fc_old), '-> with Q record',
    pct(sum(qg_old$site_code %in% fc_old), length(fc_old)), '  [all:', nrow(qg_old), ']\n')

# ---- 2. Q flags -----------------------------------------------------------
hdr('2. STREAMFLOW (q_mean) TREND FLAGS')
qf_new <- q_new %>% add_flags() %>% filter(var == 'q_mean') %>% left_join(site_status, by = 'site_code')
qf_old <- q_old %>% add_flags() %>% filter(var == 'q_mean') %>% left_join(site_status, by = 'site_code')
cat('NEW flag x ws_status:\n'); print(table(qf_new$flag, qf_new$ws_status, useNA = 'ifany'))
cat('OLD flag x ws_status:\n'); print(table(qf_old$flag, qf_old$ws_status, useNA = 'ifany'))
ne_q <- qf_new %>% filter(ws_status == 'non-experimental')
ne_q166 <- ne_q %>% filter(site_code %in% fc_new)
cat('NEW non-exp significant (inc+dec), 166-pool denominator:', pct(sum(ne_q166$flag %in% c('increasing', 'decreasing')), nrow(ne_q166)),
    '| increasing', sum(ne_q166$flag == 'increasing'), '| decreasing', sum(ne_q166$flag == 'decreasing'), '\n')
cat('NEW non-exp significant, all-Q-sites denominator:', pct(sum(ne_q$flag %in% c('increasing', 'decreasing')), nrow(ne_q)), '\n')
cat('NEW all-sites significant, 166-pool denominator:',
    pct(sum(qf_new$flag[qf_new$site_code %in% fc_new] %in% c('increasing', 'decreasing')), sum(qf_new$site_code %in% fc_new)),
    '| all-Q-sites denominator:', pct(sum(qf_new$flag %in% c('increasing', 'decreasing')), nrow(qf_new)), '\n')
cat('\nSignificant q_mean sites (NEW), with NWD grouping and mean AI', analysis_start_year, '-', analysis_end_year, ':\n')
sig <- qf_new %>% filter(flag %in% c('increasing', 'decreasing')) %>%
    select(domain, site_code, ws_status, flag, trend, n, start, end) %>%
    left_join(grp_new %>% select(site_code, grouping, coarse_grouping), by = 'site_code') %>%
    left_join(ai_new, by = 'site_code') %>%
    mutate(trend = round(trend, 4), mean_ai = round(mean_ai, 2)) %>%
    arrange(flag, domain, site_code)
print(as.data.frame(sig))
cat('\nSignificant q_mean sites (OLD, for reference):\n')
print(qf_old %>% filter(flag %in% c('increasing', 'decreasing')) %>%
      select(domain, site_code, ws_status, flag, trend, n, start, end) %>%
      mutate(trend = round(trend, 4)) %>% arrange(flag, domain) %>% as.data.frame())
cat('\nGREEN4 (Niwot) NEW status:',
    { g4 <- qf_new %>% filter(site_code == 'GREEN4'); if (nrow(g4)) g4$flag else 'no q_mean trend (insufficient record in window)' }, '\n')

# ---- 3. aridity of sites with vs without Q record -------------------------
hdr('3. ARIDITY OF NON-EXP SITES WITH vs WITHOUT A Q RECORD (NEW)')
ne_all <- grp_new %>% filter(ws_status == 'non-experimental') %>%
    left_join(ai_new, by = 'site_code') %>%
    mutate(has_q = if_else(is.na(q_flag), 'no Q record', 'has Q record'))
print(ne_all %>% group_by(has_q) %>%
      summarize(n = n(), n_with_ai = sum(!is.na(mean_ai)),
                ai_mean = round(mean(mean_ai, na.rm = TRUE), 2),
                ai_median = round(median(mean_ai, na.rm = TRUE), 2),
                ai_q25 = round(quantile(mean_ai, .25, na.rm = TRUE), 2),
                ai_q75 = round(quantile(mean_ai, .75, na.rm = TRUE), 2),
                n_arid_ai_gt_1 = sum(mean_ai > 1, na.rm = TRUE),
                pct_arid = round(100 * mean(mean_ai > 1, na.rm = TRUE), 1),
                .groups = 'drop') %>% as.data.frame())
wt <- tryCatch(wilcox.test(mean_ai ~ has_q, data = ne_all)$p.value, error = function(e) NA)
cat('Wilcoxon p (mean_ai, has Q vs no Q):', signif(wt, 3), '\n')
cat('Domains of non-exp sites LOST (in full-climate set, no Q record):\n')
print(ne_all %>% filter(has_q == 'no Q record') %>% count(domain, sort = TRUE) %>% as.data.frame())
cat('Domains of non-exp sites KEPT (with Q record):\n')
print(ne_all %>% filter(has_q == 'has Q record') %>% count(domain, sort = TRUE) %>% as.data.frame())

# ---- 4. climate / GPP flags -----------------------------------------------
hdr('4. FULL-RECORD CLIMATE AND GPP TREND FLAGS (166-site set)')
ff_new <- full_new %>% add_flags() %>% mutate(var = gpp_alias(var))
ff_old <- full_old %>% add_flags() %>% mutate(var = gpp_alias(var))
cat('NEW (', analysis_start_year, '-', analysis_end_year, '):\n', sep = ''); print(table(ff_new$var, ff_new$flag))
cat('OLD (Landsat 1986-2022, PRISM 1981-2024):\n'); print(table(ff_old$var, ff_old$flag))
for (v in c('temp_mean', 'precip_mean', 'gpp')) {
    s <- ff_new %>% filter(var == v)
    cat(sprintf('NEW %s: increasing %s | decreasing %s | non-significant %s\n', v,
                pct(sum(s$flag == 'increasing'), nrow(s)), pct(sum(s$flag == 'decreasing'), nrow(s)),
                pct(sum(s$flag == 'non-significant'), nrow(s))))
}
cat('GPP sites with no usable MODIS trend (code != good; MOD17 urban fill masked):\n')
print(full_new %>% filter(grepl('gpp', var), code != 'good') %>% select(site_code, code) %>%
      left_join(site_status, by = 'site_code') %>% as.data.frame())
ws <- read_feather(here('data_raw', 'ms', 'v2', 'watershed_summaries.feather')) %>%
    filter(site_code %in% fc_new) %>% select(site_code, area_ha = area) %>% mutate(area_km2 = area_ha / 100)
cat('Watershed area (km2) of the', nrow(ws), 'full-climate sites: median', round(median(ws$area_km2), 2),
    '| IQR', paste(round(quantile(ws$area_km2, c(.25, .75)), 2), collapse = '-'), '\n')
cat('MODIS 500 m pixel = 0.25 km2. Sites < 1 pixel:', pct(sum(ws$area_km2 < 0.25), nrow(ws)),
    '| < 4 pixels (1 km2):', pct(sum(ws$area_km2 < 1), nrow(ws)),
    '| < 10 pixels (2.5 km2):', pct(sum(ws$area_km2 < 2.5), nrow(ws)), '\n')

# ---- 5. NWD groupings and quadrants ---------------------------------------
hdr('5. NET WATER DEMAND (NWD) GROUPINGS')
show_groups <- function(d, label) {
    cat('\n', label, ' (n = ', nrow(d), ')\n', sep = '')
    cat('  fine:  '); print(sort(table(d$grouping), decreasing = TRUE))
    cg <- table(d$coarse_grouping)
    cat('  coarse: ', paste(sprintf('%s %s', names(cg), pct(as.integer(cg), nrow(d))), collapse = ' | '), '\n')
}
show_groups(grp_new, 'MS all sites, NEW')
show_groups(grp_new %>% filter(ws_status == 'non-experimental'), 'MS non-experimental, NEW')
show_groups(grid_new, 'Grid cells with complete trends, NEW')
show_groups(grp_old, 'MS all sites, OLD')
show_groups(grid_old, 'Grid, OLD')
ne_g <- grp_new %>% filter(ws_status == 'non-experimental') %>%
    mutate(quadrant = case_when(trend_temp_mean > 0 & trend_precip_mean > 0 ~ 'hotter-wetter',
                                trend_temp_mean > 0 & trend_precip_mean < 0 ~ 'hotter-drier',
                                trend_temp_mean < 0 & trend_precip_mean > 0 ~ 'cooler-wetter',
                                trend_temp_mean < 0 & trend_precip_mean < 0 ~ 'cooler-drier'),
           q_flag = replace_na(q_flag, 'no Q record'))
cat('\nQuadrants are by SIGN of the temperature / precipitation Sen slope, not significance.\n')
cat('Non-exp quadrant counts:\n'); print(table(ne_g$quadrant))
cat('Non-exp quadrant x Q flag:\n'); print(table(ne_g$quadrant, ne_g$q_flag))
cat('Non-exp quadrant x GPP flag:\n'); print(table(ne_g$quadrant, replace_na(ne_g$flag_gpp_global_500m_median, 'no GPP')))

# ---- 6. grid --------------------------------------------------------------
hdr('6. REFERENCE GRID')
ppt <- read_csv(here('data_raw', 'grid_csvs', 'ppt.csv'), show_col_types = FALSE) %>%
    distinct(FID, geometry) %>% group_by(FID) %>% slice(1) %>% ungroup() %>%
    mutate(lon = as.numeric(gsub('c\\((.*),.*', '\\1', geometry)),
           lat = as.numeric(gsub('c\\(.*,(.*)\\)', '\\1', geometry))) %>%
    select(site_code = FID, lon, lat)
cat('Grid points:', nrow(ppt), '| cells with complete trends (grid_groups.csv): new', nrow(grid_new), '| old', nrow(grid_old), '\n')
gt <- grid_tr %>% add_flags()
cat('Grid flag counts by var (NEW):\n'); print(table(gt$var, gt$flag))
region <- function(lon, lat) case_when(
    lon < -115 ~ 'West (lon < -115)',
    lon < -104 ~ 'Mountain (-115 to -104)',
    lon < -95  ~ 'Plains (-104 to -95)',
    lat >= 38 & lon < -80 ~ 'Midwest/Great Lakes (-95 to -80, lat >= 38)',
    lat >= 38 ~ 'Northeast (lon >= -80, lat >= 38)',
    TRUE ~ 'South/Southeast (lon >= -95, lat < 38)')
gneg <- grid_new %>% filter(coarse_grouping == '(-)') %>% left_join(ppt, by = 'site_code') %>%
    mutate(region = region(lon, lat))
gall <- grid_new %>% left_join(ppt, by = 'site_code') %>% mutate(region = region(lon, lat))
cat('Region scheme: 6 lon/lat bins as labelled below.\n')
cat('"(-)" grid cells by region (n = ', nrow(gneg), '); share of that region\'s cells in parentheses:\n', sep = '')
print(gneg %>% count(region) %>% left_join(gall %>% count(region, name = 'n_region'), by = 'region') %>%
      mutate(share = sprintf('%.1f%%', 100 * n / n_region)) %>% arrange(-n) %>% as.data.frame())
cat('"(-)" fine groupings:\n'); print(table(gneg$grouping))
cat('MS "(-)" sites (NEW):\n'); print(grp_new %>% filter(coarse_grouping == '(-)') %>%
      select(domain, site_code, ws_status, grouping) %>% as.data.frame())

# ---- 7. record lengths and data years -------------------------------------
hdr('7. RECORD LENGTHS AND DATA YEARS')
rl <- function(d, label) d %>% filter(code == 'good') %>% mutate(var = gpp_alias(var)) %>%
    filter(var %in% c('temp_mean', 'precip_mean', 'gpp', 'q_mean')) %>%
    group_by(var) %>% summarize(sites = n(), med_n = median(n), med_start = median(start),
                                min_start = min(start), med_end = median(end), max_end = max(end), .groups = 'drop') %>%
    mutate(set = label)
print(bind_rows(rl(full_new, 'full NEW'), rl(q_new, 'cut-to-Q NEW'),
                rl(full_old, 'full OLD'), rl(q_old, 'cut-to-Q OLD')) %>% as.data.frame())
yr <- function(col) range(ann$water_year[!is.na(ann[[col]])], na.rm = TRUE)
cat('Data years in RDS (annual rows): PRISM temp_mean', paste(yr('temp_mean'), collapse = '-'),
    '| PRISM precip_mean', paste(yr('precip_mean'), collapse = '-'),
    '| MODIS GPP', paste(yr('gpp_global_500m_median'), collapse = '-'),
    '| Landsat GPP', paste(yr('gpp_CONUS_30m_median'), collapse = '-'),
    '| q_mean', paste(yr('q_mean'), collapse = '-'), '\n')
cat('Daymet (aridity index): water years', paste(range(ai_year$year), collapse = '-'),
    '| AI climatology window used:', analysis_start_year, '-', analysis_end_year, '\n')
cat('Grid MODIS GPP: 2001-2023 (src/grid_modis_gpp_gee.py); grid PRISM: 1981-2024; grid trends filtered to',
    analysis_start_year, '-', analysis_end_year, '\n')

# ---- 8. aridity window comparison and constants ---------------------------
hdr('8. ARIDITY CLIMATOLOGY 1980-2020 vs 2001-2023 (166-site set)')
aic <- ai_new %>% inner_join(ai_old, by = 'site_code') %>% filter(site_code %in% fc_new) %>%
    mutate(shift = mean_ai - mean_ai_1980_2020,
           class_new = if_else(mean_ai > 1, 'arid', 'humid'),
           class_old = if_else(mean_ai_1980_2020 > 1, 'arid', 'humid'))
cat('Sites with both:', nrow(aic), '| r =', round(cor(aic$mean_ai, aic$mean_ai_1980_2020), 3),
    '| median shift (new - old) =', round(median(aic$shift), 3),
    '| median |shift| =', round(median(abs(aic$shift)), 3), '| max |shift| =', round(max(abs(aic$shift)), 3), '\n')
cat('Sites changing humid/arid class:', pct(sum(aic$class_new != aic$class_old), nrow(aic)), '\n')
print(aic %>% filter(class_new != class_old) %>% left_join(site_status, by = 'site_code') %>%
      select(domain, site_code, mean_ai_1980_2020, mean_ai) %>% mutate(across(where(is.numeric), ~ round(.x, 2))) %>% as.data.frame())

hdr('9. CONSTANTS FROM OTHER SCRIPTS')
cat('data_vis/gpp_landsat_modis_comparison.R (per-site matched windows, relative trends %/yr, RMA fit):\n',
    '  Landsat GPP vs MODIS GPP: n = 113 sites, r = 0.52, RMA slope = 0.75\n',
    '  Landsat GPP vs MODIS EVI: n = 120 sites, r = 0.58, RMA slope = 0.70\n',
    'data_vis/modis_gpp_vs_evi.R: MODIS GPP vs MODIS EVI: n = 116, r = 0.66, RMA slope = 0.85, 68% sign agreement\n',
    'MODIS product: MOD17A3HGF (annual GPP, 500 m); MacroSheds variable gpp_global_500m_median (watershed median);\n',
    '  MOD17 fill value 65535 x 1e-4 = 6.5535 for urban/barren/water; masked as >= 6.55 in src/metrics.R\n',
    'Grid: 5,212 points, 1 km2 footprints, MODIS/061/MOD17A3HGF via Earth Engine (src/grid_modis_gpp_gee.py)\n', sep = '')
sink()
cat('\nWritten to', out_file, '\n')
