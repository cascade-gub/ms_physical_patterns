# src/anomaly_analysis.R
# ---------------------------------------------------------------------------
# Non-trend "anomaly" analysis for the headwater streamflow paper (reviewer
# item E3 in paper/review_response_plan.md): does streamflow in a water year
# track that year's precipitation, temperature and GPP anomalies? Uses every
# site-year that passes the discharge completeness check, so short records
# contribute (trend detection needs >= 10 years; this needs min_years_anomaly).
#
# Steps
#   1. Per site, standardized anomalies (z-scores within site, water years
#      analysis_start_year-analysis_end_year) of mean streamflow (log-transformed),
#      total precipitation, mean temperature and annual MODIS GPP. Drivers are
#      standardized over every window year with data; Q over its passing years.
#   2. Within-site Spearman correlations of the Q anomaly with each driver,
#      plus GPP after removing the P signal from both (residual Spearman),
#      summarized across sites by watershed status, aridity class and NWD group.
#   3. Pooled linear mixed models (lme4): Qz ~ Pz + Tz + GPPz with random slopes
#      by site and a random intercept by water year; variants with prior-year
#      P, aridity-class interactions, and a log-log elasticity form.
#   4. Same per-site correlations for low flow, high flow, timing and
#      flashiness metrics (supplementary table).
#
# Reads (from input_dir, default data_working/): discharge_metrics_siteyear_nTest.rds,
#   ms_stream_order.csv, site_groupings_by_prsim_trend.csv, trends/best_run_prisim.csv,
#   q_summer_siteyear.rds (optional, JJA sites). Aridity (PET/P from Daymet,
#   same definition as patchwork_figures.R) is cached to
#   data_working/site_aridity_index.csv the first time it is needed.
# Writes: figures/anomaly_draft.png, data_working/anomaly_numbers.md,
#   data_working/anomaly_siteyear.csv, anomaly_site_spearman.csv,
#   anomaly_model_coefs.csv, anomaly_metric_spearman.csv. Modifies no pipeline input.
#
# Environment overrides (development against a snapshot):
#   ANOMALY_INPUT_DIR        directory with the pipeline outputs
#   ANOMALY_SKIP_ARIDITY=1   do not build the aridity cache if it is missing
#                            (the Daymet product is large; build it when the
#                            machine is otherwise idle)
# Run from the repo root after src/mega_zipper_data.R (and data_groups.R for NWD groups).
# ---------------------------------------------------------------------------
library(here)
source(here('src', 'setup.R'))
library(lme4)
library(lmerTest)

# settings ####
input_dir <- Sys.getenv('ANOMALY_INPUT_DIR', unset = here('data_working'))
out_dir   <- here('data_working')
fig_dir   <- here('figures')
min_years_anomaly <- 6       # passing water years of Q a site needs (trends need 10)
log_transform_q   <- TRUE    # z-score log(Q); Spearman results do not depend on this
aridity_cache     <- here('data_working', 'site_aridity_index.csv')
skip_aridity      <- Sys.getenv('ANOMALY_SKIP_ARIDITY') == '1'
driver_levels <- c('P', 'T', 'GPP', 'GPP | P', 'GPP | P,T', 'P (prior year)')

message('anomaly_analysis.R: inputs from ', input_dir)

# helpers ####
zscore <- function(x){
    s <- sd(x, na.rm = TRUE)
    if(is.na(s) || s == 0) return(rep(NA_real_, length(x)))
    (x - mean(x, na.rm = TRUE)) / s
}
center_log <- function(x){
    if(any(!is.na(x) & x <= 0)) return(rep(NA_real_, length(x)))
    log(x) - mean(log(x), na.rm = TRUE)
}
# Spearman rho with p-value; NA below min_years_anomaly complete pairs
spearman_tbl <- function(x, y, label){
    ok <- complete.cases(x, y)
    if(sum(ok) < min_years_anomaly) return(tibble(driver = label, rho = NA_real_, p = NA_real_, n_pairs = sum(ok)))
    ct <- suppressWarnings(cor.test(x[ok], y[ok], method = 'spearman'))
    tibble(driver = label, rho = unname(ct$estimate), p = ct$p.value, n_pairs = sum(ok))
}
# Spearman between the residuals of q and g after each is regressed (within site)
# on the control variables in `controls` (a data frame or vector)
partial_tbl <- function(q, g, controls, label){
    X <- as.data.frame(controls)
    ok <- complete.cases(q, g, X)
    if(sum(ok) < min_years_anomaly + ncol(X) - 1)
        return(tibble(driver = label, rho = NA_real_, p = NA_real_, n_pairs = sum(ok)))
    Xm <- as.matrix(X[ok, , drop = FALSE])
    rq <- resid(lm(q[ok] ~ Xm))
    rg <- resid(lm(g[ok] ~ Xm))
    ct <- suppressWarnings(cor.test(rq, rg, method = 'spearman'))
    tibble(driver = label, rho = unname(ct$estimate), p = ct$p.value, n_pairs = sum(ok))
}
site_spearman <- function(df, qvar = 'Qz'){
    df %>%
        group_by(site_code) %>%
        reframe(bind_rows(
            spearman_tbl(.data[[qvar]], Pz, 'P'),
            spearman_tbl(.data[[qvar]], Tz, 'T'),
            spearman_tbl(.data[[qvar]], Gz, 'GPP'),
            partial_tbl(.data[[qvar]], Gz, Pz, 'GPP | P'),
            partial_tbl(.data[[qvar]], Gz, cbind(Pz, Tz), 'GPP | P,T'),
            spearman_tbl(.data[[qvar]], Pz_lag1, 'P (prior year)'))) %>%
        filter(!is.na(rho)) %>%
        mutate(driver = factor(driver, levels = driver_levels))
}
summarize_rho <- function(df, ...){
    df %>%
        group_by(driver, ...) %>%
        summarize(n_sites = n(),
                  median_rho = median(rho),
                  q25 = quantile(rho, 0.25),
                  q75 = quantile(rho, 0.75),
                  frac_negative = mean(rho < 0),
                  frac_sig = mean(p < 0.05),
                  frac_sig_neg = mean(p < 0.05 & rho < 0),
                  frac_sig_pos = mean(p < 0.05 & rho > 0),
                  p_wilcoxon = if(n() >= 3) suppressWarnings(wilcox.test(rho)$p.value) else NA_real_,
                  .groups = 'drop')
}
fit_lmm <- function(formula, data){
    lmer(formula, data = data, REML = TRUE,
         control = lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 1e5)))
}
coef_tbl <- function(m, label){
    s  <- summary(m)$coefficients
    ci <- confint(m, method = 'Wald', parm = 'beta_')
    tibble(model = label,
           term = rownames(s),
           estimate = s[, 'Estimate'], se = s[, 'Std. Error'],
           df = s[, 'df'], p = s[, 'Pr(>|t|)'],
           lower = ci[, 1], upper = ci[, 2],
           n_obs = nobs(m),
           n_sites = ngrps(m)[['site_code']],
           singular = isSingular(m))
}
r2_fixed <- function(m){
    y  <- model.response(model.frame(m))
    xf <- as.vector(model.matrix(m) %*% fixef(m))
    c(R2_fixed = var(xf) / var(y), R2_fitted = cor(fitted(m), y)^2)
}
fmt <- function(x, d = 2) formatC(x, digits = d, format = 'f')

# inputs ####
metrics <- readRDS(file.path(input_dir, 'discharge_metrics_siteyear_nTest.rds')) %>%
    distinct()
hw_sites <- headwater_sites(path = file.path(input_dir, 'ms_stream_order.csv'))
groups <- read_csv(file.path(input_dir, 'site_groupings_by_prsim_trend.csv'),
                   show_col_types = FALSE) %>%
    select(site_code, coarse_grouping, q_flag) %>%
    distinct(site_code, .keep_all = TRUE)
site_meta <- ms_site_data %>%
    select(site_code, domain, ws_status, ws_area_ha, any_of(c('latitude', 'longitude'))) %>%
    distinct(site_code, .keep_all = TRUE)
trend_pool <- read_csv(file.path(input_dir, 'trends', 'best_run_prisim.csv'),
                       show_col_types = FALSE) %>%
    filter(var == 'q_mean', code == 'good') %>%
    distinct(site_code, .keep_all = TRUE) %>%
    left_join(site_meta, by = 'site_code')
# the paper's climate pool: headwater sites with a usable temperature trend (142 after D1)
climate_pool <- read_csv(file.path(input_dir, 'trends', 'full_prisim_climate.csv'),
                         show_col_types = FALSE) %>%
    filter(var == 'temp_mean', code == 'good', site_code %in% hw_sites) %>%
    pull(site_code) %>% unique()
message(length(hw_sites), ' headwater sites; ', length(climate_pool), ' in the climate pool (dropped: ',
        paste(setdiff(hw_sites, climate_pool), collapse = ', '), ')')

# aridity (PET/P, Daymet, mean over the analysis window; same code as patchwork_figures.R)
if(!file.exists(aridity_cache) && !skip_aridity){
    message('building aridity cache ', aridity_cache, ' (loads the Daymet product)')
    daymet <- ms_load_product(my_ms_dir, prodname = 'ws_attr_CAMELS_Daymet_forcings', warn = FALSE)
    p <- ms_load_product(my_ms_dir, prodname = 'ws_attr_timeseries:climate',
                         filter_vars = 'precip_median', warn = FALSE) %>%
        select(-var, -year, -pctCellErr, precip_median = val)
    ai <- inner_join(p, daymet, by = c('network', 'domain', 'site_code', 'date')) %>%
        mutate(year = as.integer(as.character(water_year(date, origin = 'usgs')))) %>%
        group_by(year, site_code) %>%
        summarize(aridity_index = sum(`pet(mm)`) / sum(`prcp(mm/day)`), .groups = 'drop') %>%
        filter(year >= analysis_start_year, year <= analysis_end_year) %>%
        group_by(site_code) %>%
        summarize(mean_ai = mean(aridity_index, na.rm = TRUE),
                  n_years_ai = sum(!is.na(aridity_index)), .groups = 'drop')
    write_csv(ai, aridity_cache)
    rm(daymet, p); invisible(gc())
}
if(file.exists(aridity_cache)){
    aridity <- read_csv(aridity_cache, show_col_types = FALSE) %>%
        mutate(aridity_class = case_when(mean_ai < 1 ~ 'humid', mean_ai >= 1 ~ 'arid'))
} else {
    message('aridity cache missing and ANOMALY_SKIP_ARIDITY=1: aridity splits skipped')
    aridity <- tibble(site_code = character(), mean_ai = numeric(), n_years_ai = integer(),
                      aridity_class = character())
}
has_aridity <- nrow(aridity) > 0

# annual table ####
ann <- metrics %>%
    filter(agg_code == 'annual',
           water_year >= analysis_start_year, water_year <= analysis_end_year,
           site_code %in% climate_pool) %>%
    select(site_code, water_year, n_q_days = n,
           q_mean, q_totsum, q_q05, q_q95, q_q50, q50_exceed, q_rbi, runoff_ratio,
           precip_total, precip_mean, temp_mean, gpp = gpp_global_500m_median) %>%
    distinct()
dup <- ann %>% count(site_code, water_year) %>% filter(n > 1)
if(nrow(dup) > 0) stop('duplicate annual site-years in metrics: ',
                       paste(dup$site_code, dup$water_year, collapse = '; '))

# driver anomalies: every window year with data (climate exists for all site-years)
drivers <- ann %>%
    group_by(site_code) %>%
    arrange(water_year, .by_group = TRUE) %>%
    mutate(Pz = zscore(precip_total),
           Tz = zscore(temp_mean),
           Gz = zscore(gpp),
           Pz_lag1 = Pz[match(water_year - 1, water_year)],
           dlogP = center_log(precip_total),
           dT = temp_mean - mean(temp_mean, na.rm = TRUE),
           dlogG = center_log(gpp),
           n_years_gpp = sum(!is.na(gpp))) %>%
    ungroup()

# streamflow anomalies: site-years passing the completeness gate used for trends
q_ok <- drivers %>%
    filter(!is.na(q_mean), n_q_days > full_year_min_obs_per_year)
q_site_n <- q_ok %>% count(site_code, name = 'n_years_q')
q_pool <- q_ok %>%
    inner_join(filter(q_site_n, n_years_q >= min_years_anomaly), by = 'site_code') %>%
    group_by(site_code) %>%
    mutate(q_transform = if_else(log_transform_q & all(q_mean > 0), 'log', 'none'),
           Qz = zscore(if(q_transform[1] == 'log') log(q_mean) else q_mean),
           dlogQ = center_log(q_mean),
           Qz_totsum = zscore(q_totsum),
           Qz_q05 = zscore(q_q05),
           Qz_q50 = zscore(q_q50),
           Qz_q95 = zscore(q_q95),
           Qz_timing = zscore(q50_exceed),
           Qz_rbi = zscore(q_rbi),
           Qz_rr = zscore(runoff_ratio)) %>%
    ungroup() %>%
    left_join(site_meta, by = 'site_code') %>%
    left_join(groups, by = 'site_code') %>%
    left_join(aridity, by = 'site_code') %>%
    mutate(q_window = 'water_year',
           in_trend_pool = site_code %in% trend_pool$site_code)

# JJA (summer-window) sites: secondary, only sites absent from the water-year pool
summer_path <- file.path(input_dir, 'q_summer_siteyear.rds')
jja_pool <- NULL
if(file.exists(summer_path)){
    qs <- readRDS(summer_path) %>%
        filter(site_code %in% hw_sites,
               !site_code %in% unique(q_pool$site_code),
               water_year >= analysis_start_year, water_year <= analysis_end_year,
               !is.na(q_mean_summer))
    jja_pool <- drivers %>%
        inner_join(select(qs, site_code, water_year, q_mean_summer),
                   by = c('site_code', 'water_year')) %>%
        group_by(site_code) %>%
        filter(n() >= min_years_anomaly) %>%
        mutate(n_years_q = n(),
               q_transform = if_else(log_transform_q & all(q_mean_summer > 0), 'log', 'none'),
               Qz = zscore(if(q_transform[1] == 'log') log(q_mean_summer) else q_mean_summer)) %>%
        ungroup() %>%
        left_join(site_meta, by = 'site_code') %>%
        left_join(groups, by = 'site_code') %>%
        left_join(aridity, by = 'site_code') %>%
        mutate(q_window = 'JJA')
}

nonexp <- q_pool %>% filter(ws_status == 'non-experimental')

# per-site Spearman ####
site_cor <- site_spearman(q_pool) %>%
    left_join(distinct(q_pool, site_code, n_years_q, domain, ws_status, ws_area_ha,
                       coarse_grouping, mean_ai, aridity_class, in_trend_pool), by = 'site_code')
site_cor_nonexp <- site_cor %>% filter(ws_status == 'non-experimental')
jja_cor <- NULL
if(!is.null(jja_pool) && nrow(jja_pool) > 0){
    jja_cor <- site_spearman(jja_pool) %>%
        left_join(distinct(jja_pool, site_code, n_years_q, domain, ws_status, aridity_class),
                  by = 'site_code')
}

# other streamflow metrics (supplement)
metric_vars <- c(`mean flow` = 'Qz', `total flow` = 'Qz_totsum', `5th pct (low) flow` = 'Qz_q05',
                 `median flow` = 'Qz_q50', `95th pct (high) flow` = 'Qz_q95',
                 `timing: day 50% of flow passed` = 'Qz_timing', `flashiness (RBI)` = 'Qz_rbi',
                 `runoff ratio` = 'Qz_rr')
metric_cor <- imap(metric_vars, function(v, lab){
    site_spearman(nonexp, qvar = v) %>% mutate(metric = lab)
}) %>% bind_rows()
metric_summary <- metric_cor %>%
    filter(driver %in% c('P', 'T', 'GPP', 'GPP | P', 'GPP | P,T')) %>%
    group_by(metric, driver) %>%
    summarize(n_sites = n(), median_rho = median(rho),
              frac_negative = mean(rho < 0), frac_sig = mean(p < 0.05), .groups = 'drop')

# pooled Spearman across site-years (non-experimental)
pooled_input <- nonexp %>% mutate(aridity_class = 'all')
if(has_aridity) pooled_input <- bind_rows(pooled_input, filter(nonexp, !is.na(aridity_class)))
pooled_cor <- pooled_input %>%
    group_by(aridity_class) %>%
    reframe(bind_rows(spearman_tbl(Qz, Pz, 'P'), spearman_tbl(Qz, Tz, 'T'),
                      spearman_tbl(Qz, Gz, 'GPP'), partial_tbl(Qz, Gz, Pz, 'GPP | P'),
                      partial_tbl(Qz, Gz, cbind(Pz, Tz), 'GPP | P,T'),
                      spearman_tbl(Qz, Pz_lag1, 'P (prior year)')))

# mixed models ####
mm_data <- nonexp %>%
    filter(!is.na(Qz), !is.na(Pz), !is.na(Tz), !is.na(Gz)) %>%
    mutate(water_year = factor(water_year))
f_pt  <- Qz ~ Pz + Tz + (0 + Pz + Tz || site_code) + (1 | water_year)
f_ptg <- Qz ~ Pz + Tz + Gz + (0 + Pz + Tz + Gz || site_code) + (1 | water_year)
f_lag <- Qz ~ Pz + Pz_lag1 + Tz + Gz + (0 + Pz + Tz + Gz || site_code) + (1 | water_year)
f_el  <- dlogQ ~ dlogP + dT + dlogG + (0 + dlogP + dT + dlogG || site_code) + (1 | water_year)

m_pt  <- fit_lmm(f_pt, mm_data)
m_ptg <- fit_lmm(f_ptg, mm_data)
m_lag <- fit_lmm(f_lag, filter(mm_data, !is.na(Pz_lag1)))
m_el  <- fit_lmm(f_el, filter(mm_data, !is.na(dlogQ), !is.na(dlogP), !is.na(dlogG)))
lrt_gpp <- anova(m_pt, m_ptg, refit = TRUE)      # ML likelihood-ratio test for adding GPP
# sensitivity: (i) a site random intercept (drivers are standardized over 23 years but Q over
# its passing years, so a short-record site sampled in wet years has a non-zero mean Pz);
# (ii) drop the GPP random slope, whose variance collapses to zero and makes the fit singular
m_ptg_int <- fit_lmm(Qz ~ Pz + Tz + Gz + (1 | site_code) + (0 + Pz + Tz + Gz || site_code) + (1 | water_year), mm_data)
m_ptg_noG <- fit_lmm(Qz ~ Pz + Tz + Gz + (0 + Pz + Tz || site_code) + (1 | water_year), mm_data)

models <- list(`non-exp: P + T + GPP` = m_ptg,
               `non-exp: P + T + GPP, + site intercept` = m_ptg_int,
               `non-exp: P + T + GPP, no GPP random slope` = m_ptg_noG,
               `non-exp: + prior-year P` = m_lag,
               `non-exp: elasticity (log-log)` = m_el)
m_class <- list()
m_arid <- NULL
if(has_aridity && n_distinct(na.omit(mm_data$aridity_class)) == 2){
    m_el_class <- list()
    for(cl in c('humid', 'arid')){
        d_cl <- filter(mm_data, aridity_class == cl)
        if(n_distinct(d_cl$site_code) >= 5){
            m_class[[cl]] <- fit_lmm(f_ptg, d_cl)
            m_el_class[[paste0('non-exp ', cl, ': elasticity (log-log)')]] <-
                fit_lmm(f_el, filter(d_cl, !is.na(dlogQ), !is.na(dlogP), !is.na(dlogG)))
        }
    }
    models <- c(models, m_el_class)
    m_arid <- fit_lmm(Qz ~ (Pz + Tz + Gz) * aridity_class + (0 + Pz + Tz + Gz || site_code) + (1 | water_year),
                      mm_data %>% filter(!is.na(aridity_class)) %>%
                          mutate(aridity_class = factor(aridity_class, levels = c('humid', 'arid'))))
    models <- c(models,
                setNames(m_class, paste0('non-exp ', names(m_class), ': P + T + GPP')),
                list(`non-exp: (P + T + GPP) x aridity class` = m_arid))
}
m_all <- fit_lmm(f_ptg, q_pool %>%
                     filter(!is.na(Qz), !is.na(Pz), !is.na(Tz), !is.na(Gz)) %>%
                     mutate(water_year = factor(water_year)))
models <- c(models, list(`all sites (exp + non-exp): P + T + GPP` = m_all))
coefs <- imap(models, ~ coef_tbl(.x, .y)) %>% bind_rows()
r2s <- imap(models, ~ tibble(model = .y, !!!as.list(r2_fixed(.x)))) %>% bind_rows()

# outputs: tables ####
write_csv(bind_rows(q_pool, jja_pool) %>%
              select(site_code, water_year, q_window, domain, ws_status, coarse_grouping,
                     mean_ai, aridity_class, n_years_q, any_of('in_trend_pool'),
                     q_mean, any_of('q_mean_summer'), precip_total, temp_mean, gpp,
                     Qz, Pz, Tz, Gz, Pz_lag1, any_of(c('dlogQ', 'dlogP', 'dT', 'dlogG'))),
          file.path(out_dir, 'anomaly_siteyear.csv'))
write_csv(bind_rows(site_cor %>% mutate(q_window = 'water_year'),
                    if(!is.null(jja_cor)) jja_cor %>% mutate(q_window = 'JJA') else NULL),
          file.path(out_dir, 'anomaly_site_spearman.csv'))
write_csv(coefs, file.path(out_dir, 'anomaly_model_coefs.csv'))
write_csv(metric_cor, file.path(out_dir, 'anomaly_metric_spearman.csv'))

# outputs: numbers ####
num_file <- file.path(out_dir, 'anomaly_numbers.md')
sink(num_file, split = FALSE)
hdr <- function(x) cat('\n## ', x, '\n\n', sep = '')
ptab <- function(df, digits = 2){
    df <- df %>%
        mutate(across(where(is.numeric) & matches('^p($|_)'), ~ formatC(.x, digits = 2, format = 'g')),
               across(where(is.numeric), ~ round(.x, digits)))
    cat(knitr::kable(df, format = 'pipe'), sep = '\n'); cat('\n')
}
cat('# Anomaly analysis (review item E3): numbers\n\n')
cat('Generated ', format(Sys.time(), '%Y-%m-%d %H:%M'), ' by src/anomaly_analysis.R from ', input_dir, '\n\n',
    'Window ', analysis_start_year, '-', analysis_end_year,
    '; headwater filter adjusted NHD order <= ', max_stream_order,
    '; min ', min_years_anomaly, ' passing water years per site; Q ',
    if(log_transform_q) 'log-transformed' else 'untransformed',
    ' before z-scoring; drivers z-scored over all window years with data.\n\n', sep = '')
cat('Regenerate this file after any pipeline rerun; numbers in the draft must come from here.\n')

hdr('1. Site pools')
hw_meta <- site_meta %>% filter(site_code %in% hw_sites)
ann_meta <- site_meta %>% filter(site_code %in% unique(ann$site_code))
pool_tbl <- bind_rows(
    tibble(pool = 'headwater_sites() (every gauge with adjusted order <= 2)', n = length(hw_sites),
           n_nonexp = sum(hw_meta$ws_status == 'non-experimental', na.rm = TRUE)),
    tibble(pool = 'climate pool: headwater sites with a usable temperature trend (paper denominator)',
           n = n_distinct(ann$site_code),
           n_nonexp = sum(ann_meta$ws_status == 'non-experimental', na.rm = TRUE)),
    tibble(pool = 'with >= 1 passing Q water year in window', n = n_distinct(q_ok$site_code),
           n_nonexp = q_ok %>% distinct(site_code) %>% left_join(site_meta, by = 'site_code') %>%
               filter(ws_status == 'non-experimental') %>% nrow()),
    tibble(pool = paste0('anomaly pool (>= ', min_years_anomaly, ' years)'), n = n_distinct(q_pool$site_code),
           n_nonexp = n_distinct(nonexp$site_code)),
    tibble(pool = 'trend pool (q_mean trend, code good, best_run_prisim.csv)', n = nrow(trend_pool),
           n_nonexp = sum(trend_pool$ws_status == 'non-experimental', na.rm = TRUE)),
    tibble(pool = 'anomaly pool sites NOT in trend pool',
           n = sum(!distinct(q_pool, site_code, in_trend_pool)$in_trend_pool),
           n_nonexp = sum(!distinct(nonexp, site_code, in_trend_pool)$in_trend_pool)),
    tibble(pool = 'anomaly pool with usable GPP (>= min years)',
           n = site_cor %>% filter(driver == 'GPP') %>% nrow(),
           n_nonexp = site_cor_nonexp %>% filter(driver == 'GPP') %>% nrow()))
ptab(pool_tbl)
cat('Site-years in anomaly pool: ', nrow(q_pool), ' (non-experimental ', nrow(nonexp), ').\n\n', sep = '')
cat('Years per site (non-experimental):\n\n')
print(table(cut(distinct(nonexp, site_code, n_years_q)$n_years_q, c(0, 5, 7, 9, 14, 19, 23))))
cat('\nSites with Q <= 0 in some year (z-scored untransformed): ',
    paste(unique(q_pool$site_code[q_pool$q_transform == 'none']), collapse = ', '), '\n', sep = '')
short_sites <- q_site_n %>% filter(n_years_q < min_years_anomaly) %>%
    left_join(site_meta, by = 'site_code')
cat('\nSites with 1-', min_years_anomaly - 1, ' passing water years (excluded): ', nrow(short_sites),
    ' (non-experimental ', sum(short_sites$ws_status == 'non-experimental', na.rm = TRUE), '); years per site:\n\n', sep = '')
print(table(short_sites$n_years_q))
cat('\nAnomaly-pool sites not in the trend pool: ',
    paste(setdiff(unique(q_pool$site_code), trend_pool$site_code), collapse = ', '), '\n', sep = '')
cat('Trend-pool sites not in the anomaly pool (fail n > ', full_year_min_obs_per_year,
    ' obs/yr here, e.g. summer-window sites): ',
    paste(setdiff(trend_pool$site_code, unique(q_pool$site_code)), collapse = ', '), '\n', sep = '')
if(!is.null(jja_pool)) cat('JJA-window sites (secondary, >= ', min_years_anomaly,
                           ' summers, not in water-year pool): ', n_distinct(jja_pool$site_code),
                           ' (', paste(unique(jja_pool$site_code), collapse = ', '), ')\n', sep = '')
if(has_aridity){
    cat('\nAridity class of anomaly pool (non-experimental):\n\n')
    print(table(distinct(nonexp, site_code, aridity_class)$aridity_class, useNA = 'ifany'))
}

hdr('2. Within-site Spearman rho of the Q anomaly with each driver, non-experimental sites')
ptab(summarize_rho(site_cor_nonexp))
hdr('2b. ... by aridity class (PET/P < 1 humid, >= 1 arid)')
if(has_aridity) ptab(summarize_rho(filter(site_cor_nonexp, !is.na(aridity_class)), aridity_class)) else cat('(aridity cache not built)\n')
hdr('2c. ... by NWD coarse grouping')
ptab(summarize_rho(filter(site_cor_nonexp, !is.na(coarse_grouping)), coarse_grouping))
hdr('2d. ... all sites by watershed status')
ptab(summarize_rho(site_cor, ws_status))
hdr('2e. ... sites not long enough for a trend vs trend-pool sites, non-experimental')
ptab(summarize_rho(site_cor_nonexp, in_trend_pool))
if(!is.null(jja_cor) && nrow(jja_cor) > 0){
    hdr('2f. JJA-window sites (secondary)')
    ptab(summarize_rho(jja_cor))
    ptab(jja_cor %>% select(site_code, domain, ws_status, n_years_q, driver, rho, p))
}

hdr('3. Pooled Spearman across all non-experimental site-years')
ptab(pooled_cor)
hdr('3b. How the drivers covary within site (non-experimental; median rho across sites)')
driver_cov <- nonexp %>%
    group_by(site_code) %>%
    reframe(bind_rows(spearman_tbl(Gz, Pz, 'GPP ~ P'), spearman_tbl(Gz, Tz, 'GPP ~ T'),
                      spearman_tbl(Pz, Tz, 'P ~ T'))) %>%
    filter(!is.na(rho)) %>%
    group_by(driver) %>%
    summarize(n_sites = n(), median_rho = median(rho), q25 = quantile(rho, 0.25), q75 = quantile(rho, 0.75),
              frac_positive = mean(rho > 0), .groups = 'drop')
ptab(driver_cov)

hdr('4. Mixed models (lme4, REML; random slopes by site, random intercept by water year)')
cat('Non-experimental site-years with Q, P, T and GPP: ', nrow(mm_data), ' at ',
    n_distinct(mm_data$site_code), ' sites.\n\n', sep = '')
cat('Likelihood-ratio test for adding GPP to P + T (ML refit): chi2 = ', fmt(lrt_gpp$Chisq[2]),
    ', df = ', lrt_gpp$Df[2], ', p = ', formatC(lrt_gpp$`Pr(>Chisq)`[2], digits = 3, format = 'g'), '\n\n', sep = '')
ptab(coefs %>% select(model, term, estimate, lower, upper, p, n_obs, n_sites, singular), 3)
ptab(r2s, 3)
cat('Random-effect SDs (P + T + GPP, non-experimental):\n\n'); print(VarCorr(m_ptg))
cat('\nRandom-effect SDs (elasticity model, non-experimental):\n\n'); print(VarCorr(m_el))
cat('\nSingular fits mean at least one random-slope variance collapsed to zero (see the singular column);',
    'fixed effects are unaffected in that case but the random-slope SD for that term is not estimable.\n')

hdr('4b. Elasticity-model temperature terms as percent change in Q per degC (exp(b) - 1)')
ptab(coefs %>% filter(term == 'dT') %>%
         transmute(model, pct_per_degC = 100 * (exp(estimate) - 1),
                   lower = 100 * (exp(lower) - 1), upper = 100 * (exp(upper) - 1), p), 1)

hdr('4c. Expected change in Q over the window from the observed T and P trends, non-experimental anomaly pool')
cat('Per site: Sen slope of temp_mean and precip_mean (full_prisim_climate.csv, code good) x ',
    analysis_end_year - analysis_start_year, ' years, converted with the class-specific elasticity-model',
    ' coefficients (dT: log Q per degC; dlogP: P elasticity). Compared with the interannual variability of Q at the same sites.\n\n', sep = '')
clim_tr <- read_csv(file.path(input_dir, 'trends', 'full_prisim_climate.csv'), show_col_types = FALSE) %>%
    filter(code == 'good', site_code %in% unique(nonexp$site_code),
           var %in% c('temp_mean', 'precip_mean')) %>%
    add_flags() %>%
    select(site_code, var, trend, flag) %>%
    pivot_wider(names_from = var, values_from = c(trend, flag))
span <- analysis_end_year - analysis_start_year
el_coef <- function(cl, term){
    m <- if(is.na(cl)) NA_character_ else paste0('non-exp ', cl, ': elasticity (log-log)')
    v <- coefs$estimate[coefs$model == m & coefs$term == term]
    if(length(v) == 0) v <- coefs$estimate[coefs$model == 'non-exp: elasticity (log-log)' & coefs$term == term]
    v[1]
}
expected <- nonexp %>%
    group_by(site_code) %>%
    summarize(aridity_class = first(aridity_class), mean_Pd = mean(precip_mean, na.rm = TRUE),
              iav_Q_pct = 100 * (exp(sd(log(q_mean))) - 1), .groups = 'drop') %>%
    inner_join(clim_tr, by = 'site_code') %>%
    mutate(dT_degC = trend_temp_mean * span,
           dP_pct = 100 * trend_precip_mean * span / mean_Pd,
           b_T = map_dbl(aridity_class, el_coef, term = 'dT'),
           e_P = map_dbl(aridity_class, el_coef, term = 'dlogP'),
           dQ_from_T_pct = 100 * (exp(b_T * dT_degC) - 1),
           dQ_from_P_pct = 100 * (exp(e_P * log1p(dP_pct / 100)) - 1),
           dQ_both_pct = 100 * (exp(b_T * dT_degC + e_P * log1p(dP_pct / 100)) - 1))
exp_summary <- function(df) df %>%
    summarize(n_sites = n(), median_dT_degC = median(dT_degC), median_dP_pct = median(dP_pct),
              median_dQ_from_T_pct = median(dQ_from_T_pct), median_dQ_from_P_pct = median(dQ_from_P_pct),
              median_dQ_both_pct = median(dQ_both_pct), median_interannual_sd_Q_pct = median(iav_Q_pct),
              .groups = 'drop')
ptab(bind_rows(expected %>% mutate(set = 'all sites with climate trends') %>% group_by(set) %>% exp_summary(),
               expected %>% filter(flag_temp_mean == 'increasing') %>%
                   mutate(set = 'sites with a significant warming trend') %>% group_by(set) %>% exp_summary(),
               expected %>% filter(!is.na(aridity_class)) %>% mutate(set = aridity_class) %>%
                   group_by(set) %>% exp_summary()), 1)
cat('T-trend flags among these sites:\n\n'); print(table(expected$flag_temp_mean, useNA = 'ifany'))
cat('\nP-trend flags among these sites:\n\n'); print(table(expected$flag_precip_mean, useNA = 'ifany'))
write_csv(expected, file.path(out_dir, 'anomaly_expected_change.csv'))

hdr('5. Other streamflow metrics: median within-site Spearman rho (non-experimental)')
ptab(metric_summary)
sink()
message('numbers written to ', num_file)

# figure ####
# Okabe-Ito pair for aridity classes (colour-vision safe), grey for unclassified
class_cols <- c(humid = '#0072B2', arid = '#D55E00', unclassified = '#7f7f7f')
fig_cor <- site_cor_nonexp %>%
    filter(driver %in% c('P', 'T', 'GPP', 'GPP | P', 'GPP | P,T')) %>%
    mutate(driver = factor(recode(as.character(driver), `GPP | P` = 'GPP,\nP removed',
                                  `GPP | P,T` = 'GPP,\nP + T removed'),
                           levels = c('P', 'T', 'GPP', 'GPP,\nP removed', 'GPP,\nP + T removed')),
           aridity_class = if(has_aridity) replace_na(aridity_class, 'unclassified') else 'unclassified',
           aridity_class = factor(aridity_class, levels = names(class_cols)))
panel_a <- ggplot(fig_cor, aes(x = driver, y = rho)) +
    geom_hline(yintercept = 0, colour = 'grey55', linewidth = 0.3) +
    geom_boxplot(aes(fill = aridity_class), outlier.shape = NA, alpha = 0.35, linewidth = 0.3,
                 position = position_dodge(width = 0.75), width = 0.65) +
    geom_point(aes(colour = aridity_class), size = 0.9, alpha = 0.7,
               position = position_jitterdodge(jitter.width = 0.18, dodge.width = 0.75, seed = 1)) +
    scale_fill_manual(values = class_cols, drop = TRUE, name = 'Aridity (PET/P)') +
    scale_colour_manual(values = class_cols, drop = TRUE, name = 'Aridity (PET/P)') +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    labs(x = NULL, y = expression('Within-site Spearman ' * rho * ' with Q anomaly'),
         subtitle = paste0('Non-experimental headwater sites with >= ', min_years_anomaly,
                           ' water years (n = ', n_distinct(fig_cor$site_code), ')')) +
    theme_few(base_size = 10) +
    theme(legend.position = 'top', legend.title = element_text(size = 9),
          legend.text = element_text(size = 9), plot.subtitle = element_text(size = 9))

# R^2 of the pooled OLS line drawn for each aridity class (simple regression, so R^2 = r^2),
# printed top-left in the class colour so the reader can see how much each line explains
r2_labels <- function(df, x, y) {
    df %>%
        group_by(aridity_class) %>%
        summarise(n = n(), r2 = cor(.data[[x]], .data[[y]])^2, .groups = 'drop') %>%
        arrange(aridity_class) %>%
        mutate(label = sprintf('%s: R² = %.2f', aridity_class, r2),
               vj = 1.3 + 1.4 * (row_number() - 1))
}
add_r2 <- function(lab) {
    geom_text(data = lab, aes(x = -Inf, y = Inf, label = label, colour = aridity_class, vjust = vj),
              hjust = -0.08, size = 2.8, show.legend = FALSE)
}

# panel b: what a real driver looks like at this scale (Q vs P), for contrast with GPP in panel c
fig_scatter <- nonexp %>% filter(!is.na(Pz), !is.na(Qz)) %>%
    mutate(aridity_class = if(has_aridity) replace_na(aridity_class, 'unclassified') else 'unclassified',
           aridity_class = factor(aridity_class, levels = names(class_cols)))
lab_b <- r2_labels(fig_scatter, 'Pz', 'Qz')
panel_b <- ggplot(fig_scatter, aes(x = Pz, y = Qz, colour = aridity_class)) +
    geom_hline(yintercept = 0, colour = 'grey80', linewidth = 0.3) +
    geom_vline(xintercept = 0, colour = 'grey80', linewidth = 0.3) +
    geom_point(size = 0.6, alpha = 0.4) +
    geom_smooth(method = 'lm', se = TRUE, linewidth = 0.7, formula = y ~ x) +
    add_r2(lab_b) +
    scale_colour_manual(values = class_cols, drop = TRUE, guide = 'none') +
    labs(x = 'P anomaly (z)', y = 'Q anomaly (z)',
         subtitle = paste0('Site-years (n = ', nrow(fig_scatter), ')')) +
    theme_few(base_size = 10) + theme(plot.subtitle = element_text(size = 9))

# P- and T-adjusted: within-site residuals of Qz and Gz after regression on Pz and Tz
fig_resid <- nonexp %>%
    filter(!is.na(Gz), !is.na(Qz), !is.na(Pz), !is.na(Tz)) %>%
    group_by(site_code) %>%
    filter(n() >= min_years_anomaly + 1) %>%
    mutate(Qz_resid = resid(lm(Qz ~ Pz + Tz)), Gz_resid = resid(lm(Gz ~ Pz + Tz))) %>%
    ungroup() %>%
    mutate(aridity_class = if(has_aridity) replace_na(aridity_class, 'unclassified') else 'unclassified',
           aridity_class = factor(aridity_class, levels = names(class_cols)))
lab_c <- r2_labels(fig_resid, 'Gz_resid', 'Qz_resid')
panel_c <- ggplot(fig_resid, aes(x = Gz_resid, y = Qz_resid, colour = aridity_class)) +
    geom_hline(yintercept = 0, colour = 'grey80', linewidth = 0.3) +
    geom_vline(xintercept = 0, colour = 'grey80', linewidth = 0.3) +
    geom_point(size = 0.6, alpha = 0.4) +
    geom_smooth(method = 'lm', se = TRUE, linewidth = 0.7, formula = y ~ x) +
    add_r2(lab_c) +
    scale_colour_manual(values = class_cols, drop = TRUE, guide = 'none') +
    labs(x = 'GPP anomaly, P + T removed', y = 'Q anomaly, P + T removed',
         subtitle = paste0('Within-site residuals (n = ', nrow(fig_resid), ')')) +
    theme_few(base_size = 10) + theme(plot.subtitle = element_text(size = 9))

# scatter-panel fit statistics for the numbers file
scatter_fits <- bind_rows(
    fig_scatter %>% mutate(panel = 'b: Qz ~ Pz', x = Pz, y = Qz),
    fig_resid %>% mutate(panel = 'c: Qz_resid ~ Gz_resid', x = Gz_resid, y = Qz_resid)) %>%
    select(panel, aridity_class, x, y) %>%
    bind_rows(mutate(., aridity_class = factor('all'))) %>%
    group_by(panel, aridity_class) %>%
    summarise(n = n(), slope = coef(lm(y ~ x))[2], p = summary(lm(y ~ x))$coefficients[2, 4],
              r2 = summary(lm(y ~ x))$r.squared, .groups = 'drop') %>%
    mutate(across(c(slope, r2), ~ round(.x, 3)))
sink(num_file, append = TRUE, split = FALSE)
cat('\n## 6. Pooled OLS lines drawn in figure panels b and c\n\n')
ptab(scatter_fits, 3)
sink()

coef_plot <- coefs %>%
    filter(model %in% c('non-exp: P + T + GPP', 'non-exp humid: P + T + GPP', 'non-exp arid: P + T + GPP'),
           term != '(Intercept)') %>%
    mutate(group = case_when(grepl('humid', model) ~ 'humid', grepl('arid', model) ~ 'arid', TRUE ~ 'all'),
           group = factor(group, levels = c('all', 'humid', 'arid')),
           term = factor(recode(term, Pz = 'P', Tz = 'T', Gz = 'GPP'), levels = c('P', 'T', 'GPP')))
coef_cols <- c(all = '#000000', humid = '#0072B2', arid = '#D55E00')
panel_d <- ggplot(coef_plot, aes(x = term, y = estimate, colour = group)) +
    geom_hline(yintercept = 0, colour = 'grey55', linewidth = 0.3) +
    geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5),
                    size = 0.3, linewidth = 0.5) +
    scale_colour_manual(values = coef_cols, name = NULL, drop = TRUE) +
    labs(x = NULL, y = 'Effect on Q anomaly (SD per SD)',
         subtitle = 'Mixed model (95% CI)') +
    theme_few(base_size = 10) +
    theme(legend.position = 'top', legend.text = element_text(size = 8),
          legend.key.width = unit(0.3, 'cm'), legend.margin = margin(0, 0, 0, 0),
          plot.subtitle = element_text(size = 9))

fig <- panel_a / (panel_b | panel_c | panel_d) +
    plot_layout(heights = c(1.15, 1)) +
    plot_annotation(tag_levels = 'a')
ggsave(file.path(fig_dir, 'anomaly_draft.png'), fig, width = 7.2, height = 7, dpi = 400, bg = 'white')
message('figure written to ', file.path(fig_dir, 'anomaly_draft.png'))
