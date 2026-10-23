# src/budyko_anomaly.R
# ---------------------------------------------------------------------------
# Budyko-space companion to src/anomaly_analysis.R (reviewer item E3).
#
# Each site-year with a complete streamflow record is placed in Budyko space:
#   aridity index      AI = PET / P        (Daymet PET, PRISM P, water-year totals)
#   evaporative index  EI = (P - Q) / P    (PRISM P, MacroSheds Q, water-year totals)
# The Fu (1981) curve EI = 1 + AI - (1 + AI^w)^(1/w) gives the ET fraction expected
# for that year's aridity, and the residual r = EI_obs - EI_Fu replaces the site's
# own long-term mean as the baseline. Because r is defined for a single year,
# every site-year contributes, including sites with 1-5 complete years that the
# within-site anomaly analysis cannot standardize. Driver anomalies (GPP, T, P,
# prior-year P) are the same within-site z-scores as in anomaly_analysis.R, which
# exist for all 23 window years at every site.
# Sign convention: a positive residual means more ET (less Q) than the curve
# expects, so a positive GPP coefficient means greener years lose more water.
#
# Reads (from input_dir, default data_working/): discharge_metrics_siteyear_nTest.rds,
#   ms_stream_order.csv, trends/full_prisim_climate.csv, site_aridity_index.csv
#   (from anomaly_analysis.R; aridity class). Builds data_working/site_year_pet.csv
#   (Daymet PET and P by site and water year) the first time it runs.
# Writes: figures/budyko_anomaly_draft.png, data_working/budyko_numbers.md,
#   data_working/budyko_siteyear.csv, budyko_model_coefs.csv. Modifies no pipeline input.
# Run from the repo root after src/anomaly_analysis.R.
# ---------------------------------------------------------------------------
library(here)
source(here('src', 'setup.R'))
library(lme4)
library(lmerTest)

# settings ####
input_dir <- Sys.getenv('ANOMALY_INPUT_DIR', unset = here('data_working'))
out_dir   <- here('data_working')
fig_dir   <- here('figures')
omega_default     <- 2.6     # Fu curve parameter (Zhang et al. 2004 global value)
min_years_anomaly <- 6       # record-length split, same as anomaly_analysis.R
min_pet_days      <- 360     # Daymet days needed for a water-year PET total
aridity_cache     <- here('data_working', 'site_aridity_index.csv')
pet_cache         <- here('data_working', 'site_year_pet.csv')

# helpers ####
zscore <- function(x){
    s <- sd(x, na.rm = TRUE)
    if(is.na(s) || s == 0) return(rep(NA_real_, length(x)))
    (x - mean(x, na.rm = TRUE)) / s
}
fu <- function(ai, w) 1 + ai - (1 + ai^w)^(1 / w)
spearman_tbl <- function(x, y, label, min_n = 6){
    ok <- complete.cases(x, y)
    if(sum(ok) < min_n) return(tibble(driver = label, rho = NA_real_, p = NA_real_, n_pairs = sum(ok)))
    ct <- suppressWarnings(cor.test(x[ok], y[ok], method = 'spearman'))
    tibble(driver = label, rho = unname(ct$estimate), p = ct$p.value, n_pairs = sum(ok))
}
fit_lmm <- function(formula, data){
    lmer(formula, data = data, REML = TRUE,
         control = lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 1e5)))
}
coef_tbl <- function(m, label){
    s  <- summary(m)$coefficients
    ci <- confint(m, method = 'Wald', parm = 'beta_')
    tibble(model = label, term = rownames(s),
           estimate = s[, 'Estimate'], se = s[, 'Std. Error'], df = s[, 'df'], p = s[, 'Pr(>|t|)'],
           lower = ci[, 1], upper = ci[, 2],
           n_obs = nobs(m), n_sites = ngrps(m)[['site_code']], singular = isSingular(m))
}
fmt <- function(x, d = 2) formatC(x, digits = d, format = 'f')

# inputs ####
metrics <- readRDS(file.path(input_dir, 'discharge_metrics_siteyear_nTest.rds')) %>% distinct()
hw_sites <- headwater_sites(path = file.path(input_dir, 'ms_stream_order.csv'))
climate_pool <- read_csv(file.path(input_dir, 'trends', 'full_prisim_climate.csv'), show_col_types = FALSE) %>%
    filter(var == 'temp_mean', code == 'good', site_code %in% hw_sites) %>%
    pull(site_code) %>% unique()
site_meta <- ms_site_data %>%
    select(site_code, domain, ws_status, ws_area_ha) %>%
    distinct(site_code, .keep_all = TRUE)
if(!file.exists(aridity_cache)) stop('run src/anomaly_analysis.R first to build ', aridity_cache)
aridity <- read_csv(aridity_cache, show_col_types = FALSE) %>%
    mutate(aridity_class = case_when(mean_ai < 1 ~ 'humid', mean_ai >= 1 ~ 'arid'))

# Daymet PET and P by site and water year (cached)
if(!file.exists(pet_cache)){
    message('building ', pet_cache, ' (loads the Daymet product)')
    daymet <- ms_load_product(my_ms_dir, prodname = 'ws_attr_CAMELS_Daymet_forcings', warn = FALSE)
    pet_sy <- daymet %>%
        mutate(water_year = as.integer(as.character(water_year(date, origin = 'usgs')))) %>%
        group_by(site_code, water_year) %>%
        summarize(pet_mm = sum(`pet(mm)`, na.rm = TRUE),
                  p_daymet_mm = sum(`prcp(mm/day)`, na.rm = TRUE),
                  n_days = sum(!is.na(`pet(mm)`)), .groups = 'drop')
    write_csv(pet_sy, pet_cache)
    rm(daymet); invisible(gc())
}
pet_sy <- read_csv(pet_cache, show_col_types = FALSE) %>% filter(n_days >= min_pet_days)

# site-year table ####
ann <- metrics %>%
    filter(agg_code == 'annual',
           water_year >= analysis_start_year, water_year <= analysis_end_year,
           site_code %in% climate_pool) %>%
    select(site_code, water_year, n_q_days = n, q_mean, q_totsum,
           precip_total, precip_mean, temp_mean, gpp = gpp_global_500m_median) %>%
    distinct()
dup <- ann %>% count(site_code, water_year) %>% filter(n > 1)
if(nrow(dup) > 0) stop('duplicate annual site-years in metrics')

drivers <- ann %>%
    group_by(site_code) %>%
    arrange(water_year, .by_group = TRUE) %>%
    mutate(Pz = zscore(precip_total), Tz = zscore(temp_mean), Gz = zscore(gpp),
           Pz_lag1 = Pz[match(water_year - 1, water_year)]) %>%
    ungroup() %>%
    left_join(pet_sy, by = c('site_code', 'water_year')) %>%
    mutate(days_in_wy = if_else(leap_year(water_year), 366L, 365L),
           AI = pet_mm / precip_total,                 # PRISM P, consistent with EI
           AI_daymet = pet_mm / p_daymet_mm) %>%       # paper's convention for the mean aridity
    group_by(site_code) %>%
    mutate(AIz = zscore(AI)) %>%
    ungroup()

bud <- drivers %>%
    filter(!is.na(q_mean), n_q_days > full_year_min_obs_per_year, !is.na(AI), precip_total > 0) %>%
    mutate(q_annual = q_mean * days_in_wy,             # mean of observed days scaled to the year
           EI = (precip_total - q_annual) / precip_total,
           runoff_ratio = q_annual / precip_total,
           EI_fu = fu(AI, omega_default),
           resid = EI - EI_fu,
           ei_in_range = EI >= 0 & EI <= 1) %>%
    group_by(site_code) %>%
    mutate(n_years_q = n()) %>%
    ungroup() %>%
    mutate(record_class = if_else(n_years_q >= min_years_anomaly,
                                  paste0('>= ', min_years_anomaly, ' years'),
                                  paste0('1-', min_years_anomaly - 1, ' years'))) %>%
    left_join(site_meta, by = 'site_code') %>%
    left_join(aridity, by = 'site_code')

nonexp <- bud %>% filter(ws_status == 'non-experimental')

# Fu curve fit ####
fit_omega <- function(df){
    tryCatch(coef(nls(EI ~ 1 + AI - (1 + AI^w)^(1 / w), data = df, start = list(w = omega_default)))[['w']],
             error = function(e) NA_real_)
}
omega_all   <- fit_omega(filter(nonexp, ei_in_range))
omega_sites <- fit_omega(nonexp %>% filter(ei_in_range) %>% group_by(site_code) %>%
                             summarize(AI = mean(AI), EI = mean(EI), .groups = 'drop'))
nonexp <- nonexp %>% mutate(resid_fit = EI - fu(AI, omega_all))
bud    <- bud %>% mutate(resid_fit = EI - fu(AI, omega_all))

# pooled and within-site correlations ####
pooled_input <- bind_rows(nonexp %>% mutate(group = 'all site-years'),
                          nonexp %>% mutate(group = paste0('record ', record_class)),
                          nonexp %>% filter(!is.na(aridity_class)) %>% mutate(group = aridity_class),
                          nonexp %>% filter(ei_in_range) %>% mutate(group = 'EI within [0, 1] only'))
pooled_cor <- pooled_input %>%
    group_by(group) %>%
    reframe(bind_rows(spearman_tbl(resid, Gz, 'GPP'), spearman_tbl(resid, Tz, 'T'),
                      spearman_tbl(resid, Pz, 'P'), spearman_tbl(resid, Pz_lag1, 'P (prior year)'),
                      spearman_tbl(resid, AIz, 'AI anomaly')))
site_cor <- nonexp %>%
    filter(n_years_q >= min_years_anomaly) %>%
    group_by(site_code) %>%
    reframe(bind_rows(spearman_tbl(resid, Gz, 'GPP'), spearman_tbl(resid, Tz, 'T'),
                      spearman_tbl(resid, Pz, 'P'), spearman_tbl(resid, Pz_lag1, 'P (prior year)'))) %>%
    filter(!is.na(rho)) %>%
    left_join(distinct(nonexp, site_code, aridity_class), by = 'site_code')
site_cor_summary <- site_cor %>%
    group_by(driver) %>%
    summarize(n_sites = n(), median_rho = median(rho), q25 = quantile(rho, 0.25), q75 = quantile(rho, 0.75),
              frac_positive = mean(rho > 0), frac_sig = mean(p < 0.05),
              p_wilcoxon = suppressWarnings(wilcox.test(rho)$p.value), .groups = 'drop')

# mixed models ####
mm <- nonexp %>% filter(!is.na(Gz), !is.na(Tz)) %>% mutate(water_year = factor(water_year))
f_main <- resid ~ Gz + Tz + (1 | site_code) + (1 | water_year)
models <- list(
    `non-exp, all site-years: GPP + T`               = fit_lmm(f_main, mm),
    `non-exp, sites >= 6 years`                      = fit_lmm(f_main, filter(mm, n_years_q >= min_years_anomaly)),
    `non-exp, sites 1-5 years only`                  = fit_lmm(f_main, filter(mm, n_years_q <  min_years_anomaly)),
    `non-exp, all: + prior-year P`                   = fit_lmm(resid ~ Gz + Tz + Pz_lag1 + (1 | site_code) + (1 | water_year),
                                                               filter(mm, !is.na(Pz_lag1))),
    `non-exp, all: + AI anomaly (curve-shape check)` = fit_lmm(resid ~ Gz + Tz + AIz + (1 | site_code) + (1 | water_year),
                                                               filter(mm, !is.na(AIz))),
    `non-exp, all: random slopes by site`            = fit_lmm(resid ~ Gz + Tz + (1 | site_code) + (0 + Gz + Tz || site_code) + (1 | water_year), mm),
    `non-exp, all: fitted omega residual`            = fit_lmm(resid_fit ~ Gz + Tz + (1 | site_code) + (1 | water_year), mm),
    `non-exp, EI within [0, 1] only`                 = fit_lmm(f_main, filter(mm, ei_in_range)))
if(n_distinct(na.omit(mm$aridity_class)) == 2){
    models[['non-exp, all: (GPP + T) x aridity class']] <-
        fit_lmm(resid ~ (Gz + Tz) * aridity_class + (1 | site_code) + (1 | water_year),
                mm %>% filter(!is.na(aridity_class)) %>%
                    mutate(aridity_class = factor(aridity_class, levels = c('humid', 'arid'))))
    for(cl in c('humid', 'arid'))
        models[[paste0('non-exp ', cl, ': GPP + T')]] <- fit_lmm(f_main, filter(mm, aridity_class == cl))
}
models[['all sites (exp + non-exp): GPP + T']] <-
    fit_lmm(f_main, bud %>% filter(!is.na(Gz), !is.na(Tz)) %>% mutate(water_year = factor(water_year)))
coefs <- imap(models, ~ coef_tbl(.x, .y)) %>% bind_rows()

# conversion of residual units (fraction of P) to percent of mean Q at the typical runoff
# ratio of the sites in each fit (class-specific for the humid / arid fits)
site_rr <- nonexp %>% group_by(site_code, aridity_class) %>% summarize(rr = mean(runoff_ratio), .groups = 'drop')
median_rr <- median(site_rr$rr)
rr_class <- site_rr %>% filter(!is.na(aridity_class)) %>% group_by(aridity_class) %>% summarize(rr = median(rr), .groups = 'drop')
coefs <- coefs %>%
    mutate(rr_used = case_when(grepl('humid', model) ~ rr_class$rr[rr_class$aridity_class == 'humid'],
                               grepl('arid', model) & !grepl('x aridity', model) ~ rr_class$rr[rr_class$aridity_class == 'arid'],
                               TRUE ~ median_rr),
           pct_of_Q_per_SD = -100 * estimate / rr_used,
           pct_lower = -100 * upper / rr_used, pct_upper = -100 * lower / rr_used)
# variance decomposition of the residual (null model): site, water year, within
m_null <- fit_lmm(resid ~ 1 + (1 | site_code) + (1 | water_year), mm)
vc_null <- as.data.frame(VarCorr(m_null)) %>% select(grp, vcov) %>% mutate(share = 100 * vcov / sum(vcov))

# outputs: tables ####
write_csv(bud %>% select(site_code, water_year, domain, ws_status, aridity_class, mean_ai, n_years_q, record_class,
                         precip_total, q_annual, pet_mm, AI, AI_daymet, EI, EI_fu, resid, resid_fit, ei_in_range,
                         Gz, Tz, Pz, Pz_lag1, AIz),
          file.path(out_dir, 'budyko_siteyear.csv'))
write_csv(coefs, file.path(out_dir, 'budyko_model_coefs.csv'))

# outputs: numbers ####
num_file <- file.path(out_dir, 'budyko_numbers.md')
sink(num_file, split = FALSE)
hdr <- function(x) cat('\n## ', x, '\n\n', sep = '')
ptab <- function(df, digits = 2){
    df <- df %>%
        mutate(across(where(is.numeric) & matches('^p($|_)'), ~ formatC(.x, digits = 2, format = 'g')),
               across(where(is.numeric), ~ round(.x, digits)))
    cat(knitr::kable(df, format = 'pipe'), sep = '\n'); cat('\n')
}
cat('# Budyko-space anomaly analysis (review item E3): numbers\n\n')
cat('Generated ', format(Sys.time(), '%Y-%m-%d %H:%M'), ' by src/budyko_anomaly.R from ', input_dir, '\n\n',
    'Window ', analysis_start_year, '-', analysis_end_year, '; climate pool (headwater, usable T trend); ',
    'site-years with > ', full_year_min_obs_per_year, ' discharge days; AI = Daymet PET / PRISM P; ',
    'EI = (PRISM P - Q) / PRISM P with Q = mean daily Q x days in the water year; ',
    'residual = EI - Fu(AI, omega = ', omega_default, '). Positive residual = more ET / less Q than expected.\n\n', sep = '')
cat('Regenerate this file after any pipeline rerun; numbers in the draft must come from here.\n')

hdr('1. Site-years and sites')
pool <- bind_rows(
    nonexp %>% summarize(set = 'non-experimental, all', n_sites = n_distinct(site_code), n_site_years = n()),
    nonexp %>% group_by(set = paste0('non-experimental, record ', record_class)) %>%
        summarize(n_sites = n_distinct(site_code), n_site_years = n(), .groups = 'drop'),
    nonexp %>% filter(!is.na(Gz)) %>% summarize(set = 'non-experimental with GPP', n_sites = n_distinct(site_code), n_site_years = n()),
    nonexp %>% filter(!is.na(Gz), n_years_q < min_years_anomaly) %>%
        summarize(set = 'non-experimental with GPP, record 1-5 years', n_sites = n_distinct(site_code), n_site_years = n()),
    bud %>% summarize(set = 'all sites (exp + non-exp)', n_sites = n_distinct(site_code), n_site_years = n()))
ptab(pool)
cat('Non-experimental site-years with EI < 0 (Q > P): ', sum(nonexp$EI < 0), '; EI > 1: ', sum(nonexp$EI > 1),
    '; sites with any EI < 0: ', paste(unique(nonexp$site_code[nonexp$EI < 0]), collapse = ', '), '\n', sep = '')
cat('Non-experimental sites by record length and aridity class:\n\n')
ptab(distinct(nonexp, site_code, record_class, aridity_class) %>%
         count(record_class, aridity_class) %>%
         group_by(record_class) %>% mutate(pct_of_record_class = 100 * n / sum(n)) %>% ungroup(), 0)

hdr('2. Budyko curve')
cat('Fu omega fitted to non-experimental site-years with EI in [0, 1]: ', fmt(omega_all),
    '; fitted to site means: ', fmt(omega_sites), '; default used for residuals: ', omega_default, '\n', sep = '')
cat('Residual (default omega): mean ', fmt(mean(nonexp$resid), 3), ', SD ', fmt(sd(nonexp$resid), 3),
    '; within-site SD (median across sites with >= ', min_years_anomaly, ' years) ',
    fmt(median(nonexp %>% filter(n_years_q >= min_years_anomaly) %>% group_by(site_code) %>% summarize(s = sd(resid)) %>% pull(s)), 3), '\n', sep = '')
cat('Variance of the residual by level (null mixed model, non-experimental site-years with GPP):\n\n'); ptab(vc_null, 4)
cat('Correlation of AI (PRISM P) with AI (Daymet P) across site-years: ', fmt(cor(nonexp$AI, nonexp$AI_daymet, use = 'complete.obs'), 3), '\n', sep = '')
cat('Median site runoff ratio, non-experimental: ', fmt(median_rr, 3), '; humid ', fmt(rr_class$rr[rr_class$aridity_class == 'humid'], 3),
    '; arid ', fmt(rr_class$rr[rr_class$aridity_class == 'arid'], 3), ' (used to express residual effects as % of mean Q)\n', sep = '')

hdr('3. Pooled Spearman rho of the Budyko residual with driver anomalies, non-experimental')
ptab(pooled_cor)
hdr('3b. Within-site Spearman of the residual, sites with >= 6 years (compare anomaly_numbers.md, GPP | P,T)')
ptab(site_cor_summary)

hdr('4. Mixed models of the residual (fraction of P) on driver anomalies; random intercepts by site and water year')
ptab(coefs %>% select(model, term, estimate, lower, upper, p, n_obs, n_sites, singular), 4)
hdr('4b. Same effects as percent of mean Q per SD of the driver (negative = less Q), at the median runoff ratio')
ptab(coefs %>% filter(term %in% c('Gz', 'Tz', 'Pz_lag1')) %>%
         select(model, term, rr_used, pct_of_Q_per_SD, pct_lower, pct_upper, p), 1)
sink()
message('numbers written to ', num_file)

# figure ####
class_cols <- c(humid = '#0072B2', arid = '#D55E00')
curve_df <- tibble(AI = seq(0.05, max(nonexp$AI, na.rm = TRUE), length.out = 300)) %>%
    mutate(`Fu, omega = 2.6` = fu(AI, omega_default),
           fitted = fu(AI, omega_all)) %>%
    pivot_longer(-AI, names_to = 'curve', values_to = 'EI') %>%
    mutate(curve = recode(curve, fitted = paste0('Fu, fitted omega = ', fmt(omega_all, 1))))
fig_pts <- nonexp %>% filter(!is.na(Gz)) %>%
    mutate(Gz_clip = pmax(pmin(Gz, 2), -2))
panel_a <- ggplot(fig_pts, aes(x = AI, y = EI)) +
    geom_hline(yintercept = 1, colour = 'grey70', linewidth = 0.3) +
    geom_abline(slope = 1, intercept = 0, colour = 'grey70', linewidth = 0.3) +
    geom_point(aes(colour = Gz_clip), size = 0.7, alpha = 0.6) +
    geom_line(data = curve_df, aes(linetype = curve), colour = 'black', linewidth = 0.5) +
    scale_colour_gradient2(low = '#762a83', mid = '#d9d9d9', high = '#1b7837', midpoint = 0,
                           limits = c(-2, 2), name = 'GPP anomaly (z)') +
    scale_linetype_manual(values = c('solid', 'dashed'), name = NULL) +
    scale_x_continuous(limits = c(0, NA)) +
    coord_cartesian(ylim = c(-0.6, 1.1)) +
    labs(x = 'Aridity index PET / P', y = 'Evaporative index (P - Q) / P',
         subtitle = paste0('Non-experimental site-years (n = ', nrow(fig_pts), ', ', n_distinct(fig_pts$site_code),
                           ' sites; ', sum(fig_pts$EI < -0.6), ' below the axis)')) +
    theme_few(base_size = 10) +
    theme(legend.position = 'inside', legend.position.inside = c(0.78, 0.22), legend.box = 'horizontal',
          legend.title = element_text(size = 8), legend.text = element_text(size = 8),
          legend.key.height = unit(0.35, 'cm'), legend.background = element_blank(),
          plot.subtitle = element_text(size = 9))

resid_pts <- fig_pts %>% mutate(aridity_class = factor(aridity_class, levels = names(class_cols)))
panel_b <- ggplot(resid_pts, aes(x = Gz, y = resid, colour = aridity_class)) +
    geom_hline(yintercept = 0, colour = 'grey70', linewidth = 0.3) +
    geom_point(size = 0.6, alpha = 0.4) +
    geom_smooth(method = 'lm', formula = y ~ x, se = TRUE, linewidth = 0.7) +
    scale_colour_manual(values = class_cols, name = 'Aridity (PET/P)') +
    coord_cartesian(ylim = c(-0.6, 0.6)) +
    labs(x = 'GPP anomaly (z)', y = 'Budyko residual (fraction of P; more ET: up)',
         subtitle = ' ') +
    theme_few(base_size = 10) +
    theme(legend.position = 'top', legend.title = element_text(size = 8), legend.text = element_text(size = 8),
          legend.margin = margin(0, 0, 0, 0), plot.subtitle = element_text(size = 9))
panel_c <- ggplot(resid_pts, aes(x = Tz, y = resid, colour = aridity_class)) +
    geom_hline(yintercept = 0, colour = 'grey70', linewidth = 0.3) +
    geom_point(size = 0.6, alpha = 0.4) +
    geom_smooth(method = 'lm', formula = y ~ x, se = TRUE, linewidth = 0.7) +
    scale_colour_manual(values = class_cols, guide = 'none') +
    coord_cartesian(ylim = c(-0.6, 0.6)) +
    labs(x = 'T anomaly (z)', y = NULL, subtitle = ' ') +
    theme_few(base_size = 10) + theme(plot.subtitle = element_text(size = 9))

coef_plot <- coefs %>%
    filter(model %in% c('non-exp, all site-years: GPP + T', 'non-exp, sites >= 6 years',
                        'non-exp, sites 1-5 years only', 'non-exp humid: GPP + T', 'non-exp arid: GPP + T'),
           term %in% c('Gz', 'Tz')) %>%
    mutate(set = recode(model, `non-exp, all site-years: GPP + T` = 'all', `non-exp, sites >= 6 years` = '>= 6 yr sites',
                        `non-exp, sites 1-5 years only` = '1-5 yr sites', `non-exp humid: GPP + T` = 'humid',
                        `non-exp arid: GPP + T` = 'arid'),
           set = factor(set, levels = c('all', '>= 6 yr sites', '1-5 yr sites', 'humid', 'arid')),
           term = factor(recode(term, Gz = 'GPP', Tz = 'T'), levels = c('GPP', 'T')))
set_cols <- c(all = '#000000', `>= 6 yr sites` = '#555555', `1-5 yr sites` = '#999999', humid = '#0072B2', arid = '#D55E00')
panel_d <- ggplot(coef_plot, aes(x = term, y = estimate, colour = set)) +
    geom_hline(yintercept = 0, colour = 'grey55', linewidth = 0.3) +
    geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.6), size = 0.3, linewidth = 0.5) +
    scale_colour_manual(values = set_cols, name = NULL) +
    labs(x = NULL, y = 'Effect on residual (fraction of P per SD)', subtitle = 'Mixed model (95% CI)') +
    theme_few(base_size = 10) +
    theme(legend.position = 'top', legend.text = element_text(size = 8), legend.key.width = unit(0.3, 'cm'),
          plot.subtitle = element_text(size = 9)) +
    guides(colour = guide_legend(nrow = 2))

fig <- panel_a / (panel_b | panel_c | panel_d) +
    plot_layout(heights = c(1.2, 1)) +
    plot_annotation(tag_levels = 'a')
ggsave(file.path(fig_dir, 'budyko_anomaly_draft.png'), fig, width = 7.2, height = 7.4, dpi = 400, bg = 'white')
message('figure written to ', file.path(fig_dir, 'budyko_anomaly_draft.png'))
