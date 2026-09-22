# Supplement figure S4: is the site-level aridity index climatology sensitive to
# the averaging window? Compares the pre-MODIS window used in earlier drafts
# (1980-2020) with the analysis window (analysis_start_year-analysis_end_year).
library(here)
source(here('src', 'setup.R'))

sites <- read_csv(here('data_working', 'site_groupings_by_prsim_trend.csv'),
                  show_col_types = FALSE) %>%
    select(site_code, ws_status)

# aridity index exactly as in grouping_histograms.R / patchwork_figures.R
daymet <- ms_load_product(here('data_raw', 'ms'),
                          prodname = 'ws_attr_CAMELS_Daymet_forcings',
                          warn = FALSE)

p <- ms_load_product(my_ms_dir,
                     prodname = 'ws_attr_timeseries:climate',
                     filter_vars = 'precip_median',
                     warn = FALSE) %>%
    select(-var, -year, -pctCellErr, precip_median = val)

d <- inner_join(p, daymet, by = c('network', 'domain', 'site_code', 'date')) %>%
    mutate(year = as.integer(as.character(water_year(date, origin = 'usgs')))) %>%
    group_by(year, site_code) %>%
    summarize(aridity_index = sum(`pet(mm)`) / sum(`prcp(mm/day)`), .groups = 'drop')

pre_window <- c(1980, 2020)

ai <- d %>%
    filter(site_code %in% sites$site_code) %>%
    group_by(site_code) %>%
    summarize(ai_pre  = mean(aridity_index[year >= pre_window[1] & year <= pre_window[2]], na.rm = TRUE),
              ai_new  = mean(aridity_index[year >= analysis_start_year & year <= analysis_end_year], na.rm = TRUE),
              .groups = 'drop') %>%
    left_join(sites, by = 'site_code') %>%
    drop_na(ai_pre, ai_new) %>%
    mutate(shift = ai_new - ai_pre,
           class_pre = if_else(ai_pre > 1, 'arid', 'humid'),
           class_new = if_else(ai_new > 1, 'arid', 'humid'),
           class_change = class_pre != class_new)

n_sites   <- nrow(ai)
r_val     <- cor(ai$ai_pre, ai$ai_new)
med_shift <- median(ai$shift)
med_abs   <- median(abs(ai$shift))
max_abs   <- max(abs(ai$shift))
n_change  <- sum(ai$class_change)
changers  <- ai %>% filter(class_change) %>% arrange(site_code)

cat('n sites:', n_sites, '\n')
cat('Pearson r:', round(r_val, 3), '\n')
cat('median shift (2001-2023 minus 1980-2020):', round(med_shift, 3), '\n')
cat('median |shift|:', round(med_abs, 3), '| max |shift|:', round(max_abs, 3), '\n')
cat('mean AI 1980-2020:', round(mean(ai$ai_pre), 3), '| mean AI 2001-2023:', round(mean(ai$ai_new), 3), '\n')
cat('sites changing humid/arid class:', n_change, '\n')
if (n_change > 0) print(as.data.frame(select(changers, site_code, ws_status, ai_pre, ai_new, class_pre, class_new)))

axis_max <- max(c(ai$ai_pre, ai$ai_new)) * 1.05

s4 <- ggplot(ai, aes(x = ai_pre, y = ai_new)) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', color = 'grey40') +
    geom_vline(xintercept = 1, color = 'orange', linewidth = 1, linetype = 'longdash') +
    geom_hline(yintercept = 1, color = 'orange', linewidth = 1, linetype = 'longdash') +
    geom_point(aes(color = ws_status, shape = ws_status), size = 2.5, alpha = 0.85) +
    geom_text_repel(data = changers, aes(label = site_code), size = 3.2,
                    min.segment.length = 0, box.padding = 0.5) +
    scale_color_manual(values = c('experimental' = '#D95F02', 'non-experimental' = '#1B4F72'),
                       name = 'Condition') +
    scale_shape_manual(values = c('experimental' = 17, 'non-experimental' = 16),
                       name = 'Condition') +
    coord_fixed(xlim = c(0, axis_max), ylim = c(0, axis_max)) +
    labs(x = paste0('Aridity index, mean ', pre_window[1], '–', pre_window[2], ' (PET/P)'),
         y = paste0('Aridity index, mean ', analysis_start_year, '–', analysis_end_year, ' (PET/P)'),
         subtitle = paste0('n = ', n_sites, ' sites | r = ', round(r_val, 3),
                           ' | median |shift| = ', round(med_abs, 3),
                           ' | max |shift| = ', round(max_abs, 2),
                           ' | ', n_change, ' site', if (n_change == 1) '' else 's',
                           ' change humid/arid class'),
         caption = 'Dashed = 1:1; orange = aridity index of 1 (humid < 1 < arid)') +
    theme_few(base_size = 13) +
    theme(plot.subtitle = element_text(size = 9),
          legend.position = 'bottom')

ggsave(here('figures', 'Figure_S4.png'), s4, width = 7, height = 7, dpi = 300)
cat('Figure saved to figures/Figure_S4.png\n')
