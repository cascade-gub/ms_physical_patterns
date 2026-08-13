library(here)
source(here('src', 'setup.R'))


# daily precip (mm) ####

# remember, "median" in this variable name refers to the way data were aggregated spatially.
# These are daily totals in mm.


# trends
trends <- read_csv(here('data_working', 'site_groupings_by_prsim_trend.csv'))
sums <- ms_load_product(my_ms_dir,
                                  prodname = "ws_attr_timeseries:all",
                                  warn = FALSE)
metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear_nTest.rds'))%>%
    filter(agg_code == 'annual')

# ms_download_ws_attr(ms_root, "time series", timeout = 10000, skip_existing = TRUE)
p <- ms_load_product(my_ms_dir,
                     prodname = 'ws_attr_timeseries:climate',
                     filter_vars = 'precip_median',
                     warn = FALSE) %>%
    select(-var, -year, -pctCellErr, precip_median = val)

# daily pET (mm) ####

#this dataset also includes precip, from daymet instead of prism

# ms_download_ws_attr(ms_root, "CAMELS Daymet forcings", timeout = 10000, skip_existing = TRUE)
daymet <- ms_load_product(my_ms_dir,
                          prodname = 'ws_attr_CAMELS_Daymet_forcings',
                          warn = FALSE)

# aET data
et <- read_csv(here('data_raw', 'ms_add_ons', 'macrosheds_et2.csv'))
et_obs <- metrics %>%
    filter(agg_code == 'annual') %>%
    select(site_code, wy = water_year, precip_total, q_totsum) %>%
    mutate(ei_obs = (precip_total - q_totsum)/precip_total)

# indices ####

# this is the same way it's calculated by CAMELS, so should be same as "aridity" in
# ws_attr_CAMELS_summaries, but daily. (https://gdex.ucar.edu/dataset/camels/file/camels_attributes_v2.0.xlsx)

d <- inner_join(p, daymet, by = c('network', 'domain', 'site_code', 'date')) %>%
    mutate(year = as.integer(as.character(water_year(date, origin = 'usgs')))) %>%
    group_by(year, site_code) %>%
    summarize(aridity_index = sum(`pet(mm)`)/sum(`prcp(mm/day)`),
              precip = sum(`prcp(mm/day)`))  %>%
    full_join(., et, by = c('site_code', 'year')) %>%
    mutate(evaporative_index = val/(precip*10)) %>%
    full_join(., et_obs, by = c('site_code', 'year' = 'wy'))


ei_scatter <- d%>%
    left_join(., ms_site_data, by = 'site_code') %>%
ggplot(., aes(x = ei_obs, y = evaporative_index, color = domain))+
    geom_point()+
    theme_few()+
    geom_abline(slope = 1, color = 'red', linewidth = 2)
    #geom_smooth(method = 'lm')
ggsave(here('figures', 'ei_obs_vs_modeled.png'), ei_scatter, width = 10, height = 8, dpi = 300)


pq_couple <- d %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    #filter(q_totsum > 1000) %>%
    filter(ws_status == "non-experimental") %>%
    filter(year > 1980) %>%
    select(precip_total, q_totsum, domain, site_code, year) %>%
    na.omit() %>%
    ggplot(., aes(x = precip_total, y = q_totsum, color = domain, group = domain))+
        geom_point(aes(label = paste0(site_code, year)), size = .5)+
        theme_few()+#base_size = 20)+
        #scale_y_log10()+
        stat_smooth(method = 'lm', inherit.aes = TRUE, se = FALSE, span = 1.1, show.legend = TRUE) +
        geom_text_repel(data = d %>% left_join(., ms_site_data, by = 'site_code') %>%
                                                        group_by(domain) %>%
                                                        summarize(x = max(precip_total, na.rm = T), y = max(q_totsum, na.rm = T)),
                        aes(label = domain, x = x, y = y),
                        size = 5)+
        geom_abline(slope = 1, color = 'red', linewidth = 2)+
        labs(title = 'P-Q coupling',
            x = 'total precip (mm)',
            y = 'total Q (mm)')+
    scale_color_viridis(discrete = T)+
    theme(legend.position = 'none')
ggsave(here('figures', 'pq_coupling.png'), pq_couple, width = 10, height = 8, dpi = 300)

# ggplotly(pq_couple)

d_all <- d %>%
    filter(year %in% 1980:2020) %>%
    na.omit() %>%
    group_by(site_code) %>%
    summarize(aridity_index_min = min(aridity_index, na.rm = T),
              aridity_index_max = max(aridity_index, na.rm = T),
              aridity_index_range = aridity_index_max - aridity_index_min,
              aridity_idex_sd = sd(aridity_index, na.rm = T),
              aridity_index = mean(aridity_index, na.rm = T),
              n = n(),
              evaporative_index_min = min(evaporative_index, na.rm = T),
              evaporative_index_max = max(evaporative_index, na.rm = T),
              evaporative_index_range = evaporative_index_max - evaporative_index_min,
              evaporative_index_sd = sd(evaporative_index, na.rm = T),
              evaporative_index = mean(evaporative_index, na.rm = T),
              ei_obs_min = min(ei_obs, na.rm = T),
              ei_obs_max = max(ei_obs, na.rm = T),
              ei_obs_range = ei_obs_max - ei_obs_min,
              ei_obs_sd = sd(ei_obs, na.rm = T),
              ei_obs = mean(ei_obs, na.rm = T),
              responsitivty = coef(lm(q_totsum ~ precip_total))[[2]],
              rsquared = summary(lm(q_totsum ~ precip_total))$r.squared) %>%
    mutate(a_min = aridity_index-aridity_idex_sd,
           a_max = aridity_index+aridity_idex_sd) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    filter(ws_status == "non-experimental") %>%
    left_join(., trends, by = 'site_code')


# * caveat! you'll find that p/pet is another common aridity index. GPT has all sorts
# of explanations for why CAMELS uses pet/p instead, including that it fits better
# with the Budyko framework. Still, you might want to check mean(pet) / mean(p) to see if it matches
# the aridity values given in ws_attr_CAMELS_summaries. It'll be most similar if you use "prcp(mm/day)"
# from daymet instead of "precip_median" from prism.

ei_range_bar <- d_all %>%
    filter(ei_obs_range > 0,
           !is.na(q_flag)) %>%
ggplot(aes(x = reorder(site_code, ei_obs_range), y = ei_obs_range, fill = q_flag))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values = c('increasing' = 'red', 'decreasing' = 'blue', 'non-significant' = 'grey'))+
    theme_few()+
    labs(fill = 'Q trend')+
    theme(axis.text.x = element_text(angle = 45))
ggsave(here('figures', 'ei_range_by_site.png'), ei_range_bar, width = 14, height = 7, dpi = 300)





budyko_plot <- d_all %>%
    filter(!is.na(q_flag)) %>%
    mutate(label = paste(domain.x, ' ', site_code)) %>%
ggplot(.,aes(x = aridity_index, y = ei_obs, color = q_flag))+
    geom_abline(slope = 1, color = 'red', linewidth = 2)+
    geom_hline(yintercept = 1, color = 'blue', linewidth = 2)+
    geom_vline(xintercept = 1, color = 'blue', linewidth = 2)+
    geom_point()+
    geom_text(aes(label = label, vjust = 1))+
    geom_errorbar(aes(xmin = a_min, xmax = a_max))+
    theme_few(base_size = 20)+
    scale_color_manual(values = c('increasing' = 'red', 'decreasing' = 'blue', 'non-significant' = 'grey'))+
    labs(color = 'Q trend',
         title = '1980-2020 average w/ sd error bars')
ggsave(here('figures', 'budyko_plot.png'), budyko_plot, width = 12, height = 10, dpi = 300)

# time series graphs
# north creek is increasing in flow
d %>%
    filter(site_code == 'north_creek',
           year > 1999) %>%
    ggplot(.,aes(x = aridity_index, y = ei_obs, color = year))+
    geom_abline(slope = 1, color = 'red', linewidth = 2)+
    geom_hline(yintercept = 1, color = 'blue', linewidth = 2)+
    geom_vline(xintercept = 1, color = 'blue', linewidth = 2)+
    geom_point()+
    geom_label_repel(aes(label = year, vjust = 1))+
    theme_few(base_size = 20)+
    #scale_color_manual(values = c('red', 'blue', 'grey'))+
    geom_segment(color="black",
                 aes(
                     xend=c(tail(aridity_index, n=-1), NA),
                     yend=c(tail(ei_obs, n=-1), NA)
                 ),
                 arrow=arrow(length=unit(0.3,"cm"))
    )+
    scale_color_viridis()+
    labs(color = 'Year',
         title = 'north creek through time')

d %>%
    filter(site_code == 'north_creek',
           year > 1999) %>%
    mutate(ratio = (ei_obs/aridity_index)) %>%
    select(year, aridity_index, ei_obs, ratio) %>%
    pivot_longer(cols = -year, names_to = 'var', values_to = 'val') %>%
    ggplot(aes(x = year, y = val))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept = 1, color = 'red', linewidth = 2)+
    theme_few()+
    facet_wrap(~var, ncol = 1)

# eb also is increasing in flow
d %>%
    filter(site_code == 'EB',
           year > 1999) %>%
    ggplot(.,aes(x = aridity_index, y = ei_obs, color = year))+
    geom_abline(slope = 1, color = 'red', linewidth = 2)+
    geom_hline(yintercept = 1, color = 'blue', linewidth = 2)+
    geom_vline(xintercept = 1, color = 'blue', linewidth = 2)+
    geom_point()+
    geom_label_repel(aes(label = year, vjust = 1))+
    theme_few(base_size = 20)+
    #scale_color_manual(values = c('red', 'blue', 'grey'))+
    geom_segment(color="black",
                 aes(
                     xend=c(tail(aridity_index, n=-1), NA),
                     yend=c(tail(ei_obs, n=-1), NA)
                 ),
                 arrow=arrow(length=unit(0.3,"cm"))
    )+
    scale_color_viridis()+
    labs(color = 'Year',
         title = 'EB through time')

d %>%
    filter(site_code == 'EB',
           year > 1999) %>%
    mutate(ratio = (ei_obs/aridity_index)) %>%
    select(year, aridity_index, ei_obs, ratio) %>%
    pivot_longer(cols = -year, names_to = 'var', values_to = 'val') %>%
    ggplot(aes(x = year, y = val))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept = 1, color = 'red', linewidth = 2)+
    theme_few()+
    facet_wrap(~var, ncol = 1)

# santa barbra has decreasing flow
d %>%
    filter(site_code == 'RS02',
           year > 1999) %>%
    ggplot(.,aes(x = aridity_index, y = ei_obs, color = year))+
    geom_abline(slope = 1, color = 'red', linewidth = 2)+
    geom_hline(yintercept = 1, color = 'blue', linewidth = 2)+
    geom_vline(xintercept = 1, color = 'blue', linewidth = 2)+
    geom_point()+
    geom_label_repel(aes(label = year, vjust = 1))+
    theme_few(base_size = 20)+
    #scale_color_manual(values = c('red', 'blue', 'grey'))+
    geom_segment(color="black",
                 aes(
                     xend=c(tail(aridity_index, n=-1), NA),
                     yend=c(tail(ei_obs, n=-1), NA)
                 ),
                 arrow=arrow(length=unit(0.3,"cm"))
    )+
    scale_color_viridis()+
    labs(color = 'Year',
         title = 'RS02 through time')

d %>%
    filter(site_code == 'RS02',
           year > 1999) %>%
    mutate(ratio = (ei_obs/aridity_index)) %>%
    select(year, aridity_index, ei_obs, ratio) %>%
    pivot_longer(cols = -year, names_to = 'var', values_to = 'val') %>%
    ggplot(aes(x = year, y = val))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept = 1, color = 'red', linewidth = 2)+
    theme_few()+
    facet_wrap(~var, ncol = 1)


# santa barbra has decreasing flow
d %>%
    filter(site_code == 'AB00',
           year > 1999) %>%
    ggplot(.,aes(x = aridity_index, y = evaporative_index, color = year))+
    geom_abline(slope = 1, color = 'red', linewidth = 2)+
    geom_hline(yintercept = 1, color = 'blue', linewidth = 2)+
    geom_vline(xintercept = 1, color = 'blue', linewidth = 2)+
    geom_point()+
    geom_label_repel(aes(label = year, vjust = 1))+
    theme_few(base_size = 20)+
    #scale_color_manual(values = c('red', 'blue', 'grey'))+
    geom_segment(color="black",
                 aes(
                     xend=c(tail(aridity_index, n=-1), NA),
                     yend=c(tail(evaporative_index, n=-1), NA)
                 ),
                 arrow=arrow(length=unit(0.3,"cm"))
    )+
    scale_color_viridis()+
    labs(color = 'Year',
         title = 'AB00 through time')

ratio_data <- d %>%
    filter(site_code == 'AB00',
           year > 1999) %>%
    mutate(ratio = (evaporative_index/aridity_index))

ratio_data %>%
    select(year, aridity_index, evaporative_index, ratio) %>%
    pivot_longer(cols = -year, names_to = 'var', values_to = 'val') %>%
    ggplot(aes(x = year, y = val))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept = 1, color = 'red', linewidth = 2)+
    theme_few()+
    #geom_smooth(method = 'lm')+
    facet_wrap(~var, ncol = 1)

mod <- lm(ratio~year, ratio_data)
summary(mod)
plot(mod)

mod <- lm(aridity_index~year, ratio_data)
summary(mod)
plot(mod)

mod <- lm(evaporative_index~year, ratio_data)
summary(mod)
plot(mod)

# w6 is a known quantity as a control
d %>%
    filter(site_code == 'w6',
           year > 1999) %>%
    ggplot(.,aes(x = aridity_index, y = ei_obs, color = year))+
    geom_abline(slope = 1, color = 'red', linewidth = 2)+
    geom_hline(yintercept = 1, color = 'blue', linewidth = 2)+
    geom_vline(xintercept = 1, color = 'blue', linewidth = 2)+
    geom_point()+
    geom_label_repel(aes(label = year, vjust = 1))+
    theme_few(base_size = 20)+
    #scale_color_manual(values = c('red', 'blue', 'grey'))+
    geom_segment(color="black",
                 aes(
                     xend=c(tail(aridity_index, n=-1), NA),
                     yend=c(tail(ei_obs, n=-1), NA)
                 ),
                 arrow=arrow(length=unit(0.3,"cm"))
    )+
    scale_color_viridis()+
    labs(color = 'Year',
         title = 'w6 through time')

d %>%
    filter(site_code == 'w6',
           year > 1999) %>%
    mutate(ratio = (ei_obs/aridity_index)) %>%
    select(year, aridity_index, ei_obs, ratio) %>%
    pivot_longer(cols = -year, names_to = 'var', values_to = 'val') %>%
    ggplot(aes(x = year, y = val))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept = 1, color = 'red', linewidth = 2)+
    #geom_smooth(method = 'lm')+
    theme_few()+
    facet_wrap(~var, ncol = 1)

# w5 got deforested
d %>%
    filter(site_code == 'w5',
           year > 1999) %>%
    ggplot(.,aes(x = aridity_index, y = ei_obs, color = year))+
    geom_abline(slope = 1, color = 'red', linewidth = 2)+
    geom_hline(yintercept = 1, color = 'blue', linewidth = 2)+
    geom_vline(xintercept = 1, color = 'blue', linewidth = 2)+
    geom_point()+
    geom_label_repel(aes(label = year, vjust = 1))+
    theme_few(base_size = 20)+
    #scale_color_manual(values = c('red', 'blue', 'grey'))+
    geom_segment(color="black",
                 aes(
                     xend=c(tail(aridity_index, n=-1), NA),
                     yend=c(tail(ei_obs, n=-1), NA)
                 ),
                 arrow=arrow(length=unit(0.3,"cm"))
    )+
    scale_color_viridis()+
    labs(color = 'Year',
         title = 'w5 through time')

d %>%
    filter(site_code == 'w5',
           year > 1999) %>%
    mutate(ratio = (ei_obs/aridity_index)) %>%
    select(year, aridity_index, ei_obs, ratio) %>%
    pivot_longer(cols = -year, names_to = 'var', values_to = 'val') %>%
    ggplot(aes(x = year, y = val))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept = 1, color = 'red', linewidth = 2)+
    #geom_smooth(method = 'lm')+
    theme_few()+
    facet_wrap(~var, ncol = 1)

# santa barbra has decreasing flow
d %>%
    filter(site_code == 'ER_RUS1',
           year > 1999) %>%
    ggplot(.,aes(x = aridity_index, y = evaporative_index, color = year))+
    geom_abline(slope = 1, color = 'red', linewidth = 2)+
    geom_hline(yintercept = 1, color = 'blue', linewidth = 2)+
    geom_vline(xintercept = 1, color = 'blue', linewidth = 2)+
    geom_point()+
    geom_label_repel(aes(label = year, vjust = 1))+
    theme_few(base_size = 20)+
    #scale_color_manual(values = c('red', 'blue', 'grey'))+
    geom_segment(color="black",
                 aes(
                     xend=c(tail(aridity_index, n=-1), NA),
                     yend=c(tail(evaporative_index, n=-1), NA)
                 ),
                 arrow=arrow(length=unit(0.3,"cm"))
    )+
    scale_color_viridis()+
    labs(color = 'Year',
         title = 'ER_RUS1')

d %>%
    filter(site_code == 'ER_RUS1',
           year > 1999) %>%
    mutate(ratio = (evaporative_index/aridity_index)) %>%
    select(year, aridity_index, evaporative_index, ratio) %>%
    pivot_longer(cols = -year, names_to = 'var', values_to = 'val') %>%
    ggplot(aes(x = year, y = val))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept = 1, color = 'red', linewidth = 2)+
    theme_few()+
    facet_wrap(~var, ncol = 1)
