# clear environment
#rm(list = ls())
# Load packages.
library(here)
library(hydroTSM)
source(here('src', 'setup.R'))
#source(here('src', 'mega_zipper_data.R'))
flag_colors <- c('increasing' = "red", 'decreasing' = 'blue', 'flat' = 'green', 'non-significant' = "grey")

# read in data####
full_prism_trends <- read_csv(here('data_working', 'trends', 'full_prisim_climate.csv')) %>%
    add_flags()
longest_run_trends <- read_csv(here('data_working', 'trends', 'best_run_prisim.csv')) %>%
    add_flags()

changing_sites <- longest_run_trends %>%
    filter(var == 'q_mean',
           p < 0.05) %>%
    .$site_code

q_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "discharge",
    warn = F,
    site_codes = changing_sites
)

# https://stackoverflow.com/questions/59914776/flow-duration-curve-using-facet-wrap-of-ggplot-in-r
for(i in changing_sites){
trend <- longest_run_trends %>%
        filter(site_code == i,
               var == 'q_mean') %>%
        .$trend

q_data %>%
        select(site_code, date, val) %>%
        filter(site_code == i) %>%
        mutate(wy = as.integer(as.character(water_year(date, origin = 'usgs')))) %>%
        filter(wy >= analysis_start_year) %>%
        group_by(wy) %>%
        na.omit() %>%
        arrange(-val) %>%
        mutate(q_pct = 1-percent_rank(val)) %>%
        ungroup() %>%


    ggplot(aes(q_pct, val, color = wy, group = wy)) +
        geom_line() +
        theme_light(base_size = 10) +
        #facet_wrap(~wy, ncol = 5) +
        scale_y_log10()+
        labs(x = "% Time flow equalled or exceeded",
             y = "Q, [m3/s]",
             title = i,
             caption = paste0('sen slope of ', trend)) +
        theme(strip.text = element_text(hjust = 0, color = "black"),
              strip.background = element_blank())+
    scale_color_viridis()
ggsave(here('figures', 'flow_duration_curves', paste0(i,'.png')), width = 10, height = 5)
}

