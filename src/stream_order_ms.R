library(tidyverse)
library(here)
library(sf)
library(arcgislayers)
library(nhdplusTools)
library(rnaturalearth)
source(here('src', 'setup.R'))

# ms gauges ####
# geospatial aoi = a polygon or point sf object. either work
# geospatial_aoi <- full_prism_trends <- read_csv(here('data_working', 'trends', 'full_prisim_climate.csv')) %>%
#     add_flags() %>%
#     left_join(.,ms_site_data, by = 'site_code') %>%
#     select(site_code, latitude, longitude) %>%
#     distinct() %>%
#     st_as_sf(coords = c("longitude","latitude"), crs = 4326)

geospatial_aoi <- ms_load_sites() %>%
    filter(site_type == 'stream_gauge') %>%
    select(site_code, latitude, longitude) %>%
    na.omit() %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

us_states <- ne_states(country = "United States of America", returnclass = "sf") %>%
    st_transform(crs = 4326)


mapview(geospatial_aoi)
mapview(us_states)


us_only <- st_join(geospatial_aoi, us_states) %>%
    filter(!is.na(name),
           name != 'Alaska')

mapview(us_only)


output <- tibble(site_code = as.character(),
                 stream_order = as.integer())

for(i in 1:nrow(us_only)){

    nearest_comid <- try(discover_nhdplus_id(us_only[i,]), silent = TRUE)

    if(!inherits(nearest_comid, 'try-error') && !is.null(nearest_comid)){
        flowline_data <- get_nhdplus(comid = nearest_comid)
        inner <- tibble(site_code = us_only$site_code[i],
                        stream_order = flowline_data$streamorde)
    } else {
        inner <- tibble(site_code = us_only$site_code[i],
                        stream_order = NA_integer_)
    }

    output <- rbind(output, inner)
}
# Extract the stream order from the flowline data

ms_sites_str_ord <- ms_load_sites() %>%
    filter(site_code %in% unique(us_only$site_code)) %>%
    full_join(., output, by = 'site_code')

ms_sites_str_ord <- ms_sites_str_ord %>%
    filter(!is.na(stream_order))

ms_sites_str_ord %>%
    filter(stream_order < 2) %>%
    select(site_code, stream_order) %>%
    write_csv(here('data_working', 'ms_stream_order.csv'))

