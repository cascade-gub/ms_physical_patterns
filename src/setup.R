# setup script to save effort

# Note, this script uses a newer version of the data (v2)
# published on 09/30/24.

# Load common packages ####
## core
library(here)
library(macrosheds)
library(foreach)
## data vis
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(ggtext)
library(viridis)
library(patchwork)
library(ggforce)
library(mapview)
library(plotly)
library(scales)
#library(ggplotify)
#library(gt)
## stats
library(e1071)
library(lfstat)
library(trend)
library(zyp)
## data carpentry
library(naniar)
library(lubridate)
library(tidyverse)
library(sf)
## util
library(feather)
library(logger)
library(foreach)

# data pointing ####
# download the ms dataset to your data_raw folder and update the script here

rdata_path <- here('data_raw', 'ms') # updated path
my_ms_dir <- here('data_raw', 'ms')

ms_site_data <- ms_load_sites()
ms_ws_attr <- read_feather('data_raw/ms/v2/watershed_summaries.feather')

# set master vars for data coverage to define 'good years'
# shared across all data coverage scripts
start_year_master <- 2001
minimum_per_week_sampling_frequency_master <- 4
good_weeks_to_year_master <- 51
minimum_continuous_record_length_master <- 10

# set flag colors for plots
flag_colors <- c('increasing' = "red",
                 'decreasing' = 'blue',
                 'flat' = 'green',
                 'non-significant' = "grey")

# set sat launch years
prisim_year <- 1980
landsat_year <- 1984
modis_year <- 2000

# helper functions ####
# set logger for other scripts
set_logger <- function(){
    script_name <- tryCatch(
        tools::file_path_sans_ext(basename(rstudioapi::getSourceEditorContext()$path)),
        error = function(e) 'script'
    )
    log_file_name <- paste0(script_name,
                            '_', format(Sys.time(), '%D_%H_%M_%S'), '.txt') %>%
        gsub('/','',.)
    log_file_path <- file.path('log', log_file_name)
    if(!dir.exists('log')) dir.create('log')
    file.create(log_file_path)

    log_appender(appender_file(log_file_path))
}


## CARPENTRY ####
# freq_check checks how many days exists per week are sampled and then how many good weeks per year exist
# freq_check needs a data frame of long format q_data with water_year, it returns a list of good years
frequency_check <- function(data_in){

    q_data <- data_in

    freq_check <- q_data %>%
    mutate(week_year = paste0(week(date), '_', water_year)) %>%
    group_by(site_code, week_year) %>%
    summarize(water_year = max(water_year),
              n = n()) %>%
    filter(n >= minimum_per_week_sampling_frequency_master) %>%
    group_by(site_code, water_year) %>%
    summarize(n = n()) %>%
    filter(n >= good_weeks_to_year_master)

    return(freq_check)
}


reduce_to_longest_site_runs <- function(data_in, metric){
    # initialize output
    out_frame <- tibble(site_code = as.character(),
                        water_year = as.integer(),
                        n = as.integer())

    # loop through sites
    for(i in unique(data_in$site_code)){
        target_site <- i

        site_data <- data_in %>%
            filter(site_code == target_site)

        interest <- site_data %>%
            filter(var == metric) %>%
            select(water_year, val) %>%
            drop_na(val)

        years <- sort(unique(interest$water_year))
        # https://stackoverflow.com/questions/26639110/find-longest-consecutive-number-in-r
        s <- split(years, cumsum(c(TRUE, diff(years) != 1)))
        out_years <- s[[which.max(lengths(s))]]

        out_data <- site_data %>%
            filter(water_year %in% out_years)

        out_frame <- rbind(out_frame, out_data) %>%
            distinct()
    }
    return(out_frame)
}

# reduces long data frame to years of data with at least 60% coverage of a given parameter
reduce_to_best_range <- function(data_in, metric, max_missing = 0.4) {

    output <- head(data_in, n =0)

    max_missing = max_missing
    target_metric = metric

for (i in unique(data_in$site_code)){
    target_site = i
    data <- data_in %>%
        filter(site_code == target_site,
               var == target_metric) %>%
        distinct() %>%
        na.omit()

    if(nrow(data) > 9){

    year_seq <- seq(min(data$water_year), max(data$water_year))
    df <- tibble(water_year = year_seq, is_present = year_seq %in% data$water_year)%>%
        mutate(group = cumsum(lag(is_present == FALSE, default = FALSE)))


    # Initialize variables to track the best range
    best_start <- NA
    best_end <- NA
    longest_length <- 0

    # Loop through all possible starting and ending points
    for (start_idx in seq_along(df$water_year)) {
        for (end_idx in start_idx:length(df$water_year)) {

            # Extract the subrange of years
            subrange <- df[start_idx:end_idx, ]

            # Calculate the percentage of missing years
            total_years <- nrow(subrange)
            missing_years <- sum(!subrange$is_present)
            missing_pct <- missing_years / total_years

            # If the missing percentage is below the threshold
            # and the range is longer, update
            if (missing_pct <= max_missing && total_years > longest_length) {
                longest_length <- total_years
                best_start <- subrange$water_year[1]
                best_end <- subrange$water_year[nrow(subrange)]
            } # end if
        } # end start index
    } # end end index
    inner <- data_in %>%
        filter(site_code == target_site,
               water_year %in% best_start:best_end)

    output <- rbind(output, inner)

}else{
inner <- head(data) %>%
    add_case(site_code = target_site)

output <- rbind(output, inner)
} # end 10 year data check
} # end for loop for sites
return(output)
}   # end function

# writing new generic function that reduces long data frame to min. 10 years
# of data with at least 60% coverage of a given parameter
# not filtering by number of observations per unit of time currently
# need to figure out how to notate confidence intervals on the back end
gen_reduce_to_best_range <- function(data_in, # core data set
                                     solute, # solute of interest
                                     pair_data = 'NO', # whether other data should be cut to solute of interest
                                     aggregation, # timestep of data aggregation
                                     max_missing = 0.4) { # maximum missingness allowed in a time series

    output <- head(data_in, n = 0) # sets up column names

    max_missing <- max_missing # sets maximum allowable missingness
    target_analyte <- solute # sets analyte of interest
    target_aggregation <- aggregation # sets aggregation of interest
    target_pairing <- pair_data # sets pairing rule

    for (i in unique(data_in$site_code)){ # flips through sites

        target_site <- i

        data <- data_in %>%
            filter(site_code == target_site, # selects for site of interest
                   analyte == target_analyte, # selects analyte of interest
                   timestep == target_aggregation) %>% # selects for timestep of interest
            distinct() %>% # removes duplicates
            na.omit() # drops rows containing NAs (e.g., all McMurdo sites)

        if(nrow(data) > 9){ # since trends need a minimum of 10 years

            year_seq <- seq(min(data$water_year), max(data$water_year)) # makes list of years
            df <- tibble(water_year = year_seq, is_present = year_seq %in% data$water_year)%>%
                mutate(group = cumsum(lag(is_present == FALSE, default = FALSE)))
            # makes column of all possible years, another column of whether it has data,
            # and a third column that creates a new column to denote missing years as a group

            # Initialize variables to track the best range
            best_start <- NA
            best_end <- NA
            longest_length <- 0

            # Loop through all possible starting and ending points
            for (start_idx in seq_along(df$water_year)) {
                for (end_idx in start_idx:length(df$water_year)) {

                    # Extract the subrange of years
                    subrange <- df[start_idx:end_idx, ]

                    # Calculate the percentage of missing years
                    total_years <- nrow(subrange)
                    missing_years <- sum(!subrange$is_present) # sums FALSEs
                    missing_pct <- missing_years / total_years

                    # If the missing percentage is below the threshold
                    # and the range is longer, update
                    # else it will move on to the next time series length
                    if (missing_pct <= max_missing && total_years > longest_length) {
                        longest_length <- total_years
                        best_start <- subrange$water_year[1]
                        best_end <- subrange$water_year[nrow(subrange)]
                    } # end if
                } # end start index
            } # end end index

            if(target_pairing == "NO"){ # This is if you only want one analyte's output.

            inner <- data %>% # Pre-filtered to analyte of interest
                filter(water_year %in% best_start:best_end) # and select only WY in best range.

            }else{ # This is if you want other analytes trimmed to the superior one.

            inner <- data_in %>% # Include all possible chem data,
                filter(site_code == target_site, # select site of interest,
                       timestep == target_aggregation, # select aggregation of interest,
                       water_year %in% best_start:best_end) # and select only WY in best range.

            }

            output <- rbind(output, inner) # bind this result to the original column names

        }else{ # if there is <= 9 years of data

            inner <- head(data) %>% # Pre-filtered to analyte of interest and
                add_case(site_code = target_site) # adds row of NAs.

            output <- rbind(output, inner) # keeps all records

            } # end 10 year data check

        } # end for loop for sites

    return(output)

}   # end function

## TRENDS #####

# detect trends does the actual sen's slope testing
# df_in needs to be a long dataframe with site,
# water_year, var, and val
# outputs a
detect_trends <- function(df_in ){#, diag_string){
    com_long <- df_in
    #  initialize output
    out_frame <- tibble(site_code = as.character(),
                        var = as.character(),
                        start = as.integer(),
                        end = as.integer(),
                        n = as.integer(),
                        trend = as.numeric(),
                        trend_upper = as.numeric(),
                        trend_lower = as.numeric(),
                        intercept = as.numeric(),
                        intercept_upper = as.numeric(),
                        intercept_lower = as.numeric(),
                        code = as.character())


    #foreach(i = unique(com_long$site_code)) %do%{
    for(i in unique(com_long$site_code)){
        target_site <- filter(com_long, site_code == i)

        dat_check <- na.omit(target_site)

        if(nrow(dat_check) > 1){
            #loop  through metrics
            #foreach(j = unique(target_site$var)) %do%{
            for(j in unique(target_site$var)){
                target_solute <- filter(target_site, var == j)  %>%
                    arrange(water_year) %>%
                    distinct() %>%
                    na.omit()

                if(nrow(target_solute) > 9 ){

                    start <- min(target_solute$water_year)
                    end <- max(target_solute$water_year)
                    n <- nrow(target_solute)


                    # OLD SENS SLOPE CALC
                    # slope_data <- target_solute %>%
                    #     select(val) %>%
                    #     as.ts()
                    #
                    # rownames(slope_data) <- target_solute$water_year
                    # test <- sens.slope(slope_data)
                    # trend <- test[[1]]
                    # p <- test[[3]]
                    #
                    #
                    # inner <- tibble(site_code = i,
                    #                 var = j,
                    #                 start = start,
                    #                 end = end,
                    #                 n = n,
                    #                 trend = trend,
                    #                 p = p,
                    #                 code = 'good')
                    #

                    # new sens slope calc
                    slope_data <- target_solute %>%
                        select(water_year, val) %>%
                        na.omit()

                    out<-zyp.sen(val~water_year, slope_data)
                    ints <- confint.zyp(out)


                    inner <- tibble(site_code = i,
                                    var = j,
                                    start = start,
                                    end = end,
                                    n = n,
                                    trend = out$coefficients[[2]],
                                    trend_upper = ints[2,2],
                                    trend_lower = ints[2,1],
                                    intercept = out$coefficients[[1]],
                                    intercept_upper = ints[1,2],
                                    intercept_lower = ints[1,1],
                                    code = 'good')



                    # bind out
                    out_frame <- rbind(out_frame, inner)

                    # currently commenting our diag plots to save on run time
                    # uncomment below to create a scatterplot for each good site/solute when run
                    #
                    # diag <- ggplot(target_solute, aes(x = water_year, y = val)) +
                    #     labs(title = paste0(i, ' ', j),
                    #          caption = paste0('n = ', n))+
                    #     geom_point()+
                    #     theme_few()
                    #
                    # quietly(ggsave(plot = diag, filename = here('data_working', 'diag_plots', diag_string, i, paste0(i,'_',j,'.png')),
                    #                create.dir = T, width = 7, height = 7))
                    # }else{
                    #
                    #     gap_starts <- paste0((target_solute$water_year[which(check_vec != 1)]), collapse = ',')
                    #
                    #     inner <- tibble(site_code = i,
                    #                       var = j,
                    #                       start = start,
                    #                       end = end,
                    #                       n = n,
                    #                       trend = NA,
                    #                       p = NA,
                    #                       code = paste0('gaps_at_', gap_starts))
                    #     # bind out
                    #     out_frame <- rbind(out_frame, inner)
                    #     }


                }else{inner <- tibble(site_code = i,
                                      var = j,
                                      start = NA,
                                      end = NA,
                                      n = NA,
                                      trend = NA,
                                      trend_upper = NA,
                                      trend_lower = NA,
                                      intercept = NA,
                                      intercept_upper = NA,
                                      intercept_lower = NA,
                                      code = 'under_10')

                # bind out
                out_frame <- rbind(out_frame, inner)
                } #solute level data avail check
            }# end solute loop
            }else{next} # site level data avail check
    } #end site loop
return(out_frame)
} #end function

# make function to add flags to trend data
# needs a p-value in column called 'p'
# needs slope in column called 'trend'
add_flags <- function(data_in){
    data_out <- data_in %>%
        filter(
            #p > 0.05, # sig trends only
            var != 'a_flow_sig',
            var != 'b_flow_sig') %>%
        mutate(flag = case_when(sign(trend_upper)!= sign(trend_lower) ~ 'non-significant',
                                sign(trend_upper) == sign(trend_lower) & sign(trend) > 0 ~ 'increasing',
                                sign(trend_upper) == sign(trend_lower) & sign(trend) < 0 ~ 'decreasing',
                                sign(trend_upper)!= sign(trend_lower) & trend == 0 ~ 'flat',
                                trend == NA ~ 'insufficient data')) %>%
        filter(!is.nan(flag),
               !is.na(flag))
    return(data_out)
}

