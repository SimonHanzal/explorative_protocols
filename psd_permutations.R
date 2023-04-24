library(tidyverse)
library(devtools)

# Imports and wrangling
curve_young <- read_csv("data/curve_young.csv", col_names=FALSE) %>%
    tibble::rownames_to_column() %>%  
    pivot_longer(-rowname) %>% 
    pivot_wider(names_from=rowname, values_from=value) %>%
    select(-1) %>%
    mutate(time = seq(-2000,2000,20)) %>%
    mutate(age_group = "young") %>%
    rename(signal = `1`) %>%
    filter(signal != "NaN")
curve_older <- read_csv("data/curve_older.csv", col_names=FALSE) %>%
    tibble::rownames_to_column() %>%  
    pivot_longer(-rowname) %>% 
    pivot_wider(names_from=rowname, values_from=value) %>%
    select(-1) %>%
    mutate(time = seq(-2000,2000,20)) %>%
    mutate(age_group = "older") %>%
    rename(signal = `1`) %>%
    filter(signal != "NaN")

curves <- curve_young %>%
    full_join(curve_older)

# Get package
install_github("dalejbarr/clusterperm")

library(clusterperm)

set.seed(2023) # for reproducibility

nmc <- 1000L # number of monte carlo runs

mod_form <- curves$age_group ~ curves$signal


## calculate statistics on the original data
dat2 <- aov_by_bin(curves, time, mod_form)
orig <- detect_clusters_by_effect(dat2, effect, bin, stat, p)
