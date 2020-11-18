# Following code builds initial library set ----
library(tidyverse)
library(haven)

# Following code build initial household id base data frames ----

# Following code loads SEC0x data int individual data frames "base_nh_id_x" data frame.  
nh_id_1_wip <- read_dta("01_raw_data/glss4_new/sec0a.dta")

# Following code loads agg2  data int individual data frames "base_nh_profit" data frame.  
nh_profit_wip <- read_dta("01_raw_data/glss4_new/aggregates/agg2.dta")

# Following code extracts variables and creates keys for joins 
# into wip dataframes that will be joined later ----

# Following code extracts the region, district, eanum, nh, and clust from sec0a ----
# from the base_nh_id data frames.  
# The unite code concatenating clust and nh to create a unique nh key for joins ---- 
# Since there is a unique nh number for each clust.  The combination of nh and clust 
# provide unique id's for each house hold. 
# WIP is work in progress and identifies partly processed data frames 
nh_id_1_wip <- select(nh_id_1_wip, factor("region"), factor("district"), "nh", "clust") %>% 
  unite(key, c("clust", "nh"), sep = "_")

# Following code extracts nh, clust, and corrected profit from base_nh_profit
nh_profit_wip <- select(nh_profit_wip, "nh", "clust", "agri1c") %>% 
  unite(key, c("clust", "nh"), sep = "_") %>% 
  rename(profit = "agri1c")

# Following code joins base_nh_id_1_wip and profit_nh_wip into a base nh_profit 
# dataframe

nh_profile_base_wip <- nh_id_1_wip %>% inner_join(nh_profit_wip, by = "key")

nh_exp_profile_base_wip <- nh_id_1_wip %>% inner_join(nh_profit_wip, by = "key")







