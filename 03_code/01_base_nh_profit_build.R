# Following code builds initial library set ----
library(tidyverse)
library(haven)

# Following code loads data into initial household id and profit base data frames ----

# Following code loads SEC0x data into an individual data frame (df) named:
# "base_nh_id_x".  
nh_id_1_wip <- read_dta("01_raw_data/glss4_new/sec0a.dta")

# Following code loads agg2 data into an individual df named "base_nh_profit".  
nh_profit_wip <- read_dta("01_raw_data/glss4_new/aggregates/agg2.dta")

# Following code extracts variables of interest and creates a key for future joins 
# in a  wip df's ----

# Following code extracts the region, district, ecological zone, location code, nh, 
# person id and clust from sec0a

# The unite code concatenated clust and nh into a single unique id 
# for joins.  Since there is a unique nh number for each clust, and each clust is unique
# the combination of nh and clust provide unique id's for each house hold. 
# This key reduces the number of column names needed in unite arguments. 

# WIP is work in progress and identifies partly processed data frames 
nh_id_1_wip <- select(nh_id_1_wip, factor("region"), factor("district"), "nh", "clust", 
                      factor("ez"), factor("loc2")) %>% unite(key, c("clust", "nh"), sep = "_")

# Following code extracts nh, clust, and corrected profit from base_nh_profit
nh_profit_wip <- select(nh_profit_wip, "nh", "clust", "agri1c") %>% 
  unite(key, c("clust", "nh"), sep = "_") %>% 
  rename(profit = "agri1c")

# Following code joins nh_id_1_wip and nh_profit_wip into a single nh_profile_wip 
# df.

nh_profile_base_wip <- nh_id_1_wip %>% inner_join(nh_profit_wip, by = "key")

nh_exp_profile_base_wip <- nh_id_1_wip %>% inner_join(nh_profit_wip, by = "key")







