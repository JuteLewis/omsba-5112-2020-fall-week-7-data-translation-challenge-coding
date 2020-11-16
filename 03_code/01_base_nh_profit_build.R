# Following code builds initial library set ----
library(tidyverse)
library(haven)

# Following code build initial nhousehold id base data frames ----

# Following code loads SEC0x data int individual data frames "base_nh_id_x" data frame.  
base_nh_id_1 <- read_dta("01_raw_data/glss4_new/sec0a.dta")

# DO NOT USE until profession response to slack question - sec0c does not have regions  
base_nh_id_2 <- read_dta("01_raw_data/glss4_new/sec0b.dta")
base_nh_id_3 <- read_dta("01_raw_data/glss4_new/sec0c.dta")

# Following code loads agg2  data int individual data frames "base_nh_profit" data frame.  
base_nh_profit <- read_dta("01_raw_data/glss4_new/aggregates/agg2.dta")

# Following code extracts variables and creates keys for joins 
# into wip dataframes that will be joined later ----

# Following code extracts the region, district, eanum, nh, and clust from sec0a ----
# from the base_nh_id data frames.  
# The unite code concatenating clust and nh to create a unique nh key for joins ---- 
# Since there is a unique nh number for each clust.  The combination of nh and clust 
# provide unique id's for each house hold. 
# WIP is work in progress and identifies partly processed data frames 
base_nh_id_1_wip <- select(base_nh_id_1, "region", "district", "eanum", "nh", "clust") %>% 
  unite(key, c("clust", "nh"), sep = "_")

# DO NOT USE until profession response to slack question  - sec0c does not have regions 
base_nh_id_2_wip <- select(base_nh_id_2, "region", "district", "eanum", "nh", "clust") %>% 
  unite(key, c("clust", "nh"), sep = "_")

base_nh_id_3_wip <- select(base_nh_id_3, "nh", "clust")

# Following code extracts nh, clust, and corrected profit from base_nh_profit
profit_nh_wip <- select(base_nh_profit, "nh", "clust", "agri1c") %>% 
  unite(key, c("clust", "nh"), sep = "_")

# Following code joins base_nh_id_1_wip and profit_nh_wip into a base nh_profit 
# dataframe

profit_nh_base <- base_nh_id_1_wip %>% inner_join(profit_nh_wip, by = "key")






