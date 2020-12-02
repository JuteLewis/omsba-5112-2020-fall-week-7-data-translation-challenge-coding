# Data Loading Code ----
# Following code loads subagg 22 into a rent wip df
nh_rent_wip <- read_dta("01_raw_data/glss4_new/aggregates/subagg22.dta")

# Following code loads subagg 23 into a crop input wip df
nh_crop_wip <- read_dta("01_raw_data/glss4_new/aggregates/subagg23.dta")

# Following code loads subagg 26 into a consumption of produce into a wip df
nh_consume_wip <- read_dta("01_raw_data/glss4_new/aggregates/subagg26.dta")

# Following code loads subagg 29 into a food exspense into a wip df
nh_food_wip <- read_dta("01_raw_data/glss4_new/aggregates/subagg29.dta")

# The following series of code joins clust with nh id into a key for joining
nh_rent_wip <- nh_rent_wip %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") 

nh_crop_wip <- nh_crop_wip %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") 

nh_consume_wip <- nh_consume_wip %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") 

nh_food_wip <- nh_food_wip %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") 

# Following code joins expense dataframes with nh_exs_profile_wip  
nh_exp_profile_base_wip <- nh_exp_profile_base_wip %>%  left_join(nh_rent_wip)
nh_exp_profile_base_wip <- nh_exp_profile_base_wip %>% left_join(nh_crop_wip)
nh_exp_profile_base_wip <- nh_exp_profile_base_wip %>% left_join(nh_consume_wip)
nh_exp_profile_base <- nh_exp_profile_base_wip %>% left_join(nh_food_wip)


