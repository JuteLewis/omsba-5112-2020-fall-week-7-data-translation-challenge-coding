# Following code loads SUBAGG22 into a base general land rental cost data frame ----
base_nh_rent_wip <- read_dta("01_raw_data/glss4_new/aggregates/subagg22.dta")

# Unites nh and clust into a key for joins ---- 
# Since there is a unique nh number for each clust.  The combination of nh and clust 
# provide unique id's for each house hold. 
base_nh_rent_wip <- na.omit(base_nh_rent_wip) %>% 
  unite(key, c("clust", "nh"), sep = "_")

# # Following code loads SUBAGG23 into a base general land rental cost data frame ----
base_nh_excrop_wip <- read_dta("01_raw_data/glss4_new/aggregates/subagg23.dta")

# Unites nh and clust into a key for joins ---- 
# Since there is a unique nh number for each clust.  The combination of nh and clust 
# provide unique id's for each house hold. 
base_nh_excrop_wip <- na.omit(base_nh_excrop_wip) %>% 
  unite(key, c("clust", "nh"), sep = "_")

#Following code loads SUBAGG26 into a base general land rental cost data frame ----
base_nh_homeprod_wip <- read_dta("01_raw_data/glss4_new/aggregates/subagg26.dta")

# Unites nh and clust into a key for joins ---- 
# Since there is a unique nh number for each clust.  The combination of nh and clust 
# provide unique id's for each house hold. 
base_nh_homeprod_wip <- na.omit(base_nh_homeprod_wip) %>% 
  unite(key, c("clust", "nh"), sep = "_")


#Following code loads SUBAGG29 into a base general land rental cost data frame ----
base_nh_food_wip <- read_dta("01_raw_data/glss4_new/aggregates/subagg29.dta")

# Unites nh and clust into a key for joins ---- 
# Since there is a unique nh number for each clust.  The combination of nh and clust 
# provide unique id's for each house hold. 
base_nh_food_wip <- na.omit(base_nh_food_wip) %>% 
  unite(key, c("clust", "nh"), sep = "_")

# Following code consolidates the expense wips created above with ----
#profit_education base. 
nh_profit_ed_rent_base <- nh_profit_ed_base %>% left_join(base_nh_rent_wip)
nh_profit_ed_rent_homeprod_base <- nh_profit_ed_rent_base %>% left_join(base_nh_homeprod_wip)
nh_profit_exp_base <- nh_profit_ed_rent_homeprod_base %>% left_join(base_nh_food_wip)


