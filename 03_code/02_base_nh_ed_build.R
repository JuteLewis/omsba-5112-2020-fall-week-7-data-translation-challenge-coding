# Following code loads SEC2a into a base general education data frame ----
base_nh_ed_wip <- read_dta("01_raw_data/glss4_new/sec2a.dta")

# Following code selected the desired education variables then removes NA the ----
# unites nh and clust into a key for joins. 
# Since there is a unique nh number for each clust.  The combination of nh and clust 
# provide unique id's for each house hold. 
base_nh_ed_wip <- select(base_nh_ed_wip, "nh", "clust", "s2aq2") %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_")

# Following code groups the dataframe by key that summaries the mean of the 
# education per household 
# Since there are multiple members per household by group_by the key, we can now 
# average education per household 
base_nh_ed_wip <- base_nh_ed_wip %>% group_by(key) %>% 
  summarise(av_yrs_ed = round(mean(s2aq2), 1)) 

ed_nh_base <- profit_nh_base %>% left_join(base_nh_ed_wip)
            

