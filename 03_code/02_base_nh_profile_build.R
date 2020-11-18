
# Data Loading Code ----
# Following code loads SEC1 into a base general family roster data frame
nh_roster_wip <- read_dta("01_raw_data/glss4_new/sec1.dta")

# Following code loads SEC2a into a base general education data frame
nh_ed_wip <- read_dta("01_raw_data/glss4_new/sec2a.dta")

# Following code loads SEC2C into a base general literate data frame
nh_lit_wip <- read_dta("01_raw_data/glss4_new/sec2c.dta")

# Following code loads SEC8C into a base general literate data frame 
nh_field_help_wip <- read_dta("01_raw_data/glss4_new/sec8c1.dta")



# Data cleansing and variable aggregation code ----
# Following code selected the desired variables removes NA then unites nh and clust 
# into a key for joins. Since there is a unique nh number for each clust.  
# The combination of nh and clust provide unique id's for each house hold.

# The code groups the dataframe by key then summaries the mean of the 
# family age, total male and female members of house. 
nh_roster_age_wip <- select(nh_roster_wip, "nh", "clust", "agey") %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") %>% 
  group_by(key)%>% 
  summarise(agey = round(mean(agey), 1)) %>% 
  rename(av_hh_age = "agey")

# Following code converts sex to a factor to counting
nh_roster_sex_wip <- select(nh_roster_wip, "nh", "clust", factor("sex")) %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") %>% 
  group_by(key) %>% 
  count(sex) %>% 
  pivot_wider(names_from = sex, values_from = n) %>% 
  rename(male = "1", female = "2")

nh_ed_wip <- select(nh_ed_wip, "nh", "clust", factor("s2aq2")) %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") %>% 
  group_by(key)

nh_lit_wip <- select(nh_lit_wip, "nh", "clust", "s2cq1", "s2cq2", 
  "s2cq3", "s2cq4", "s2cq5") %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") %>% 
  group_by(key)

nh_field_help_wip <- select(nh_field_help_wip,"nh", "clust","s8cq17a", 
  "s8cq17b") %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") %>% 
  group_by(key) %>% 
  summarise(sum(s8cq17a), sum(s8cq17b)) %>% 
  rename(male_help = "sum(s8cq17a)", female_help = "sum(s8cq17b)")
  

# Following code joins education wip and literacy wip and joins education profile ----
# with nh_profile_base.  
nh_education_profile_base <- nh_ed_wip %>% left_join(nh_lit_wip)
nh_profile_base_wip <- nh_profile_base_wip %>%  left_join(nh_education_profile_base)

# Following code joins age, sex, field_help, and profit dataframes with nh_profile_base ----
nh_profile_base_wip <- nh_profile_base_wip %>%  left_join(nh_ed_wip)

nh_profile_base_wip <- nh_profile_base_wip %>% left_join(nh_roster_age_wip)
nh_profile_base_wip <- nh_profile_base_wip %>% left_join(nh_roster_sex_wip)
nh_profile_base_wip <- nh_profile_base_wip %>% left_join(nh_field_help_wip)

# Adding a mutate column to add total family size to profile.
nh_profile_base_wip <- mutate(nh_profile_base_wip, family_size = male + female)

# Converting NA to 0 for calculating family size
nh_profile_base_wip[is.na(nh_profile_base_wip)] = 0

nh_profile_base <- nh_profile_base_wip[c("key", "region", "district", "male", 
                    "female", "family_size", "av_hh_age", "s2aq2", "male_help",
                    "female_help", "profit")]


test_df <- filter(nh_profile_base, district == c(3, 4, 6, 10, 11, 12, 13, 14))




            

