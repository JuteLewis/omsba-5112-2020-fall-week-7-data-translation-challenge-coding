
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
  summarise(agey = round(mean(agey), 1))

# Following code converts sex to a factor to counting
nh_roster_sex_wip <- select(nh_roster_wip, "nh", "clust", factor("sex")) %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") %>% 
  group_by(key) %>% 
  count(sex) %>% 
  pivot_wider(names_from = sex, values_from = n) %>% 
  rename(Male = "1", Female = "2")

nh_ed_wip <- select(nh_ed_wip, "nh", "clust", "s2aq2") %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") %>% 
  group_by(key) %>% 
  summarise(av_yrs_ed = round(mean(s2aq2), 1))

nh_lit_wip <- select(nh_lit_wip, "nh", "clust", factor("s2cq1"), factor("s2cq2"), 
  factor("s2cq3"), factor("s2cq4"), factor("s2cq5")) %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") %>% 
  group_by(key)

nh_field_help_wip <- select(nh_field_help_wip,"nh", "clust","s8cq17a", 
  "s8cq17b") %>% 
  na.omit() %>% 
  unite(key, c("clust", "nh"), sep = "_") %>% 
  group_by(key) %>% 
  summarise(sum(s8cq17a), sum(s8cq17b)) %>% 
  rename(Male_Help = "sum(s8cq17a)", Female_help = "sum(s8cq17b)")
  


# Following code joins ed, field_help, sex wip dataframes with nh_profit_base ----
# Following code does not intergate lit due to the different dataframe structure
profit_age_wip <- profit_nh_base %>% left_join(nh_roster_age_wip)
profit_age_sex_wip <- profit_ed_age_wip %>% left_join(nh_roster_sex_wip)
profit_age_sex_ed_wip <- profit_ed_age_sex_wip %>% left_join(nh_ed_wip)

nh_propfile_base <- profit_ed_age_sex_wip %>% left_join(nh_field_help_wip)



            

