library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)


#general agriculture income variables

profit <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/aggregates/agg2.dta') %>%
  unite(key, c("clust", "nh"), sep = "_") %>%
  select("key","agri1c")


# load data
# specific agriculture income variables
income10 <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/aggregates/inc10.dta')  # Revenue from sale of cash crops
income11 <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/aggregates/inc11.dta')  # Revenue from sale of roots/fruit/vegetables (at level of each individual crop)
income12 <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/aggregates/inc12.dta')  # Revenue from other agricultural income (at household level)
income13 <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/aggregates/inc13.dta')  # Revenue from transformed crops (at level of each individual product)

# specific agriculture expenditure variables
expenditure3 <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/aggregates/exp3.dta')  # Expenditure on renting farm land (at level of individual farm)
expenditure4 <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/aggregates/exp4.dta')  # Expenditure on crop inputs (at level of each individual input) 
expenditure5 <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/aggregates/exp5.dta')  # Expenditure on livestock inputs (at level of each individual input)
expenditure6 <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/aggregates/exp6.dta')  # Labor costs on food processing
expenditure7 <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/aggregates/exp7.dta')  # Consumption of home production

# Agriculture land, livestock and Fishing, Equipment data

agri_plot <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/sec8b.dta')                  #  Plot details
agri_land <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/sec8a1.dta')                 #  Agriculture Land
agri_livestock_Fishing <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/sec8a2.dta')    #  Livestock and Fishing
agri_equipment <- read_dta('E:/R_projects/week_7_assignment/glss4_new/glss4_new/sec8a3.dta')            #  Agriculture  equipment 


# data transforming & wrangling 


# income variables transforming and wrangling
income10$cropsv1 + income10$cropsv2 -> income10$crops    # Add revenue from cash crops (main outlet + other outlet) and create new variable

income10 <-  select(income10, "clust", "nh", "crops", "cropcd") %>%
  unite(key, c("clust", "nh"), sep = "_")  %>%                    # transfer categorical value
  mutate(crop_name = case_when(cropcd == 0  ~'Cocoa_c',
                               cropcd == 1  ~'Coffee_c',
                               cropcd == 2  ~'Rubber_c',
                               cropcd == 3  ~'Coconut_c',
                               cropcd == 4  ~'Oil_palm_c',
                               cropcd == 5  ~'Plantain_c',
                               cropcd == 6  ~'Banana_c',
                               cropcd == 8  ~'Wood_c',
                               cropcd == 10 ~'Cola_nut_c',
                               cropcd == 11 ~'Kenef_c',
                               cropcd == 12 ~'Cotton_c',
                               cropcd == 13 ~'Groundnut_peanut_c',
                               cropcd == 14 ~'Tobacco_c',
                               cropcd == 15 ~'Pineapple_c',
                               cropcd == 16 ~'Sugar_cane_c',
                               cropcd == 17 ~'Cassava_c',
                               cropcd == 18 ~'Yam_c',
                               cropcd == 19 ~'Potatoes_c',
                               cropcd == 21 ~'Maize_c',
                               cropcd == 22 ~'Rice_c',
                               cropcd == 23 ~'Guinea_corn_Sorghum_millet_Ropes_c',
                               cropcd == 24 ~'Tomatoes',
                               cropcd == 25 ~'Okro_c',
                               cropcd == 26 ~'Garden_egg_Egg_plant_c',
                               cropcd == 27 ~'Beans_peas',
                               cropcd == 28 ~'Pepper_c',
                               cropcd == 29 ~'Leafy_vegetables_c',
                               cropcd == 31 ~'Other_crops_c',
                               cropcd == 32 ~'Onion_c',
                               cropcd == 34 ~'Mango_c',
                               cropcd == 35 ~'Pawpaw_c')) %>%
  group_by(key, crop_name) %>%
  summarise(crops_all_years = sum(crops))     # eliminate duplicate value

income10_final <- income10 %>% 
  pivot_wider(id_cols = key, names_from = crop_name, values_from = crops_all_years, values_fill = 0)   # pivoted data



income11 <- income11 %>% unite(key, c("clust", "nh"), sep = "_") %>%      # transfer categorical value
  mutate( root_name = case_when(rootcd == 5 ~ "Oil_palm_r",
                                rootcd == 6 ~ "Plantain_r",
                                rootcd == 7 ~ "Banana_r",
                                rootcd == 8 ~ "Oranges_r",
                                rootcd == 9 ~ "Other_fruit_trees_r",
                                rootcd == 11 ~ "Cola_nut_r",
                                rootcd == 16 ~ "Pineapple_r",
                                rootcd == 18 ~ "Cassava_r",
                                rootcd == 19 ~ "Yam_r",
                                rootcd == 20 ~ "Cocoyam_r",
                                rootcd == 21 ~ "Potatoes_r",
                                rootcd == 25 ~ "Tomatoes_r",
                                rootcd == 26 ~ "Okro_r",
                                rootcd == 27 ~ "Garden_egg_Egg_plant_r",
                                rootcd == 29 ~ "Pepper_r",
                                rootcd == 30 ~ "Leafy_vegetables_r",
                                rootcd == 31 ~ "Other_vegetables_r",
                                rootcd == 33 ~ "Onion_r",
                                rootcd == 34 ~ "Avocado_pear_r",
                                rootcd == 35 ~ "Mango_r",
                                rootcd == 36 ~ "Pawpaw_r")) %>%
  group_by(key, root_name) %>%
  summarise(roots_all_years = sum(rootsv))              # eliminate duplicate value



income11_final <- income11 %>%
  pivot_wider(id_cols = key, names_from = root_name, values_from = roots_all_years, values_fill = 0)



income12_final <- income12 %>% unite(key, c("clust", "nh"), sep = "_")


income13 <- income13 %>% unite(key, c("clust", "nh"), sep = "_") %>%      # transfer categorical value
  mutate(trans_crops_name = case_when(proagrcd == 1  ~'Maize_flour_tc',
                                      proagrcd == 2  ~'Flour_from_other_grains_tc',
                                      proagrcd == 3  ~'Husked_polished_riceHusked_polished_rice_tc',
                                      proagrcd == 4  ~'Home_brewed_drink_tc',
                                      proagrcd == 5  ~'Cassava_flour_tc',
                                      proagrcd == 6  ~'Shelled_groundnut_tc',
                                      proagrcd == 7  ~'Processed_fish_tc',
                                      proagrcd == 8  ~'Gari_tc',
                                      proagrcd == 9 ~'Sheabutter_tc',
                                      proagrcd == 10 ~'Other_nuts_tc',
                                      proagrcd == 11 ~'Other_tc')) %>%
  group_by(key, trans_crops_name) %>%
  summarise(inctrcrp_all_years = sum(inctrcrp))   # eliminate duplicate value




income13_final <- income13 %>%
  pivot_wider(id_cols = key, names_from = trans_crops_name, values_from = inctrcrp_all_years, values_fill = 0)



# expenditure variables transforming and wrangling

expenditure3 <- expenditure3 %>% unite(key, c("clust", "nh"), sep = "_") %>%
  group_by(key, farmcd) %>%
  summarise(landexp_all_years = sum(landexp))

expenditure3_final <- expenditure3 %>%
  pivot_wider(id_cols = key, names_from = farmcd, values_from = landexp_all_years, values_fill = 0)


expenditure4 <- expenditure4 %>% unite(key, c("clust", "nh"), sep = "_")  %>%
  mutate(crops_exp_name  = case_when(crpexpcd == 1  ~'Fertilizer (Inorganic)',
                                      crpexpcd == 2  ~'Organic fertilizer',
                                      crpexpcd == 3  ~'Insecticides',
                                      crpexpcd == 4  ~'Herbicides',
                                      crpexpcd == 5  ~'Storage of crops',
                                      crpexpcd == 6  ~'Purchased seeds',
                                      crpexpcd == 7  ~'Irrigation',
                                      crpexpcd == 8  ~'Bags, containers, strings',
                                      crpexpcd == 9 ~'Petrol/diesel/oil',
                                      crpexpcd == 10 ~'Spare parts',
                                      crpexpcd == 11 ~'Hired labour',
                                      crpexpcd == 12 ~'Transport of crops',
                                      crpexpcd == 13 ~'Renting animals',
                                      crpexpcd == 14 ~'Renting equipment',
                                      crpexpcd == 15 ~'Hand tools local',
                                      crpexpcd == 16 ~'Hand tools imported',
                                      crpexpcd == 17 ~'Repairs/maintenance',
                                      crpexpcd == 18 ~'Other crop cost')) %>%
  group_by(key, crops_exp_name) %>%
  summarise(cropexp_all_years = sum(cropexp))    

expenditure4_final <- expenditure4 %>%
  pivot_wider(id_cols = key, names_from = crops_exp_name, values_from = cropexp_all_years, values_fill = 0)


expenditure5 <- expenditure5 %>% unite(key, c("clust", "nh"), sep = "_") %>%
  group_by(key, crpexpcd) %>%
  summarise(livexp_all_years = sum(livexp))

expenditure5_final <- expenditure5 %>%
  pivot_wider(id_cols = key, names_from = crpexpcd, values_from = livexp_all_years, values_fill = 0)


expenditure6$fdprexp1 + expenditure6$fdprexp2 -> expenditure6$fdpr

expenditure6 <- select(expenditure6, -fdprexp1, -fdprexp2) %>%
  unite(key, c("clust", "nh"), sep = "_") %>%
  mutate(labor_exp_name  = case_when(proagrcd == 1  ~'Maize flour',
                                     proagrcd == 2  ~'Flour from other grains',
                                     proagrcd == 3  ~'Husked/polished rice',
                                     proagrcd == 4  ~'Home brewed drink',
                                     proagrcd == 5  ~'Cassava flour',
                                     proagrcd == 6  ~'Shelled groundnut',
                                     proagrcd == 7  ~'Processed fish',
                                     proagrcd == 8  ~'Gari',
                                     proagrcd == 9  ~'Sheabutter',
                                     proagrcd == 10 ~'Other nuts',
                                     proagrcd == 11 ~'Other')) %>%
  group_by(key, labor_exp_name) %>%
  summarise(fdpr_all_years = sum(fdpr))

expenditure6_final <- expenditure6 %>%
  pivot_wider(id_cols = key, names_from = labor_exp_name, values_from = fdpr_all_years, values_fill = 0)


expenditure7<- select(expenditure7, -months) %>%
  unite(key, c("clust", "nh"), sep = "_") %>%
  group_by(key, homagrcd) %>%
  summarise(hp_all_years = sum(hp))

expenditure7_final <- expenditure7 %>%
  pivot_wider(id_cols = key, names_from = homagrcd, values_from = hp_all_years, values_fill = 0)



# create a data frame for income and profit
inc_prof_data_frame <- left_join(profit,income10_final, by = "key") %>%
  left_join(income11_final, by = "key") %>% 
  left_join(income12_final, by = "key") %>%
  left_join(income13_final, by = "key") 

inc_prof_data_frame[is.na(inc_prof_data_frame)] = 0


# Regression models profit vs. income
# profit vs. income 10
profit_vs_inc_cash_crops <- lm(agri1c ~ Cocoa_c + Coffee_c + Rubber_c + Coconut_c + Oil_palm_c + Plantain_c +
                                 Banana_c + Wood_c + Cola_nut_c + Kenef_c + Cotton_c + Groundnut_peanut_c +
                                 Tobacco_c + Pineapple_c + Sugar_cane_c + Cassava_c + Yam_c + Potatoes_c + Maize_c +
                                 Rice_c + Guinea_corn_Sorghum_millet_Ropes_c + Tomatoes + Okro_c + Garden_egg_Egg_plant_c +
                                 Beans_peas + Pepper_c + Leafy_vegetables_c + Other_crops_c +
                                 Onion_c + Mango_c, data = inc_prof_data_frame)
summary(profit_vs_inc_cash_crops)


# profit vs. income 11
profit_vs_inc_roots_fruit_vegetables <- lm(agri1c ~ Oil_palm_r + Plantain_r + Banana_r + Oranges_r + Other_fruit_trees_r + Cola_nut_r +
                                             Pineapple_r + Cassava_r + Yam_r + Cocoyam_r + Potatoes_r + Tomatoes_r + Okro_r +
                                             Garden_egg_Egg_plant_r + Pepper_r + Leafy_vegetables_r + Other_vegetables_r + Onion_r +
                                             Avocado_pear_r + Mango_r + Pawpaw_r, data = inc_prof_data_frame)
summary(profit_vs_inc_roots_fruit_vegetables)


# profit vs. income 12
profit_vs_inc_other_agri <- lm(agri1c ~ othaginc, data = inc_prof_data_frame)
summary(profit_vs_inc_roots_fruit_vegetables)


# profit vs. income 13
profit_vs_inc_trans_crop <- lm(agri1c ~ Maize_flour_tc + Flour_from_other_grains_tc + Husked_polished_riceHusked_polished_rice_tc +
                                 Home_brewed_drink_tc + Cassava_flour_tc + Shelled_groundnut_tc + Processed_fish_tc +
                                 Gari_tc + Sheabutter_tc + Other_nuts_tc + Other_tc, data = inc_prof_data_frame)
summary(profit_vs_inc_trans_crop)

  
