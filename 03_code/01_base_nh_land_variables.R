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
  mutate(farm_exp_code = case_when(farmcd == 1  ~ 'farm_code_1',
                                   farmcd == 2  ~ 'farm_code_2',
                                   farmcd == 3  ~ 'farm_code_3',
                                   farmcd == 4  ~ 'farm_code_4',
                                   farmcd == 5  ~ 'farm_code_5',
                                   farmcd == 6  ~ 'farm_code_6',
                                   farmcd == 7  ~ 'farm_code_7',
                                   farmcd == 8  ~ 'farm_code_8',
                                   farmcd == 9  ~ 'farm_code_9',
                                   farmcd == 10 ~ 'farm_code_10',
                                   farmcd == 11 ~ 'farm_code_11',
                                   farmcd == 12 ~ 'farm_code_12',
                                   farmcd == 13 ~ 'farm_code_13',)) %>%
  group_by(key, farm_exp_code) %>%
  summarise(landexp_all_years = sum(landexp))

expenditure3_final <- expenditure3 %>%
  pivot_wider(id_cols = key, names_from = farm_exp_code, values_from = landexp_all_years, values_fill = 0)


expenditure4 <- expenditure4 %>% unite(key, c("clust", "nh"), sep = "_")  %>%
  mutate(crops_exp_name  = case_when(crpexpcd == 1   ~'Fertilizer_Inorganic',
                                      crpexpcd == 2  ~'Organic_fertilizer',
                                      crpexpcd == 3  ~'Insecticides',
                                      crpexpcd == 4  ~'Herbicides',
                                      crpexpcd == 5  ~'Storage_of_crops',
                                      crpexpcd == 6  ~'Purchased_seeds',
                                      crpexpcd == 7  ~'Irrigation',
                                      crpexpcd == 8  ~'Bags_containers_strings',
                                      crpexpcd == 9  ~'Petrol_diesel_oil',
                                      crpexpcd == 10 ~'Spare_parts',
                                      crpexpcd == 11 ~'Hired_labour',
                                      crpexpcd == 12 ~'Transport_of_crops',
                                      crpexpcd == 13 ~'Renting_animals',
                                      crpexpcd == 14 ~'Renting_equipment',
                                      crpexpcd == 15 ~'Hand_tools_local',
                                      crpexpcd == 16 ~'Hand_tools_imported',
                                      crpexpcd == 17 ~'Repairs_maintenance',
                                      crpexpcd == 18 ~'Other_crop_cost')) %>%
  group_by(key, crops_exp_name) %>%
  summarise(cropexp_all_years = sum(cropexp))    

expenditure4_final <- expenditure4 %>%
  pivot_wider(id_cols = key, names_from = crops_exp_name, values_from = cropexp_all_years, values_fill = 0)


expenditure5 <- expenditure5 %>% unite(key, c("clust", "nh"), sep = "_") %>%
  mutate(livsto_exp_name  = case_when(crpexpcd == 51 ~'livstc_51',
                                      crpexpcd == 52 ~'livstc_52',
                                      crpexpcd == 53 ~'livstc_53',
                                      crpexpcd == 54 ~'livstc_54',
                                      crpexpcd == 55 ~'livstc_55',
                                      crpexpcd == 56 ~'livstc_56',
                                      crpexpcd == 57 ~'livstc_57',
                                      crpexpcd == 58 ~'livstc_58',
                                      crpexpcd == 59 ~'livstc_59',
                                      crpexpcd == 61 ~'livstc_61',
                                      crpexpcd == 62 ~'livstc_62',
                                      crpexpcd == 63 ~'livstc_63',
                                      crpexpcd == 64 ~'livstc_64',
                                      crpexpcd == 65 ~'livstc_65')) %>%
  group_by(key, livsto_exp_name) %>%
  summarise(livexp_all_years = sum(livexp))

expenditure5_final <- expenditure5 %>%
  pivot_wider(id_cols = key, names_from = livsto_exp_name, values_from = livexp_all_years, values_fill = 0)


expenditure6$fdprexp1 + expenditure6$fdprexp2 -> expenditure6$fdpr

expenditure6 <- select(expenditure6, -fdprexp1, -fdprexp2) %>%
  unite(key, c("clust", "nh"), sep = "_") %>%
  mutate(labor_exp_name  = case_when(proagrcd == 1  ~'Maize_flour',
                                     proagrcd == 2  ~'Flour_from_other_grains',
                                     proagrcd == 3  ~'Husked_polished_rice',
                                     proagrcd == 4  ~'Home_brewed_drink',
                                     proagrcd == 5  ~'Cassava_flour',
                                     proagrcd == 6  ~'Shelled_groundnut',
                                     proagrcd == 7  ~'Processed_fish',
                                     proagrcd == 8  ~'Gari',
                                     proagrcd == 9  ~'Sheabutter',
                                     proagrcd == 10 ~'Other_nuts',
                                     proagrcd == 11 ~'Other')) %>%
  group_by(key, labor_exp_name) %>%
  summarise(fdpr_all_years = sum(fdpr))

expenditure6_final <- expenditure6 %>%
  pivot_wider(id_cols = key, names_from = labor_exp_name, values_from = fdpr_all_years, values_fill = 0)


expenditure7<- select(expenditure7, -months) %>%
  unite(key, c("clust", "nh"), sep = "_") %>%
  mutate(consumptionr_exp_name  = case_when(homagrcd == 1  ~'consumption_home_1',
                                            homagrcd == 2  ~'consumption_home_2',
                                            homagrcd == 3  ~'consumption_home_3',
                                            homagrcd == 4  ~'consumption_home_4',
                                            homagrcd == 5  ~'consumption_home_5',
                                            homagrcd == 6  ~'consumption_home_6',
                                            homagrcd == 7  ~'consumption_home_7',
                                            homagrcd == 8  ~'consumption_home_8',
                                            homagrcd == 9  ~'consumption_home_9',
                                            homagrcd == 10 ~'consumption_home_10',
                                            homagrcd == 11 ~'consumption_home_11',
                                            homagrcd == 12 ~'consumption_home_12',
                                            homagrcd == 13 ~'consumption_home_13',
                                            homagrcd == 14 ~'consumption_home_14',
                                            homagrcd == 15 ~'consumption_home_15',
                                            homagrcd == 16 ~'consumption_home_16',
                                            homagrcd == 17 ~'consumption_home_17',
                                            homagrcd == 20 ~'consumption_home_20',
                                            homagrcd == 21 ~'consumption_home_21',
                                            homagrcd == 22 ~'consumption_home_22',
                                            homagrcd == 23 ~'consumption_home_23',
                                            homagrcd == 24 ~'consumption_home_24',
                                            homagrcd == 25 ~'consumption_home_25',
                                            homagrcd == 26 ~'consumption_home_26',
                                            homagrcd == 27 ~'consumption_home_27',
                                            homagrcd == 28 ~'consumption_home_28',
                                            homagrcd == 29 ~'consumption_home_29',
                                            homagrcd == 30 ~'consumption_home_30',
                                            homagrcd == 31 ~'consumption_home_31',
                                            homagrcd == 31 ~'consumption_home_31',
                                            homagrcd == 32 ~'consumption_home_32',
                                            homagrcd == 33 ~'consumption_home_33',
                                            homagrcd == 34 ~'consumption_home_34',
                                            homagrcd == 35 ~'consumption_home_35',
                                            homagrcd == 36 ~'consumption_home_36',
                                            homagrcd == 37 ~'consumption_home_37',
                                            homagrcd == 40 ~'consumption_home_40',
                                            homagrcd == 41 ~'consumption_home_41',
                                            homagrcd == 42 ~'consumption_home_42',
                                            homagrcd == 43 ~'consumption_home_43',
                                            homagrcd == 44 ~'consumption_home_44',
                                            homagrcd == 45 ~'consumption_home_45',
                                            homagrcd == 46 ~'consumption_home_46',
                                            homagrcd == 47 ~'consumption_home_47',
                                            homagrcd == 48 ~'consumption_home_48',
                                            homagrcd == 60 ~'consumption_home_60',
                                            homagrcd == 61 ~'consumption_home_61',
                                            homagrcd == 62 ~'consumption_home_62',
                                            homagrcd == 63 ~'consumption_home_63',
                                            homagrcd == 64 ~'consumption_home_64',
                                            homagrcd == 65 ~'consumption_home_65',
                                            homagrcd == 66 ~'consumption_home_66',
                                            homagrcd == 67 ~'consumption_home_67',
                                            homagrcd == 68 ~'consumption_home_68',
                                            homagrcd == 69 ~'consumption_home_69',
                                            homagrcd == 70 ~'consumption_home_70',
                                            homagrcd == 71 ~'consumption_home_71',
                                            homagrcd == 90 ~'consumption_home_90',
                                            homagrcd == 91 ~'consumption_home_91')) %>%
  group_by(key, consumptionr_exp_name) %>%
  summarise(hp_all_years = sum(hp))

expenditure7_final <- expenditure7 %>%
  pivot_wider(id_cols = key, names_from = consumptionr_exp_name, values_from = hp_all_years, values_fill = 0)


# Aggregate(Agriculture land, livestock and Fishing, Equipment) variables transforming and wrangling

#plot details

agri_plot <- select(agri_plot, clust, nh, s8bq4a, s8bq4b, s8bq5) %>%
  unite(key, c("clust", "nh"), sep = "_") %>%
  mutate(farm_unit_measure = case_when(s8bq4b == 1 ~ 'Acres',
                                       s8bq4b == 2 ~ 'Poles',
                                       s8bq4b == 3 ~ 'Ropes',
                                       s8bq4b == 4 ~ 'Others')) %>%
  group_by(key, farm_unit_measure)%>%
  summarise(space = sum(s8bq4a))
  
agri_plot <- agri_plot %>%
  pivot_wider(id_cols = key, names_from = farm_unit_measure, values_from = space, values_fill = 0) 
  
agri_plot_final <-mutate(agri_plot,Poles_to_Acres = Poles/210) %>% mutate(Ropes_to_Acres = Ropes/9) %>%
  select(key, Acres, Poles_to_Acres, Ropes_to_Acres) %>% mutate(space_in_acres = Acres + Poles_to_Acres + Ropes_to_Acres) %>%
  select(key, space_in_acres)


#land details
agri_land_final <- select(agri_land, nh, clust, s8aq1) %>% unite(key, c("clust", "nh"), sep = "_")


#live stock details
agri_livestock_Fishing <- select(agri_livestock_Fishing, livstcd, nh, clust, s8aq22a) %>% 
  unite(key, c("clust", "nh"), sep = "_") %>%   mutate(live_stock_name = case_when(livstcd == 1 ~  'Draught_animals',
                                                                                   livstcd == 2 ~  'Cattle_including_cows',
                                                                                   livstcd == 3 ~  'Sheep',
                                                                                   livstcd == 5 ~  'Goats',
                                                                                   livstcd == 6 ~  'Pigs',
                                                                                   livstcd == 7 ~  'Rabbits',
                                                                                   livstcd == 8 ~  'Chicken',
                                                                                   livstcd == 9 ~  'Other_poultry',
                                                                                   livstcd == 10 ~ 'Other_livestock',
                                                                                   livstcd == 11 ~ 'Fish',
                                                                                   livstcd == 12 ~ 'Crab',
                                                                                   livstcd == 12 ~ 'Other')) 

agri_livestock_Fishing_final <- agri_livestock_Fishing %>%
  pivot_wider(id_cols = key, names_from = live_stock_name, values_from = s8aq22a, values_fill = 0)

# equipment details
agri_equipment <- select(agri_equipment, nh, clust, eqcdown, s8aq34) %>% unite(key, c("clust", "nh"), sep = "_") %>%
  mutate(equipment_name = case_when(eqcdown == 21 ~ 'Tractor',
                                     eqcdown == 22 ~ 'Plough',
                                     eqcdown == 31 ~ 'Trailer/Cart',
                                     eqcdown == 41 ~ 'Other animal drawn equipment',
                                     eqcdown == 42 ~ 'Other tractor drawn equipment',
                                     eqcdown == 51 ~ 'Sprayer',
                                     eqcdown == 61 ~ 'Canoe',
                                     eqcdown == 62 ~ 'Other_poultry',
                                     eqcdown == 63 ~ 'Net',
                                     eqcdown == 64 ~ 'Safety equip',
                                     eqcdown == 65 ~ 'Other')) %>%
  group_by(key, equipment_name)%>%
  summarise(number_of_equipment = sum(s8aq34))

agri_equipment_final <- agri_equipment %>%
  pivot_wider(id_cols = key, names_from = equipment_name, values_from = number_of_equipment, values_fill = 0)




# create a data frame for income and profit
inc_prof_data_frame <- left_join(profit,income10_final, by = "key") %>%
  left_join(income11_final, by = "key") %>% 
  left_join(income12_final, by = "key") %>%
  left_join(income13_final, by = "key") %>%
  left_join(expenditure3_final, by ="key")  %>%
  left_join(expenditure4_final, by = "key") %>%
  left_join(expenditure5_final, by = "key") %>%
  left_join(expenditure6_final, by = "key") %>%
  left_join(expenditure7_final, by = "key") %>%
  left_join(agri_plot_final, by = "key")       %>%
  left_join(agri_land_final, by = "key")       %>%
  left_join(agri_equipment_final, by = "key")  %>%
  left_join(agri_livestock_Fishing_final, by = "key")

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

  
# Regression models profit vs. expenditure
# profit vs expenditure 3
profit_vs_exp_renting_farm <- lm(agri1c ~ farm_code_1 + farm_code_2 + farm_code_3 + farm_code_4 + farm_code_5 + farm_code_6 + farm_code_7 +
                                   farm_code_8 + farm_code_9 + farm_code_10 + farm_code_11 + farm_code_12 + farm_code_13,
                                 data = inc_prof_data_frame)
summary(profit_vs_exp_renting_farm)

# profit vs expenditure 4
profit_vs_exp_crops_input <- lm(agri1c ~ Fertilizer_Inorganic + Organic_fertilizer + Insecticides + Herbicides + Storage_of_crops +
                                  Purchased_seeds + Irrigation + Bags_containers_strings + Petrol_diesel_oil + Spare_parts + Hired_labour +
                                  Transport_of_crops + Renting_animals + Renting_equipment + Hand_tools_local + Hand_tools_imported +
                                  Repairs_maintenance + Other_crop_cost, data = inc_prof_data_frame)
summary(profit_vs_exp_crops_input)

# profit vs expenditure 5
profit_vs_exp_livsto_input <- lm(agri1c ~ livstc_51 + livstc_52 + livstc_53 + livstc_54 + livstc_55 + livstc_56 + livstc_57 +
                                 livstc_58 + livstc_59 + livstc_61 + livstc_62 + livstc_63 + livstc_64 +
                                 livstc_65, data = inc_prof_data_frame)
summary(profit_vs_exp_livsto_input)

# profit vs expenditure 6
profit_vs_exp_fooproc_input <- lm(agri1c ~ Maize_flour + Flour_from_other_grains + Husked_polished_rice + Home_brewed_drink +
                                  Cassava_flour + Shelled_groundnut + Processed_fish + Gari + Sheabutter + Other_nuts +
                                  Other, data = inc_prof_data_frame)
summary(profit_vs_exp_fooproc_input)

# profit vs expenditure 7
profit_vs_exp_homcons_input <- lm(agri1c ~ consumption_home_1 + consumption_home_2 + consumption_home_3 + consumption_home_4 +
                                  consumption_home_5  + consumption_home_6  + consumption_home_7  + consumption_home_8  + consumption_home_9 +
                                  consumption_home_10 + consumption_home_11 + consumption_home_12 + consumption_home_13 + consumption_home_14 +
                                  consumption_home_15 + consumption_home_16 + consumption_home_17 + consumption_home_20 + consumption_home_21 +
                                  consumption_home_22 + consumption_home_23 + consumption_home_24 + consumption_home_25 + consumption_home_26 +
                                  consumption_home_27 + consumption_home_28 + consumption_home_29 + consumption_home_30 + consumption_home_31 +
                                  consumption_home_31 + consumption_home_32 + consumption_home_33 + consumption_home_34 + consumption_home_35 +
                                  consumption_home_36 + consumption_home_37 + consumption_home_40 + consumption_home_41 + consumption_home_42 +
                                  consumption_home_43 + consumption_home_44 + consumption_home_45 + consumption_home_46 + consumption_home_47 +
                                  consumption_home_48 + consumption_home_60 + consumption_home_61 + consumption_home_62 + consumption_home_63 +
                                  consumption_home_64 + consumption_home_65 + consumption_home_66 + consumption_home_67 + consumption_home_68 + 
                                  consumption_home_69 + consumption_home_70 + consumption_home_71 + consumption_home_90 + consumption_home_91, data = inc_prof_data_frame)

summary(profit_vs_exp_homcons_input)

# Regression models profit vs. aggregates
# profit vs land plot

profit_vs_land_plot <- lm(agri1c ~ space_in_acres, data = inc_prof_data_frame)
summary(profit_vs_land_plot)

