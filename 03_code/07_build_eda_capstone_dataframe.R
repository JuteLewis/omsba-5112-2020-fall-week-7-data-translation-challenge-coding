# Following code builds initial library set ----
library(tidyverse)
library(haven)

# Following code joins the household profile dataframe with the land assessment 
# dataframe into a capstone dataframe for analysis.

capstone_df <- nh_profile_base %>% left_join(inc_prof_data_frame, by = 'key') %>% 
              select(- "agri1c")
summary(capstone_df)

# Initial Descriptive Stat - Correlation of independent variables
cor_df <- select(capstone_df, 'profit', 'Flour_from_other_grains', 'Husked_polished_rice',
                 'Home_brewed_drink', 'Processed_fish', 'Gari', 'Sheabutter', 'Other_nuts',
                 'Pigs', 'Chicken', 'Other_poultry', 'Fish')
round(cor(cor_df), 2)

# Following code will model independent variable identified in the individual 
# household and land assessment phases. The agricultural variables will focus on 
# those items that can be sold - contributing to profit. 

agr_prod_profit_base <- lm(profit ~ factor(region*ez) + factor(loc2) + 
     Flour_from_other_grains + Husked_polished_rice + Home_brewed_drink + 
     Processed_fish + Gari + Sheabutter + Other_nuts + Pigs + Chicken + 
     Other_poultry + Fish , data = capstone_df)
summary(agr_prod_profit_base)

ggplot(agr_prod_profit_base, aes(x=rstandard(agr_prod_profit_base))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "agr_prod_profit_base Assumption 6 (Normality) Review") 

ggplot(agr_prod_profit_base, aes(x = fitted(agr_prod_profit_base), y = rstandard(agr_prod_profit_base))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "agr_prod_profit_base Constant Varaince Review") 

# Following code removes the region ez interaction.

agr_prod_profit_1 <- lm(profit ~ factor(region) + factor(ez) + factor(loc2) + 
                             Flour_from_other_grains + Husked_polished_rice + Home_brewed_drink + 
                             Processed_fish + Gari + Sheabutter + Other_nuts + Pigs + Chicken + 
                             Other_poultry + Fish , data = capstone_df)
summary(agr_prod_profit_1)

ggplot(agr_prod_profit_1, aes(x=rstandard(agr_prod_profit_1))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "agr_prod_profit_1 Assumption 6 (Normality) Review") 

ggplot(agr_prod_profit_1, aes(x = fitted(agr_prod_profit_1), y = rstandard(agr_prod_profit_1))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "agr_prod_profit_1 Constant Varaince Review") 

# Following code adds the male household/quadratic from the household profile
# to the model

agr_prod_profit_test1 <- lm(profit ~ factor(region) + factor(ez) + factor(loc2) +
                 male + I(male^2) + Flour_from_other_grains + Husked_polished_rice + 
                 Home_brewed_drink + Processed_fish + Gari + Sheabutter + Other_nuts + 
                 Pigs + Chicken + Other_poultry + Fish , data = capstone_df)
summary(agr_prod_profit_test1)

ggplot(agr_prod_profit_test1, aes(x=rstandard(agr_prod_profit_test1))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "agr_prod_profit_test1 Assumption 6 (Normality) Review") 

ggplot(agr_prod_profit_test1, aes(x = fitted(agr_prod_profit_test1), y = rstandard(agr_prod_profit_test1))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "agr_prod_profit_test1 Constant Varaince Review")
#ASSESSMENT: Adding the male component helped flattened the residual v. fitted

# Following code removes the animal products
agr_prod_profit_test2 <- lm(profit ~ factor(region) + factor(ez) + factor(loc2) +
                        male + I(male^2) + Flour_from_other_grains + Husked_polished_rice + 
                        Home_brewed_drink + Gari + Sheabutter + Other_nuts, data = capstone_df)
summary(agr_prod_profit_test2)

ggplot(agr_prod_profit_test2, aes(x=rstandard(agr_prod_profit_test2))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "agr_prod_profit_test2 Assumption 6 (Normality) Review") 

ggplot(agr_prod_profit_test2, aes(x = fitted(agr_prod_profit_test2), y = rstandard(agr_prod_profit_test2))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "agr_prod_profit_test2 Constant Varaince Review") 
#ASSESSMENT: The removal of the animal products produced a worse model. 

# Following code returns the animal products then 
# rotates which plant product is removed.

agr_prod_profit_test3a <- lm(profit ~ factor(region) + factor(ez) + factor(loc2) +
                              male + I(male^2) + Husked_polished_rice +                   # Flour_from_other_grains removed
                              Home_brewed_drink + Gari + Sheabutter + Other_nuts + 
                              Pigs + Chicken + Other_poultry + Fish + Processed_fish, data = capstone_df)
summary(agr_prod_profit_test3a)

ggplot(agr_prod_profit_test3a, aes(x=rstandard(agr_prod_profit_test3a))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "agr_prod_profit_test3a Assumption 6 (Normality) Review") 

ggplot(agr_prod_profit_test3a, aes(x = fitted(agr_prod_profit_test3a), y = rstandard(agr_prod_profit_test3a))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "agr_prod_profit_test3a Constant Varaince Review") 
# ASSESSMENT: Model improved.

agr_prod_profit_test3b <- lm(profit ~ factor(region) + factor(ez) + factor(loc2) +
                              male + I(male^2) + Husked_polished_rice + Gari +      # Home_brewed_drink removed
                              Sheabutter + Other_nuts + Pigs + Chicken + 
                              Other_poultry + Fish + Processed_fish, data = capstone_df)
summary(agr_prod_profit_test3b)

ggplot(agr_prod_profit_test3b, aes(x=rstandard(agr_prod_profit_test3b))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "agr_prod_profit_test3b Assumption 6 (Normality) Review") 

ggplot(agr_prod_profit_test3b, aes(x = fitted(agr_prod_profit_test3b), y = rstandard(agr_prod_profit_test3b))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "agr_prod_profit_test3b Constant Varaince Review") 
#ASSESSMENT: No real change in model performance

# 3c removes the variables with highest p-value: Other_nuts, Other_poultry
agr_prod_profit_test3c <- lm(profit ~ factor(region) + factor(ez) + factor(loc2) +
                               male + I(male^2) + Husked_polished_rice + Gari +     
                               Sheabutter + Pigs + Chicken +  Fish + Processed_fish, 
                               data = capstone_df)
summary(agr_prod_profit_test3c)

ggplot(agr_prod_profit_test3c, aes(x=rstandard(agr_prod_profit_test3c))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "agr_prod_profit_test3c Assumption 6 (Normality) Review") 

ggplot(agr_prod_profit_test3c, aes(x = fitted(agr_prod_profit_test3c), y = rstandard(agr_prod_profit_test3c))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "agr_prod_profit_test3c Constant Varaince Review") 
#ASSESSMENT: No significant change to model performance

#Following code evaluted interactions to between variables
agr_prod_profit_test3d <- lm(profit ~ factor(ez) + factor(loc2) +
                               male + I(male^2) + Husked_polished_rice + Gari +      
                               (Husked_polished_rice:Gari) + Sheabutter + Pigs + 
                               Chicken +  Fish + Processed_fish + (Fish:Processed_fish), 
                             data = capstone_df)
summary(agr_prod_profit_test3d)

ggplot(agr_prod_profit_test3d, aes(x=rstandard(agr_prod_profit_test3d))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "agr_prod_profit_test3d Assumption 6 (Normality) Review") 

ggplot(agr_prod_profit_test3d, aes(x = fitted(agr_prod_profit_test3d), y = rstandard(agr_prod_profit_test3d))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "agr_prod_profit_test3d Constant Varaince Review") 
# ASSESSMENT: none of the interactions really improved the model

agr_prod_profit_test3e <- lm(profit ~ factor(region) + factor(ez) + 
                               factor(loc2) + male + I(male^2) + Husked_polished_rice + 
                               I(Husked_polished_rice^2) + Gari + I(Gari^2) + 
                               Sheabutter + I(Sheabutter^2) + 
                               Pigs + Chicken  + Fish + Processed_fish, 
                               data = capstone_df)
summary(agr_prod_profit_test3e)

ggplot(agr_prod_profit_test3e, aes(x=rstandard(agr_prod_profit_test3e))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "agr_prod_profit_test3e Assumption 6 (Normality) Review") 

ggplot(agr_prod_profit_test3e, aes(x = fitted(agr_prod_profit_test3e), y = rstandard(agr_prod_profit_test3e))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "agr_prod_profit_test3e Constant Varaince Review")
#ASSESSMENT: quadratic of Husked_polished_rice and Gari actually bent the right 
# end of the fitted line above zero

# Following code removes final variables - ez, loc2 and removing the Husked_polished_rice
# quadratic
agr_prod_profit_test3f <- lm(profit ~ factor(region) + male + I(male^2) + 
                               Husked_polished_rice + Gari + I(Gari^2) + 
                               Sheabutter + I(Sheabutter^2) + 
                               Pigs + Fish + Processed_fish + 
                               space_in_acres + I(space_in_acres^2), 
                             data = capstone_df)
summary(agr_prod_profit_test3f)

ggplot(agr_prod_profit_test3f, aes(x=rstandard(agr_prod_profit_test3f))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "agr_prod_profit_test3f Assumption 6 (Normality) Review") 

ggplot(agr_prod_profit_test3f, aes(x = fitted(agr_prod_profit_test3f), y = rstandard(agr_prod_profit_test3f))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "agr_prod_profit_test3f Constant Variance Review")

# Following implements code that calculates and displays information on the model's
# polynomials 

lnd_x <- (coef(agr_prod_profit_test3f)["space_in_acres"] / 
      (2 * -(coef(agr_prod_profit_test3f)["I(space_in_acres^2)"])))
lnd_x

ggplot(capstone_df, aes(x = space_in_acres, y= profit, group=1)) + 
  geom_point(color='black', alpha=0.3)+
  stat_smooth(method='lm', formula = y~poly(x,2))

gari_x <- (coef(agr_prod_profit_test3f)["Gari"] / 
             (2 * -(coef(agr_prod_profit_test3f)["I(Gari^2)"])))
gari_x

ggplot(capstone_df, aes(x = Gari, y= profit, group=1)) + 
  geom_point(color='green', alpha=0.3)+
  stat_smooth(method='lm', formula = y~poly(x,2))

sheabutter_x <- (coef(agr_prod_profit_test3f)["Sheabutter"] / 
                   (2 * -(coef(agr_prod_profit_test3f)["I(Sheabutter^2)"])))
sheabutter_x

ggplot(capstone_df, aes(x = Sheabutter, y= profit, group=1)) + 
  geom_point(color='orange', alpha=0.3)+
  stat_smooth(method='lm', formula = y~poly(x,2))

male_x <- (coef(agr_prod_profit_test3f)["male"] / 
                   (2 * -(coef(agr_prod_profit_test3f)["I(male^2)"])))
male_x

ggplot(capstone_df, aes(x = male, y= profit, group=1)) + 
  geom_point(color='blue', alpha=0.3)+
  stat_smooth(method='lm', formula = y~poly(x,2))


anova(agr_prod_profit_base, agr_prod_profit_test3f)