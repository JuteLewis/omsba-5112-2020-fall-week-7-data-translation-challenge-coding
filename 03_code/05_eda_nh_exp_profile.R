# Following code builds initial library set ----
library(tidyverse)
library(haven)

# Initial Descriptive Stat of nh exp profile ----
summary(nh_exp_profile_base)

# Initial model examining profit against household a exspenses ----

# Variable selection ----
# Using Backward Variable Selection
exp_bkwd_base <- lm(profit ~ factor(district) + expland + expcrop + homepro + 
                 expfood, data = nh_exp_profile_base)
summary(exp_bkwd_base)

ggplot(exp_bkwd_base, aes(x=rstandard(exp_bkwd_base))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for Bkwd Base") 

ggplot(exp_bkwd_base, aes(x = fitted(exp_bkwd_base), y = rstandard(exp_bkwd_base))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review for Bkwd Base") 


exp_bkwd_1 <- lm(profit ~ factor(district) + expcrop + homepro + 
                      expfood, data = nh_exp_profile_base)
summary(exp_bkwd_1)

ggplot(exp_bkwd_1, aes(x=rstandard(exp_bkwd_1))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for Bkwd Base") 

ggplot(exp_bkwd_1, aes(x = fitted(exp_bkwd_1), y = rstandard(exp_bkwd_1))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review for Bkwd Base") 

exp_bkwd_2 <- lm(profit ~ factor(district) + expcrop + homepro, 
                 data = nh_exp_profile_base)
summary(exp_bkwd_2)

ggplot(exp_bkwd_2, aes(x=rstandard(exp_bkwd_2))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for Bkwd Base") 

ggplot(exp_bkwd_2, aes(x = fitted(exp_bkwd_2), y = rstandard(exp_bkwd_2))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review for Bkwd Base") 

exp_bkwd_3 <- lm(profit ~ expcrop + homepro, data = nh_exp_profile_base)
summary(exp_bkwd_3)

ggplot(exp_bkwd_3, aes(x=rstandard(exp_bkwd_3))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for Bkwd Base") 

ggplot(exp_bkwd_3, aes(x = fitted(exp_bkwd_3), y = rstandard(exp_bkwd_3))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review for Bkwd Base") 
