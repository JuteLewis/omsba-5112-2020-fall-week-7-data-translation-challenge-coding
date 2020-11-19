# Initial Descriptive Stat of nh profile ----
summary(nh_profile_base)

# Initial model examining profit against household average age, average years of ----
# education, and number of male and female household members.

# Variable selection ----
# Using Backward Variable Selection
bkwd_base <- lm(profit ~ factor(district) + factor(s2aq2) +
                av_hh_age + male + female + male_help + female_help, 
                data = nh_profile_base)
summary(bkwd_base)

ggplot(bkwd_base, aes(x=rstandard(bkwd_base))) +
  geom_histogram(binwidth = .10) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for Bkwd Base") 

ggplot(bkwd_base, aes(x = fitted(bkwd_base), y = rstandard(bkwd_base))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review for Bkwd Base") 

bkwd_1 <- lm(profit ~ factor(district) + factor(s2aq2) +
            male + female + male_help + male_help + female_help, 
            data = nh_profile_base)
summary(bkwd_1)

bkwd_1_a <- lm(profit ~ factor(district) + factor(s2aq2) + av_hh_age +
               male + female + male_help + I(male_help^2) + female_help, 
             data = nh_profile_base)
summary(bkwd_1_a)

ggplot(bkwd_1_a, aes(x=rstandard(bkwd_1_a))) +
  geom_histogram(binwidth = .10) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for Bkwd Base 1a") 

ggplot(bkwd_1_a, aes(x = fitted(bkwd_1_a), y = rstandard(bkwd_1_a))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review Bkwd Base 1a") 

# Add the quadratic for male help greatly improved the fit of the model.  WIll try 
# adding a female help quadratic. 

bkwd_1_b <- lm(profit ~ factor(district) + factor(s2aq2) +
                 male + female + male_help + I(male_help^2) + female_help + 
                 I(female_help^2), data = nh_profile_base)
summary(bkwd_1_b)

ggplot(bkwd_1_b, aes(x=rstandard(bkwd_1_b))) +
  geom_histogram(binwidth = .10) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for Bkwd Base 1b") 

ggplot(bkwd_1_b, aes(x = fitted(bkwd_1_b), y = rstandard(bkwd_1_b))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review Bkwd Base 1 b") 

# bkwd_2 alternating district factor and education factor assessing if any new 
# levels take on statistical significance  

bkwd_2_a <- lm(profit ~ factor(s2aq2) + male + female + male_help +
               female_help, data = nh_profile_base)
summary(bkwd_2_a)

ggplot(bkwd_2_a, aes(x=rstandard(bkwd_2_a))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(bkwd_2_a, aes(x = fitted(bkwd_2_a), y = rstandard(bkwd_2_a))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 

bkwd_2_b <- lm(profit ~ factor(district) + male + female + male_help +
                 female_help, data = nh_profile_base)
summary(bkwd_2_b)

ggplot(bkwd_2_b, aes(x=rstandard(bkwd_2_b))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(bkwd_2_b, aes(x = fitted(bkwd_2_b), y = rstandard(bkwd_2_b))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Proft", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 

# bkwd_3 removing female help variable

bkwd_3 <- lm(profit ~ factor(district)+ male + female + male_help, data = nh_profile_base)
summary(bkwd_3)

ggplot(bkwd_3, aes(x=rstandard(bkwd_3))) +
  geom_histogram(binwidth = .5) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 
# ASSESSMENT - Error still has a right tail but is shorter than bkwd_1 and 2 but
# also indicates significant outliers.

ggplot(bkwd_3, aes(x = fitted(bkwd_3), y = rstandard(bkwd_3))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 

# bkwd_4 removing household female
bkwd_4 <- lm(profit ~ factor(district)+ male + male_help, data = nh_profile_base)
summary(bkwd_4)

ggplot(bkwd_4, aes(x=rstandard(bkwd_4))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(bkwd_4, aes(x = fitted(bkwd_4), y = rstandard(bkwd_4))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "TBD", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 




