# A mom texts, “Hi! Son, what does IDK, LY, & TTYL mean?” He texts back, 
# “I Don’t Know, Love You, & Talk To You Later.” The mom texts him, 
# “It’s ok, don’t worry about it. I’ll ask your sister, love you too.”

# Initial Descriptive Stat of nh profile ----
summary(nh_profile_base)

# Initial model examining profit against household district, ecological zone, ----
# location code, average age, average years of education, and number of male 
# and female household members.

# Variable selection and model build 
# Using Backward Variable Selection

# Following model includes all variables
bkwd_base <- lm(profit ~ factor(district) + factor(s2aq2) + factor(ez) + 
                factor(loc2) + av_hh_age + male + female + male_help + 
                female_help, data = nh_profile_base)
summary(bkwd_base)
# Model fit tests
ggplot(bkwd_base, aes(x=rstandard(bkwd_base))) +
  geom_histogram(binwidth = .10) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for bkwd_base") 

ggplot(bkwd_base, aes(x = fitted(bkwd_base), y = rstandard(bkwd_base))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review for bkwd_base") 

# Following code removes the education and ecological zone variables
# The education factor levels evaluated to being statically insignificant and 
# account for a small portion of the model.. The models show that 
#ecological variable introduce more heterskedasticity.
bkwd_1 <- lm(profit ~ factor(district) + factor(loc2) + av_hh_age + male + 
            female + male_help + male_help + female_help, 
            data = nh_profile_base)
summary(bkwd_1)
# Model fit test
ggplot(bkwd_1, aes(x = fitted(bkwd_1), y = rstandard(bkwd_1))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review Bkwd Base 1a") 
# Following code expands the bkwd_1 model by exploring the affects of introducing 
# a quadratic variant of male_help
bkwd_1_a <- lm(profit ~ factor(district) + factor(loc2) + av_hh_age +
              male + female + male_help + I(male_help^2) + female_help, 
              data = nh_profile_base)
summary(bkwd_1_a)
# Model fit tests 
ggplot(bkwd_1_a, aes(x=rstandard(bkwd_1_a))) +
  geom_histogram(binwidth = .10) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for bkwd_base_1_a") 

ggplot(bkwd_1_a, aes(x = fitted(bkwd_1_a), y = rstandard(bkwd_1_a))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review bkwd_1_a") 
#ASSESSMENT - The bkwd_1_a has evaluated out to be the best fitted model 
# explaining the influence of the variables on expected annual household profit

# The following model expands the quadratic exploration by introducing a 
# quadratic variation of female_help. 
bkwd_1_b <- lm(profit ~ factor(district) + factor(ez) + factor(loc2) 
               + av_hh_age + male + female + male_help + I(male_help^2) + female_help + 
                 I(female_help^2), data = nh_profile_base)
summary(bkwd_1_b)
# Model fit test
ggplot(bkwd_1_b, aes(x=rstandard(bkwd_1_b))) +
  geom_histogram(binwidth = .10) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for Bkwd Base 1b") 

ggplot(bkwd_1_b, aes(x = fitted(bkwd_1_b), y = rstandard(bkwd_1_b))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review Bkwd Base 1 b") 

# bkwd_2  (a, b, c) alternates the district, ecological and loc factors 
# assessing if any new levels take on statistical significance  
bkwd_2_a <- lm(profit ~ factor(ez) + av_hh_age + male + female + male_help +
               female_help, data = nh_profile_base)
summary(bkwd_2_a)
# Model fit test
ggplot(bkwd_2_a, aes(x=rstandard(bkwd_2_a))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(bkwd_2_a, aes(x = fitted(bkwd_2_a), y = rstandard(bkwd_2_a))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 

bkwd_2_b <- lm(profit ~ factor(loc2) + male + female + male_help + av_hh_age +
                 female_help, data = nh_profile_base)
summary(bkwd_2_b)
# Model fit test
ggplot(bkwd_2_b, aes(x=rstandard(bkwd_2_b))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(bkwd_2_b, aes(x = fitted(bkwd_2_b), y = rstandard(bkwd_2_b))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 

bkwd_2_c <- lm(profit ~ factor(district) + av_hh_age + male + female + male_help +
                 female_help, data = nh_profile_base)
summary(bkwd_2_c)
# Model fit test
ggplot(bkwd_2_c, aes(x=rstandard(bkwd_2_c))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(bkwd_2_c, aes(x = fitted(bkwd_2_c), y = rstandard(bkwd_2_c))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 




# ASSESSMENT - The district variable proved a better variable (influence)
# on expect annual household profit

# The following model - bkwd_3 - retains the district factor but 
# removes the female help variable
bkwd_3 <- lm(profit ~ factor(district)+ male + female + male_help, data = nh_profile_base)
summary(bkwd_3)
# Model fit test
ggplot(bkwd_3, aes(x=rstandard(bkwd_3))) +
  geom_histogram(binwidth = .5) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(bkwd_3, aes(x = fitted(bkwd_3), y = rstandard(bkwd_3))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 

# The following code - bkwd_4 - removes the household female variable.
bkwd_4 <- lm(profit ~ factor(district)+ male + male_help, data = nh_profile_base)
summary(bkwd_4)
# model fit test
ggplot(bkwd_4, aes(x=rstandard(bkwd_4))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(bkwd_4, aes(x = fitted(bkwd_4), y = rstandard(bkwd_4))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 




