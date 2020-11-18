# Inital Discriptive Stat of nh profile ----
summary(nh_profile_base)

# Initial model examining profit against household average age, average years of ----
# education, and number of male and female household members.

# Variable selection ----
# Using Backward Variable Selection
bkwd_base <- lm(profit ~ factor(district) + agey + av_yrs_ed + Male + Female + family_size
                + Male_Help + Female_help, data = nh_profile_base)
summary(bkwd_base)

ggplot(bkwd_base, aes(x=rstandard(bkwd_base))) +
  geom_histogram(binwidth = .5) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 
# ASSESSMENT - Error is not normally distributed with a Right tail.

ggplot(bkwd_base, aes(x = fitted(bkwd_base), y = rstandard(bkwd_base))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "TBD", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT - Model is not very good.  There is a large curve in the mean.

# bkwd_1 removing age since it has the largest p value

bkwd_1 <- lm(profit ~ factor(district) + av_yrs_ed + Male + Female + Male_Help +
                  Female_help, data = nh_profile_base)
summary(bkwd_1)

ggplot(bkwd_1, aes(x=rstandard(bkwd_1))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 
# ASSESSMENT - Error is not normally distributed with a right tail.

ggplot(bkwd_1, aes(x = fitted(bkwd_1), y = rstandard(bkwd_1))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "TBD", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT - Model is not very good.  There is still a large curve in the mean.

# bkwd_2 removing average household education since it has the largest p value

bkwd_2 <- lm(profit ~ factor(district) + Male + Female + Male_Help +
               Female_help, data = nh_profile_base)
summary(bkwd_2)
# ASSESSMENT - The intercept and all coefficients are significant at .001

ggplot(bkwd_2, aes(x=rstandard(bkwd_2))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 
# ASSESSMENT - Error still has a right tail but is shorter than bkwd_1

ggplot(bkwd_2, aes(x = fitted(bkwd_2), y = rstandard(bkwd_2))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "TBD", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT - Model is not very good.  There is still a large curve in the mean.

# bkwd_3 removing district factor since it impact is so varied

bkwd_3 <- lm(profit ~ Male + Female + Male_Help + Female_help, data = nh_profile_base)
summary(bkwd_3)
# ASSESSMENT - Intercept significance dropped the 5% 

ggplot(bkwd_3, aes(x=rstandard(bkwd_3))) +
  geom_histogram(binwidth = .5) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 
# ASSESSMENT - Error still has a right tail but is shorter than bkwd_1 and 2 but
# also indicates significant outliers.

ggplot(bkwd_3, aes(x = fitted(bkwd_3), y = rstandard(bkwd_3))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "TBD", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT - Model is not very good.  There is still a large curve in the mean.

# bkwd_4 removing household female help since it has the largest p value

bkwd_4 <- lm(profit ~ Male + Female + Male_Help, data = nh_profile_base)
summary(bkwd_4)
# ASSESSMENT - Intercept significance still at the 5% level

ggplot(bkwd_4, aes(x=rstandard(bkwd_4))) +
  geom_histogram(binwidth = .5) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 
# ASSESSMENT - Error still has a right tail bis about the same as bkwd 3

ggplot(bkwd_4, aes(x = fitted(bkwd_4), y = rstandard(bkwd_4))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "TBD", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 




