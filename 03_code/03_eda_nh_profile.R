# A mom texts, “Hi! Son, what does IDK, LY, & TTYL mean?” He texts back, 
# “I Don’t Know, Love You, & Talk To You Later.” The mom texts him, 
# “It’s ok, don’t worry about it. I’ll ask your sister, love you too.”

# Initial Descriptive Stat of nh-profile_base ----
summary(nh_profile_base)
# Initial Descriptive Stat - Correlation of independent variables
cor_df <- select(nh_profile_base, "male", "female", "family_size", "av_hh_age",
          "male_help", "female_help","profit")
round(cor(cor_df), 2)

# Initial/base model examining profit against all independent variables ----

# Following model includes all variables
base_model <- lm(profit ~ factor(region) + factor(ez) + factor(loc2) + 
           male + female + family_size + av_hh_age + factor(s2aq2) + factor(s2cq1) + 
           factor(s2cq2) + factor(s2cq3) + factor(s2cq4) + factor(s2cq5) +
           male_help + female_help, data = nh_profile_base)
summary(base_model)
# Model fit tests
ggplot(base_model, aes(x=rstandard(base_model))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for base model") 

ggplot(base_model, aes(x = fitted(base_model), y = rstandard(base_model))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review for base model") 

# model_1 ----
# Following looks to identify the best model by using a backward variable process:
# removing code that code has the lowest p-score. All education (s2aq2) factors 
# were statically insignificant thus s2aq2 was removed as a variable. Additionally, 
# singe family_size is highly correlated with the count of male and female household
# members it was also removed.

model_1 <- lm(profit ~ factor(region) + factor(ez) + factor(loc2) + 
                   male + female + av_hh_age + factor(s2cq1) + 
                   factor(s2cq2) + factor(s2cq3) + factor(s2cq4) + factor(s2cq5) +
                   male_help + female_help, data = nh_profile_base)
summary(model_1)
# Model fit test
ggplot(model_1, aes(x=rstandard(model_1))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Model 1 Assumption 6 (Normality) Review") 

ggplot(model_1, aes(x = fitted(model_1), y = rstandard(model_1))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Model 1 Constant Varaince Review") 


# Model 2 ----
# Except for being able to speak a dominate language in the country would directly influence 
# the ability to conduct business and make a profit. 
# Most of the remaining levels in the literacy factors were not statistically significant.
# The literacy factor for what languages did nh read and write were kept with the others removed. 

model_2 <- lm(profit ~ factor(region) + factor(ez) + factor(loc2) + 
                male + female + av_hh_age + factor(s2cq2) + factor(s2cq4) + male_help + 
                female_help, data = nh_profile_base)
summary(model_2)
# Model fit tests 
ggplot(model_2, aes(x=rstandard(model_2))) +
  geom_histogram(binwidth = .10) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for model 2") 

ggplot(model_2, aes(x = fitted(model_2), y = rstandard(model_2))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review model 2") 

# Model 3 ----
# The literacy variables - again except for level 4 - were statistically not significant.
# These variables are removed in model_3
model_3 <- lm(profit ~ factor(region) + factor(ez) + factor(loc2) + 
                male + female + av_hh_age + male_help + 
                female_help, data = nh_profile_base)
summary(model_3)
# Model fit test
ggplot(model_3, aes(x=rstandard(model_3))) +
  geom_histogram(binwidth = .10) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review for model 3") 

ggplot(model_3, aes(x = fitted(model_3), y = rstandard(model_3))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review model 3") 

# Model 4 ----
# While model 3 showed that the independent variables and most of the region, district
# factor levels are statistically significant, the errors are not normally distributed 
# with a right tail and the fitted v residuals dose not show constant variation.
# Model four will remove the non-factor variable with the highest p-score - av_hh_age.   

model_4 <- lm(profit ~ factor(region) + factor(ez) + factor(loc2) + 
                male + female +  male_help + female_help, data = nh_profile_base)
summary(model_4)
# Model fit test
ggplot(model_4, aes(x=rstandard(model_4))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_4, aes(x = fitted(model_4), y = rstandard(model_4))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review model 4") 

# Model 6 ----
# Model 5 showed that all factor levels for ecological zone and location have 
# the lowest p-score when compared to the district factor. This could be because 
# ecological zone and urban/rural levels are subsets to districts. Model 6 a-c will 
# alternate the removal the district, ez and loc2 factor variables to assess impact . 

model_6a <- lm(profit ~ factor(ez) + factor(loc2) + 
                male + female +  male_help + female_help, data = nh_profile_base)
summary(model_6a)
# Model fit test
ggplot(model_6a, aes(x=rstandard(model_6a))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_6a, aes(x = fitted(model_6a), y = rstandard(model_6a))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Model 6a Constant Varaince Review") 
#ASSESSMENT - the residual v. fitted got worse and a noticeable right tail in the histogram. 

model_6b <- lm(profit ~ factor(region) + factor(loc2) + 
                 male + female +  male_help + female_help, data = nh_profile_base)
summary(model_6b)
# Model fit test
ggplot(model_6b, aes(x=rstandard(model_6b))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_6b, aes(x = fitted(model_6b), y = rstandard(model_6b))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT - While the residual v. fitted improved it is still not very good
# and the histogram still has a noticeable right tail in the histogram. 

model_6c <- lm(profit ~ factor(region) + factor(ez) + 
                 male + female +  male_help + female_help, data = nh_profile_base)
summary(model_6c)
# Model fit test
ggplot(model_6c, aes(x=rstandard(model_6c))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_6c, aes(x = fitted(model_6c), y = rstandard(model_6c))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT - While the residual v. fitted is still not very good
# the histogram improved with the left tail growing and a reduction the right tail.

# Model 6 cont ----
# Model 6 e-g will assess if there is an interaction in district, ez and loc variables since 
# ex and loc2 can be subsets of district.
model_6e <- lm(profit ~ factor(region * ez) + factor(ez) + factor(loc2) + 
                 male + female +  male_help + female_help, data = nh_profile_base)
summary(model_6e)
# Model fit test
ggplot(model_6e, aes(x=rstandard(model_6e))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_6e, aes(x = fitted(model_6e), y = rstandard(model_6e))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT - The residual v. fitted improved greatly (probably the best so far) but the 
# histogram worsened  with a lengthen of the right tail.
# Model 6e_2 will expand on 6e by removing loc2 to determine is impact.  
model_6e_2 <- lm(profit ~ factor(region * ez) +  male + female +  male_help + 
                   female_help, data = nh_profile_base)
summary(model_6e_2)

ggplot(model_6e_2, aes(x=rstandard(model_6e_2))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_6e_2, aes(x = fitted(model_6e_2), y = rstandard(model_6e_2))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT - No significant change. 

model_6f <- lm(profit ~ factor(region * loc2) + factor(ez) +  male + female +  male_help + 
                   female_help, data = nh_profile_base)
summary(model_6f)

ggplot(model_6f, aes(x=rstandard(model_6f))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_6e_2, aes(x = fitted(model_6e_2), y = rstandard(model_6e_2))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT - The fitted v residuals worsened  

model_6g <- lm(profit ~ factor(region) + factor(ez * loc2) +  male + female +  male_help + 
                 female_help, data = nh_profile_base)
summary(model_6g)

ggplot(model_6g, aes(x=rstandard(model_6g))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_6g, aes(x = fitted(model_6g), y = rstandard(model_6g))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 

# Model 7 ----
# Model 6e and g proved to be the best fit with the expected p-scores.
# Model 7 will assess the impact the male, female, hired
# help has on the profit.  Model 6e will be the base. 7a will remove the female 
# help variable since it have the lowest p-score.
model_7a <- lm(profit ~ factor(region * ez) + factor(loc2) +
                 male + female + male_help, data = nh_profile_base)
summary(model_7a)
# Model fit test
ggplot(model_7a, aes(x=rstandard(model_7a))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_7a, aes(x = fitted(model_7a), y = rstandard(model_7a))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT: The histogram still has a right tail if not worse than 6e but 
# residual v. fit appears to be the same.
# Model 7b will remove female from the model
model_7b <- lm(profit ~ factor(region * ez) + factor(loc2) +
                 male + male_help, data = nh_profile_base)
summary(model_7b)

ggplot(model_7b, aes(x=rstandard(model_7b))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_7b, aes(x = fitted(model_7b), y = rstandard(model_7b))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT: the fit tests appear same as 7a

# Model 8 ----
# Model 8 will look at quadratics of male and other variable impact.
model_8a <- lm(profit ~ factor(region * ez) + factor(loc2) +
                 male + I(male^2) + male_help, data = nh_profile_base)
summary(model_8a)

ggplot(model_8a, aes(x=rstandard(model_8a))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_8a, aes(x = fitted(model_8a), y = rstandard(model_8a))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT: Histogram still has right tail but residuals v. fit the best overall so far. 
model_8b <- lm(profit ~ factor(region * ez) + factor(loc2) +
                 male + I(male^2) + male_help + I(male_help^2), data = nh_profile_base)
summary(model_8b)

ggplot(model_8b, aes(x=rstandard(model_8b))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Assumption 6 (Normality) Review") 

ggplot(model_8b, aes(x = fitted(model_8b), y = rstandard(model_8b))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
# Model 8c removes the help impact.  One hypothesis is because only a few farms have 
# hired help in large qualities and they generally have higher profits,
# the associate residual errors are bending the fit line.  
model_8c <- lm(profit ~ factor(region * ez) + factor(loc2) +
                 male + I(male^2), data = nh_profile_base)
summary(model_8c)

ggplot(model_8c, aes(x=rstandard(model_8c))) +
  geom_histogram(binwidth = .1) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Model 8c Assumption 6 (Normality) Review") 

ggplot(model_8c, aes(x = fitted(model_8c), y = rstandard(model_8c))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Model 8c Constant Varaince Review") 

#ASSESSMENT: 8c provides the best fit v residuals

# Model 9 ----
# Using 8b as a base the following ads back individual variables. 
model_9a <- lm(profit ~ factor(region) + factor(ez) + factor(region * ez) + factor(loc2) + male +
               I(male^2) + male_help + I(male_help^2) + female_help, 
               data = nh_profile_base)
summary(model_9a)

ggplot(model_9a, aes(x=rstandard(model_9a))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Model 9 Assumption 6 (Normality) Review") 

ggplot(model_9a, aes(x = fitted(model_9a), y = rstandard(model_9a))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Constant Varaince Review") 
#ASSESSMENT: The female variable improved the fit but the histogram is still with a right tail

# Model 10 ----
# Model 10 for experiment removes all workforce composition variables. 

model_10 <- lm(profit ~ factor(region) + factor(ez) + factor(region*ez) + factor(loc2),
               data = nh_profile_base)
summary(model_10)

ggplot(model_10, aes(x=rstandard(model_10))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardized Residuals", y = "Residual Count", 
       title = "Model 10 Assumption 6 (Normality) Review") 

ggplot(model_10, aes(x = fitted(model_10), y = rstandard(model_10))) + 
  geom_point() + 
  geom_smooth() + 
  labs(x = "Profit", y = "Standardized Residuals", 
       title = "Model 10 Constant Varaince Review") 

anova(model_10, model_8c)
anova(base_model, model_8c)
anova(base_model, model_10)