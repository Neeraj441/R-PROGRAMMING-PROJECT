diabetic_sample$age <- as.factor(diabetic_sample$age)

diabetic_sample$race <- as.factor(diabetic_sample$race)

#creating multiple linear regression model
diabetic_sample_M.3 <- lm(time_in_hospital ~ num_medications + readmitted + age + race, data = diabetic_sample)

#Summary
summary(diabetic_sample_M.3)