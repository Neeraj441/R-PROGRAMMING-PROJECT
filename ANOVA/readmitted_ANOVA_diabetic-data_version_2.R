#ANOVA and Logistic Regression

#Ruchi Dilip Kukde
#Neeraj Kumar Reddy Panta

#Required Libraries
library(ggplot2)
library(multcomp)

#taking sample of 500 rows from the diabetic_data dataset.
x <- sample(1:nrow(diabetic_data), 500)

#displays the 500 rows
diabetic_sample <- diabetic_data[x, ]

#Linear regression models for the new dataset
#######################################################
#Simple Linear Regression for patients who have readmitted in hospital in less than 30 days
plot(x = diabetic_sample$time_in_hospital, y=diabetic_sample$num_medications)

#In our linear regression model 
#we are assuming "time_in_hospital" as target variable and "num_medications" as predictor variable.

#Creating a regression model
diabetic_sample_M.1 <- lm(time_in_hospital ~ num_medications, data = diabetic_sample)

smoothScatter(x = diabetic_sample$time_in_hospital, y=diabetic_sample$num_medications, 
              transformation = function(x) x ^ 0.4,
              colramp = colorRampPalette(c("#000099", "#00FEFF", "#45FE4F",
                                           "#FCFF00", "#FF9400", "#FF3100")))

library(MASS)
kern_linear_sample <- kde2d(x = diabetic_sample$time_in_hospital, y=diabetic_sample$num_medications)

contour(kern_linear_sample, drawlabels = FALSE, nlevels = 6,
        col = rev(heat.colors(6)), add = TRUE, lwd = 3)
abline(reg = diabetic_sample_M.1, col = "red", lty = 2)

#since we can't interpret the exact relationship between the 2 variables 
#it is better if we use pearson and spearman correlation methods to identify and assess the correlation
cor.test(x = diabetic_sample$time_in_hospital, y=diabetic_sample$num_medications, 
         alternative = "two.sided",method = "pearson")

cor.test(x = diabetic_sample$time_in_hospital, y=diabetic_sample$num_medications, 
         alternative = "two.sided",method = "spearman")

summary(diabetic_sample_M.1)

#Confidence Interval of the Slope
confint(diabetic_sample_M.1, level = 0.95)

#Goodness of fit by RSE
summary(diabetic_sample_M.1)$sigma / mean(diabetic_data.less$time_in_hospital)

#Using Categorical variables to define the groups
plot(x = diabetic_sample$num_medications, y = diabetic_sample$time_in_hospital, pch=16, col=diabetic_sample$readmitted,main = "Categorical variables to define groups")
legend(x = "topright",2, 4, legend=c("<30", ">30","NO"), 
       fill = c("red","black","green"))
################################################################################################

#creating multiple linear regression model
diabetic_sample_M.2 <- lm(time_in_hospital ~ num_medications + readmitted, data = diabetic_sample)

#Summary
summary(diabetic_sample_M.2)

#Plotting Fitting Values
plot(x = diabetic_sample$num_medications, y =  diabetic_sample$time_in_hospital, col=diabetic_sample$readmitted)
points(x = diabetic_sample$num_medications, y = diabetic_sample_M.2$fitted.values, col=diabetic_sample$readmitted, pch=16)

#Distribution of patients time in Hospital
par(mfrow=c(3,1))
hist(x = diabetic_sample$time_in_hospital[diabetic_sample$readmitted=="<30"], col=5)
hist(x = diabetic_sample$time_in_hospital[diabetic_sample$readmitted==">30"], col=6)
hist(x = diabetic_sample$time_in_hospital[diabetic_sample$readmitted=="NO"], col=7)

#########################################################

#ANOVA 

#Data is being Factored
# diabetic_sample$readmitted <- as.factor(diabetic_sample$readmitted)

boxplot(diabetic_sample$time_in_hospital ~ diabetic_sample$readmitted, horizontal = TRUE)

anova_m1 <- lm(time_in_hospital~readmitted, data=diabetic_sample)
summary(anova_m1)

anova.1 <- aov(anova_m1)

summary(anova.1)

#Our model 1 is explaining considerable amount of variability based on the Mean SQ since 19.19 > 9.36

diabetic_sample$new_readmitted <- factor(diabetic_sample$readmitted, levels = c("NO",">30","<30"))

anova_m2 <- lm(time_in_hospital ~ new_readmitted, data=diabetic_sample)

summary(anova_m2)

anova.2 <- aov(anova_m2)

summary(anova.2)

postHocs<-glht(anova.1, linfct = mcp(readmitted = "Tukey"))
summary(postHocs)

confint(postHocs)
