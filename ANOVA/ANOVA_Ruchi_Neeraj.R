#ANOVA and Logistic Regression

#Ruchi Dilip Kukde
#Neeraj Kumar Reddy Panta

library(readr)
diabetic_data <- read_csv("diabetic_data.csv", 
                          col_types = cols(encounter_id = col_integer(), 
                                           patient_nbr = col_integer(), weight = col_integer(), 
                                           admission_type_id = col_integer(), 
                                           discharge_disposition_id = col_integer(), 
                                           admission_source_id = col_integer(), 
                                           time_in_hospital = col_integer(), 
                                           payer_code = col_integer(), num_lab_procedures = col_integer(), 
                                           num_procedures = col_integer(), num_medications = col_integer(), 
                                           number_outpatient = col_integer()))
View(diabetic_data)

#Required Libraries
library(ggplot2)
library(multcomp)

#taking sample of 500 rows from the diabetic_data dataset.
x <- sample(1:nrow(diabetic_data), 500)

#displays the 500 rows
diabetic_sample <- diabetic_data[x, ]

plot(x = diabetic_sample$num_medications, y= diabetic_sample$time_in_hospital)

#Linear regression models for the new dataset
#######################################################
#In our linear regression model 
#we are assuming "time_in_hospital" as outcome variable and "num_medications" as predictor variable.

#Creating a regression model
diabetic_sample_M.1 <- lm(time_in_hospital ~ num_medications, data = diabetic_sample)

smoothScatter(x = diabetic_sample$num_medications, y=diabetic_sample$time_in_hospital, 
              transformation = function(x) x ^ 0.4,
              colramp = colorRampPalette(c("#000099", "#00FEFF", "#45FE4F",
                                        "#FCFF00", "#FF9400", "#FF3100")))

#since we can't interpret the exact relationship between the 2 variables 
#it is better if we use pearson correlation method to identify and assess the correlation
cor.test(x = diabetic_sample$num_medications, y=diabetic_sample$time_in_hospital, 
         alternative = "two.sided",method = "pearson")

library(MASS)
kern_linear_sample <- kde2d(x = diabetic_sample$num_medications, y=diabetic_sample$time_in_hospital)
contour(kern_linear_sample, drawlabels = FALSE, nlevels = 6,
        col = rev(heat.colors(6)), add = TRUE, lwd = 3)
abline(reg = diabetic_sample_M.1, col = "red", lty = 2, lwd= 5)

summary(diabetic_sample_M.1)

#Confidence Interval of the Slope
confint(diabetic_sample_M.1, level = 0.95)

#Goodness of fit by RSE
summary(diabetic_sample_M.1)$sigma / mean(diabetic_sample$time_in_hospital)

#Using Categorical variables to define the groups
diabetic_sample$readmitted<- as.factor(diabetic_sample$readmitted)
plot(x = diabetic_sample$num_medications, y = diabetic_sample$time_in_hospital, pch=16, col=diabetic_sample$readmitted)
legend("topright",inset=0.05, legend= c("<30", ">30","NO"), fill =c("red","black","green"))
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
par(mfrow=c(1,1))
#Data is being Factored
boxplot(diabetic_sample$time_in_hospital ~ diabetic_sample$readmitted, horizontal = TRUE)

anova_m1 <- lm(time_in_hospital~readmitted, data=diabetic_sample)
summary(anova_m1)

anova.1 <- aov(anova_m1)
summary(anova.1)

#Our model is explaining considerable amount of variability based on the Mean SQ since 19.19 > 9.36

postHocs<-glht(anova.1, linfct = mcp(readmitted = "Tukey"))
summary(postHocs)
confint(postHocs)

####end of code
