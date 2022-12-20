#Linear Regression

#Ruchi Dilip Kukde
#Neeraj Kumar Reddy Panta

# Importing Diabetics Data set

library(readr)
diabetic_data <- read_csv("Project/STAT METHODS project/dataset_diabetes/dataset_diabetes/diabetic_data.csv", 
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

#Selecting Factor - readmitted 
typeof(diabetic_data$readmitted)
#Selecting Factor - readmitted 
typeof(diabetic_data$gender)

#Checking for Null Values
sum(is.na(diabetic_data$readmitted))
sum(is.na(diabetic_data$age))
# Here we have No null values for the Specific Columns

#Split the data according to factor - readmitted
diabetic_data.less<-split(diabetic_data, diabetic_data$readmitted)$"<30"
#Here since data set is large we are just trying to focus only on a sub sample of our data for better understanding.

################################################################################################

#Simple Linear Regression for patients who have readmitted in hospital in less than 30 days
plot(x = diabetic_data.lessF$time_in_hospital, y=diabetic_data.lessF$num_medications)

#In our linear regression model 
#we are assuming "time_in_hospital" as target variable and "num_medications" as predictor variable.

#Creating a regression model
diabetic_less_M.1 <- lm(time_in_hospital ~ num_medications, data = diabetic_data.less)

plot(x = diabetic_data.less$time_in_hospital, y = diabetic_data.less$num_medications)
abline(reg = diabetic_less_M.1, col = "red", lty = 2)

smoothScatter(x = diabetic_data.less$time_in_hospital, y=diabetic_data.less$num_medications, 
              transformation = function(x) x ^ 0.4,
              colramp = colorRampPalette(c("#000099", "#00FEFF", "#45FE4F",
                                           "#FCFF00", "#FF9400", "#FF3100")))

library(MASS)
kern_less <- kde2d(x = diabetic_data.less$time_in_hospital, y=diabetic_data.less$num_medications)

contour(kern_less, drawlabels = FALSE, nlevels = 6,
        col = rev(heat.colors(6)), add = TRUE, lwd = 3)
abline(reg = diabetic_less_M.1, col = "red", lty = 2)

#since we can't interpret the exact relationship between the 2 variables 
#it is better if we use pearson and spearman correlation methods to identify and assess the correlation
cor.test(x = diabetic_data.less$time_in_hospital, y=diabetic_data.less$num_medications, 
         alternative = "two.sided",method = "pearson")

cor.test(x = diabetic_data.less$time_in_hospital, y=diabetic_data.less$num_medications, 
         alternative = "two.sided",method = "spearman")

summary(diabetic_less_M.1)

#Confidence Interval of the Slope
confint(diabetic_less_M.1, level = 0.95)

#Goodness of fit by RSE
summary(diabetic_less_M.1)$sigma / mean(diabetic_data.less$time_in_hospital)

#plotting residuals
plot(diabetic_less_M.1$residuals, pch = 16, col = "red")

################################################################################################

#Creating a multidimensional model with 2 predictors and 1 outcome variable.
diabetic_multi <- diabetic_data.less[,c('time_in_hospital','num_medications','num_lab_procedures')]

View(diabetic_multi)

#preliminary analysis to see which are correlated
pairs(diabetic_multi)

#Finding correlation between multiple variables
cor(diabetic_multi)

#creating multiple linear regression model
diabetic_less_M.2 <- lm(time_in_hospital ~ num_medications + num_lab_procedures, data = diabetic_multi)

#Summary
summary(diabetic_less_M.2)

#Confidence Interval
confint(diabetic_less_M.2)
#Goodness of fit by RSE
summary(diabetic_less_M.2)$sigma / mean(diabetic_data.less$time_in_hospital)
