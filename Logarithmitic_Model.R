# Importing Diabetics Data set
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

#Split the data to obtain focus group of patients readmitted within 30 days
diabetic_data.less<-split(diabetic_data, diabetic_data$readmitted)$"<30"

#We are assuming "time_in_hospital" as target variable and "num_medications" as predictor variable.
#Creating a regression model
diabetic_less_M.1 <- lm(time_in_hospital ~ num_medications, data = diabetic_data.less)
summary(diabetic_less_M.1)
#Creating a multidimensional model with 2 predictors and 1 outcome variable.
diabetic_multi <- diabetic_data.less[,c('time_in_hospital','num_medications','num_lab_procedures')]
#creating multiple linear regression model
diabetic_less_M.2 <- lm(time_in_hospital ~ num_medications + num_lab_procedures, data = diabetic_multi)
summary(diabetic_less_M.2)

######################################################################################
#Working on ln(Y) =  b0 + b1X1 for simple linear model, and
#Working on ln(Y) =  b0 + b1X1 + b2X2 for multiple linear model.

#Creating a New Data set to work on Logarithmic Model
logarithmitic_dataset <- diabetic_data.less[,c('time_in_hospital','num_medications','num_lab_procedures')]

#summary of original Simple linear regression Model
summary(diabetic_less_M.1)

#########################################################################################
#Transforming Response variable using ln for logarithmic Model of the Simple Linear Model
logarithmitic_dataset$L_time_in_hospital <- log(logarithmitic_dataset$time_in_hospital)

#Converting the DF to CSV
library(readr)
write.csv(logarithmitic_dataset,file='C:/My_Files/Fall 2022/QMST 5334 - STAT METHODS/R_Directory/log_dataset.csv')

log_M1 <- lm(L_time_in_hospital~num_medications, data=logarithmitic_dataset)
summary(log_M1)

#Analysis of Residuals for Original Simple linear Model
plot(x = diabetic_data.less$num_medications, y = diabetic_less_M.1$residuals, pch=16)
plot(diabetic_less_M.1)

#Analysis of Residuals Logarithmic Simple Linear Model
plot(x = logarithmitic_dataset$num_medications, y = log_M1$residuals, pch=16)
plot(log_M1)

#Mean of residuals
mean(log_M1$residuals)
#Mean is -2.852719e-16, Therefore, the first condition that errors have expectation zero is satisfied.

library(lmtest)
#compute correlation using Durbin Watson test 
dwtest(log_M1)
#test for homoscedasticity using Breusch Pagan Test
bptest(log_M1)
#Both Durbin Watson and Breusch Pagan test Fail due to low P values of 0.014 for Durbin and <2.2e-16 for Pagan

#Histogram for the Log_Model residuals
hist(log_M1$residuals)

#test for normality Shapiro-Wilk test
shapiro.test(log_M1$residuals)
#Failed due to too many samples

#Exponent Of Coefficients
#From the Summary we have the log intercept value for response variable as (0.6810574)

#ln(y) = 0.6810574 + 0.0394301(X1)
exp(0.6810574)
exp(0.0394301)

1.04^10

#Applying e to both sides, 
#Y = 1.975966 + 1.040218^X ----------------> 1

#We can use equation 1 to predict the response variable, y, based on the value of the predictor variable, X1. 
#For example, if X1 = 10, then we would predict that y would be 3.45925, i.e.(1.9759 + 1.4833500331)

#Analysis
#From Summary the slope coefficients are Statistically significant.
# For an increase in 1% of num_medications, log(time_in_hospital) increases by 0.0394%
plot(x = logarithmitic_dataset$num_medications, y =logarithmitic_dataset$time_in_hospital , main="Learning Curve")
points(x = logarithmitic_dataset$num_medications, y=exp(log_M1$fitted.values), col="red", pch=16)

#Residual Standard Error
head(cbind(logarithmitic_dataset$time_in_hospital, exp(log_M1$fitted.values), logarithmitic_dataset$time_in_hospital-exp(log_M1$fitted.values)))

# Learning rate
2^(log_M1$coefficients[1])
# Learning Rate of num_medications is 1.027708

#Residual Standard Error
sqrt(sum((logarithmitic_dataset$num_medications- exp(log_M1$fitted.values))^2)/log_M1$df.residual)

#Real R^2
log_M1_SSE <- sum((logarithmitic_dataset$L_time_in_hospital-exp(log_M1$fitted.values))^2)
log_M1_TSE <- sum((logarithmitic_dataset$L_time_in_hospital-mean(logarithmitic_dataset$L_time_in_hospital))^2)

1- log_M1_SSE/log_M1_TSE

#########################################################################################
#logarithmic Model for the multiple Linear Model

summary(diabetic_less_M.2)
log_M2 <- lm(L_time_in_hospital~num_medications + num_lab_procedures, data=logarithmitic_dataset)
summary(log_M2)

#Analysis of Residuals for Original Multiple linear Model
plot(x = diabetic_multi$num_medications, y = diabetic_less_M.2$residuals)
plot(diabetic_less_M.2)

#Analysis of Residuals Logarithmic Multiple Linear Model
plot(x = logarithmitic_dataset$num_medications, y = log_M2$residuals, pch=16)
plot(x = logarithmitic_dataset$num_lab_procedures, y = log_M2$residuals, pch=16)
plot(log_M2)

#Mean of residuals
mean(log_M2$residuals)
#Mean is 4.349063e-17, Therefore, the first condition that errors have expectation zero is satisfied.

library(lmtest)
#compute correlation using Durbin Watson test 
dwtest(log_M2)
#test for homoscedasticity using Breusch Pagan Test
bptest(log_M2)
#Both Durbin Watson and Breusch Pagan test Fail due to low P values of 0.0404 for Durbin and <2.2e-16 for Pagan

#Histogram for the Log_Model residuals
hist(log_M2$residuals)

#test for normality Shapiro-Wilk test
shapiro.test(log_M2$residuals)
#Failed due to too many samples

#Multicolinearity
#Checking Correlation between Log(time_in_hospital) with other predictors.
pairs(logarithmitic_dataset)
round(cor(logarithmitic_dataset),4)
#There is corelation of 0.4651 between Log(time_in_hospital)and num_medications(X1)
#There is corelation of 0.3293 between Log(time_in_hospital)and num_lab_procedures(X2)

#Varience Inflation Factor
library(car)
vif(log_M2)

#Learning curve 
plot(x = dat$Units, y = dat$Cost, main="Learning Curve")
points(x = dat$Units, y=exp(m5$fitted.values), col="red", pch=16)

#Both the logarithimatic transformation residual graphs show CONCAVE plots
#Both the QQplots are a lot better, as only a few points are outside the confidence bands when compared to the original
#Therefore in both the tests mean of residual is close to zero, Histogram indicates normality, 
#but still durbin watson, breusch pagan test and shapiro watso tests failed.
#There also exists multi-collinearity between variables.









     