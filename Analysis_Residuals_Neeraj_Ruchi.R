# Analysis of residuals

#Ruchi Dilip Kukde
#Neeraj Kumar Reddy Panta

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

library(lmtest) # DW and BP Test
library(car) # qqPlot
#plotting residuals against the predictor for model 1
plot(x = diabetic_data.less$num_medications, y = diabetic_less_M.1$residuals) 
#compute average residuals for model 1
mean(diabetic_less_M.1$residuals)
#compute correlation using Durbin Watson test 
dwtest(diabetic_less_M.1)
#test for homoscedasticity using Breusch Pagan Test
bptest(diabetic_less_M.1)
#Examination of distribution of residuals using histogram and qqplots
hist(diabetic_less_M.1$residuals, col="tan")
qqPlot(diabetic_less_M.1$residuals)
#test for normality Shapiro-Wilk test
shapiro.test(diabetic_less_M.1$residuals)
#plotting residuals against the two predictors for model 2
plot(x = diabetic_multi$num_medications, y = diabetic_less_M.2$residuals)
plot(x = diabetic_multi$num_lab_procedures, y = diabetic_less_M.2$residuals)
#compute average residuals for model 2
mean(diabetic_less_M.2$residuals)
#compute correlation using Durbin Watson test 
dwtest(diabetic_less_M.2)
#test for homoscedasticity using Breusch Pagan Test
bptest(diabetic_less_M.2)
#Examination of distribution of residuals using histogram and qqplots
hist(diabetic_less_M.2$residuals, col="tan")
qqPlot(diabetic_less_M.2$residuals)
#multicollinearity
pairs(diabetic_multi)
round(cor(diabetic_multi),4)
vif(diabetic_less_M.2)
###end of code