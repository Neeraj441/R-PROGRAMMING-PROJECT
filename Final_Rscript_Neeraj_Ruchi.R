# Exploratory analysis on Time in Hospital
# Ruchi Dilip Kukde
# Neeraj Kumar Reddy Panta

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


#Checking for Null Values
sum(is.na(diabetic_data$time_in_hospital))
# Here we have No null values for the Specific Column

#Displaying Variable Type
typeof(diabetic_data$time_in_hospital)

#Creating a Histogram
hist(diabetic_data$time_in_hospital, main = 'Time Spent in Hospital by Patients(Units: Days)', 
     xlab = 'Number of days')

#Computing Mean, Median and Modal Class
#los defines(Length of stay)

mean_los <- mean(diabetic_data$time_in_hospital)

median_los <- median(diabetic_data$time_in_hospital)

mean_los 
median_los

library(modeest)

# Compute the modal class

mode_los <- mlv(diabetic_data$time_in_hospital)
mode_los

#Using Tabulate Function to check numerical values of frequency in the variable (time_in_hospital)
tabulate(diabetic_data$time_in_hospital)

#Plotting the mean, median, modal class using abline in Histogram
hist(diabetic_data$time_in_hospital, main="Time Spent in Hospital by Patients (Units: Days)",
     xlab="Number of days")
abline(v = mean(diabetic_data$time_in_hospital), col="red")
abline(v = median(diabetic_data$time_in_hospital), col="green")
abline(v = mlv(diabetic_data$time_in_hospital), col="blue")
legend("topright", c("Mean", "Median","Mode"), col=c("red", "green","blue"), lwd=2)

#Creating a BoxPlot for the selected Variable

boxplot(diabetic_data$time_in_hospital, horizontal = T, main = "Boxplot",xlab = "Number of Days")

#Displaying the Quartile Values which correspond exactly with the BoxPlot Values
fivenum(diabetic_data$time_in_hospital)

#Finding Range of the time_in_hospital Variable using Range and diff functions
range(diabetic_data$time_in_hospital)

diff(range(diabetic_data$time_in_hospital))

#Variance of the Variable
var(diabetic_data$time_in_hospital)

#Standard deviation of the Variable
sd(diabetic_data$time_in_hospital)

#Displaying the InterQuartile Range
fivenum(diabetic_data$time_in_hospital)[4]-fivenum(diabetic_data$time_in_hospital)[2]

#Strip Chart for Showing the dispersion of the Variable data, along with the Mean

stripchart(x = diabetic_data$time_in_hospital, method = "jitter",
           main="Time Spent in Hospital by Patients (Units: Days)",
           xlab="Number of days")
abline(v = mean(diabetic_data$time_in_hospital), col="red")
legend("topright", c("Mean"), col=c("red"), lwd=2)

#Displaying the density graph to understand skewness of the data.
#Based on our plot we are dealing with Positive Skewness
plot(density(diabetic_data$time_in_hospital), main="Time Spent in Hospital by Patients (Units: Days)",
     xlab="Number of days")
abline(v = mean(diabetic_data$time_in_hospital), col="red")
legend("topright", c("Mean"), col=c("red"), lwd=2)

#########################################################################################################

# Estimation of mean

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
diabetic_data.greater<-split(diabetic_data, diabetic_data$readmitted)$">30"
diabetic_data.no<-split(diabetic_data, diabetic_data$readmitted)$"NO"

#Splitting data according to gender of patients
diabetic_data.lessF<-split(diabetic_data.less, diabetic_data.less$gender)$"Female"
diabetic_data.lessM<-split(diabetic_data.less, diabetic_data.less$gender)$"Male"
diabetic_data.greaterF<-split(diabetic_data.greater, diabetic_data.greater$gender)$"Female"
diabetic_data.greaterM<-split(diabetic_data.greater, diabetic_data.greater$gender)$"Male"
diabetic_data.noF<-split(diabetic_data.no, diabetic_data.no$gender)$"Female"
diabetic_data.noM<-split(diabetic_data.no, diabetic_data.no$gender)$"Male"

#Calculating Mode
library(modest)

mlv(diabetic_data.lessF$time_in_hospital)

#Visual Inspection - Comparison using histogram

#Female patients in hospital Within 30 days
hist(diabetic_data.lessF$time_in_hospital, main = "Female Patients in Hospital admitted within 30 Days", 
     xlab = "Number of Days")
abline(v = mean(diabetic_data.lessF$time_in_hospital), col="red")
abline(v = median(diabetic_data.lessF$time_in_hospital), col="green")
abline(v = mlv(diabetic_data.lessF$time_in_hospital), col="blue")
legend("topright", c("Mean", "Median","Mode"), col=c("red", "green","blue"), lwd=2)

#Female patients in hospital after 30 days
hist(diabetic_data.greaterF$time_in_hospital, main = "Female Patients in Hospital admitted after 30 Days", 
     xlab = "Number of Days")
abline(v = mean(diabetic_data.greaterF$time_in_hospital), col="red")
abline(v = median(diabetic_data.greaterF$time_in_hospital), col="green")
abline(v = mlv(diabetic_data.greaterF$time_in_hospital), col="blue")
legend("topright", c("Mean", "Median","Mode"), col=c("red", "green","blue"), lwd=2)

#Male patients in hospital Within 30 days
hist(diabetic_data.lessM$time_in_hospital, main = "Male Patients in Hospital admitted within 30 Days", 
     xlab = "Number of Days")
abline(v = mean(diabetic_data.lessM$time_in_hospital), col="red")
abline(v = median(diabetic_data.lessM$time_in_hospital), col="green")
abline(v = mlv(diabetic_data.lessM$time_in_hospital), col="blue")
legend("topright", c("Mean", "Median","Mode"), col=c("red", "green","blue"), lwd=2)

#Male patients in hospital after 30 days
hist(diabetic_data.greaterM$time_in_hospital, main = "Male Patients in Hospital admitted after 30 Days", 
     xlab = "Number of Days")
abline(v = mean(diabetic_data.greaterM$time_in_hospital), col="red")
abline(v = median(diabetic_data.greaterM$time_in_hospital), col="green")
abline(v = mlv(diabetic_data.greaterM$time_in_hospital), col="blue")
legend("topright", c("Mean", "Median","Mode"), col=c("red", "green","blue"), lwd=2)

#Female Patients Not re-admitted
hist(diabetic_data.noF$time_in_hospital, main = "Female Patients not readmitted", 
     xlab = "Number of Days")
abline(v = mean(diabetic_data.noF$time_in_hospital), col="red")
abline(v = median(diabetic_data.noF$time_in_hospital), col="green")
abline(v = mlv(diabetic_data.noF$time_in_hospital), col="blue")
legend("topright", c("Mean", "Median","Mode"), col=c("red", "green","blue"), lwd=2)

#Male Patients Not re-admitted
hist(diabetic_data.noM$time_in_hospital, main = "Male Patients not readmitted", 
     xlab = "Number of Days")
abline(v = mean(diabetic_data.noM$time_in_hospital), col="red")
abline(v = median(diabetic_data.noM$time_in_hospital), col="green")
abline(v = mlv(diabetic_data.noM$time_in_hospital), col="blue")
legend("topright", c("Mean", "Median","Mode"), col=c("red", "green","blue"), lwd=2)

#Visual Inspection - Comparison using box plots
boxplot(diabetic_data.lessF$time_in_hospital,diabetic_data.greaterF$time_in_hospital, 
        diabetic_data.noF$time_in_hospital, diabetic_data.lessM$time_in_hospital,
        diabetic_data.greaterM$time_in_hospital, 
        diabetic_data.noM$time_in_hospital, names=c("G1", 
                                                    "G2", 
                                                    "G3",
                                                    "G4", 
                                                    "G5", 
                                                    "G6"),
        main="Comparison of the entire distrbution of values",
        col=c("cyan","magenta", "yellow", "blue", "green", "red"), 
        horizontal = TRUE, xlab = "Number of Days")

#Point estimates
mean(diabetic_data.lessF$time_in_hospital)
mean(diabetic_data.greaterF$time_in_hospital)
mean(diabetic_data.noF$time_in_hospital)
mean(diabetic_data.lessM$time_in_hospital)
mean(diabetic_data.greaterM$time_in_hospital)
mean(diabetic_data.noM$time_in_hospital)

#Create an interval for the mean of LOS for each group
t.test(diabetic_data.lessF$time_in_hospital,conf.level=0.95)
t.test(diabetic_data.greaterF$time_in_hospital,conf.level=0.95)
t.test(diabetic_data.noF$time_in_hospital,conf.level=0.95)
t.test(diabetic_data.lessM$time_in_hospital,conf.level=0.95)
t.test(diabetic_data.greaterM$time_in_hospital,conf.level=0.95)
t.test(diabetic_data.noM$time_in_hospital,conf.level=0.95)

#Confidence interval for difference in means
t.test(diabetic_data.lessF$time_in_hospital,
       diabetic_data.lessM$time_in_hospital,
       conf.level=0.95)
t.test(diabetic_data.greaterF$time_in_hospital,
       diabetic_data.greaterM$time_in_hospital,
       conf.level=0.95)
t.test(diabetic_data.noF$time_in_hospital,
       diabetic_data.noM$time_in_hospital,
       conf.level=0.95)

#########################################################################################################

# Test of significance by Fisher
# HYPOTHESIS TESTING

#Our hypothesis is that the Number of female patients and 
# Male patients who have been readmitted within 30 days is equal.

#Selecting Factor - readmitted 
typeof(diabetic_data$readmitted)
#Selecting Factor - readmitted 
typeof(diabetic_data$gender)

#Checking for Null Values
sum(is.na(diabetic_data$readmitted))
# Here we have No null values for the Specific Columns

#Split the data according to factor - readmitted
diabetic_data.less<-split(diabetic_data, diabetic_data$readmitted)$"<30"
diabetic_data.greater<-split(diabetic_data, diabetic_data$readmitted)$">30"
diabetic_data.no<-split(diabetic_data, diabetic_data$readmitted)$"NO"

#Splitting data according to gender of patients
diabetic_data.lessF<-split(diabetic_data.less, diabetic_data.less$gender)$"Female"
diabetic_data.lessM<-split(diabetic_data.less, diabetic_data.less$gender)$"Male"
diabetic_data.greaterF<-split(diabetic_data.greater, diabetic_data.greater$gender)$"Female"
diabetic_data.greaterM<-split(diabetic_data.greater, diabetic_data.greater$gender)$"Male"
diabetic_data.noF<-split(diabetic_data.no, diabetic_data.no$gender)$"Female"
diabetic_data.noM<-split(diabetic_data.no, diabetic_data.no$gender)$"Male"

#Visual Inspection - Comparison of time in hospital for patients readmitted 
#within 30 days using box plots
boxplot(diabetic_data.lessF$time_in_hospital,diabetic_data.lessM$time_in_hospital,
        names=c("G1","G2"),
        main="Comparison of time in hospital for patients readmitted within 30 days",
        col=c("cyan","magenta"), 
        horizontal = TRUE, 
        xlab = "Time in Hospital (Number of Days)")
#Visual Inspection - Comparison of time in hospital for patients readmitted 
#after 30 days
boxplot(diabetic_data.greaterF$time_in_hospital,diabetic_data.greaterM$time_in_hospital,
        names=c("G1","G2"),
        main="Comparison of time in hospital for patients readmitted after 30 days",
        col=c("cyan","magenta"), 
        horizontal = TRUE, 
        xlab = "Time in Hospital (Number of Days)")
#Visual Inspection - Comparison of time in hospital for patients not readmitted 
boxplot(diabetic_data.noF$time_in_hospital,diabetic_data.noM$time_in_hospital,
        names=c("G1","G2"),
        main="Comparison of time in hospital for patients not reported to be readmitted",
        col=c("cyan","magenta"), 
        horizontal = TRUE, 
        xlab = "Time in Hospital (Number of Days)")

#Initializing "Car" package for running qqplot's
library(car)

#qqplots for Female and Male patients readmitted within 30 Days
qqPlot(x = diabetic_data.lessF$time_in_hospital, 
       ylab = "Time in Hospital (Number of Days)", 
       main = "Female Patients readmitted within 30 Days")
qqPlot(x = diabetic_data.lessM$time_in_hospital, 
       ylab = "Time in Hospital (Number of Days)", 
       main = "Male Patients readmitted within 30 Days")
#qqplots for Female and Male patients readmitted after 30 Days
qqPlot(x = diabetic_data.greaterF$time_in_hospital, 
       ylab = "Time in Hospital (Number of Days)", 
       main = "Female Patients readmitted after 30 Days")
qqPlot(x = diabetic_data.greaterM$time_in_hospital, 
       ylab = "Time in Hospital (Number of Days)", 
       main = "Male Patients readmitted after 30 Days")
#qqplots for Female and Male patients not readmitted
qqPlot(x = diabetic_data.noF$time_in_hospital, 
       ylab = "Time in Hospital (Number of Days)", 
       main = "Female Patients not readmitted")
qqPlot(x = diabetic_data.noM$time_in_hospital, 
       ylab = "Time in Hospital (Number of Days)", 
       main = "Male Patients not readmitted")

#Most of The Data points of Our qqplot's visualization aren't 
#entirely in the confidence bonds because we,
#have a huge data set and our data is discrete.

#Test of Significance
t.test(diabetic_data.lessF$time_in_hospital,diabetic_data.lessM$time_in_hospital,
       alternative = "greater")
t.test(diabetic_data.greaterF$time_in_hospital,diabetic_data.greaterM$time_in_hospital,
       alternative = "greater")
t.test(diabetic_data.noF$time_in_hospital,diabetic_data.noM$time_in_hospital,
       alternative = "greater")

#########################################################################################################

#Linear Regression

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

##########################################################

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

##############################################################

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

#########################################################################################################

# Analysis of residuals

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

#########################################################################################################

#ANOVA and Logistic Regression

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
#################################################

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

###################################################################################################
####end of code