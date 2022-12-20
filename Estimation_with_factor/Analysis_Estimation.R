# Estimation of mean
# Ruchi Dilip Kukde
# Neeraj Kumar Reddy Panta


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
#end of code