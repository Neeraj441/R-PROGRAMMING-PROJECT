# Ruchi Dilip Kukde
# Neeraj Kumar Reddy Panta

# Test of significance by Fisher
# HYPOTHESIS TESTING

#Our hypothesis is that the Number of female patients and 
# Male patients who have been readmitted within 30 days is equal.

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
#end of code
