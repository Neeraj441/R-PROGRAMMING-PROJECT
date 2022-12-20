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

#Splitting the age group of (30-40) from the rest
diabetic_data.lessF_30to40<-split(diabetic_data.lessF, diabetic_data.lessF$age)$"[30-40)"
diabetic_data.lessM_30to40<-split(diabetic_data.lessM, diabetic_data.lessM$age)$"[30-40)"

#Splitting the age group of (40-50) from the rest
diabetic_data.lessF_40to50<-split(diabetic_data.lessF, diabetic_data.lessF$age)$"[40-50)"
diabetic_data.lessM_40to50<-split(diabetic_data.lessM, diabetic_data.lessM$age)$"[40-50)"

#Splitting the age group of (50-60) from the rest
diabetic_data.lessF_50to60<-split(diabetic_data.lessF, diabetic_data.lessF$age)$"[50-60)"
diabetic_data.lessM_50to60<-split(diabetic_data.lessM, diabetic_data.lessM$age)$"[50-60)"

#Splitting the age group of (60-70) from the rest
diabetic_data.lessF_60to70<-split(diabetic_data.lessF, diabetic_data.lessF$age)$"[60-70)"
diabetic_data.lessM_60to70<-split(diabetic_data.lessM, diabetic_data.lessM$age)$"[60-70)"

#Splitting the age group of (70-80) from the rest
diabetic_data.lessF_70to80<-split(diabetic_data.lessF, diabetic_data.lessF$age)$"[70-80)"
diabetic_data.lessM_70to80<-split(diabetic_data.lessM, diabetic_data.lessM$age)$"[70-80)"

#Visual Inspection - Comparison of age group 30-40 and gender along with readmission within 30 days using box plots
boxplot(diabetic_data.lessF_30to40$time_in_hospital,diabetic_data.lessM_30to40$time_in_hospital,
        names=c("G1","G2"),
        main="Age group: [30-40] Comparison of Female and Male patients readmitted within 30 days",
        col=c("cyan","magenta"), 
        horizontal = TRUE, xlab = "Time in Hospital (Number of Days)")

#Visual Inspection - Comparison of age group 40-50 and gender along with readmission within 30 days using box plots
boxplot(diabetic_data.lessF_40to50$time_in_hospital,diabetic_data.lessM_40to50$time_in_hospital,
        names=c("G1","G2"),
        main="Age group: [40-50] Comparison of Female and Male patients readmitted within 30 days",
        col=c("cyan","magenta"), 
        horizontal = TRUE, xlab = "Time in Hospital (Number of Days)")

#Visual Inspection - Comparison of age group 50-60 and gender along with readmission within 30 days using box plots
boxplot(diabetic_data.lessF_50to60$time_in_hospital,diabetic_data.lessM_50to60$time_in_hospital,
        names=c("G1","G2"),
        main="Age group: [50-60] Comparison of Female and Male patients readmitted within 30 days",
        col=c("cyan","magenta"), 
        horizontal = TRUE, xlab = "Time in Hospital (Number of Days)")

#Visual Inspection - Comparison of age group 60-70 and gender along with readmission within 30 days using box plots
boxplot(diabetic_data.lessF_60to70$time_in_hospital,diabetic_data.lessM_60to70$time_in_hospital,
        names=c("G1","G2"),
        main="Age group: [60-70] Comparison of Female and Male patients readmitted within 30 days",
        col=c("cyan","magenta"), 
        horizontal = TRUE, xlab = "Time in Hospital (Number of Days)")

#Visual Inspection - Comparison of age group 70-80 and gender along with readmission within 30 days using box plots
boxplot(diabetic_data.lessF_70to80$time_in_hospital,diabetic_data.lessM_70to80$time_in_hospital,
        names=c("G1","G2"),
        main="Age group: [70-80] Comparison of Female and Male patients readmitted within 30 days",
        col=c("cyan","magenta"), 
        horizontal = TRUE, xlab = "Time in Hospital (Number of Days)")

#Initializing "Car" package for running qqplot's
library(car)

#qqplots for female and Male patients readmitted within 30 Days
qqPlot(x = diabetic_data.lessF$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Female Patients readmitted within 30 Days")
qqPlot(x = diabetic_data.lessM$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Male Patients readmitted within 30 Days")

#qqplots for Age Group of (30-40) with female and Male patients readmitted within 30 Days
qqPlot(x = diabetic_data.lessF_30to40$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Female Patients in Age group (30-40) readmitted within 30 Days")
qqPlot(x = diabetic_data.lessM_30to40$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Male Patients in Age group (30-40) readmitted within 30 Days")

#qqplots for Age Group of (40-50) with female and Male patients readmitted within 30 Days
qqPlot(x = diabetic_data.lessF_40to50$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Female Patients in Age group of (40-50) readmitted within 30 Days")
qqPlot(x = diabetic_data.lessM_40to50$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Male Patients in Age group of (40-50) readmitted within 30 Days")

#qqplots for Age Group of (50-60) with female and Male patients readmitted within 30 Days
qqPlot(x = diabetic_data.lessF_50to60$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Female Patients in Age group of (50-60) readmitted within 30 Days")
qqPlot(x = diabetic_data.lessM_50to60$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Male Patients in Age group of (50-60) readmitted within 30 Days")

#qqplots for Age Group of (60-70) with female and Male patients readmitted within 30 Days
qqPlot(x = diabetic_data.lessF_60to70$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Female Patients in Age group of (60-70) readmitted within 30 Days")
qqPlot(x = diabetic_data.lessM_60to70$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Male Patients in Age group of (60-70) readmitted within 30 Days")

#qqplots for Age Group of (70-80) with female and Male patients readmitted within 30 Days
qqPlot(x = diabetic_data.lessF_70to80$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Female Patients in Age group of (70-80) readmitted within 30 Days")
qqPlot(x = diabetic_data.lessM_70to80$time_in_hospital, ylab = "Time in Hospital (Number of Days)", main = "Male Patients in Age group of (70-80) readmitted within 30 Days")

#Most of The Data points of Our qqplot's visualization aren't entirely in the confidence bonds because we,
#have a huge data set and our data is discrete.

#Test of Significance for Age Group of (30-40)
t.test(diabetic_data.lessM_30to40$time_in_hospital,diabetic_data.lessF_30to40$time_in_hospital,
       alternative = "greater")

#Test of Significance for Age Group of (40-50)
t.test(diabetic_data.lessM_40to50$time_in_hospital,diabetic_data.lessF_40to50$time_in_hospital,
       alternative = "greater")

#Test of Significance for Age Group of (50-60)
t.test(diabetic_data.lessM_50to60$time_in_hospital,diabetic_data.lessF_50to60$time_in_hospital,
       alternative = "greater")

#Test of Significance for Age Group of (60-70)
t.test(diabetic_data.lessM_60to70$time_in_hospital,diabetic_data.lessF_60to70$time_in_hospital,
       alternative = "greater")

#Test of Significance for Age Group of (70-80)
t.test(diabetic_data.lessM_70to80$time_in_hospital,diabetic_data.lessF_70to80$time_in_hospital,
       alternative = "greater")

#end of code
