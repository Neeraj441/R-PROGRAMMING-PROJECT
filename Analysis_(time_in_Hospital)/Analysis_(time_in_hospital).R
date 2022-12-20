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

library(modest)

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

### end of code