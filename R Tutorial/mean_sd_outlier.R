# LECTURE 2: mean, stanndard deviation and outliers

# 1. Basic Data Structures
# Integer
integer_value <- 25
integer_value

# numeric (decimal number)
numeric_value <- 25.5
numeric_value
                                        # note: character value, integer_value, they are just single objects
# character                             # but patient_names, patient_blood_pressure, they are vectors
character_value <- "Hello"              # (character, numeric vectors in the order)
character_value

# logical
logical_value <- TRUE
logical_value

# 2. Vectors
# Patient Names
patient_names <- c("Yusuf", "Omer", "Enise", "Baris", "Fatih", "Guler")
# Age Vector
patient_ages <- c(22, 22, 22, 46, 51, 46)
# Gender Vector
patient_genders <- c("Male", "Male", "female", "Female", "Male", "Female")
# Blood Pressure Levels
patient_blood_pressure <- c(120, 123, 117, 114, 125, 119)
# Patient Pulse
patient_pulse_vector <- c(80, 75, 90, 85, 88, 82)

# Print Vectors
patient_names
patient_ages
patient_genders
patient_blood_pressure

# 3. Metrices
# create a matrix containing patients' health indicators (blood pressure and pulse)
patient_indicator_matrix <- cbind("Patient Name" = patient_names, "Blood Pressur" = patient_blood_pressure, "Pulse" = patient_pulse_vector)
patient_indicator_matrix

# 4. Data Frames - we use data frames instead of metrices most of the time
patient_data <- data.frame(patient_names, patient_ages, patient_genders, patient_blood_pressure)
patient_data

# Give Better Column Names
patient_data <- data.frame(
  Name = patient_names,
  Age = patient_ages,
  Gender = patient_genders,
  BloodPressure = patient_blood_pressure
)

# Structure of data
str(patient_data)

patient_data
# Want to change one cell
patient_data$Gender[patient_data$Gender == "female"] <- "Female"
patient_data
patient_data$Gender
## patient_indicator_matrix$Gender diyemem. bu $ işaretini data framelerde kullanabilirim

str(patient_data)


# 5. Importing a Sample Health Dataset into RStudio
write.csv(patient_data, "patient_data.csv", row.names = FALSE)

# Read CSV file again
patient_data_new <- read.csv("irregular_patient_data.csv")
patient_data_new
# we dont need first column, so we remove it 
patient_data_new <- patient_data_new [,-1]
patient_data_new
# convert column names to english
names(patient_data_new) <- c("Name", "Age", "Gender", "BloodPressure", "Status", "BodyTemperature")
patient_data_new
# change gender as 0: Female, 1: Male
patient_data_new$Gender <- ifelse(patient_data_new$Gender == "Erkek", 1, 0)
patient_data_new

str(patient_data_new$Gender)  # thinks they are numeric, we should convert them to factor
patient_data_new$Gender <- as.factor(patient_data_new$Gender)
str(patient_data_new)

# -------------------

# Chech working directory
getwd()
# I dont lose my patient_data_new and I have my irregular_patient_data to working with
irregular_patient_data <- patient_data_new 
# irregular_patient_data[1,3] <- NA # first row and third col is NA. Imputation was performed using a library

# irregular_patient_data <- read.csv("irregular_patient_data2")  // error bcz I didn't create irregular_patient_data2

# categorical variables: nominal, dichotomous, ordinal
# quantitative variables: ratio scale, interval scale

str(irregular_patient_data$Status)
irregular_patient_data$Status <- as.factor(irregular_patient_data$Status)
str(irregular_patient_data$Status)
irregular_patient_data$Status <- ordered(irregular_patient_data$Status)
str(irregular_patient_data$Status)

str(irregular_patient_data$Gender)
irregular_patient_data$Gender <- as.factor(irregular_patient_data$Gender)

# are there any missing?
sum(is.na(irregular_patient_data))
is.na(irregular_patient_data)

average_age <- mean(irregular_patient_data$Age, na.rm = TRUE)

# Fill (impute) missing values (replace with mean)
irregular_patient_data$Age[is.na(irregular_patient_data$Age)] <- average_age
irregular_patient_data$Age

average_bp <- mean(irregular_patient_data$BloodPressure, na.rm = TRUE)
irregular_patient_data$BloodPressure[is.na(irregular_patient_data$BloodPressure)] <- average_bp
irregular_patient_data$BloodPressure

# Fill missing categorical values with "Unknown"

# print cleaned data
irregular_patient_data

# outlier detection
# calculate the mean and standard deviation of blood pressure values
mean_bp <- mean(irregular_patient_data$BloodPressure)
sd_bp <- sd(irregular_patient_data$BloodPressure)

# identify outliers (values beyond 2 standard deviation from the mean)
upper_limit <- (mean_bp + 2 * sd_bp)
lower_limit <- (mean_bp - 2 * sd_bp)
higher_limit <- irregular_patient_data$BloodPressure > upper_limit
belowlower_limit <- irregular_patient_data$BloodPressure < lower_limit

outliers <- higher_limit | belowlower_limit

#print outliers
print(outliers)

write.csv(patient_data_new, "patient_data_new.csv", row.names = FALSE)
write.csv(irregular_patient_data, "irregular_patient_data2.csv", row.names = FALSE)
write.csv(patient_data, "patient_data.csv", row.names = FALSE)


