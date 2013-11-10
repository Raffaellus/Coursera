#################################################
# Coursera - Computing for Data Analysis        #
# Programming Assignment 1 Quiz                 #
#################################################

data <- read.csv("hw1_data.csv", header=TRUE)

# Q1: column names of the dataset
names(data)

# Q2: first two rows
head(data)

# Q3: number of observations
str(data)

# Q4: last two rows
tail(data)

# Q5: value of Ozone in the 47th row
data["Ozone"][47,]

# Q6: How many missing values are in the Ozone column?
sum(is.na(data["Ozone"]))

# Q7: mean of the Ozone column (Exclude missing values)
new_Ozone <- data["Ozone"][!is.na(data["Ozone"])]
mean(new_Ozone)

# Q8: Extract the subset of rows of the data where 
# Ozone values are above 31 and Temp values are above 90.
# What is the mean of Solar.R?
index <- (data["Ozone"] > 31 & data["Temp"] > 90 & !is.na(data["Ozone"]))
new_solar <- data["Solar.R"][index]
mean(new_solar)

# Q9: mean of Temp when Month = 6
june_temp <- data["Temp"][data["Month"] == 6]
mean(june_temp)

# Q10: max of Ozone value in May
may_ozone <- data["Ozone"][data["Month"] == 5]
max(may_ozone[!is.na(may_ozone)])