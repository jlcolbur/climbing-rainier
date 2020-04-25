#############################################
### Mount Rainier Weather & Climbing Analysis
## Author: Joshua Colburn
#############################################

### Part 1: Data Exploration
## Data Preperation Part 1
#Importing climbing stats and weather
library(readr)

climbing_statistics <- read_csv("IST-707/Project-Mt Rainier/Data/climbing_statistics.csv")
weather_data <- read_csv("IST-707/Project-Mt Rainier/Data/Rainier_Weather.csv")

# Extracting out the year and month- for climbing data, idea and partial code pulled from
# github user lily1917 -> https://www.kaggle.com/lily1917/full-analysis-of-successful-climbs-with-r

climbing_statistics$DATE<- as.Date(climbing_statistics$Date,"%m/%d/%Y")
climbing_statistics$YEAR<- as.numeric(format(climbing_statistics$DATE,"%Y"))
climbing_statistics$MONTH<-as.numeric(format(climbing_statistics$DATE,'%m'))

str(climbing_statistics)
head(climbing_statistics, n=10)
View(climbing_statistics)

# Doing the same for weather data
weather_data$DATE<-as.Date(weather_data$Date, format="%m/%d/%Y")
weather_data$YEAR<-as.numeric(format(weather_data$DATE, "%Y"))
weather_data$MONTH<-as.numeric(format(weather_data$DATE,"%m"))

View(weather_data)
head(weather_data, n=10)

# Merging together the two data frames using date
rainier <- merge(weather_data, climbing_statistics, by ="DATE")
View(rainier)
str(rainier)

#exploratory analysis:
plot(rainier$Attempted, rainier$Succeeded, col="blue",
     main="Attempts and Successes",
     xlab="Attempts",
     ylab="Successes")

# Getting rid of outliers...(where there were more successes than attempts)
rainier_clean <- rainier[which(rainier$Succeeded <= rainier$Attempted), ]
View(rainier_clean)
# correlation plot 

rainier_correlation <- rainier_clean
keeps <- c("Temperature AVG", "Battery Voltage AVG", "Relative Humidity AVG", "Wind Speed Daily AVG", "Wind Direction AVG",
           "Solare Radiation AVG", "Attempted", "Succeeded", "Success Percentage")

rainier_correlation <- rainier_correlation[keeps]

str(rainier_correlation)
library(ggcorrplot)
corr<-cor(rainier_correlation)

ggcorrplot(corr)


# dropping extra columns
str(rainier_clean)
head(rainier_clean)

keeps <- c("DATE", "Temperature AVG", "Battery Voltage AVG", "Relative Humidity AVG", "Wind Speed Daily AVG", "Wind Direction AVG", "Solare Radiation AVG"
           , "Route", "Attempted", "Succeeded", "Success Percentage")
df <- rainier_clean[keeps]
head(df)

## Discretizing weather and attempts to use association rule mining

# Success Percentage to Low, Medium, and High
df_ar <- df
b <- c(-Inf, 0.25, 0.75, Inf)
group_names <- c("Low", "Medium", "High")
df_ar$SuccBin <- cut(df_ar$`Success Percentage`, breaks=b,labels=group_names)

# Temperature into Freezing, Cold, and Warm
hist(df_ar$`Temperature AVG`)
b <- c(-Inf, 32, 45, Inf)
group_names <- c("Freezing", "Cold", "Warm")
df_ar$TempBin <- cut(df_ar$`Temperature AVG`, breaks=b, labels=group_names)
df_ar

# Radiation into high middle and low
hist(df_ar$`Solare Radiation AVG`)
max_radiation <- max(df_ar$`Solare Radiation AVG`)
bins = 3 
width=(max_radiation - 0)/bins; #lowest solare radiation is 0
df_ar$SolarBin = cut(df_ar$`Solare Radiation AVG`, breaks=seq(min_radiation, max_radiation, width),
                      labels=c("low radiation", "middle radiation", "high radiation"))
View(df_ar)

# Attempts into solo, small group, medium group, and large group
hist(df_ar$Attempted)
b <- c(0, 1, 4, 10, 12)
group_names <- c("Solo", "Small Group", "Medium Group", "Large Group")
df_ar$GroupSize <- cut(df_ar$Attempted, breaks=b, labels=group_names)
df_ar

#wind speed to groups
hist(df_ar$`Wind Speed Daily AVG`)
b <- c(-Inf, 10, 30, Inf)
group_names <- c("Breezy", "Windy", "Really Windy")
df_ar$Wind <- cut(df_ar$`Wind Speed Daily AVG`, breaks=b, labels=group_names)
View(df_ar)

#battery voltage to groups
hist(df_ar$`Battery Voltage AVG`)
max_battery <- max(df_ar$`Battery Voltage AVG`)
min_battery <- min(df_ar$`Battery Voltage AVG`)
bins = 3 
width=(max_battery - min_battery)/bins; #lowest solare radiation is 0
df_ar$BatteryBin = cut(df_ar$`Battery Voltage AVG`, breaks=seq(min_battery, max_battery, width),
                     labels=c("low battery", "middle battery", "high battery"))
View(df_ar)

#Histograms of main variables

par(mfrow=c(2,3))
hist(df_ar$`Temperature AVG`, main= "Temperature", xlab="Temperature")
hist(df_ar$`Success Percentage`, main= "Success Percentage", xlab="Success Percentage")
hist(df_ar$`Wind Speed Daily AVG`, main= "Wind Speed", xlab="Wind Speed")
hist(df_ar$Attempted, main= "Attempted", xlab="Attempted")
hist(df_ar$`Solare Radiation AVG`, main= "Solar Radiation", xlab="Solar Radiation")
hist(df_ar$`Battery Voltage AVG`, main= "Battery Voltage", xlab="Battery Voltage")


str(df_ar_clean)

#Creating a data frame with just categories for AR Mining
keeps <- c("BatteryBin", "Route", "SuccBin", "TempBin", "SolarBin", "GroupSize", "Wind")
df_ar_clean <- df_ar[keeps]
head(df_ar_clean)

na.omit(df_ar_clean)
View(df_ar_clean)

# Association Rule Mining
library(plyr)
library(dplyr)
library(arules)


df_ar_clean$Route <- as.factor(df_ar_clean$Route)

## Loading the transformed data into the apriori algorithm 

myRules = apriori(df_ar_clean[], parameter = list(supp = 0.01, conf = 0.9, maxlen = 4))

# Apriori algorithm with target RHS

myRulesSuccess = apriori(data = df_ar_clean, parameter = list(supp = 0.005, conf = 0.60, maxlen = 3),
                     appearance = list(default="lhs", rhs =c("SuccBin=High")),
                     control = list(verbose = F))
myRulesFailure = apriori(data = df_ar_clean, parameter = list(supp = 0.01, conf = 0.9, maxlen = 3),
                       appearance = list(default="lhs", rhs =c("SuccBin=Low")),
                       control = list(verbose = F))

## Parameters and Experiments in order to obtain strong rules
inspect(head(sort(myRules, by ="lift", decreasing=T), 50))
inspect(head(sort(myRulesSuccess, by ="lift", decreasing=T), 25))
inspect(head(sort(myRulesFailure, by ="lift", decreasing=T), 25))

# RANDOM FOREST DATA PREP
# exporting to CSV to look at the data in an easier way and do minor clean3 up.
write.csv(df,"C:/Users/joshu/OneDrive/Documents/IST-719/Project-Mt Rainier/cleaned_data.csv", row.names = FALSE)

#reimporting cleaned data, merged some routes that were the same with mispellings and removed routes with fewer than 7 rows.
library(readr)
cleaned_data <- read_csv("IST-719/Project-Mt Rainier/cleaned_data.csv", 
                           +     col_types = cols(DATE = col_date(format = "%m/%d/%Y")))

dummy_data <- cleaned_data

# creating bins to discretize success rate

b <- c(-Inf, 0.25, 0.75, Inf)

group_names <- c("Low", "Medium", "High")

dummy_data$SuccBin <- cut(dummy_data$`Success Percentage`, breaks=b,labels=group_names)

dummy_data

View(dummy_data)

# Creating csv of dummy data to manually split out a data frame for the top three routes
write.csv(dummy_data,"C:/Users/joshu/OneDrive/Documents/IST-719/Project-Mt Rainier/dummy_data.csv", row.names = FALSE)

# importing data files for top three routes
emmons <- read_csv("IST-719/Project-Mt Rainier/Data/emmons.csv")
dissapointment <- read_csv("IST-719/Project-Mt Rainier/Data/dissapointment.csv")
kautz <- read_csv("IST-719/Project-Mt Rainier/Data/kautz.csv")

kautz$SuccBin <- as.factor(kautz$SuccBin)
emmons$SuccBin <- as.factor(emmons$SuccBin)
dissapointment$SuccBin <- as.factor(dissapointment$SuccBin)

#dropping sucess metrics
kautz$Succeeded <- NULL 
kautz$`Success Percentage` <- NULL
emmons$Succeeded <- NULL 
emmons$`Success Percentage` <- NULL
dissapointment$Succeeded <- NULL 
dissapointment$`Success Percentage` <- NULL


# creating test and train data set for each route
# kautz
smp_size <- floor(0.75*nrow(kautz))
set.seed(123)
kautz_ind <- sample(seq_len(nrow(kautz)), size = smp_size)
train_kautz <- kautz[kautz_ind,]
test_kautz <- kautz[-kautz_ind,]

head(test_kautz)
View(test_kautz)
#emmons
smp_size <- floor(0.75*nrow(emmons))
set.seed(123)
emmons_ind <- sample(seq_len(nrow(emmons)), size = smp_size)
train_emmons <- emmons[emmons_ind,]
test_emmons <- emmons[-emmons_ind,]

head(test_emmons)
#dissapointment
smp_size <- floor(0.75*nrow(dissapointment))
set.seed(123)
dissapointment_ind <- sample(seq_len(nrow(dissapointment)), size = smp_size)
train_dissapointment <- dissapointment[dissapointment_ind,]
test_dissapointment <- dissapointment[-dissapointment_ind,]

head(test_dissapointment)

## Part 2: Models
# KNN Model for evaluating success bins
library(ggplot2)
library(caret)
library(mlbench)

model_knn_dis <- caret::train(SuccBin ~ ., data=train_dissapointment, method = "knn")
print(model_knn_dis)
predict_knn_dis <- predict(model_knn_dis, newdata = test_dissapointment)
confusionMatrix(predict_knn_dis, test_dissapointment$SuccBin, positive = "pos")

model_knn_emmons <- caret::train(SuccBin ~ ., data=train_emmons, method = "knn")
print(model_knn_emmons)
predict_knn_emmons <- predict(model_knn_emmons, newdata = test_emmons)
confusionMatrix(predict_knn_emmons, test_emmons$SuccBin, positive = "pos")

model_knn_kautz <- caret::train(SuccBin ~ ., data=train_kautz, method = "knn")
print(model_knn_kautz)
predict_knn_kautz <- predict(model_knn_kautz, newdata = test_kautz)
confusionMatrix(predict_knn_kautz, test_kautz$SuccBin, positive = "pos")



# Random Forest
model_rf_dis <- caret::train(SuccBin ~ ., data=train_dissapointment, method = "rf")

print(model_rf_dis)

predict_rf_dis <- predict(model_rf_dis, newdata = test_dissapointment)

confusionMatrix(predict_rf_dis, test_dissapointment$SuccBin, positive = "pos")

varimp_rf_dis <- varImp(model_rf_dis)
varimp_rf_dis

plot(varimp_rf_dis, main = "Variable Importance with Random Forest")

str(df)
