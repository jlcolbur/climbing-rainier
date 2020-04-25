#############################################
### Visualization Preparation: Mount Rainier Weather & Climbing Analysis
## Author: Joshua Colburn
#############################################

## Part 1: Data Preparation
# Importing climbind stats and weather
library(readr)
library(ggplot2)
library(tidyr)
library(ggcorrplot)
library(lubridate)
library(dplyr)

climbing_statistics <- read_csv("IST-719/Project-Mt Rainier/Data/climbing_statistics.csv")
weather_data <- read_csv("IST-719/Project-Mt Rainier/Data/Rainier_Weather.csv")

# Extracting out the year and month- for climbing data, idea and partial code pulled from
# github user lily1917 -> https://www.kaggle.com/lily1917/full-analysis-of-successful-climbs-with-r

climbing_statistics$DATE<- as.Date(climbing_statistics$Date,"%m/%d/%Y")
climbing_statistics$YEAR<- as.numeric(format(climbing_statistics$DATE,"%Y"))
climbing_statistics$MONTH<-as.numeric(format(climbing_statistics$DATE,'%m'))

head(climbing_statistics, n=10)

# Doing the same for weather data
weather_data$DATE<-as.Date(weather_data$Date, format="%m/%d/%Y")
weather_data$YEAR<-as.numeric(format(weather_data$DATE, "%Y"))
weather_data$MONTH<-as.numeric(format(weather_data$DATE,"%m"))

head(weather_data, n=10)

# Merging together the two data frames using date
rainier <- merge(weather_data, climbing_statistics, by ="DATE")
rainier
str(rainier)

# Checking for outliers
plot(rainier$Attempted, rainier$Succeeded, col="blue",
     main="Attempts and Successes",
     xlab="Attempts",
     ylab="Successes")

# Getting rid of outliers...(where there were more successes than attempts)
rainier_clean <- rainier[which(rainier$Succeeded <= rainier$Attempted), ]

## Part 2: Single Dimension Plots
# How Many people climb together?
par = par
h1 <- as.factor(rainier_clean$Attempted)
plot(h1, col="#4ea2a6")

# How often do they summit?

h2 = hist(rainier_clean$`Success Percentage`)
h2$density = h2$counts/sum(h2$counts)*100
plot(h2,freq=FALSE, col="#004043", ylim=c(0, 50))

## Part 3: Multi-Dimension Plots
# when do most people climb Mt. Rainier? and When are they most successful?
df_index <- rainier_clean[rainier_clean$YEAR.x==2015,] #looking just at 2015
df1 <- df_index %>% group_by(month=floor_date(DATE, "month")) %>%
  summarize(Attempted=sum(Attempted)) #summing attempted by month
df2 <- df_index %>% group_by(month=floor_date(DATE, "month")) %>%
  summarize(SuccPerc=mean(`Success Percentage`)) #averaging success % by month
r1 <- merge(df1, df2, by ="month") # joining aggregated dataframes
r1$month<-as.numeric(format(r1$month,'%m')) #converting month to numeric
# Sequential color scheme. 
# Specify the colors for low and high ends of gradient
r_when <- ggplot(r1, aes(x= factor(month), y = Attempted)) + 
  geom_bar(width=1, stat="identity", aes(fill = SuccPerc)) + 
  scale_fill_gradient(high = "#E66B92", low = "#FFDF77")
r_when + coord_polar() + ggtitle("When do people attempt to summit?")

# Where are the most successful routes?
df_route1 <- df_index %>% group_by(Route) %>%
  summarize(Attempted=sum(Attempted)) #summing attempted by route
df_route2 <- df_index %>% group_by(Route) %>%
  summarize(SuccPerc=mean(`Success Percentage`)) #averaging success % by route
df_route <- merge(df_route1, df_route2, by ="Route") # joining aggregated dataframes
str(df_route)

# Sequential color scheme. 
# Specify the colors for low and high ends of gradient

r_where <- ggplot(df_route, aes(x= factor(Route), y = SuccPerc)) + 
  geom_bar(width=1, stat="identity", aes(fill = Attempted)) + 
  scale_fill_gradient(high = "#23666e", low = "#FFDF77", trans="log")

r_where + ggtitle("What Routes see the most successful summits?") + 
  theme(axis.text.x = element_text(angle=90))


# Weather to climb or not
# correlation plot:
rainier_correlation <- rainier_clean
keeps <- c("Temperature AVG", "Battery Voltage AVG", "Relative Humidity AVG", "Wind Speed Daily AVG", "Wind Direction AVG",
           "Solare Radiation AVG", "Success Percentage")
rainier_correlation <- rainier_correlation[keeps]
str(rainier_correlation)
corr<-cor(rainier_correlation)
ggcorrplot(corr,method="circle", colors=c("#004043","white","#E66B92"))

# from the correlation plot we can see that success percentage is postively correlated to Solare Radiation, and negatively correlated to wind speed daily average
r_weather <- ggplot(rainier_clean, aes(x= `Wind Speed Daily AVG`, y = `Solare Radiation AVG`)) + 
  geom_point(size=3, shape=23, aes(fill = `Success Percentage`)) + 
  scale_fill_gradient(high = "#E66B92", low = "#FFDF77") + ggtitle("Sun, Wind, and Success")

r_weather
