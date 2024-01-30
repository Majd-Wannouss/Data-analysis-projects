#import the libraries
library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(zoo)
library(lubridate)
library(tseries)
library(palmerpenguins) 
library(ggplot2)

#set the work directory 
setwd("D:/New folder")

# import the data
dat <- read_csv("Walmart_Store_sales.csv")

# subset the data to get the data on the first store
df_main=subset(dat, Store == 1)

# Count the number of missing values in each column
missing_counts <- colSums(is.na(df_main))

# Display the result
print(missing_counts)
# check if weekly sales have negative values 
subset(df_main, Weekly_Sales < 0)

# calculate correlation 
corrr= cor(df_main[,3:8])

#change the type of the Date column to (Date type format)
df_main$Date <- as.Date(df_main$Date, format = ("%d-%m-%Y"))

# plot the all the main columns in the dataset and export the plot to png file
png("test.png", 600, 1000)
layout(matrix(c(1,2,3,4,5), ncol=1, byrow=TRUE))
plot(df_main$Date,df_main$Weekly_Sales, type = "l", col = 3, 
     xlab = "Year",ylab="Sales",
     cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4)
plot(df_main$Date,df_main$Temperature, type = "l", col = 5,
     xlab = "Year",ylab="Temperature",
     cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4)
plot(df_main$Date,df_main$Fuel_Price, type = "l", col = 6,
     xlab = "Year",ylab="Fuel Price",
     cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4)
plot(df_main$Date,df_main$CPI, type = "l", col = 7,
     xlab = "Year",ylab="CPI",
     cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4)
plot(df_main$Date,df_main$Unemployment, type = "l", col = 8,
     xlab = "Year",ylab="Unemployment",
     cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4)
dev.off()


#set the index of the dataset to be Date column to insure the the dataset
#is ranked correctly throughout the code
df <- df %>% 
  arrange(Date) %>%  # Ensure the data is sorted by date
  tibble::column_to_rownames(var = "Date")

#split the dataset
df_train <- df[1:115,]
df_test <- df[116:143,]

#create a time series object
dat_ts <- ts(df_train[1:115,2], freq=365.25/7, start=decimal_date(ymd("2010-02-05")))

#create a function to calcuclate MAPE values
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

# plot the time series decomposition and exported to png file
tscomponents <- decompose(dat_ts)
png("test_2.png", 600, 800)
plot(tscomponents)
dev.off()


# Perform Augmented Dickey-Fuller test
adf_result <- adf.test(df[1:143,2])

# Print the ADF test results
print(adf_result)

#ARIMA models:
#all the forcasts of the ARIMA models will be exported as png files

#ARIMA without external variables
arima_model <- auto.arima(dat_ts,seasonal = TRUE,trace = TRUE)
summary(arima_model)
fore_arima = forecast(arima_model, h=28)
summary(fore_arima)
png("ARIMA_1.png", 600, 400)
autoplot(forecast(arima_model, h=28), ylab="weekly sales")
dev.off()
df_arima = data.frame(fore_arima)
df_test$arima = df_arima$`Point.Forecast`
mape(df_test$Weekly_Sales, df_test$arima)


#ARIMA with the first external variable with highest correlation value:

arima_model <- auto.arima(dat_ts,xreg = as.matrix(df_train[,6]),
                          seasonal = TRUE,trace = TRUE)
summary(arima_model)
fore_arima = forecast(arima_model, xreg = as.matrix(df_test[,6]))
summary(fore_arima)
png("ARIMA_2.png", 600, 400)
autoplot(fore_arima, ylab="weekly sales")
dev.off()
df_arima = data.frame(fore_arima)
df_test$arima = df_arima$`Point.Forecast`
mape(df_test$Weekly_Sales, df_test$arima)



#ARIMA with the first two external variables with highest correlation value:

arima_model <- auto.arima(dat_ts,xreg = as.matrix(df_train[,c(4,6)]),
                          seasonal = TRUE,trace = TRUE)
summary(arima_model)
fore_arima = forecast(arima_model, xreg = as.matrix(df_test[,c(4,6)]))
summary(fore_arima)
png("ARIMA_3.png", 600, 400)
autoplot(fore_arima, ylab="weekly sales")
dev.off()
df_arima = data.frame(fore_arima)
df_test$arima = df_arima$`Point.Forecast`
mape(df_test$Weekly_Sales, df_test$arima)


#ARIMA with the firest three external variables with highest correlation value:

arima_model <- auto.arima(dat_ts,xreg = as.matrix(df_train[,c(3,4,6)]),
                          seasonal = TRUE,trace = TRUE)
summary(arima_model)
fore_arima = forecast(arima_model, xreg = as.matrix(df_test[,c(3,4,6)]))
summary(fore_arima)
png("ARIMA_4.png", 600, 400)
autoplot(fore_arima, ylab="weekly sales")
dev.off()
df_arima = data.frame(fore_arima)
df_test$arima = df_arima$`Point.Forecast`
mape(df_test$Weekly_Sales, df_test$arima)


















