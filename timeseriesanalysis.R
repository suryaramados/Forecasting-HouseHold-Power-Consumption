
#time series forecasing power consumption


install.packages("RMySQL",type = "source")
library(RMySQL)
library(DBI)

library(dplyr)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'iris')


irisALL <- dbGetQuery(con, "SELECT * FROM iris")
str(irisALL)

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")
str(irisSELECT)


str(yr6)
summary(yr6)
head(yr6) 
tail(yr6)

dbListFields(con,'yr_2007')



yr6 <- dbGetQuery(con,"SELECT Date,Time, Sub_metering_1,Sub_metering_2, Sub_metering_3  FROM yr_2006")
str(yr6)
summary(yr6)


yr7 <- dbGetQuery(con,"SELECT Date,Time, Sub_metering_1,Sub_metering_2, Sub_metering_3  FROM yr_2007")
summary(yr7)
yr7
yr8 <- dbGetQuery(con,"SELECT Date,Time, Sub_metering_1,Sub_metering_2, Sub_metering_3  FROM yr_2008")
summary(yr8)
yr8

yr9 <- dbGetQuery(con,"SELECT Date,Time, Sub_metering_1,Sub_metering_2, Sub_metering_3  FROM yr_2009")
summary(yr9)


yr10 <- dbGetQuery(con,"SELECT Date,Time, Sub_metering_1,Sub_metering_2, Sub_metering_3  FROM yr_2010")
summary(yr10)

yr <- bind_rows(yr6,yr7,yr8,yr9,yr10)
yr
## Combine Date and Time attribute values in a new attribute column
newdf <- cbind(yr,paste(yr$Date,yr$Time), stringsAsFactors=FALSE)
newdf

colnames(newdf)[6] <-"DateTime"

## Move the DateTime attribute within the dataset
newdf <- newdf[,c(ncol(newdf), 1:(ncol(newdf)-1))]
head(newdf)


## Convert DateTime from character to POSIXct 
newdf$DateTime <- as.POSIXct(newdf$DateTime, "%Y/%m/%d %H:%M:%S")
newdf$DateTime

## Add the time zone to prevent warning messages
attr(newdf$DateTime, "tzone") <- "Europe/Paris"

install.packages("lubridate")

library(lubridate)

## Create "year" attribute with lubridate

newdf$year <- year(newdf$DateTime)
newdf$year

summary(newdf)

## Inspect the data types
str(newdf)


yr1 <- bind_rows(yr7,yr8,yr9)
str(yr1)

plot(yr1$Sub_metering_1)

## Subset the second week of 2008 - All Observations
houseWeek <- yr8[8:14,]
houseWeek


## Subset the first week of 2010 - All Observations
houseWeek10 <- yr10[71:77,]
houseWeek10

table(is.na(yr8))
yr8

yr9
table(is.na(yr9))
## Subset the 9th day of January 2008 - All observations
houseDay <- filter(yr8, Date== "2008-01-09")
houseDay

## Subset the 3rd day of January 2010 - All observations
houseDay03 <- filter(yr10, Date== "2010-01-03")
houseDay03


#
plot(houseDay$Sub_metering_1)

## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)



library(shiny)
devtools::install_version("shiny", version = "0.14")



library(plotly)

## Subset the 9th day of January 2008 - All observations
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$Time, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')



## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$Time, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



## Plot sub-meter 1, 2 and 3 with title, 3rd jan 2010- All observations 
plot_ly(houseDay03, x = ~houseDay03$Time, y = ~houseDay03$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay03$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay03$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 3rd, 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(houseDay, Time =="00:00:00" | Time == "00:10:00" | Time == "00:20:00" | Time == "00:30:00" | Time == "00:40:00" | Time == "00:50:00")
houseDay10



## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$Time, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))




## Plot sub-meter 1, 2 and 3 with title, All observations first week 2010
plot_ly(houseWeek10, x = ~houseWeek10$Time, y = ~houseWeek10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January first week, 2010",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
yr7



house07weekly <- filter(yr7, Time== "20:00:00")
house07weekly


house08weekly <- filter(yr8, Time== "20:00:00")
house08weekly

house09weekly <- filter(yr9, Time== "20:00:00")
house09weekly

house070809weekly <- rbind(house07weekly,house08weekly, house09weekly)
house070809weekly

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, house070809weekly$Time== "20:00:00", frequency=52, start=c(2007,1), end=c(2009,12))
tsSM3_070809weekly


## Create TS object with SubMeter2
tsSM3_070809weeklysubmeter2 <- ts(house070809weekly$Sub_metering_2, house070809weekly$Time== "20:00:00", frequency=26, start=c(2008,1), end=c(2009,12))
tsSM3_070809weeklysubmeter2

## Create TS object with SubMeter1
tsSM3_070809weeklysubmeter1 <- ts(house070809weekly$Sub_metering_1, house070809weekly$Time== "20:00:00", frequency=40, start=c(2007,8), end=c(2009,5))
tsSM3_070809weeklysubmeter1


library(ggplot2)
library(ggfortify)
autoplot(tsSM3_070809weekly)
autoplot(tsSM3_070809weeklysubmeter2)
autoplot(tsSM3_070809weeklysubmeter1)


## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")

autoplot(tsSM3_070809weeklysubmeter2, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")


autoplot(tsSM3_070809weeklysubmeter1, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")


## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)
plot.ts(tsSM3_070809weeklysubmeter2)
plot.ts(tsSM3_070809weeklysubmeter1)





## Apply time series linear regression to the sub-meter  ts object and use summary to obtain R2 and RMSE from the model you built
library(forecast)
fitSM2 <- tslm(tsSM3_070809weeklysubmeter2 ~ trend + season) 
summary(fitSM2)


fitSM1 <- tslm(tsSM3_070809weeklysubmeter1 ~ trend + season) 
summary(fitSM1)



fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)




## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create the forecast for sub-meter 2. Forecast ahead 20 time periods 
forecastfitSM2 <- forecast(fitSM2, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM2)


## Create the forecast for sub-meter 1. Forecast ahead 20 time periods 
forecastfitSM1 <- forecast(fitSM1, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM1)



# Create sub-meter 3 forecast with confidence levels 80 and 90. forecast portion that is above zero. 
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")




# Create sub-meter 2 forecast with confidence levels 80 and 90. forecast portion that is above zero. 
forecastfitSM2c <- forecast(fitSM2, h=20, level=c(80,90))

## Plot sub-meter 2 forecast, limit y and add labels
plot(forecastfitSM2c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")





# Create sub-meter 1 forecast with confidence levels 80 and 90. forecast portion that is above zero. 
forecastfitSM1c <- forecast(fitSM1, h=20, level=c(80,90))

## Plot sub-meter 1 forecast, limit y and add labels
plot(forecastfitSM1c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")




## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)


## Create TS object with SubMeter2
tsSM3_070809weeklysm2 <- ts(house070809weekly$Sub_metering_2, house070809weekly$Time== "20:00:00", frequency=52, start=c(2007,1), end=c(2009,12))
tsSM3_070809weeklysm2

## Create TS object with SubMeter1
tsSM3_070809weeklysm1 <- ts(house070809weekly$Sub_metering_1, house070809weekly$Time== "20:00:00", frequency=52, start=c(2007,1), end=c(2009,12))
tsSM3_070809weeklysm1





#Sub-meter 1 decomposed plot with your choice of frequency and time period
#Sub-meter 2 decomposed plot with your choice of frequency and time


## Decompose Sub-meter 2 into trend, seasonal and remainder
components070809SM2weekly <- decompose(tsSM3_070809weeklysm2)


## Plot decomposed sub-meter 2 
plot(components070809SM2weekly)
## Check summary statistics for decomposed sub-meter 2 
summary(components070809SM2weekly)



## Decompose Sub-meter 1 into trend, seasonal and remainder
components070809SM1weekly <- decompose(tsSM3_070809weeklysm1)


## Plot decomposed sub-meter 1 
plot(components070809SM1weekly)
## Check summary statistics for decomposed sub-meter 1 
summary(components070809SM1weekly)




## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)


## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- tsSM3_070809weeklysm2 - components070809SM2weekly$seasonal
autoplot(tsSM2_070809Adjusted)

## Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- tsSM3_070809weeklysm1 - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)



## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))
plot(decompose(tsSM2_070809Adjusted))
plot(decompose(tsSM1_070809Adjusted))


## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

## Holt Winters Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25))


## Holt Winters Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25))




## HoltWinters forecast & plot
#Having created a ts object that contains exponentially smoothed data with no seasonality, 
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")


## HoltWinters forecast & plot
#Having created a ts object that contains exponentially smoothed data with no seasonality, 
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")


## HoltWinters forecast & plot
#Having created a ts object that contains exponentially smoothed data with no seasonality, 
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")







#Lastly, let's change the the confidence levels and then plot only the forecasted area. 
#Think of this just as you would when a weatherperson forecasts the weather: 
#They don't include the preceding years, weeks and days. 

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))



## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))



## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))



