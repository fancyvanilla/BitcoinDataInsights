#installing libraries
install.packages("ggplot2")
install.packages("lubridate")
install.packages("GGally")
install.packages("prophet")


#loading libraries
library("ggplot2")
library("lubridate")
library("GGally")
library("prophet")

#setting the working directory (it depends on your wd)
setwd("C:/Users/marye/Documents/R")

#reading the data
train <- read.csv("data.csv",header=TRUE)

#viewing the data
head(train)
summary(train$Price)
View(train)
str(train)

#cleaning the data
train$Price <- as.numeric(gsub(",", "", train$Price))
train$Open <- as.numeric(gsub(",", "", train$Open))
train$High <- as.numeric(gsub(",", "", train$High))
train$Low <- as.numeric(gsub(",", "", train$Low))
train$Price <- as.numeric(train$Price)
train$Date <- mdy(train$Date)
train$Change <- as.numeric(gsub("%", "", train$Change))
train$Change <- train$Change/100
train$Vol <- as.numeric(gsub("K", "", train$Vol))*1000
train$Vol. <- NULL
train$Change.. <- NULL

# Check for missing values in each column
sapply(train, function(x) sum(is.na(x)))

# Descriptive statistics
summary(train[, c("Price", "Open", "High", "Low", "Vol", "Change")])

# Plot Bitcoin Closing Price over Time
cp <- ggplot(train, aes(Date, Price)) + geom_line() + scale_x_date(date_breaks = "years", date_labels = "%Y" ,limits = as.Date(c("2010-07-18","2024-02-09"))) + ylab("Price ($)") + xlab("Year") + ylim(0,70000)
cp + theme_bw() + labs(title="Bitcoin Price") + geom_line(size = 1, colour = "red")

# Scatterplot Matrix
ggpairs(train[, c("Price", "Open", "High", "Low", "Vol", "Change")])

# Simple Linear Regression
model <- lm(Price ~ Open + High + Low + Vol + Change, data = train)

# View summary of the model
summary(model)

# Residuals vs Fitted Values Plot
plot(model, which = 1)

# Call the Prophet Function to Fit the Model
colnames(train)[colnames(train) == "Date"] <- "ds"
colnames(train)[colnames(train) == "Price"] <- "y"

Model <- prophet(train,daily.seasonality=TRUE)
future <- make_future_dataframe(Model, periods = 365)
tail(future)

#Forecast Proper
forecast <- predict(Model, future)
tail(forecast[c('ds','yhat','yhat_lower','yhat_upper')])
max(train$y)
class(forecast)
View(forecast)

#Plotting the forecast
dyplot.prophet(Model, forecast)

#patterns in the data
prophet_plot_components(Model, forecast)



