#installing libraries
install.packages("forecast")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("GGally")

#loading libraries
library("forecast")
library("ggplot2")
library("lubridate")
library("GGally")

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

# Create forecast dataframe with dates
forecast_dates <- seq.Date(max(train$Date) + 1, max(train$Date) + 30, by = "day")

forecast_df <- data.frame(
  Date = forecast_dates,
  Open = rep(mean(train$Open, na.rm = TRUE), 30),  # Placeholder for Open prices
  High = rep(mean(train$High, na.rm = TRUE), 30),  # Placeholder for High prices
  Low = rep(mean(train$Low, na.rm = TRUE), 30),    # Placeholder for Low prices
  Vol = rep(mean(train$Vol, na.rm = TRUE), 30),  # Placeholder for Volume
  Change = rep(mean(train$Change, na.rm = TRUE), 30)  # Placeholder for Change %
)

# Make sure forecast_df has the same column names as train
colnames(forecast_df) <- colnames(train)

# Predict using the linear regression model
forecast_values <- predict(model, newdata = forecast_df)

# Add forecast values to forecast_df
forecast_df$Forecast <- forecast_values

# Plot forecast
ggplot() +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast), color = "red") +
  labs(title = "Linear Regression Forecast for Bitcoin Price",
       x = "Date",
       y = "Price") +
  scale_x_date(limits = c(min(train$Date), max(forecast_df$Date)), 
               date_breaks = "1 month", 
               date_labels = "%b %Y", 
               expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





