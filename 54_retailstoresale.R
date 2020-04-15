#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggplot2")
library(ggplot2) # Data visualization
#install.packages("readr")
library(readr) # CSV file I/O, e.g. the read_csv function
#install.packages(dplyr)
library(dplyr)
#install.packages("zoo")
library(zoo)
#install.packages("forecast")
library(forecast)
#install.packages("DAAG")
library(DAAG)

#read the csv file 
Path<-"C:/Users/Parth Shah/Desktop/krish/R/54_retailstoresaletrain.csv"
train <- read.csv(Path, header =  TRUE, sep = ',')

Path<-"C:/Users/Parth Shah/Desktop/krish/R/54_retailstoresale.csv"
store <- read.csv(Path, header =  TRUE, sep = ',')

train
store

#Display all the column names
names(train)
names(store)

#str to find the structure of the data
str(train)
str(store)

#statistical description
summary(train)
summary(store)

#to find if any null value in columns
colSums(is.na(train))
colSums(is.na(store))

#to find if any null value in rows
rowSums(is.na(train))
rowSums(is.na(store))

#Checking for missing values in the entire dataframe
any(is.na(train))
any(is.na(store))

#Checking for the total number of missing values in the entire dataframe
sum(is.na(train))
sum(is.na(store))

#Checking for the total number of missing values in a particular column
sum(is.na(train$Store))
sum(is.na(train$Sales))
sum(is.na(train$Customers))

sum(is.na(store$Store))
sum(is.na(store$StoreType))
sum(is.na(store$Assortment))

#Eliminating missing values completely from the entire dataframe
train = na.omit(train)
store = na.omit(store)

#Checking for missing values in the entire dataframe
any(is.na(train))
any(is.na(store))

#Histogram for Sales
hist(train$Sales, 100)

#Histogram for sales when the store was not closed
hist(aggregate(train[train$Sales != 0,]$Sales, by = list(train[train$Sales != 0,]$Store), mean)$x, 100, main = "mean sales per store when store was not closed")

#Histogram of Customers
hist(train$Customers, 100)

#Histogram of customers in store when store was not closed
hist(aggregate(train[train$Sales != 0,]$Customers, by = list(train[train$Sales != 0,]$Store), mean)$x, 100, main = "Mean customers per store when store was not closed")

#Sales of store 85 on Sunday
ggplot(train[train$Store == 85,], 
       aes(x = Date, y = Sales, 
           color = factor(DayOfWeek == 7), shape = factor(DayOfWeek == 7))) + 
    geom_point(size = 3) + ggtitle("Sales of store 85 on Sunday")
    

#Merge store and train data
data = merge(train,store,by="Store")
summary(data)

#Which are the top 10 Store who has highest Sales?
top = data %>%
group_by(Store,StoreType) %>%
summarize(Total.Sales=sum(Sales),No.Cust=sum(Customers),Avg.Sales=mean(Sales),Comp.Dist=mean(CompetitionDistance)) %>%
mutate(Avg.Sales.per.Cust=Total.Sales/No.Cust)%>%
arrange(desc(Total.Sales))

head(top,n=10)

#Which are the Store who has Lowest Sales?
tail(top,n=10)

#install.packages("ggplot2")
library(ggplot2)

#Distribution of Total number of customer visited the store vs total sales
ggplot(top,aes(No.Cust,Total.Sales,color=StoreType))+geom_point()+ggtitle("Total Number of Customers Visted vs Total Sales")+xlab("Total Number of Customers Visted ")+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#F0E442"))

#Competitor store distance vs total sales based on the store type
ggplot(top,aes(Comp.Dist,Total.Sales,shape=StoreType,color=Avg.Sales.per.Cust))+geom_point()+ggtitle("Competitor Store Distance vs Total Sales")+xlab("Competitor Store Distance")

#top 10 store based on total sales revenue
top10 = head(top,n=10)
list10 = unique(top10$Store)

#Lubridate library to handle date functions easily
top10data = filter(data,Store %in% list10)
library(lubridate)
top10data$Date = ymd(top10data$Date)
top10data$year = year(top10data$Date)
top10data$month = month(top10data$Date)
top10data$wday = wday(top10data$Date)
top10data$day = day(top10data$Date)
top10data$week = week(top10data$Date)
top10data$yday = yday(top10data$Date)

head(top10data)

#Sales of top 10 stores based on Date
ggplot(top10data,aes(Date,Sales,color=factor(Promo)))+geom_point()+facet_wrap(~Store)

#Sales based of weekday
top10data %>%
filter(Store==262 | Open == 1,Sales > 1) %>%
ggplot(aes(factor(wday),Sales,color=factor(Promo)))+geom_bar(stat="identity")+facet_wrap(~year)

#storewise revenue
stores_revenue <- train %>% group_by(Store) %>% summarise(revenue = sum(Sales))
g <- ggplot(data = stores_revenue, aes(x= Store, y = revenue))
g <- g + geom_line()
g <- g + xlab('Stores') + ylab('Total Revenue')
g <- g + ggtitle('Revenue of stores')
plot(g)
rm(g)
rm(stores_revenue)

# Sales/Revenue distribution by week days i.e DayOfWeek
revenue_by_DayOfWeek <- train %>% group_by(DayOfWeek) %>% summarise(revenue = sum(Sales))
g <- ggplot(data = revenue_by_DayOfWeek, aes(x= DayOfWeek, y = revenue))
g <- g + geom_bar(stat="identity")
g <- g + xlab('Day of Week') + ylab('Total Revenue')
g <- g + ggtitle('Revenue on each days')
plot(g)
rm(g)
rm(stores_revenue)

train_store <- merge(train, store, by = "Store")

#The different store types and assortment types imply different overall levels of sales and seem to be exhibiting different trends
ggplot(train_store[train$Sales != 0,], 
       aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + 
  geom_smooth(size = 2)

#Customer growth based on Store type over a period of time
ggplot(train_store[train$Customers != 0,], 
       aes(x = as.Date(Date), y = Customers, color = factor(StoreType))) + 
  geom_smooth(size = 2)

#The effect of the distance to the next competitor is a little counterintuitive
salesByDist <- aggregate(train_store[train_store$Sales != 0 & !is.na(train_store$CompetitionDistance),]$Sales, 
                         by = list(train_store[train_store$Sales != 0 & !is.na(train_store$CompetitionDistance),]$CompetitionDistance), mean)
colnames(salesByDist) <- c("CompetitionDistance", "MeanSales")
ggplot(salesByDist, aes(x = log(CompetitionDistance), y = log(MeanSales))) + geom_point() + geom_smooth()

names(train)

# Drop the columns of the dataframe
traina =  select (train,-c(Date, Promo, StateHoliday, SchoolHoliday))
names(traina)
dim(traina)

# first 1500 observations
trainb <- traina[1:1500,]
names(trainb)
dim(trainb)

#correlation using pearson method of correlation
cor(trainb, use="complete.obs", method="pearson")

#covariance
cov(trainb, use="complete.obs")

#Multiple regression
# Create the relationship model
model <- lm(Sales~Customers+Store+Open+DayOfWeek, data = trainb)

#show model
print(model)

# Get the Intercept and coefficients as vector elements.
print("The Coefficient Values")

a <- coef(model)[1]
print(a)

XCustomers <- coef(model)[2]
XStore <- coef(model)[3]
XOpen <- coef(model)[4]
XDayOfWeek <- coef(model)[5]

print(XCustomers)
print(XStore)
print(XOpen)
print(XDayOfWeek)

#Create Equation for Regression Model
print("Y = a+XCustomers.x1+XStore.x2+XOpen.x3+XDayOfWeek.x4")
print("Y = -2577.432+(6.493)*x1+(-0.211)*x2+(3256.018)*x3+(574.323)*x4")

#Apply Equation for predicting New Values

trainb[458, "Sales"]

-2577.432+(6.493)*779+(-0.211)*458+(3256.018)*1+(574.323)*5

#The regression line predicts that at 779 Customers, 
#at store 458, and Open(1) and DayOfWeek(5)
#the Sales is 8511.61 
#In reality, it had 9276 Sales.

#Multiple Regression Performance
summary(model)
#Mean Square Error
#The MSE measures the performance of the regression line. 
print("mean square error : 1739")
#R squared value
#It measures the percent of variation in the y variable that can be explained by the independent variable.
print("R squared value : 0.6747")