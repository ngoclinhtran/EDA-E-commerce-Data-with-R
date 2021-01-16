library(tidyverse)

#Import
library(readr)
online_retail_II <- read_csv("C:/Users/Tan/Desktop/kitty/R/R Projects/E-commerce Data/online_retail_II.csv", 
                             col_types = cols(Invoice = col_character(), 
                                              Quantity = col_integer(), InvoiceDate = col_datetime(format = "%d/%m/%Y %H.%M")))

#Data Pre-processing

#Remove rows with negative quantity
online_retail_II <- online_retail_II[online_retail_II$Quantity > 0,]

#Remove missing values library(mice) md.pattern(online_retail_II)

online_retail_II <- na.omit(online_retail_II)
summary(online_retail_II)

#Change name of Customer ID to CustomerID
online_retail_II<- rename(online_retail_II, "CustomerID" = "Customer ID")

#Change data type of Country, CustomerID, Invoice, StockCode, Description to Factor
online_retail_II$Country <- as.factor(online_retail_II$Country)
online_retail_II$CustomerID <- as.factor(online_retail_II$CustomerID)
online_retail_II$Invoice <- as.factor(online_retail_II$Invoice)
online_retail_II$StockCode <- as.factor(online_retail_II$StockCode)
online_retail_II$Description <- as.factor(online_retail_II$Description)

#Add Amount Spend column by multiply quantity with price
online_retail_II$Amount_spent <- online_retail_II$Quantity * online_retail_II$Price

#Add Day_of_the_week, Month_YR, Hour columns and change them to factor data type
online_retail_II$Day_of_the_week <- as.factor(weekdays(online_retail_II$InvoiceDate)) #there is no order on Saturday
online_retail_II$Month_Yr <- as.factor(format(online_retail_II$InvoiceDate, "%Y-%m"))
online_retail_II$Hour <- as.factor(format(online_retail_II$InvoiceDate, "%H"))

str(online_retail_II)
summary(online_retail_II)



#Duplicate removal (do not remove the duplicate in this case since there is a chance that customer buy a product twice at a time)
online_retail_II[duplicated(online_retail_II),]

distinct(online_retail_II)
 
#DETECTION OF OUTLIERS  
summary(online_retail_II)
boxplot(online_retail_II$Price)
boxplot(online_retail_II$Quantity)

#ESD (six sigma) method
x = online_retail_II$Price
t = 3
m = mean(x, na.rm = F); m
s = sd(x, na.rm = F); s
b1 = m-s*t; b1
b2 = m+s*t; b2
y = ifelse(x>= b1 & x<= b2, 0, 1); y

plot(x, col= y+2)
outl = which(y==1)
online_retail_II[outl,] #There are 149 outliers detected in Price column, ask questions: is it possible for a value to be that high?, does the outlier look like a typo?, what does the data achitect say about the outliers?
#come to the conclusion what to do with outliers

xs = online_retail_II$Quantity
ts = 3
ms = mean(x,na.rm = F); ms
ss = sd(x, na.rm = F); ss
b1s = ms - ss*ts; b1s
b2s = ms + ss*ts; b2s
ys = ifelse(xs>= b1s & xs<= b2s, 0,1); ys

plot(xs,col= ys+2)
outls = which(ys==1)
online_retail_II[outls,] #There are 1772 outliers detected in Quantity column

#Box Plot method
boxplot(x)
boxplot.stats(x) #can try to change coef = 2 instead of 1.5 like default
boxplot.stats(x, coef = 2)

boxplot(xs)
boxplot.stats(xs)
boxplot.stats(x, coef = 2)

#Hypothesis tests for outliers, installed 'outliers'
library(outliers)

grubbs.test(x)
grubbs.test(x, type = 10, opposite = T)

grubbs.test(xs)
grubbs.test(xs, type = 10, opposite = T)


#Plausibility check and value replacement for factor and date-time data type
summary(online_retail_II)  #no invalid value detected

#EDA

#Data quality

  
    