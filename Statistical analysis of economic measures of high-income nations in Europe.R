#Reads Rectangular Data like (csv)
library(readr)

#It helps to manipulate, clean and summarize unstructured data
library(dplyr)

#multivariate analysis and scale construction using factor analysis, 
#principal component analysis, cluster analysis and reliability analysis.
library(psych)

#data wrangling, tidying, reading/writing, parsing, and visualizing.
library(tidyverse)

# provides a visual exploratory tool on correlation matrix
library(corrplot) 

#Provides a visual exploratory tool for Q-Q plot
library(qqplotr)


# providing tools to neatly and quickly summarize data
library(summarytools)

#Harrell Miscellaneous useful for data analysis
library(Hmisc)

#Companion to Applied Regression to perform Applied Regression techniques
library(car)

#User Contributed Packages
#Technical Trading Rules for technical analysis
library(TTR)

#Forecast for displaying and analysing univariate time series
library(forecast)


# Reading the dataset
GDP <- read.csv("GDP.csv", header= TRUE)

#Creating dataframe for Denmark and Switzerland for Comparison
GDP_Denmark <- (GDP[GDP$Country == "Denmark",])
GDP_Switzerland <- (GDP[GDP$Country == "Switzerland",])

#Viewing the dataset
head(GDP)

#Checking the summary
summary(GDP)

#Viewing the name of each column in the data frame
colnames(GDP)


#create summary descriptive statistics table, grouped by 'Country' variable
describeBy(GDP[ , c('Imports', 'Exports', 'FDI_inflows', 'FDI_outflows',
                    'Capital_formation', 'Expenditure', 'Savings',
                    'Trade', 'GDP_per_capita_growth', 'GDP_growth')], 
           GDP$Country)


#Correlation Analysis for all the nations
#defining the Cor_GDP
cor_GDP = round(cor(GDP[, 3:12]),
                    digits = 2 # rounded to 2 decimals
)

#Viewing the correlation analysis
cor_GDP


#visualizing the correlation using corrplot

#Showing all sides
corrplot(cor_GDP, tl.cex= 0.5)

# show only upper side
plot.new()
corrplot(cor_GDP,
         type = "upper",
         tl.cex= 0.5 
)



#Correlation coefficients
coef_GDP <- rcorr(as.matrix(GDP[, 3:12])) #
coef_GDP 

#Correlation for Denmark
round(cor(GDP_Denmark[, 3:12]),digits = 2)
rcorr(as.matrix(GDP_Denmark[, 3:12]))

#Correlation for Switzerland
round(cor(GDP_Switzerland[, 3:12]),digits = 2)
rcorr(as.matrix(GDP_Switzerland[, 3:12]))



#Regression Analysis
#objective of the regression analysis
#To determine the relation between dependent and independent variables


#1st model for the Multiple Regression for the reduced GDP

#Creating reducedGDP for the variables not closely correlated
ReducedGDP1 <- GDP[ ,c('Capital_formation','Trade',
                      'GDP_per_capita_growth', 'GDP_growth')]

#Viewing the reducedGDP correlation 
cor_ReducedGDP1 = cor(ReducedGDP1)

#Showing all sides
corrplot(cor_ReducedGDP1, tl.cex= 0.5)

# show only upper side
plot.new()
corrplot(cor_ReducedGDP1,
         type = "upper",
         tl.cex= 0.5 
)

#Performing the multiple linear regression for the 2nd model

Reg_GDP <-lm(GDP_growth ~  Capital_formation + Trade + GDP_per_capita_growth, ReducedGDP1)
Reg_GDP

#Viewing the summary
summary.lm(Reg_GDP)

# 2nd Model for the Multiple Regression for the reduced GDP

#All Countries
# Creating a dataframe with the selected indicators
ReducedGDP2 <- GDP[ ,c('Capital_formation',
                       'Expenditure','Trade', 'GDP_per_capita_growth', 'GDP_growth')]

cor(ReducedGDP2)

cor_ReducedGDP2 = cor(ReducedGDP2)

#Showing all sides
corrplot(cor_ReducedGDP2, tl.cex= 0.5)

# show only upper side
plot.new()
corrplot(cor_ReducedGDP2,
         type = "upper",
         tl.cex= 0.5 
)

#Performing the multiple linear regression for the 2nd model

Reg_GDP2 <-lm(GDP_growth ~  Expenditure + Trade + GDP_per_capita_growth + Capital_formation, ReducedGDP2)
summary.lm(Reg_GDP2)
vif(Reg_GDP2)

#checking the columns for visualization
colnames(ReducedGDP2)

#Decision: The 2nd model with 98% was selected

# Making sure the fitted model meets MLR assumptions
#. MLR Assumptions

#1. Linearity: scatter plot matrix
pairs(ReducedGDP2[,c(5,1,2,3,4)], lower.panel = NULL, pch = 19,cex = 0.2)

#2. Residuals’ Independence
plot(Reg_GDP2, 1)

#3. Normality of residuals
plot(Reg_GDP2, 2)

#4. Equal variances of the residuals (Homoscedasticity)
plot(Reg_GDP2, 3)


#5. No multicollinearity : Variance inflation factor (VIF)
vif(Reg_GDP2)

#All 5 assumptions were met.




#Time series
#Creating the time series dataframe

GDP_Growthseries <- ts(GDP_Denmark$GDP_growth,start=c(2008))

#View the time series
GDP_Growthseries

#plotting the GDP Time series
plot.ts(GDP_Growthseries)

###################### Decomposition Time Series ########################

#####  Decomposing Non-Seasonal Data ####

GDP_GrowthseriesSMA2 <- SMA(GDP_Growthseries,n=2)

plot.ts(GDP_GrowthseriesSMA2)



##################### Forecasts using Smoothing ##########################

##### Holt-Winters Exponential Smoothing (additive model with increasing or decreasing trend and no seasonality) #####
#Smoothing using holt winter
GDP_Growthseriesforecasts <- HoltWinters(GDP_Growthseries, gamma=FALSE)
GDP_Growthseriesforecasts 



#Fitted the series

GDP_Growthseriesforecasts$fitted
GDP_Growthseriesforecasts$SSE

#Plotting the holt winter exponential smoothing
plot(GDP_Growthseriesforecasts)

#Forecasting for ten years

GDP_Growthseriesforecasts2 <- forecast(GDP_Growthseriesforecasts,h=10)

plot(GDP_Growthseriesforecasts2)

#make a correlogram and a box test

acf(GDP_Growthseriesforecasts2$residuals, lag.max=9, na.action = na.pass)
acf(GDP_Growthseriesforecasts2$residuals, lag.max=9, na.action = na.pass, plot=FALSE) 

Box.test(GDP_Growthseriesforecasts2$residuals, lag=9, type="Ljung-Box")



# make time series plot
plot.ts(GDP_Growthseriesforecasts2$residuals) # make time series plot



#Define the plotForecastErrors

GDP_Growthseriesforecasts2$residuals <-GDP_Growthseriesforecasts2$residuals[!is.na(GDP_Growthseriesforecasts2$residuals)]


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4 
  mysd	<- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

#Plotting Histogram of Forecast error
plotForecastErrors(GDP_Growthseriesforecasts2$residuals) # make a histogram



###################Arima #########################

#### Differencing a Time Series #####

GDP_Growthseriesdiff1 <- diff(GDP_Growthseries, differences=1)
plot.ts(GDP_Growthseriesdiff1)



####### Forecasting Using an ARIMA Model #######
#Getting the best ARIMA model using auto.arima
auto.arima(GDP_Denmark$GDP_growth)

# fit to the auto arima model
GDPArima1 <- arima(GDP_Growthseriesdiff1, order=c(0,1,0)) 
GDPArima1

#Forecasting for 10 years

GDPforecasts1 <- forecast(GDPArima1 , h=10)
GDPforecasts1

#Plotting the 10 years forecast
plot(GDPforecasts1)

# plot a correlogram
acf(GDPforecasts1$residuals, lag.max=9)
acf(GDPforecasts1$residuals, lag.max=9, plot=FALSE) 
Box.test(GDPforecasts1$residuals, lag=9, type="Ljung-Box")

#Partial correlogram
pacf(GDPforecasts1$residuals, lag.max=9) 

#plot GDPforecasts1$residuals
plot.ts(GDPforecasts1$residuals)

#plot plotForecastErrors
plotForecastErrors(GDPforecasts1$residuals) # make a histogram



#Comparative Analysis

#Hypothesis Testing

#Comparing Trade in Northern Europe and Other Regions

#Creating the Regions
Trade_Hypothesis <- GDP |> mutate(Region = case_when(Country == 'Denmark' ~ "Northern Region",
                                                   Country == 'Finland' ~ "Northern Region",
                                                   Country == 'Norway' ~ "Northern Region",
                                                   Country == 'Sweden' ~ "Northern Region",
                                                   Country == 'Netherlands' ~ "Northern Region",
                                                   Country == 'Germany' ~ "Other Regions",
                                                   Country == 'Hungary' ~ "Other Regions",
                                                   Country == 'Poland' ~ "Other Regions",
                                                   Country == 'Spain' ~ "Other Regions",
                                                   Country == 'Switzerland' ~ "Other Regions"))

# Selecting the required columns using select()
Trade_Hypothesis <- Trade_Hypothesis %>% select(c(Region, Trade))
Trade_Hypothesis



#Changing It to a factor and review the summary()
Trade_Hypothesis$Region <- as.factor(Trade_Hypothesis$Region)
summary(Trade_Hypothesis)


# Using a box plot to compare Northern and Others
boxplot(Trade ~ Region, data=Trade_Hypothesis, names=c("Northern Region", "Other Regions"),
        xlab="Northern Region or Other Regions", ylab="Trade",
        main="Trade for Northern and Other Regions")



# Visualizing to check for normality for Northern Region
NorthernTrade<-Trade_Hypothesis$Trade[Trade_Hypothesis$Region=="Northern Region"]
ggplot(mapping = aes(sample = NorthernTrade)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")


#Plotting histogram for the North
hist(NorthernTrade)



# Visualizing to check for normality for Other Region
OtherRegionsTrade <-Trade_Hypothesis$Trade[Trade_Hypothesis$Region=="Other Regions"]
ggplot(mapping = aes(sample = OtherRegionsTrade)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")


#Plotting histogram for Other Regions
hist(OtherRegionsTrade)


#performing Shapiro-Wilk normality test
shapiro.test(Trade_Hypothesis$Trade)



# Using Wilcox Hypothesis test because of the outliers
#They are capable of handling outliers and 
#do not require the premise of normality of distributions.
wilcox.test(Trade ~ Region, data=Trade_Hypothesis)


wilcox.test(Trade ~ Region, data=Trade_Hypothesis,
            alternative = "less")










