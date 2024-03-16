# Time Series Analysis and Modelling
***
## Project Prompt: [Project2.pdf](https://github.com/kivatmojo/econ_104/files/14426504/Project2.pdf)
## Final Paper: [Project_2.pdf](https://github.com/kivatmojo/econ_104/files/14441056/Project_2.pdf)

***

Datasets Used:  

1. S&P 500 Stocks  
https://www.kaggle.com/datasets/andrewmvd/sp-500-stocks?select=sp500\_stocks.csv 

2. Daily News Sentiment Index  
https://www.frbsf.org/economic-research/indicators-data/daily-news-sentiment-index/  

3. 10-Year Real Interest Rate  
https://fred.stlouisfed.org/series/REAINTRATREARAT10Y  
***
## Table of Contents
1. Load and Prepare Data  
2. Analysis of Observations  
3. Analysis of Variables
4. AR(p)
5. ARDL(p,q)
6. VAR(p): Vector AutoRegression
7. [Conclusions and Findings](#conclusions-and-findings)

***
## Load and Prepare Data

```r
# Load Base Data

stockmovement <- read.csv("sp500_stocks.csv")
sentiment <- read_excel("news_sentiment_data.xlsx")
realinterest <- read.csv("REAINTRATREARAT10Y.csv")

# Ensure dates are formatted as date
stockmovement$Date <- as.Date(stockmovement$Date)
sentiment$Date <- as.Date(sentiment$Date)
realinterest$DATE <- as.Date(realinterest$DATE)

# Filter data to only include data from 2010-07-01 (Where TSLA data starts)
TSLA <- subset(stockmovement,Symbol == "TSLA")
TSLA <- filter(TSLA, format(Date, "%Y-%m-%d") >= "2010-07-01")

NVDA <- subset(stockmovement,Symbol == "NVDA")
NVDA <- filter(NVDA, format(Date, "%Y-%m-%d") >= "2010-07-01")

sentiment <- filter(sentiment, format(Date, "%Y-%m-%d") >= "2010-07-01")
realinterest <- filter(realinterest, format(DATE, "%Y-%m-%d") >= "2010-07-01")

# Monthly Data Tables
TSLAmonth <- TSLA %>%
  arrange(Date) %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarize(MonthlyClose = last(Adj.Close)) %>%
  ungroup() %>%
  mutate(MonthlyPctChange = (MonthlyClose / lag(MonthlyClose) - 1) * 100)

NVDAmonth <- NVDA %>%
  arrange(Date) %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarize(MonthlyClose = last(Adj.Close)) %>%
  ungroup() %>%
  mutate(MonthlyPctChange = (MonthlyClose / lag(MonthlyClose) - 1) * 100)

sentiment_month <- sentiment %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(MonthYear = floor_date(Date, "month")) %>%
  group_by(MonthYear) %>%
  summarize(AvgSentiment = mean(Sentiment, na.rm = TRUE)) %>%
  ungroup()

# Combine Tables
TSLA.table <- TSLAmonth %>% 
  inner_join(sentiment_month, by = c("YearMonth"="MonthYear")) %>% 
  inner_join(realinterest, by = c("YearMonth"="DATE"))
TSLA.table <- TSLA.table[-1,] # Remove NA row

TSLA.table <- TSLA.table %>%
  rename(
    date = YearMonth,
    close = MonthlyClose,
    percentagechange = MonthlyPctChange,
    sentiment = AvgSentiment,
    interest = REAINTRATREARAT10Y)

TSLA.table <- TSLA.table %>%
  dplyr::select(-close)

NVDA.table <- NVDAmonth %>% 
  inner_join(sentiment_month, by = c("YearMonth"="MonthYear")) %>% 
  inner_join(realinterest, by = c("YearMonth"="DATE"))
NVDA.table <- NVDA.table[-1,] # Remove NA row

NVDA.table <- NVDA.table %>%
  rename(
    date = YearMonth,
    close = MonthlyClose,
    percentagechange = MonthlyPctChange,
    sentiment = AvgSentiment,
    interest = REAINTRATREARAT10Y)

NVDA.table <- NVDA.table %>%
  dplyr::select(-close)

```

For this project, what we chose to analyze were TESLA and NVIDIA stock prices, and the effect, if any, both stocks have on one another's price. TESLA is a technology and clean energy company that builds electric cars and energy storage products. NVIDIA Corporation is a multinational technology company primarily known for designing graphics processing units (GPUs) for the gaming and professional markets.  

As TESLA builds its self-driving technology and neural networks, they have become a recent customer of NVIDIA and their chips. We believe this relationship will result in some correlation in the company's stock prices.  

In this project, we are using stock price data from 2010 to present day as TESLA went public on June 29, 2010. We chose to use news sentiment data, expected inflation rates, and real interest rates as our predictors for determining future stock prices. We converted the stock prices to monthly percentage change, and had all our predictors be a monthly figure.  

Sentiment data refers to information gathered from various sources, such as social media, news articles, financial reports, and other textual data, to gauge the overall sentiment or mood surrounding the world. We are also using real interest rates and expected inflation rates from FRED. We believe these predictors can influence investors' decisions to save and invest in the economy, and therefore will affect company stock prices.  
***

## Analysis of Observations

```r
summary(TSLA.table)
```
<img width="566" alt="Screen Shot 2024-03-15 at 18 55 40" src="https://github.com/kivatmojo/econ_104/assets/137433466/8381f937-fe68-42e0-9b1c-8513dcb877b2">

```r
summary(NVDA.table)
```
<img width="552" alt="Screen Shot 2024-03-15 at 18 55 56" src="https://github.com/kivatmojo/econ_104/assets/137433466/4e834fa2-0af7-4967-8ea8-2d45166d8514">

```r
par(mfrow = c(1,2))

boxplot(TSLA.table$percentagechange, xlab="Monthly % Change", main= "TSLA")
boxplot(NVDA.table$percentagechange, xlab="Monthly % Change", main= "NVDA")
```
<img width="447" alt="Screen Shot 2024-03-15 at 18 56 23" src="https://github.com/kivatmojo/econ_104/assets/137433466/9809faad-9574-4d2d-86a4-03a120210df1">

```r
par(mfrow = c(1,3))
boxplot(TSLA.table$sentiment, xlab="Monthly Sentiment")
boxplot(TSLA.table$interest, xlab="Monthly Interest Rates")

mtext("Macroeconomic Data", side=3, outer=TRUE, line=-2, cex=1.5)
```
<img width="370" alt="Screen Shot 2024-03-15 at 18 58 01" src="https://github.com/kivatmojo/econ_104/assets/137433466/659832ab-dbbe-44e4-80f3-f2d7a76eb645">

The interest rates ranged from -0.4% to 2.09%. A higher interest rate means higher cost of borrowing and higher return on investment. Sentiment data ranged from -0.63 units to 0.31 units.  

In looking at the box plots of the macroeconomic predictors, we found that the outliers in sentiment data were all on the lower end, and monthly interest rate outliers were on the upper end. This makes sense as these companies were impacted by the COVID-19 pandemic over the last couple of years. COVID-19 lead to significant fiscal policy to stimulate the economy by printing more money. Later, the economy was overheating and so the FED increased interest rates to slow down the economy. This is why we see many upper-end outliers of interest rates in the data.

The lowest percentage change in TESLA’s stock in a month is -36% and the highest change was 81%. This is expected as TESLA has been known to be very volatile in the past due to a lot of contributing factors. NVIDIA’s monthly price changes ranged from -32% to 55%. This is also expected as NVIDIA has come out with very innovative technology and chip products, but has also experienced bottlenecks in production during the COVID-19 pandemic. With that being said, TESLA’s box plot shows more outliers, which suggest TESLA is more volatile - consistent with how our group feels in actuality. 

```r
par(mfrow = c(1,2))

hist(TSLA.table$percentagechange, xlab="Monthly % Change", main="TSLA")
hist(NVDA.table$percentagechange, xlab="Monthly % Change", main="NVDA")
```
<img width="460" alt="Screen Shot 2024-03-15 at 18 58 11" src="https://github.com/kivatmojo/econ_104/assets/137433466/9a754a7c-4113-4a2a-886c-788e2e720ca6">

```r
par(mfrow = c(1,3))
hist(TSLA.table$sentiment, xlab="Monthly Sentiment", main= "")
hist(TSLA.table$interest, xlab="Monthly Interest Rates", main= "")

mtext("Macroeconomic Data", side=3, outer=TRUE, line=-2, cex=1.5)
```
<img width="350" alt="Screen Shot 2024-03-15 at 18 58 18" src="https://github.com/kivatmojo/econ_104/assets/137433466/6b9c93fc-bdac-4fcf-a93e-ea9217cc1a00">

In looking at the percent change in stock prices, TESLA’s is skewed toward the right and NVIDIA’s shows a relatively normal distribution. Our hypothesis for why TESLA experienced more drops in stock price is because their stock value is based on the potential of the company and not what the actual value of the company is today. For example, their price to earnings ratio is approximately 43 times.

```r
par(mfrow = c(1,2))

cor.TSLA = cor(TSLA.table %>% dplyr::select("% change" = "percentagechange", sentiment, interest))
corrplot(cor.TSLA, method = "square", tl.col="black", main= "TSLA")

cor.NVDA = cor(NVDA.table %>% dplyr::select("% change" = "percentagechange", sentiment, interest))
corrplot(cor.NVDA, method = "square", tl.col="black", main= "NVDA")
```
<img width="490" alt="Screen Shot 2024-03-15 at 18 58 28" src="https://github.com/kivatmojo/econ_104/assets/137433466/4b3c395b-b778-4faa-8101-a2d52efb4e92">

```r
par(mfrow = c(1,3))

plot(percentagechange~sentiment, data=TSLA.table, ylab="% Change", xlab="Sentiment")
plot(percentagechange~interest, data=TSLA.table, ylab="% Change", xlab="Interest")

mtext("TSLA", side=3, outer=TRUE, line=-2, cex=1.5)

```
<img width="340" alt="Screen Shot 2024-03-15 at 18 58 47" src="https://github.com/kivatmojo/econ_104/assets/137433466/6858dc24-60cd-4e33-bf6c-88e225714c34">


```r
par(mfrow = c(1,3))

plot(percentagechange~sentiment, data=NVDA.table, ylab="% Change", xlab="Sentiment")
plot(percentagechange~interest, data=NVDA.table, ylab="% Change", xlab="Interest")

mtext("NVDA", side=3, outer=TRUE, line=-2, cex=1.5)

```
<img width="321" alt="Screen Shot 2024-03-15 at 18 58 57" src="https://github.com/kivatmojo/econ_104/assets/137433466/241befbe-39f7-44c6-a1ce-ef5fbe18f5df">

The correlation plot reveals high positive correlation between Interest Rates and Inflation rates for both TESLA and NVIDIA. In other words, as interest rates rise, inflation also rises. This makes sense as the interest rate represents the value of the dollar (cost of borrowing). As such, if interest rates rise, prices will also rise, and inflation will follow.  


With TESLA, we also see a negative correlation between %change in stock prices in a month and inflation and interest rates. In other words, when inflation and interest rates rise, TESLA stock drops. This makes sense because as Inflation rises, the price of goods increases, and therefore the demand for goods, such as TESLA’s, will decrease. Additionally, the cost of leveraging debt, which TESLA is known to do, will become more expensive and therefore can stunt growth. This is also supported by the scatter plot where we see a negative trend (negative slope for AB line) between stock prices and inflation and interest rates.  


On the other hand, NVIDIA displays weak correlation between Inflation rates and %change in stock compared to TESLA. As such, the scatter plot shows no trends as well. This makes sense because NVIDIA is a more mature company and leverages less debt compared to TESLA. Additionally, NVIDIA's GPU chips can be seen as more of a necessity than TESLA cars. 
***

## Time Series Analysis on Variables

```r
# Convert Price Change Data into Time Series
TSLA.ts<-ts(TSLA.table)

NVDA.ts <- ts(NVDA.table)
```

### Macroeconomic Data

```r
tsdisplay(TSLA.ts[,"sentiment"], main="sentiment")
```
<img width="471" alt="Screen Shot 2024-03-15 at 18 59 27" src="https://github.com/kivatmojo/econ_104/assets/137433466/6884d9cd-607c-4d7a-8b8c-1e5e12a3b776">

```r
tsdisplay(TSLA.ts[,"interest"], main="interest")
```
<img width="454" alt="Screen Shot 2024-03-15 at 18 59 42" src="https://github.com/kivatmojo/econ_104/assets/137433466/fad5aa16-238c-4441-8258-d2ae7035d1a4">

### TSLA and NVDA
```r
plot(TSLA.ts[,"percentagechange"], ylab= "% change")
lines(TSLA.ts[,"percentagechange"],col="red")
lines(NVDA.ts[,"percentagechange"],col="black")
legend("topright", legend=c("TSLA", "NVDA"), text.col=c("red","black"),bty="n")
```
<img width="507" alt="Screen Shot 2024-03-15 at 18 59 54" src="https://github.com/kivatmojo/econ_104/assets/137433466/f6232a37-d16d-4024-80a9-4d22a77d0e90">

### TSLA

```r
# tsdisplay plot
tsdisplay(TSLA.ts[,"percentagechange"], main="TSLA % change")

```
<img width="478" alt="Screen Shot 2024-03-15 at 19 00 06" src="https://github.com/kivatmojo/econ_104/assets/137433466/c4727ffa-746e-4d5a-9a01-c91cdec239ce">

### NVDA

```r
# tsdisplay plot
tsdisplay(NVDA.ts[,"percentagechange"], main="NVDA % change")
```
<img width="496" alt="Screen Shot 2024-03-15 at 19 00 12" src="https://github.com/kivatmojo/econ_104/assets/137433466/556f066a-4c4e-417e-8193-b72cbc10956b">

In plotting the percentage change in price of TESLA’s stock across time, we see a stationary plot where the price changes around 20%. NVIDIA also displays a stationary plot in percentage change in price, but at 0%.  


In plotting the Autocorrelation Function (ACF) for TESLA, we don’t see strong correlation between errors in stock price changes with the exception of approximately 4 months (lags) back. Consistent with the ACF, the Partial Autocorrelation Function (PACF) shows no strong correlation in errors across time except for the fourth-period lag.  


When looking at the ACF for NVIDIA, we see no strong correlation in errors across time. However, the highest correlation, although not significant, also occurs 4 periods back. This may be a sign that it takes 4 months for the market to accurately account for events that impact valuation. In looking at the PCF, we see a strong significant correlation in error terms 16 months back. This data is very surprising as we did not expect stock price changes 16 months back to affect stock price level changes today.

***

## AR(p) Models

### TSLA
```r
# Fit AR(p) model
y1 <- TSLA.ts[,"percentagechange"]
AR4y1 <- dynlm(y1~L(y1,4))
```

```r
# ACF & PACF of Residuals
par(mfrow = c(1,2))

acf(AR4y1$residuals)
pacf(AR4y1$residuals)
```
<img width="468" alt="Screen Shot 2024-03-15 at 19 00 32" src="https://github.com/kivatmojo/econ_104/assets/137433466/4b3cf5b3-1d76-48a9-b8ca-b7552b7a2a02">

```r
# Re-Fit AR(p) model
AR10y1 <- dynlm(y1~L(y1,1)+L(y1,4)+L(y1,10))
```

```r
# ACF & PACF of Residuals
par(mfrow = c(1,2))

acf(AR10y1$residuals)
pacf(AR10y1$residuals)
```
<img width="519" alt="Screen Shot 2024-03-15 at 19 00 40" src="https://github.com/kivatmojo/econ_104/assets/137433466/229938cf-05d1-4644-bbba-b51114b499c8">

In plotting the Partial Autocorrelation Function of our AR(4) model of TESLA, we found large spikes but ultimately insignificant correlation in errors of lag one, and ten. As such, in our next model, we included these lags. The AR(10) model  PACF now has no significant spikes from lagged periods.

```r
# Train Test Split
## MODEL 1: AR(4)

y1var <- TSLA.table$percentagechange
y1df <- data.frame(y1var)
y1df$lag4y <- dplyr::lag(y1var,4)

n_train1 <- as.integer(nrow(y1df)*2/3)
y1df_train <- y1df[1:n_train1,]
y1df_test <- y1df[(n_train1+1):nrow(y1df),]

ar4y1 <- lm(y1var ~ lag4y, data=y1df_train)
y1df_test$y1_test <- predict(ar4y1,y1df_test)

# Out-of-sample :
MSE1y1 <- mean( (y1df_test$y1var - y1df_test$y1_test)^2 )
RMSE1y1 <- sqrt(MSE1y1)
RMSE1y1
```
<img width="146" alt="Screen Shot 2024-03-15 at 19 01 02" src="https://github.com/kivatmojo/econ_104/assets/137433466/a635146a-0c7a-4b91-9e68-fabe07f1ebfb">


```r
# Train Test Split
## MODEL 2: AR(10)

y1var <- TSLA.table$percentagechange
y1df2 <- data.frame(y1var)
y1df2$lag1y <- dplyr::lag(y1var,1)
y1df2$lag4y  <-  dplyr::lag(y1var,4)
y1df2$lag10y  <-  dplyr::lag(y1var,10)

n_train2  <-  as.integer(nrow(y1df2)*2/3)
y1df2_train  <-  y1df2[1:n_train2,]
y1df2_test  <-  y1df2[(n_train2+1):nrow(y1df2),]

ar10y1  <-  lm(y1var ~ lag1y + lag4y + lag10y, data=y1df2_train)
y1df2_test$y1_test  <-  predict(ar10y1,y1df2_test)

# Out-of-sample :
MSE2y1  <-  mean((y1df2_test$y1var - y1df2_test$y1_test)^2 )
RMSE2y1 <-  sqrt(MSE2y1)
RMSE2y1
```
<img width="161" alt="Screen Shot 2024-03-15 at 19 01 29" src="https://github.com/kivatmojo/econ_104/assets/137433466/1269214a-48af-4bfc-b76d-0796bc1dc3ca">

Next, we want to compare our two models, AR(4) and AR(10), to see which is more accurate. First, we split our data in two-thirds and one-thirds to find the actual and predicted values of the model. Our AR(4)  model produced an RMSE of 22.04397 and our AR(10) Model produced an RMSE of 22.23962. This tells us that our model prediction is on average off by approximately 22% for TESLA. Given the context of TESLA stock and its volatility, the accuracy of our models are solid.

```r
# AIC
AIC(AR4y1, AR10y1)
```

<img width="170" alt="Screen Shot 2024-03-15 at 19 01 44" src="https://github.com/kivatmojo/econ_104/assets/137433466/2b4a3e74-0d56-49c9-9faa-9f8376b2c645">

```r
# BIC
BIC(AR4y1, AR10y1)
```

<img width="156" alt="Screen Shot 2024-03-15 at 19 01 52" src="https://github.com/kivatmojo/econ_104/assets/137433466/de1f72c7-ffa9-4719-92d8-b61a050c2968">

However, in analyzing the AIC and BIC, the results show that the AR(10) model produced a lower (better) score than the AR(4) model. This tells us that the AR(10) model is a better forecast model but has very marginally more error. Additionally, this is telling us that the additional accuracy from the first, second, and tenth lag outweigh the penalty of adding more variables.

```r
y1.ar4 = ar(y1var, aic=FALSE, order.max=4, method="ols")
plot(forecast(y1.ar4, 10))
```

<img width="562" alt="Screen Shot 2024-03-15 at 19 02 01" src="https://github.com/kivatmojo/econ_104/assets/137433466/91671acf-7348-48ac-944f-3a61145fecc8">

```r
y1.ar10 = ar(y1var, aic=FALSE, order.max=10, method="ols")
plot(forecast(y1.ar10, 10))

```

<img width="579" alt="Screen Shot 2024-03-15 at 19 02 17" src="https://github.com/kivatmojo/econ_104/assets/137433466/59ff5591-8774-43c6-a45d-edcabbd40b38">

From the RMSE, AIC, BIC, and how the forecasts look for both models, we will decide that the AR(10) model is better at predicting the monthly percentage change of TSLA.

### NVDA
```r
# Fit AR(p) model

y2 <- NVDA.ts[,"percentagechange"]
AR4y2 <- dynlm(y2~L(y2,4))
```

```r
# ACF & PACF of Residuals
par(mfrow=c(1,2))

acf(AR4y2$residuals)
pacf(AR4y2$residuals)
```
<img width="445" alt="Screen Shot 2024-03-15 at 19 07 15" src="https://github.com/kivatmojo/econ_104/assets/137433466/9570f3de-e4a0-449a-b549-7fac2775fa95">

The PACF of residuals for NVIDIA for our AR(4) model showed one significant correlation at 16 periods back. As such, we created an AR(16) model that includeded the fourth, and sixteenth period lags which eliminated all significant correlations of stock prices in the past.

```r
# Re-fit AR model
AR16y2 <- dynlm(y2~L(y2,4)+L(y2,15)+L(y2,16))
```

```r
# ACF & PACF of Residuals

par(mfrow=c(1,2))
acf(AR16y2$residuals)
pacf(AR16y2$residuals)
```
<img width="493" alt="Screen Shot 2024-03-15 at 19 07 29" src="https://github.com/kivatmojo/econ_104/assets/137433466/0904d18a-0003-44c6-ac55-b817210d7701">

The new model's PACF shows no significant correlation across error terms in the past.

```r
# Train Test Split
## MODEL 1

y2var <- NVDA.table$percentagechange
y2df <- data.frame(y2var)
y2df$lag4y <- dplyr::lag(y2var,4)

n_train3 <- as.integer(nrow(y2df)*2/3)
y2df_train <- y2df[1:n_train3,]
y2df_test <- y2df[(n_train3+1):nrow(y2df),]

ar4y2 <- lm(y2var ~ lag4y, data=y2df_train)
y2df_test$y2_test <- predict(ar4y2,y2df_test)


# Out-of-sample :
MSE1y2 <- mean( (y2df_test$y2var - y2df_test$y2_test)^2 )
RMSE1y2 <- sqrt(MSE1y2)
RMSE1y2
```
<img width="135" alt="Screen Shot 2024-03-15 at 19 07 40" src="https://github.com/kivatmojo/econ_104/assets/137433466/546ea55d-b1e5-4a4e-8fed-0f9bcfd983b5">

```r
# Train Test Split
## MODEL 2

y2var <- NVDA.table$percentagechange
y2df2 <- data.frame(y2var)
y2df2$lag4y <- dplyr::lag(y2var,4)
y2df2$lag15y <- dplyr::lag(y2var,15)
y2df2$lag16y <- dplyr::lag(y2var,16)

n_train4 = as.integer(nrow(y2df2)*2/3)
y2df2_train <- y2df2[1:n_train4,]
y2df2_test <- y2df2[(n_train4+1):nrow(y2df2),]

ar16y2 <- lm(y2var ~ lag4y + lag15y + lag16y, data=y2df2_train)
y2df2_test$y2_test <- predict(ar16y2,y2df2_test)

# Out-of-sample :
MSE2y2 <- mean((y2df2_test$y2var - y2df2_test$y2_test)^2 )
RMSE2y2 <- sqrt(MSE2y2)
RMSE2y2
```
<img width="115" alt="Screen Shot 2024-03-15 at 19 07 49" src="https://github.com/kivatmojo/econ_104/assets/137433466/a75cf52e-1489-42c3-81f4-2165d59c87e7">

To determine which model is better, we split the data and used test data to evaluate the performance of each model. The Root Mean Squared Error (RMSE) for our AR(16) model is 14.46 compared to 14.27 from our AR(4) model.

```r
# AIC
AIC(AR4y2, AR16y2)
```

<img width="162" alt="Screen Shot 2024-03-15 at 19 08 00" src="https://github.com/kivatmojo/econ_104/assets/137433466/7e10abd4-80d5-4d4d-a812-69e392d39b29">

```r
# BIC
BIC(AR4y2, AR16y2)
```
<img width="176" alt="Screen Shot 2024-03-15 at 19 08 08" src="https://github.com/kivatmojo/econ_104/assets/137433466/d9cc5edf-2141-4a2b-a58f-c3b8740010c7">

However, the AR(16) model produced a lower AIC and BIC of 1151.132 and 1166.050 respectively. This means the benefit in marginal prediction accuracy from adding the sixteenth lag outweighs the penalty of adding new variables in the model. Given this, we prefer to use the AR(16) model.

```r
y2.ar4 = ar(y2var, aic=FALSE, order.max=4, method="ols")
plot(forecast(y2.ar4, 10))
```

<img width="569" alt="Screen Shot 2024-03-15 at 19 08 21" src="https://github.com/kivatmojo/econ_104/assets/137433466/39545e75-22b1-431e-bdba-869d41a21bd8">

```r
y2.ar16 = ar(y2var, aic=FALSE, order.max=16, method="ols")
plot(forecast(y2.ar16, 10))
```
<img width="567" alt="Screen Shot 2024-03-15 at 19 08 52" src="https://github.com/kivatmojo/econ_104/assets/137433466/2c93e8f2-1338-4d5b-a972-68cefbf2b42c">

To conclude our analysis of our AR models, we used both models to try and forecast what the next 10 months will look like for NVIDIA's stock in terms of percentage change.  

From what we see, the second model seems to have a more realistic forecast compared to the first model's practically flat forecast.  

This solidifies that when wanting to use an AR model, using the lags on our AR(16) model is better.

***

## ARDL(p,q)

```r
x1 <- TSLA.ts[,"sentiment"]
x2 <- TSLA.ts[,"interest"]

x1mean <- mean(x1)
x2mean <- mean(x2)
```

### TSLA

```r
# First Model using Sentiment as a Variable

ARDL1y1 <- dynlm(y1~L(y1,1)+L(y1,4)+L(x1,1)+L(x1,2))
ardl1y1.mod <- ardlDlm(percentagechange~sentiment,data=TSLA.table, p=4,q=2)
```

Our first ARDL model for TESLA's stock uses our news sentiment data to try and predict the stock's monthly percentage change. From the PACF plot for both percentage change and news sentiment, we decided to include percentage change 4 lags back, and news sentiment 2 lags back into our model.

```r
# ACF & PACF

par(mfrow=c(1,2))
acf(ARDL1y1$residuals)
pacf(ARDL1y1$residuals)
```

<img width="464" alt="Screen Shot 2024-03-15 at 19 09 12" src="https://github.com/kivatmojo/econ_104/assets/137433466/3eb88d29-c952-4483-8ccc-03a1223ed44d">

Looking at our model's residual's PACF plot, we see no other significiant autocorrelation spikes, thus we move forward with this model.

```r
# Second Model using Interest Rates as a Variable

ARDL2y1 <- dynlm(y1~L(y1,1)+L(y1,4)+L(x2,1)+L(x2,3))
ardl2y1.mod <- ardlDlm(percentagechange~interest,data=TSLA.table, p=4,q=3)
```
<img width="465" alt="Screen Shot 2024-03-15 at 19 09 25" src="https://github.com/kivatmojo/econ_104/assets/137433466/3c996363-36f1-427a-b5b6-b8694cb4ebd7">

Our second ARDL model for TESLA's stock uses our interest rate data to try and predict the stock's monthly percentage change. From the PACF plot for both percentage change and interest rate, we decided to keep including percentage change 4 lags back, but use interest rates 3 lags back into our model.

```r
# ACF & PACF

par(mfrow=c(1,2))
acf(ARDL2y1$residuals)
pacf(ARDL2y1$residuals)
```

Looking at our PACF of the residuals of our second model, there is also no significant autocorrelation spikes, and we also move forward with this model.

```r
# Train Test Split
# Model 1
y1var <- TSLA.table$percentagechange
x1var <- TSLA.table$sentiment
x2var <- TSLA.table$interest

y1ardl1 <- data.frame(y1var)
y1ardl1$lag1y <- dplyr::lag(y1var,1)
y1ardl1$lag4y <- dplyr::lag(y1var,4)
y1ardl1$lag1x1 <- dplyr::lag(x1var,1)
y1ardl1$lag2x1 <- dplyr::lag(x1var,2)

n_train5 <- as.integer(nrow(y1ardl1)*2/3)
y1ardl1_train <- y1ardl1[1:n_train5,]
y1ardl1_test <- y1ardl1[(n_train5+1):nrow(y1ardl1),]

ardly1 <- lm(y1var ~ lag1y + lag4y + lag1x1 + lag2x1, data=y1ardl1_train)
y1ardl1_test$y1_test <- predict(ardly1,y1ardl1_test)

# Out-of-sample :
MSE3y1 <- mean( (y1ardl1_test$y1var - y1ardl1_test$y1_test)^2 )
RMSE3y1 <- sqrt(MSE3y1)
RMSE3y1
```
<img width="122" alt="Screen Shot 2024-03-15 at 19 09 34" src="https://github.com/kivatmojo/econ_104/assets/137433466/f0746bec-f0b5-4f94-aca3-04e815ace564">

```r
# Train Test Split
# Model 2

y1ardl2 <- data.frame(y1var)
y1ardl2$lag1y <- dplyr::lag(y1var,1)
y1ardl2$lag4y <- dplyr::lag(y1var,4)
y1ardl2$lag1x2 <- dplyr::lag(x2var,1)
y1ardl2$lag3x2 <- dplyr::lag(x2var,3)


n_train6 <- as.integer(nrow(y1ardl2)*2/3)
y1ardl2_train <- y1ardl2[1:n_train6,]
y1ardl2_test <- y1ardl2[(n_train5+1):nrow(y1ardl1),]

ardl2y1 <- lm(y1var ~ lag1y + lag4y 
              + lag1x2 + lag3x2 
              , data=y1ardl2_train)
y1ardl2_test$y1_test <- predict(ardl2y1,y1ardl2_test)

# Out-of-sample :
MSE4y1 <- mean( (y1ardl2_test$y1var - y1ardl2_test$y1_test)^2 )
RMSE4y1 <- sqrt(MSE4y1)
RMSE4y1
```
<img width="114" alt="Screen Shot 2024-03-15 at 19 09 43" src="https://github.com/kivatmojo/econ_104/assets/137433466/6764f158-17a3-4198-a341-78a1a388b8b9">

Splitting our data 2/3-1/3 as the train-test split respectively, we calculated the RMSE for both models to assess how far off our model predicts the percentage change in the test data time frame to the actual points in the test data time frame. The first model is on average off by 22.66% and the second model is on average off by 22.50%. Using interest rates is slightly better at predicting percentage change.

Though 22.5% seems large for a stock, we can rationalize this value considering the volatility of TESLA's stock.

```r
# AIC
AIC(ARDL1y1, ARDL2y1)
```

<img width="163" alt="Screen Shot 2024-03-15 at 19 10 01" src="https://github.com/kivatmojo/econ_104/assets/137433466/e7edaf09-5668-466a-a637-a1b3bad6517f">

```r
# BIC
BIC(ARDL1y1, ARDL2y1)
```
<img width="163" alt="Screen Shot 2024-03-15 at 19 10 10" src="https://github.com/kivatmojo/econ_104/assets/137433466/806e95db-3c58-464e-9d2c-1d98b61f933d">

To further investigate which variable is a better predictor of TESLA's stock monthly percentage change, we calculated the AIC and BIC values of both models and since the second model shows to be the better performer, we can conclude that interest rates are a better predictor.

```r
# 10 step ahead forecast

fcast1y1 <- dLagM::forecast(ardl1y1.mod, h=10, c(x1mean,x1mean,x1mean,x1mean,x1mean,x1mean,x1mean,x1mean,x1mean,x1mean), interval=TRUE)

plot(fcast1y1$forecasts[,2],type="l", ylim=c(-50,50))
lines(fcast1y1$forecasts[,1], col="red")
lines(fcast1y1$forecasts[,3],col="red")
```

<img width="504" alt="Screen Shot 2024-03-15 at 19 10 30" src="https://github.com/kivatmojo/econ_104/assets/137433466/db8849be-56f7-4932-b754-e2bda9f44775">

```r
fcast2y1 <- dLagM::forecast(ardl2y1.mod, h=10, c(x2mean,x2mean,x2mean,x2mean,x2mean,x2mean,x2mean,x2mean,x2mean,x2mean), interval=TRUE)

plot(fcast2y1$forecasts[,2],type="l", ylim=c(-50,50))
lines(fcast2y1$forecasts[,1], col="red")
lines(fcast2y1$forecasts[,3],col="red")
```

<img width="466" alt="Screen Shot 2024-03-15 at 19 10 56" src="https://github.com/kivatmojo/econ_104/assets/137433466/154bf441-1596-4f36-a2ea-9bb1e63ae5b0">

To conclude our analysis of our ARDL models, we used both models to try and forecast what the next 10 months will look like for TESLA's stock in terms of percentage change.  

From what we see, the second model seems to have a more realistic forecast compared to the first model's practically flat forecast.  

This solidifies that when wanting to use an ARDL model, using interest rates as a predictor is better than using news sentiment as a predictor.

### NVDA 
```r
# First Model using Sentiment as a Variable

ARDL1y2 <- dynlm(y2~L(y2,1)+L(y2,4)+L(y2,16)+L(x1,1)+L(x1,2))
ardl1y2.mod <- ardlDlm(percentagechange~sentiment,data=NVDA.table, p=16,q=2)
```

Similar to our models for TESLA's stock, our first ARDL model for NVIDIA's stock uses our news sentiment data to try and predict the stock's monthly percentage change. From the PACF plot for both percentage change and news sentiment, we decided to include percentage change 4 and 16 lags back, and news sentiment 2 lags back into our model.

```r
# ACF & PACF

par(mfrow=c(1,2))
acf(ARDL1y2$residuals)
pacf(ARDL1y2$residuals)
```
<img width="465" alt="Screen Shot 2024-03-15 at 19 11 08" src="https://github.com/kivatmojo/econ_104/assets/137433466/5421c201-9750-4b5d-a37a-56c23303d243">

Looking at our model's residual's PACF plot, we see no other significiant autocorrelation spikes, thus we move forward with this model.

```r
# Second Model using Interest Rates as a Variable

ARDL2y2 <- dynlm(y2~L(y2,1)+L(y2,4)+L(y2,16)+L(x2,1)+L(x2,3))
ardl2y2.mod <- ardlDlm(percentagechange~interest,data=NVDA.table, p=16,q=3)
```

Our second ARDL model for NVIDIA's stock uses our interest rate data to try and predict the stock's monthly percentage change. From the PACF plot for both percentage change and interest rate, we decided to keep including percentage change 4 and 16 lags back, but use interest rates 3 lags back into our model.

```r
# ACF & PACF

par(mfrow=c(1,2))
acf(ARDL2y2$residuals)
pacf(ARDL2y2$residuals)
```
<img width="458" alt="Screen Shot 2024-03-15 at 19 11 18" src="https://github.com/kivatmojo/econ_104/assets/137433466/e9a8dd69-8051-4e12-80c5-985ed68645e2">

Looking at our PACF of the residuals of our second model, there is also no significant autocorrelation spikes, and we also move forward with this model.

```r
# Train Test Split
# Model 1
y2var <- NVDA.table$percentagechange
x1var <- NVDA.table$sentiment
x2var <- NVDA.table$interest

y2ardl1 <- data.frame(y2var)
y2ardl1$lag1y <- dplyr::lag(y2var,1)
y2ardl1$lag4y <- dplyr::lag(y2var,4)
y2ardl1$lag16y <- dplyr::lag(y2var,16)
y2ardl1$lag1x1 <- dplyr::lag(x1var,1)
y2ardl1$lag2x1 <- dplyr::lag(x1var,2)

n_train5 <- as.integer(nrow(y2ardl1)*2/3)
y2ardl1_train <- y2ardl1[1:n_train5,]
y2ardl1_test <- y2ardl1[(n_train5+1):nrow(y2ardl1),]

ardly2 <- lm(y2var ~ lag1y + lag4y + lag16y + lag1x1 + lag2x1, data=y2ardl1_train)
y2ardl1_test$y2_test <- predict(ardly2,y2ardl1_test)

# Out-of-sample :
MSE3y2 <- mean( (y2ardl1_test$y2var - y2ardl1_test$y2_test)^2 )
RMSE3y2 <- sqrt(MSE3y2)
RMSE3y2
```
<img width="109" alt="Screen Shot 2024-03-15 at 19 11 28" src="https://github.com/kivatmojo/econ_104/assets/137433466/ec796ed7-77b8-494c-874c-a5fd1c708f50">

```r
# Train Test Split
# Model 2

y2ardl2 <- data.frame(y2var)
y2ardl2$lag1y <- dplyr::lag(y2var,1)
y2ardl2$lag4y <- dplyr::lag(y2var,4)
y2ardl2$lag16y <- dplyr::lag(y2var,16)
y2ardl2$lag1x2 <- dplyr::lag(x2var,1)
y2ardl2$lag3x2 <- dplyr::lag(x2var,3)


n_train6 <- as.integer(nrow(y2ardl2)*2/3)
y2ardl2_train <- y2ardl2[1:n_train6,]
y2ardl2_test <- y2ardl2[(n_train5+1):nrow(y2ardl1),]

ardl2y2 <- lm(y2var ~ lag1y + lag4y + lag16y 
              + lag1x2 + lag3x2 
              , data=y2ardl2_train)
y2ardl2_test$y2_test <- predict(ardl2y2,y2ardl2_test)

# Out-of-sample :
MSE4y2 <- mean( (y2ardl2_test$y2var - y2ardl2_test$y2_test)^2 )
RMSE4y2 <- sqrt(MSE4y2)
RMSE4y2
```
<img width="114" alt="Screen Shot 2024-03-15 at 19 11 34" src="https://github.com/kivatmojo/econ_104/assets/137433466/779be6a8-6830-4bf9-a865-7dbcc9983a65">

Splitting our data 2/3-1/3 as a train-test split respectively, we calculated the RMSE for both models to assess how far off our model predicts the percentage change in the test data time frame to the actual points in the test data time frame. The first model is on average off by 15.26% and the second model is on average off by 14.29%. Using interest rates is slightly better at predicting percentage change.  

Though 14.29% seems large for a stock, we can rationalize this value considering the volatility of NVIDIA's stock, as well as NVIDIA's stock being less volatile than TESLA's stock.

```r
# AIC
AIC(ARDL1y2, ARDL2y2)
```
<img width="162" alt="Screen Shot 2024-03-15 at 19 11 48" src="https://github.com/kivatmojo/econ_104/assets/137433466/b9f96916-8be2-41c9-8516-0db058192961">

```r
# BIC
BIC(ARDL1y2, ARDL2y2)
```
<img width="167" alt="Screen Shot 2024-03-15 at 19 11 54" src="https://github.com/kivatmojo/econ_104/assets/137433466/1c3373b2-71b6-487d-adc1-f84290424afa">

To further investigate which variable is a better predictor of NVIDIA's stock monthly percentage change, we calculated the AIC and BIC values of both models, and it shows that the first model, using sentiment, shows to be the better performer by 1 point which is not much.

```r
# 10 step ahead forecast

fcast1y2 <- dLagM::forecast(ardl1y2.mod, h=10, c(x1mean,x1mean,x1mean,x1mean,x1mean,x1mean,x1mean,x1mean,x1mean,x1mean), interval=TRUE)

plot(fcast1y2$forecasts[,2],type="l", ylim=c(-50,50))
lines(fcast1y2$forecasts[,1], col="red")
lines(fcast1y2$forecasts[,3],col="red")
```

<img width="455" alt="Screen Shot 2024-03-15 at 19 12 04" src="https://github.com/kivatmojo/econ_104/assets/137433466/d0cc1e0e-d849-4c9a-95fc-9645a179e79d">

```r
fcast2y2 <- dLagM::forecast(ardl2y2.mod, h=10, c(x2mean,x2mean,x2mean,x2mean,x2mean,x2mean,x2mean,x2mean,x2mean,x2mean), interval=TRUE)

plot(fcast2y2$forecasts[,2],type="l", ylim=c(-50,50))
lines(fcast2y2$forecasts[,1], col="red")
lines(fcast2y2$forecasts[,3],col="red")
```
<img width="443" alt="Screen Shot 2024-03-15 at 19 12 16" src="https://github.com/kivatmojo/econ_104/assets/137433466/f04e27ba-a583-45eb-83a0-9fce08ffb08e">

To conclude our analysis on our ARDL models, we used both models to try and forecast what the next 10 month will look like for TESLA's stock in terms of percentage change.  

From what we see, the second model seems to also have a more realistic forecast compared to the first model's forecast which is relatively flatter, though a bit more "volatile" than our first ARDL model for TESLA.  

This solidifies that when wanting to use an ARDL model, using interest rates as a predictor is better than using news sentiment as a predictor.

***

## VAR(p): Vector AutoRegression

```r
# CCF

ccf(y1,y2, ylab="CCF", main="")
```
<img width="460" alt="Screen Shot 2024-03-15 at 19 12 31" src="https://github.com/kivatmojo/econ_104/assets/137433466/bf23521b-a783-411a-a086-0166fa20cd8c">

To start modelling our VAR model, we first looked at the Cross-Correlation Function (CCF) between TESLA (y1) and NVIDIA (y2).  

When looking at the CCF plot, TESLA’s stock price today appears to be lead by NVIDIA's stock price negatively 12 months back and positively 4 months back, which could suggest that NVIDIA's quarterly earnings report's effect on NVIDIA's stock may have predictive power over TESLA's stock price today. Two interesting spikes were that NVIDIA's stock price is negatively lead by TESLA’s price at 13 and 17 months back. These spikes could infer a negative relationship between NVIDIA's stock price with what TESLA's earning was a year ago and a quarter ago. 

```r
# Combine variables into 1 data frame

y <- cbind(y1var,y2var)
ycomb <- data.frame(y)
VARselect(ycomb, lag.max=12)
```
<img width="582" alt="Screen Shot 2024-03-15 at 19 12 54" src="https://github.com/kivatmojo/econ_104/assets/137433466/3556af45-52fe-4476-a6af-8592be0deb41">

We tested different lag structures to determine if there is any significant correlation between 1 and 12 months back for both TESLA and NVIDIA. From looking at the results, the models that included 1 and 4 lags back produced the lowest VAR AIC score. To choose between using a model with 1 or 4 lags back of variables, the model that includes data from 4 periods back provides more economic insight due to the relevant quarterly earnings reports.

```r
yVAR <- VAR(ycomb, p=4)
summary(yVAR)
```

Our model for TESLA that includes both lagged values of TESLA and NVIDIA in the past is a better model than a model that only includes lagged values of TESLA. This is because the P-value for this model is below 5%, meaning the coefficient for the NVIDIA variable is not zero. This reveals that there is some information in NVIDIA that provides predictive power for TESLA stock.

```r
# Granger-Causality
grangertest(y1~y2, order=4)
```

<img width="354" alt="Screen Shot 2024-03-15 at 19 13 40" src="https://github.com/kivatmojo/econ_104/assets/137433466/63d78672-67c1-490f-9cbc-963258d75987">

```r
grangertest(y2~y1, order=4) 
```
<img width="334" alt="Screen Shot 2024-03-15 at 19 13 55" src="https://github.com/kivatmojo/econ_104/assets/137433466/ef9cc617-43e8-4ad2-a5da-f51de35ba5fb">

In regressing TESLA stock on lagged values of TESLA and NVIDIA, the Granger-Causality test produced a high P-value of 0.409 which means we fail to reject the null hypothesis that NVIDIA stock price does not provide information to help predict TESLA stock price today.  

When regressing NVIDIA stock on lagged values of NVIDIA and TESLA, the granger-causality test also produced a high p-value of 0.565 which means historical TESLA prices do not provide predictive power of NVIDIA's stock price.

```r
# IRFs

irfVAR <- irf(yVAR, n.ahead=12)
plot(irfVAR)
```
<img width="330" alt="Screen Shot 2024-03-15 at 19 14 08" src="https://github.com/kivatmojo/econ_104/assets/137433466/5e9bf368-5a25-4ddf-826b-c7abd295d533">
<img width="343" alt="Screen Shot 2024-03-15 at 19 14 19" src="https://github.com/kivatmojo/econ_104/assets/137433466/cb2aa2d1-5668-46a2-9042-824049287519">

The impulse response function reveals that information from lagged values of TESLA and NVIDIA on TESLA and NVIDIA are significant at one period and four periods back. For example, when regressing TESLA on TESLA and NVIDIA, a shock in NVIDIA's stock price will have an effect on TESLA's stock price in the most recent period and four periods back - vice/versa. The increase in the effect of shocks four periods back is odd but makes sense when thinking about financial reporting and quarterly earnings reports.

```r
# Plot (Data, fitted, ACF, PACF)
plot(yVAR)
```
<img width="606" alt="Screen Shot 2024-03-15 at 19 14 49" src="https://github.com/kivatmojo/econ_104/assets/137433466/a80fde04-84a4-448d-bfad-4de3b6bdee78">
<img width="622" alt="Screen Shot 2024-03-15 at 19 14 55" src="https://github.com/kivatmojo/econ_104/assets/137433466/d51c4e3b-93ba-42b1-9683-4545c4b37605">

```r
plot(y)
```
<img width="500" alt="Screen Shot 2024-03-15 at 19 15 02" src="https://github.com/kivatmojo/econ_104/assets/137433466/6d60089a-1e3f-4a00-9466-d4394b0443e4">

```r
# Train Test Split for y1

VAR <- data.frame(ycomb)
nVAR <- nrow(VAR)

VAR$lag1y1 <- dplyr::lag(y1var,1)
VAR$lag2y1 <- dplyr::lag(y1var,2)
VAR$lag3y1 <- dplyr::lag(y1var,3)
VAR$lag4y1 <- dplyr::lag(y1var,4)

VAR$lag1y2 <- dplyr::lag(y2var,1)
VAR$lag2y2 <- dplyr::lag(y2var,2)
VAR$lag3y2 <- dplyr::lag(y2var,3)
VAR$lag4y2 <- dplyr::lag(y2var,4)


VAR_train1 <- VAR[1:121,]
VAR_test1 <- VAR[122:nVAR,]

VARmod1 <- lm(y1var ~ lag1y1 + lag2y1 + lag3y1 + lag4y1 + lag1y2 + lag2y2 + lag3y2 + lag4y2, data=VAR_train1)
VAR_test1$y1_test <- predict(VARmod1,VAR_test1)

# Out-of-sample :
MSE1VAR <- mean( (VAR_test1$y1var - VAR_test1$y1_test)^2 )
RMSE1VAR <- sqrt(MSE1VAR)
RMSE1VAR
```
<img width="119" alt="Screen Shot 2024-03-15 at 19 15 14" src="https://github.com/kivatmojo/econ_104/assets/137433466/a1586407-ddf8-4823-9f27-bf53066279f5">

```r
# Train Test Split for y2

VAR_train2 <- VAR[1:121,]
VAR_test2 <- VAR[122:nVAR,]

VARmod2 <- lm(y2var ~ lag1y1 + lag2y1 + lag3y1 + lag4y1 + lag1y2 + lag2y2 + lag3y2 + lag4y2, data=VAR_train2)
VAR_test2$y2_test <- predict(VARmod2,VAR_test2)

# Out-of-sample :
MSE2VAR <- mean( (VAR_test2$y2var - VAR_test2$y2_test)^2 )
RMSE2VAR <- sqrt(MSE2VAR)
RMSE2VAR
```
<img width="155" alt="Screen Shot 2024-03-15 at 19 15 19" src="https://github.com/kivatmojo/econ_104/assets/137433466/71219337-4417-465d-a472-303978d4a913">

When dividing the dataset and testing the VAR model of TESLA on TESLA and NVIDIA, we produced a RMSE of 19.08. When Regressing NVIDIA on NVIDIA and TESLA, we found a RMSE of 15.57 - an almost 25% lower error. This shows that it's far easier to predict the future stock price of NVIDIA than TESLA, and this makes sense because TESLA is historically a more volatile stock.

```r
# AIC 
AIC(yVAR)
```
<img width="119" alt="Screen Shot 2024-03-15 at 19 15 53" src="https://github.com/kivatmojo/econ_104/assets/137433466/b277a574-296b-41d3-a4b1-b00c2025d8c8">

```r
# BIC
BIC(yVAR)
```
<img width="143" alt="Screen Shot 2024-03-15 at 19 15 59" src="https://github.com/kivatmojo/econ_104/assets/137433466/473f6f40-e418-4ae2-bda8-bab5f2c53ed3">

```r
# n-step-ahead Forecast
varpredict <- predict(object=yVAR, n.ahead=12)
plot(varpredict)
```
<img width="414" alt="Screen Shot 2024-03-15 at 19 16 07" src="https://github.com/kivatmojo/econ_104/assets/137433466/68e9d81d-f051-43f2-ab59-56c6b4680efd">

Our 10-step ahead forecast for both TESLA and NVIDIA predicts relatively stable changes in stock prices between 0% and 5%. This makes sense as the average economic growth rate for a healthy economy is around 2-3%.

```r
# FEVD
plot(fevd(yVAR, n.ahead=12))
```
<img width="445" alt="Screen Shot 2024-03-15 at 19 16 14" src="https://github.com/kivatmojo/econ_104/assets/137433466/82b747ba-68ed-4cd0-941f-b2a1cf47bbae">

The FEVD plot for TESLA shows that the forecasted stock price error variance is attributable to historical TESLA data. On the same token, NVIDIA’s forecasted stock price errors are attributed to historical data of itself. This is consistent with our findings in the IRF where the prediction power of the model is heavily dependent on the lagged values of the response variable compared to other variables.

```r
# CUSUM - Cumulative Sum Plot
plot(stability(yVAR, type="Rec-CUSUM"),plot.type="single")
```
<img width="462" alt="Screen Shot 2024-03-15 at 19 16 28" src="https://github.com/kivatmojo/econ_104/assets/137433466/38be0070-817b-4d5e-90b3-b3cdf72d917b">
<img width="464" alt="Screen Shot 2024-03-15 at 19 16 35" src="https://github.com/kivatmojo/econ_104/assets/137433466/ff351bc2-75fe-4fbe-9893-3a2bcd44155e">

Our CUSUM plot shows that our model's black line (mean cumulative sum of recursive errors) is close to 0 and between the confidence interval - telling us that both of our models are stable and show no parameter instabilities which means that the coefficients of the model does not change significantly over time. Additionally, our model residuals are homoskedastic, not auto-correlated, and will perform relatively well both in and out of sample data.  

One important note is that as time progresses and we forecast more into the future, the cumulative sum of recursive errors increases past 0.4 units of time. This is expected as it becomes harder to predict stock prices further into the future.

***

## Conclusions and Findings

After analyzing all the findings from our project, we conclude that no significant relationship exists between NVIDIA and TESLA. Even though Elon Musk had said himself that TESLA relies on the hardware that NVIDIA produces, it seems that based on their relationship with NVIDIA is not enough to predict TESLA’s percentage change in stocks. We found that increasing the lags would make better predictions, but the Granger causality test and FEVD agree that using NVIDIA’s lagged values is no better than just using TESLA’s lagged values. Referencing the p-values from Granger and the visual confirmation from FEVD. It seems like using each stock data respectively would explain their individual growth would be better than trying to use each other to predict some relationship, possibly a case of correlation is not causation.  


This conclusion is also backed by the overall performance of our models by looking at the RMSE, AIC, and BICs of all our AR, ARDL, and VAR models which shows that for both TESLA and NVIDIA stock, there is a much better predictive power over their respective stocks when using their own lags, or an external variable and its lags. In our case here, the best variable to use to predict a stock's monthly percentage change is interest rates over news sentiment.

