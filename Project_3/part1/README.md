#  Panel Data Modelling

***

## Project Prompt: [Project_3.pdf](https://github.com/kivatmojo/econ_104/files/14553702/Project_3.pdf)

***

## Final Paper: 

***

Datasets used:  

Financial Statements of Major Companies (2009 - 2023)  

https://www.kaggle.com/datasets/rish59/financial-statements-of-major-companies2009-2023?resource=download  
  

***

Table of Contents:  
1. Business Question  
2. Analysis of Variables  
3. Model Fitting  
4. [Conclusion](#conclusion)

***

## Business Question

Here we aim to analyze financial statement data to assess the potential predictability of a company's market capitalization, Our hypothesis posits that the data holds predictive power since market capitalization is closely linked to share price, which in turn is influenced by investor sentiment.  

In pursuit of building the most precise model, we will evaluate the accuracy across pooled, fixed, and random effects models. The pooled model assumes uniform predictive power across all categories, irrespective of the firm and time. Conversely, the fixed effect and random effects models consider variations in predictive power among financial statement items across different firms and time.

## Analysis of Variables

```r
# Import Data
financials <- read.csv("finstatements.csv") 

# Convert to Panel Data Frame
financials <- pdata.frame(financials,c("Company","Year")) 

# Remove Company Category Column
financials <- financials[,-3] 

# Rename Columns
financials <- financials %>%
  rename(
    marketcap = Market.Cap.in.B.USD.,
    revenue = Revenue,
    eps = Earning.Per.Share,
    currentratio = Current.Ratio,
    der = Debt.Equity.Ratio,
    roe = ROE,
    roa = ROA,
    fcfps = Free.Cash.Flow.per.Share,
    inflation = Inflation.Rate.in.US.) 

# Remove any NA Values
financials <- na.omit(financials) 

# Keep columns of interest
financials <- financials[,c("Year", "Company","marketcap","revenue","currentratio","eps","der",
                            "roe","roa","fcfps","inflation")]
```

Due to expected multicolinearity between our variables, we reduced the dataset to certain columns. The variables we decided to keep are based on explainability of our dependent variable, and their respective accounting definition not overlapping with each other

```r
# Check if we have a balanced panel
pdim(financials)
```

We see that we do not have a balanced panel and will keep this in mind but due to the nature of our unbalanced panel, this does not keep us from continuing with out dataset.

```r
# Five-Number Summary
summary(financials)
```
Our data contains financial statement data for some of the largest companies such as Apple and Amazon, as well as a few bankrupted companies. As such, that is why we see a wide range of values for free cash flow, debt to equity ratio, and earnings per share. For example, we see a minimum debt to equity ratio of -11.7750, which signifies the company is highly leveraged and may potentially be insolvent depending on their assets.

```r
# Box Plots
par(mfrow = c(1,3))

boxplot(financials$marketcap, main="Market Cap in $billion")
boxplot(financials$revenue, main="Revenue")
boxplot(financials$eps, main="EPS")
boxplot(financials$currentratio, main="Current Ratio")
boxplot(financials$der, main="DER")
boxplot(financials$roe, main="ROE")
boxplot(financials$roa, main="ROA")
boxplot(financials$fcfps, main="FCFPS")
boxplot(financials$inflation, main="Inflation")

```
Our box plot data shows that many of the companies in the dataset are performing well off of multiple metrics.  There are many upper-quartile outliers for market cap, revenue, and current ratio. This suggests that there are many growth stocks in the dataset, as well as companies with strong financials due to their current ratio and balanced debt to equity ratio. Lastly, inflation displays 2 upper-quartile outliers which may be attributed to the Covid-19 pandemic and fiscal policy in early years.

```r
# Histograms
par(mfrow = c(2,3))

hist(financials$marketcap, main="Market Cap in $billion", xlab = "")
hist(financials$revenue, main="Revenue", xlab = "")
hist(financials$eps, main="EPS", xlab = "")
hist(financials$currentratio, main="Current Ratio", xlab = "")
hist(financials$der, main="DER", xlab = "")
hist(financials$roe, main="ROE", xlab = "")
hist(financials$roa, main="ROA", xlab = "")
hist(financials$fcfps, main="FCFPS", xlab = "")
hist(financials$inflation, main="Inflation", xlab = "")

```
The market cap and histogram are consistent and skewed to the right. This is expected because Revenues influence share price, which therefore ties to market cap. On the same token, the current ratio is also skewed to the right, signifying that many companies have relatively small current ratios and a few companies have large current ratios. Large current ratios aren’t necessarily a good thing as assets may become unproductive. Return on assets is normally distributed which makes sense as returns vary by industry but generally grow at the same rate as the economy at maturity ~ approximately 3%.

```r
cor.fin <- cor(financials %>% dplyr::select("marketcap" = "revenue", "eps","currentratio","der","roe","roa","fcfps", "inflation"))
corrplot(cor.fin, method = "square", tl.col="black")
```

The first notable relationship is the negative correlation between earnings per share and free cash flow per share. When the cash flow after paying off all liabilities increases, the earnings per share decreases. This is very surprising and unexpected as free cash flow is closely related to net Income. Another correlation is the positive relationship between current ratio and return on assets. This means as you accumulate more assets, your return on them will increase. This reveals that companies are efficiently using assets to generate additional income.

```r
par(mfrow = c(2,2))

plot(marketcap~revenue, data=financials, xlab = "Revenue", ylab = "Market Cap in $billion")
plot(marketcap~eps, data=financials, xlab = "EPS", ylab = "Market Cap in $billion")
plot(marketcap~currentratio, data=financials, xlab = "Current Ratio", ylab = "Market Cap in $billion")
plot(marketcap~der, data=financials, xlab = "DER", ylab = "Market Cap in $billion")
plot(marketcap~roe, data=financials, xlab = "ROE", ylab = "Market Cap in $billion")
plot(marketcap~roa, data=financials, xlab = "ROA", ylab = "Market Cap in $billion")
plot(marketcap~fcfps, data=financials, xlab = "FCFPS", ylab = "Market Cap in $billion")
plot(marketcap~inflation, data=financials, xlab = "Inflation", ylab = "Market Cap in $billion")
```

Revenue, current ratio, return on assets, and inflation showed interesting relationships to market cap. As revenue and return on assets increased, market cap also increased as expected. This is because high revenues and productive uses of assets are attractive to investors. Next, as the current ratio increased, the market cap of companies decreased. This suggests that there are also many companies who are inefficiency allocating their assets to generate revenue. This is consistent with the idea that more assets doesn't necessarily mean more growth or larger market cap. Lastly, there have been companies with varying market caps at different inflation levels. This makes sense as the economy will go through cycles but companies are more robust if their balance sheets are strong. There does seem to be more smaller market cap companies when inflation is lower. This makes sense as lower inflation levels encourage investment into the economy such as starting new businesses. 

## Model Fitting

For all our models, we have decided to log our dependent variable, market cap, and one of our predictors, revenue, due to the scaling of these variables.

```r
# Pooled Model
pool <- plm(log(marketcap) ~ log(revenue) + eps + currentratio + der + roe + roa + 
              fcfps + inflation, data = financials, model = "pooling")

pool.lm <- lm(log(marketcap) ~ log(revenue) + eps + currentratio + der + roe + roa + 
              fcfps + inflation, data = financials)

# Full Fixed Effects Model - Time and Firm
full <- plm(log(marketcap) ~ log(revenue) + eps + currentratio + der + roe + roa + 
              fcfps + inflation, data = financials, model = "within", effect = "twoways")

full.lm <- lm(log(marketcap) ~ log(revenue) + eps + currentratio + der + roe + roa + 
              fcfps + inflation + Year + Company, data = financials)

# Time Effects
time <- plm(log(marketcap) ~ log(revenue) + eps + currentratio + der + roe + roa + 
              fcfps + inflation, data = financials, model = "within", effect = "time")

time.lm <- lm(log(marketcap) ~ log(revenue) + eps + currentratio + der + roe + roa + 
              fcfps + inflation + Year, data = financials)

# Firm Effects
firm <- plm(log(marketcap) ~ log(revenue) + eps + currentratio + der + roe + roa + 
              fcfps + inflation, data = financials, model = "within", effect = "individual")

firm.lm <- lm(log(marketcap) ~ log(revenue) + eps + currentratio + der + roe + roa + 
              fcfps + inflation + Company, data = financials)
```

```r
# Time and Firm
pFtest(full,pool)
```
The results of the F test between our full and pooled model states that there are differences between either firm or time. To investigate, we run the same test against our other models.

```r
# Time
pFtest(time,pool)
```
The result of this F test between our pooled model and our time effects model concludes that some of the difference between our models is contributed by time. We next run a test using our firm effects model.

```r
# Firm
pFtest(firm,pool)
```
As our test results are, we can conclude that our firm fixed effects model contribute to the differences. While the differences between firms seem larger due to the much lower p-value on our F-test compared to our p-value on our F-test when evaluating our time fixed effects model, it was still significant and thus we want to move forward with having our fixed effects model take into account both time and firm, keeping in mind that firms has a larger effect on our model. 

Plotting below the coefficients of both firm and time effects shows that there are more differences in the coefficients of each company than each time period.

```r
# Coefficient Plot by Time
coefplot(full.lm, predictors = "Year")
```

```r
# Coefficient Plot by Firm
coefplot(full.lm, predictors = "Company")
```

Plotting the means and CIs below of both Year and Company against market cap will provide us an additional visual proof of the differences in our predictors.

```r
# Mean Plots by Time
plotmeans(log(marketcap)~Year, data=financials, xaxt='n')
par(las=2)
axis(side=1, at=1:length(unique(financials$Year)), labels=unique(financials$Year), cex.axis=0.7)
```

```r
# Mean Plots by Firm
plotmeans(log(marketcap)~Company, data=financials, xaxt='n')
par(las=2)
axis(side=1, at=1:length(unique(financials$Company)), labels=unique(financials$Company), cex.axis=0.7)
```

As we can see, our plot grouped by time shows a constant overlap between every single time period, while our plot grouped by firms seem to have much more of a difference between every firm. 

To further explore this conclusion, the plots below shows that when grouping our data by firm shows more of a grouping effect between firms compared to a quite random scatter on the plot between time periods.

```r
# Market Cap vs Revenue by Firm
ggplot(financials, aes(x=log(revenue), y=log(marketcap), colour=factor(Company))) + 
  geom_point() + xlab("log(revenue)") + ylab("log(marketcap)") + 
  scale_colour_discrete(name="Company")
```

```r
# Market Cap vs Revenue by Time
ggplot(financials, aes(x=log(revenue), y=log(marketcap), colour=factor(Year))) + 
  geom_point() + xlab("log(revenue)") + ylab("log(marketcap)") + 
  scale_colour_discrete(name="Year")
```


```r
# Final Fixed Effects Model
summary(full)
```

```r
# Random Effects Model
random <-  plm(log(marketcap) ~ log(revenue) + eps + currentratio + der + roe + roa + 
              fcfps + inflation, data = financials, model = "random")

summary(random)
```

```r
# Hausman Test
phtest(full, random)
```
Due to the low p-value and a high chi-squared test statistic, we reject our H0. We conclude that our full fixed effects model is better.  

## Conclusion

In conclusion, our preferred model to predict a company's market cap given their financial information is our full fixed effects model that accounts both time and firm differences.
