#  Qualitative Dependent Variable Modelling (Linear Probability, Probit, Logit)

***

## Project Prompt: [Project_3.pdf](https://github.com/kivatmojo/econ_104/files/14553702/Project_3.pdf)


***

## Final Paper: [Project_3.pdf](https://github.com/kivatmojo/econ_104/files/14621854/Project_3.pdf)

***

Datasets used:  
NFL Team Data (2003 - 2023)  
https://www.kaggle.com/datasets/nickcantalupa/nfl-team-data-2003-2023

***

Table of Contents:  
1. Business Question  
2. Analysis of Variables  
3. Model Fitting  
4. [Conclusion](#conclusion) 

***

## Business Question

What we aim to do here is to find the relationship between what an NFL team does during games throughout the season and their probability of having a winning season record (.500 or over), and how to best predict that using either a linear probability model, a probit model, or a logit model.

## Analysis of Variables
```r
# Load Data
stats <- read.csv("nflstats.csv")
stats_og <- read.csv("nflstats.csv")

# Convert dependent variable into binary
stats$winning <- ifelse(stats$win_loss_perc >= 0.500,1,0)

# Keep variables of interest
varinterest <- c("winning","pass_att","pass_net_yds_per_att","rush_att","rush_yds_per_att","pass_int","turnovers","penalties")
sumstats <- stats[,varinterest]

# Rename certain variables
sumstats <- sumstats %>% 
  rename(
    avgpassyds_peratt = pass_net_yds_per_att,
    avgrushyds_peratt = rush_yds_per_att
  )

stats_og <- stats_og %>% 
  rename(
    avgpassyds_peratt = pass_net_yds_per_att,
    avgrushyds_peratt = rush_yds_per_att
  )

summary(sumstats)
```
Above, we converted the original win-lose percentage in our dataset into a binary variable: 1 if .500 or over (not a losing record), 0 if under .500 (losing record).  

We also selected which variables we wanted to keep based on multicolinearity and knowledge of the sport. We decided to make our models using a team's pass attempts, pass yards per attempt, rush attempts, rush yards per attempt, passes intercepted, turnovers committed, and penalties committed.  

We decided not to include variables such as total yards, offensive snaps played, fumbles lost, and any touchdown metrics due to multicolinearity between these and what we decided to keep, and also to not include variables such as expected total points, points, and first downs due to its irrelevance or the metric simply being a metric where it is obvious that when the larger number is, the lower percentage of a losing record there is such as touchdowns.

```r
# Bar Plot for Binary Variable

ggplot(data = sumstats, aes(x = winning)) + 
  geom_bar() +
  ggtitle("Season Record")
```
The bar plot for our dependent variable shows that in our dataset, there are more teams that has had a winning season record than a losing season record. The differences between the two is not significant and allows us to continue with this dataset.  

```r
# Box Plots

par(mfrow = c(2,4))
boxplot(sumstats$pass_att, main="Pass Attempts")
boxplot(sumstats$avgpassyds_peratt, main="Avg Pass Yards per Attempt")
boxplot(sumstats$rush_att, main="Rush Attempts")
boxplot(sumstats$avgrushyds_peratt, main="Avg Rush Yards per Attempt")
boxplot(sumstats$pass_int, main="Passed Intercepted")
boxplot(sumstats$turnovers, main="Turnovers")
boxplot(sumstats$penalties, main="Penalites Committed")

```

Our box plots of our explanatory variables reveal few outliers. Due to the nature of the game, where most teams have similar overall play-calling, this makes sense.

```r
# Histograms
par(mfrow = c(2,4))

hist(sumstats$winning, main="Season Record", xlab = "") # No need?
hist(sumstats$pass_att, main="Pass Attempts", xlab = "")
hist(sumstats$avgpassyds_peratt, main="Avg Pass Yards per Attempt", xlab = "")
hist(sumstats$rush_att, main="Rush Attempts", xlab = "")
hist(sumstats$avgrushyds_peratt, main="Avg Rush Yards per Attempt", xlab = "")
hist(sumstats$pass_int, main="Passes Intercepted", xlab = "")
hist(sumstats$turnovers, main="Turnovers", xlab = "")
hist(sumstats$penalties, main="Penalites Committed", xlab = "")

```

Our data shows realtively normal distributions across all statistics except for season records. At the elite levels, these statistics should converge to roughly the same means across different variables for different teams. If not, the league would be relatively noncompetitive. Season record is not normally distributed because it is a binary variable, not continuous.

```r
cor.nfl <- cor(sumstats %>% dplyr::select("winning" = "pass_att","avgpassyds_peratt","rush_att","avgrushyds_peratt","pass_int","turnovers","penalties"))
corrplot(cor.nfl, method = "square", tl.col="black")
```

Our correlation plot shows an interesting relationship between rush attempts and winning, as well as turnovers and pass interceptions. Rush attempts and winning have a negative relationship, which is something somewhat unexpected given the 49ers and Christian McCaffrey's latest season. However, this statistic may also signify the strength of passing in the league today. Turnovers and pass interceptions are positively correlated because a pass interception is a turnover.  

```r
par(mfrow = c(2,2))

plot(winning~pass_att, data=sumstats, xlab = "Pass Attempts", ylab = "Season Record")
plot(winning~avgpassyds_peratt, data=sumstats, xlab = "Avg Pass Yards per Attempt", ylab = "Season Record")
plot(winning~rush_att, data=sumstats, xlab = "Rush Attempts", ylab = "Season Record")
plot(winning~avgrushyds_peratt, data=sumstats, xlab = "Avg Rush Yards per Attempt", ylab = "Season Record")
plot(winning~pass_int, data=sumstats, xlab = "Passes Intercepted", ylab = "Season Record")
plot(winning~turnovers, data=sumstats, xlab = "Turnovers", ylab = "Season Record")
plot(winning~penalties, data=sumstats, xlab = "Penalites Committed", ylab = "Season Record")
```
When regressing the season record on each explanatory variable, there seems to be a relatively equal distribution among the data. However, consistent with the plots above, pass yards and winning have a positive correlation, and pass interceptions and losing have a positive correlation.

## Model Fitting

### Linear Probability Model

```r
# OLS Model / Linear Probability Model 
probmod <- lm(winning ~ pass_att + avgpassyds_peratt + rush_att + avgrushyds_peratt + pass_int + turnovers + penalties, data=sumstats)
```

```r
# Predicted vs Actual

lp.pred.class <- ifelse(fitted(probmod) >= 0.500,1,0)
table(lp.pred.class, sumstats$winning)
```

```r
# Accuracy

mean(lp.pred.class == sumstats$winning)
```
Our model correctly predicts 79.177% of the time.  

```r
# AME

coef(probmod)
```

While our probit and logit model will require us to not interpret our coefficients but instead their AMEs, the coefficients for our linear probability model is interpetable as the marginal effect of a one unit increase of the variable. For instance, for every pass attempt done in a season, that increases the probability of not having a losing record by 0.05% and when the number of pass attempts can go as high as 751 pass attempts, that means only by attempting to pass a ball 751 times in a season, a team can increase their .500 or over record probability to 37.55% according to our linear probability model.  

A note on the coefficient for average pass yards per attempt: it can be seen that it is incredibly high at 25.61% per attempt. This does not mean that by every yard gained on a pass attempt simply increases their probability of winning by 25%. Instead, this means that for a team to increase thei probability of not having a losing record by the end of a season by 25%, the team needs to on average gain 1 yard on every single pass attempt in every single game. This makes more sense as the more yardage a team gets, the more chances of winning a game they will have.  

Also to note, the reduction of probability due to bad statistics (passes intercepted, turnovers, and penalties) makes sense.  

Most of these ideas will also translate for our probit and logit models.

### Probit Model
```r
probitmod = glm(winning ~ pass_att + avgpassyds_peratt + rush_att + avgrushyds_peratt + pass_int + turnovers + penalties, family=binomial(link="probit"), data=sumstats)
```

```r
# Predicted vs Actual

pb.pred.class <- ifelse(fitted(probitmod) >= 0.5,1,0)
table(pb.pred.class, sumstats$winning)
```

```r
# Accuracy

mean(pb.pred.class == sumstats$winning)
```
Our model correctly classified 78.6% of our observations. 

Below, we will test the model's accuracy by doing a 5-fold cross validation test on our model that uses 75% of the data as training data, and testing the resulted model against our actual test data. 

```r
# Train-Test 5-fold Cross Validation

intrain <- createDataPartition(sumstats$winning, p=.75, list=FALSE)
train <- sumstats[intrain,]
test <- sumstats[-intrain,]

train_control <- trainControl(method="cv",
                              number = 5)

probit <- train(as.factor(winning)~.,
               data = train,
               method = "glm",
               family = binomial(link="probit"),
               trControl = train_control)

pred_winning_pb = predict(probit,newdata=test)

confusionMatrix(pred_winning_pb, as.factor(test$winning), mode = "everything", positive = "1")
```
The conclusion we get here is that the model is 75.6% accurate. While lower than our previously calculated accuracy value, it is close enough to say that our model performs well when tested on out of sample data.

```r
# AME

sum_phi <- mean(dnorm(predict(probitmod, type = "link")))
ame_pb = sum_phi*coef(probitmod)
print(ame_pb)
```

The average marginal effect for our variables shows to be lower in magnitude for our probit model compared to our linear probability model's effects. As expected, positive stats have positive signs and negative stats have negative signs.

### Logit Model
```r
logitmod = glm(winning ~ pass_att + avgpassyds_peratt + rush_att + avgrushyds_peratt + pass_int + turnovers + penalties, family=binomial(link="logit"), data=sumstats)
```

```r
# Predicted vs Actual

log.pred.class <- ifelse(fitted(logitmod) >= 0.5,1,0)
table(log.pred.class, sumstats$winning)
```

```r
# Accuracy

mean(log.pred.class == sumstats$winning)
```
Our model correctly classified 78.7% of our observations. 

When getting our fitted values using the model extracted from using our entire data, our model correctly predicted the actual values 78.7% of the time.

```r
# AME

sum_log <- mean(dnorm(predict(logitmod, type = "link")))
ame_log = sum_log*coef(logitmod)
print(ame_log)
```

The average marginal effect for our variables also shows to be lower in magnitude for our logit model compared to our linear probability model's effects, but higher than our probit model. This shows the logit model is more of a middle ground in terms of variable effects.  

The logit model can be interpreted as a change in the log odds of the outcome for a one unit change in the explanatory variable.

```r
# Train-Test 5-fold Cross Validation

logit <- train(as.factor(winning)~.,
               data = train,
               method = "glm",
               family = binomial(link="logit"),
               trControl = train_control)

pred_winning_log = predict(logit,newdata=test)

confusionMatrix(pred_winning_log, as.factor(test$winning), mode = "everything", positive = "1")
```
Here, we see that all the statistics of the logit's train test model is the exact same as the probit's train test model. This is a result of having a relatively low sample size, and that the function and curvature of both probit function and logit function are similar in its end calculation and predictive ability.

## Conclusion

Using all our test results, we have decided to use our logit model as our best model at predicting whether an NFL team will have a losing record or not by the end of the NFL season.  

This is due to the marginal differences of accuracy between all our models, and our logit model beating that tie breaker by simply how the logit function is set up and constructed. We believe that the logit model has a higher reliability compared to our linear probability model due to the possibility of LPMs giving us a probability lower than 0 and higher than 1 which is not possible, and that a logit model is more interpretable than a probit model. 

With that said, below we will make 4 predictions of a team's record using our logit model using different values of rush attempts within the season, while keeping all other statistics constant around its mean. The number of rushing attempts throughout the season we decided to use to predict how rushing attempts affect the probability of a team not having a losing record is the 1st quantile value, the mean, the 3rd quantile value, and the max value.  

Rush attempts is an interesting variable because it does show that it has the highest correlation to a team's record based on the correlation plot.

```r
stats = data.frame(pass_att = 546.9,
                          avgpassyds_peratt = 6.188,
                          rush_att = c(401,437.8,472,618),
                          avgrushyds_peratt = 4.2,
                          pass_int = 14.65,
                          turnovers = 24.71,
                          penalties = 101.5)

predict(logitmod, stats, type="response")
```

Based on our results, the probability of a team not having a losing record by the end of the season gets higher when going for more rush attempts throughout the season. This is assuming that teams perform averagely on other variables in our model. We believe that if we lowered the literal number of "negative" statistics such as turnovers, the probability at every interval and as a whole will increase. Same goes if we increase the literal number of our "positive" statistics.  

To comment on the reliability of our model, even at around 75% accuracy, sensitivty, and specificity, our model is quite accurate considering the nature of our data and outcome variable. Teams can input different numbers for each variable to see what their target for the season should be while keeping the play by play efficiency high when trying to reach that goal in order to increase their proability of not having a losing record.
