# Multiple Regression Modelling
***

## Project Prompt: [Project1.pdf](https://github.com/kivatmojo/econ_104/files/14426495/Project1.pdf)
## Final Paper: [Project_1.pdf](https://github.com/kivatmojo/econ_104/files/14412101/Project_1.pdf)

***

## Project Prompt:  
For this project, you will work with any dataset you like, however, it must contain at least 10 different predictors and one response variable you will aim to predict. Your task will be to find a reasonable model by following the 11 steps outlined below. As an illustration of a good dataset, the file diamonnds.csv contains the prices and other attributes of almost 54,000 diamonds. The data description and file can be accessed directly from [Kaggle](https://www.kaggle.com/shivam2503/diamonds) and the goal is to predict diamond prices.  

***

Dataset: [Most Streamed Songs on Spotify in 2023](https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023) on Kaggle 

***

## Table of Contents
1. Load and Prepare Data  
2. Analysis of Data  
3. Regression Model  
4. Analysis of Observations  
5. Variable Selection  
6. Multicollinearity
7. Residuals
8. RESET Test
9. Test for Heteroskedasticity
10. Adjusting for Heteroskedasticity
11. Model Selection and Comparison
12. Estimate Model Performance
13. [Conclusions and Findings](#conclusions-and-findings)

***
## Load and Prepare Data
```r
# Load Data
original_data <- read.csv("spotify-2023.csv") # Reference Data
music_data <- read.csv("spotify-2023.csv") # Working Data

# Change "streams" from character to integer, and scale down the figures
original_data$streams <- as.integer(original_data$streams)
totalstreams <- as.integer(music_data$streams)

# Scale down streams to thousands
streams_thousands <- totalstreams/1000
streams_thousands <- as.integer(streams_thousands)

# Create a new table with relevant variables
music_data <- data.frame (streams_thousands, music_data$bpm, music_data$artist_count, 
                       music_data$in_apple_charts, 
                       music_data$in_apple_playlists, music_data$in_spotify_charts, 
                       music_data$in_spotify_playlists, music_data$danceability_.,
                       music_data$valence_., music_data$energy_., 
                       music_data$acousticness_., music_data$instrumentalness_., 
                       music_data$liveness_., music_data$speechiness_.)

# Get rid of observations with NA values - Removed 26 observations
music_data <- na.omit(music_data)

# Rename Columns
colnames(music_data) <- c("streams_thousands", "bpm", "artist_count", "in_apple_charts", 
                       "in_apple_playlists", "in_spotify_charts", "in_spotify_playlists", 
                       "danceability", "valence", "energy", "acousticness", 
                       "instrumentalness", "liveness", "speechiness")

```
***
## Analysis of Data  
### 1. Five Number Summary
```r
summary(music_data)
```
<img width="487" alt="Screen Shot 2024-03-15 at 18 27 54" src="https://github.com/kivatmojo/econ_104/assets/137433466/f7f85fbf-a6c5-4761-9c77-8b387a90a8ba">

These tables give us the five statistical summaries of each of our response and explanatory variables. The point of this section is to give us a simple explanation of the character of our data.  

One variable we found interesting is “instrumentalness”. The median is 0 and the mean is 1.6 - telling us that our dataset contains songs that are not instrumental.  

Another important factor is the “streams” variable. Our data contains songs with streams as little as 2.8 thousand to 2,135,158 thousand. As such, our data is very comprehensive.  

\newpage

### 2. Box Plot

```r
par(mfrow = c(2,3))

boxplot(music_data$streams_thousands, ylab = "Total Streams in Thousands") 
boxplot(music_data$bpm, ylab = "BPM") 
boxplot(music_data$in_spotify_playlists, ylab = "# of Spotify Playlists")
boxplot(music_data$in_spotify_charts, ylab = "# of Spotify Charts")
boxplot(music_data$in_apple_playlists, ylab = "# of Apple Playlists")
boxplot(music_data$in_apple_charts, ylab = "# of Apple Charts")
```
<img width="419" alt="Screen Shot 2024-03-15 at 18 28 23" src="https://github.com/kivatmojo/econ_104/assets/137433466/f057820a-de93-4428-b862-d13f2593a4c3">

```r
music_character <- gather(music_data, key = "Characteristics", value = "Percentage", 
                          danceability, valence, energy, acousticness, instrumentalness, 
                          liveness, speechiness)
ggplot(music_character, aes(x = Characteristics, y = Percentage)) + geom_boxplot()
```
<img width="395" alt="Screen Shot 2024-03-15 at 18 28 33" src="https://github.com/kivatmojo/econ_104/assets/137433466/fbc407b4-26af-48f3-a9cb-b4eb981ca245">

The box plots are a visual representation of the five statistical summary data shown above. The box represents the data between the 25th and 75th percentile while the bold black line indicates the mean of the data.  

One interesting variable we found interesting were “streams” because there were numerous outliers past the 1.5 standard deviation. This makes sense because the average song has low streams but there are also many individually popular songs.  

### 3. Histograms
```r
par(mfrow = c(3,3))

hist(music_data$streams_thousands, main="Streams in Thousands")
hist(music_data$bpm, main="BPM")
hist(music_data$in_spotify_playlists, main="# of Spotify Playlists")
hist(music_data$in_spotify_charts, main="# of Spotify Charts")
hist(music_data$in_apple_playlists, main="# of Apple Playlists")
hist(music_data$in_apple_charts, main="# of Apple Charts")
hist(music_data$danceability, main="Danceability %") 
hist(music_data$valence, main="Valence %")
hist(music_data$energy, main="Energy %")
hist(music_data$acousticness, main="Acousticness %")
hist(music_data$instrumentalness, main="Instrumentalness %")
hist(music_data$liveness, main="Liveness %")
hist(music_data$speechiness, main="Speechiness %")
```
<img width="377" alt="Screen Shot 2024-03-15 at 18 28 44" src="https://github.com/kivatmojo/econ_104/assets/137433466/c0b0be79-dee6-4ca8-9b90-2bbf22267ea6">
<img width="392" alt="Screen Shot 2024-03-15 at 18 28 50" src="https://github.com/kivatmojo/econ_104/assets/137433466/11c65ca6-8547-4ea6-9983-fce93d037ba7">

When looking at the histogram of streams, we see a skewed distribution to the right - meaning there are many low-streamed songs and relatively low high-streamed songs. This makes sense as it’s harder to create popular songs.  

“Acousticness”, “instrumentalness”, and “liveliness” is skewed to the left - showing that most songs exhibit low “liveliness”, “instrumentalness”, and “acousticness”. The fact that many popular songs displayed low “liveliness” scores was surprising to us as we thought this was a strong music preference.  

Speechiness - the amount of spoken words in a song, is also skewed to the left. This means many consumers prefer songs with fewer words. This was an interesting finding as our group tends to enjoy songs with lyrics. However, there are also many genres that don’t include lyrics - such as EDM and binaural beats.  

The spotify playlist - the amount of playlist each song is in, and spotify charts variable, are as expected. It's harder to create well-produced and popular songs.  

### 4. Correlation Plot
```r
cor.table = cor(music_data)
corrplot(cor.table, method = "square", tl.col="black")
```
<img width="449" alt="Screen Shot 2024-03-15 at 18 29 04" src="https://github.com/kivatmojo/econ_104/assets/137433466/aa9c412f-1f41-4be2-a3a7-00081413584f">

The correlation plot represents how each variable relates to each other. For example, “accousticness” and “liveliness” are negatively correlated which is consistent with what we hear in songs we listen to.   

Another thing that we notice is how positively correlated a song being in apple playlists and spotify playlists are. This means that the population is consistent with their playlist behavior across both streaming platforms.  

We see a high correlation between the number of songs the playlist is in and the number of streams. However, this is fine because streams is the responsive variable, and playlist is an explanatory variable.  

### 5. Scatter Plot
```r
par(mfrow = c(2,3))

plot(streams_thousands~bpm, data=music_data, ylab="Streams", xlab="BPM")
plot(streams_thousands~artist_count, data=music_data, ylab="Streams", xlab="Artist Count")
plot(streams_thousands~in_spotify_playlists, data=music_data, ylab="Streams", xlab="Spotify Playlists")
plot(streams_thousands~in_spotify_charts, data=music_data, ylab="Streams", xlab="Spotify Charts")
plot(streams_thousands~in_apple_playlists, data=music_data, ylab="Streams", xlab="Apple Playlists")
plot(streams_thousands~in_apple_charts, data=music_data, ylab="Streams", xlab="Apple Charts")
```
<img width="457" alt="Screen Shot 2024-03-15 at 18 29 19" src="https://github.com/kivatmojo/econ_104/assets/137433466/226b830e-a596-4349-aa8c-48f3c1ae9776">

```r
plot(streams_thousands~energy, data=music_data, ylab="Streams", xlab="Energy %")
plot(streams_thousands~danceability, data=music_data, ylab="Streams", xlab="Danceability %")
plot(streams_thousands~liveness, data=music_data, ylab="Streams", xlab="Liveness %")
plot(streams_thousands~speechiness, data=music_data, ylab="Streams", xlab="Speechiness %")
plot(streams_thousands~acousticness, data=music_data, ylab="Streams", xlab="Acousticness %")
plot(streams_thousands~instrumentalness, data=music_data, ylab="Streams", xlab="Instrumentalness %")
```
<img width="455" alt="Screen Shot 2024-03-15 at 18 29 29" src="https://github.com/kivatmojo/econ_104/assets/137433466/d1c4f525-b783-4ee3-b519-f23f478459c9">

In efforts to further understand our data, we created scatter plots for each explanatory variable and its relationship to the number of streams a song earns. We noticed that both Apple and Spotify charts and playlist variables had a similar distribution - which validates the significance of their prediction power on number of streams. Additionally, we found the instrulmentalness data distribution to be very different, with a steep drop off when songs become more instrumental. This makes sense as consumers listen to much less classical music compared to several hundred years ago.  

Lastly, one variable I found interesting was the liveliness because the data shows that the more lively the song is, the more likely the song will earn less streams. This may pose an interesting reflection in consumer preferences where we prefer more relaxing than energetic songs. 
***
## Regression Model  
```r
regmod <- lm(streams_thousands ~ bpm + artist_count + 
                       in_apple_charts +  
                       in_apple_playlists + in_spotify_charts + 
                       in_spotify_playlists + danceability + 
                       valence + energy + 
                       acousticness + instrumentalness + 
                       liveness + speechiness, data = music_data)
summary(regmod) 
```
<img width="498" alt="Screen Shot 2024-03-15 at 18 29 58" src="https://github.com/kivatmojo/econ_104/assets/137433466/17fb42ac-eb58-4f7e-8f7d-3750c1875d2a">
In looking at the regression summary, artist count, Apple playlist, Spotify playlist, Spotify charts, and valence are statistically significant - meaning there is a low chance that the prediction power of the variable is due to chance. One interesting trend I noticed was that many characteristic variables such as “energy” and “liveliness" were not statistically significant. This may be due to the fact that the measurement of these variables is subjective.  

Spotify and Apple playlist variables represent the number of playlists the streamed song is in. As such, if a song is added into more playlists, the more streams it is likely to have. Artist count represents the number of artists within the song. Valence represents the amount of positivity a song conveys. A positive correlation between valence and streams makes sense as many people listen to music during leisure time and to feel good.  

Artist count has a negative estimate, meaning that the more artists you add, the fewer streams the song will have. Playlist, Spotify charts, and valence are positive, consistent with the previously mentioned logic.  
***
## Analysis of Observations
```r
cooksd <- cooks.distance(regmod)
n <- nrow(music_data)
k <- length(regmod$coefficients) - 1
cutoff <- 4/(n-k-1)

music_screened <- music_data[cooksd < cutoff, ]

# Comparing original data with screened data
par(mfrow = c(1,2))

boxplot(music_data$streams_thousands, ylab = "Total Streams in Thousands", main= "Original Data") 
boxplot(music_screened$streams_thousands, ylab = "Total Streams in Thousands", main= "Screened Data")
```
<img width="400" alt="Screen Shot 2024-03-15 at 18 30 34" src="https://github.com/kivatmojo/econ_104/assets/137433466/f2dc49b8-3870-445b-966e-4378cda2ae65">

```r
# Comparing 5 number statistics between original data and screened data
summary(music_screened)
```
<img width="478" alt="Screen Shot 2024-03-15 at 18 31 11" src="https://github.com/kivatmojo/econ_104/assets/137433466/c4d7486e-1d5a-40ce-8db3-6fccc5320b8b">
After creating box plots for acousticness, danceability, energy, instrulmentalness, liveness, speechiness, we noticed outliers for all these variables. One variable we found interesting was danceability because there were many outliers below the mean. This was interesting because I thought easy-to-dance to songs are more likely to become popular from TikTok.  

There does not seem to be much difference between the data with "no outliers" and the original data through the boxplots. But when comparing the summary of the original dataset to the screened dataset, removing the outliers seems to have "shrunk" the dataset to not include some of those top-end outliers. 

We decided to remove the outliers for these variables to create a model that more accurately predicts the number of streams any given song will have.  

```r
# Replace music_screened as music_data

music_data <- music_data[cooksd < cutoff, ]
```
***
## Variable Selection
### 1. Mallows CP  

```r
#Call and plot Mallows CP, and limit the y-axis to zoom in more on the plot

ss=regsubsets(streams_thousands ~ bpm + artist_count + in_apple_charts 
              + in_apple_playlists + in_spotify_charts + in_spotify_playlists 
              + danceability + valence + energy + acousticness + instrumentalness 
              + liveness + speechiness, data = music_data)

subsets(ss, statistic="cp", legend=F, main= "Mallows CP"
        , col= "steelblue4",ylim=c(0,8))
```
<img width="438" alt="Screen Shot 2024-03-15 at 18 31 33" src="https://github.com/kivatmojo/econ_104/assets/137433466/2c39bc9d-73f2-4d04-b210-c63741fff053">
<img width="234" alt="Screen Shot 2024-03-15 at 18 31 43" src="https://github.com/kivatmojo/econ_104/assets/137433466/69bc7e16-d545-4d27-a86e-e70f88f741fa">

Based on the data, Mallows CP identified the optimal model choice to have 7 parameters: artist count, in_apple_playlists, in_apple_charts, in_spotify_charts, in_spotify_playlists, valence, and instrumentalness into the regression model.  

### 2. Boruta  

```r
# Call Boruta
Bor.res <- Boruta(streams_thousands~., data=music_data, doTrace=2)

plot(Bor.res, las=3, xlab="")
```
<img width="417" alt="Screen Shot 2024-03-15 at 18 31 58" src="https://github.com/kivatmojo/econ_104/assets/137433466/2e4ad4c0-b157-45ca-90f5-891f2ca08735">

```r
boruta_signif <- names(Bor.res$finalDecision[Bor.res$finalDecision%in%c("Confirmed","Tentative")])
boruta_signif_conf <- names(Bor.res$finalDecision[Bor.res$finalDecision%in%c("Confirmed")])
boruta_signif_rej <- names(Bor.res$finalDecision[Bor.res$finalDecision%in%c("Rejected")])
boruta_signif_tent <- names(Bor.res$finalDecision[Bor.res$finalDecision%in%c("Tentative")])

print(boruta_signif_conf)
```
<img width="506" alt="Screen Shot 2024-03-15 at 18 32 28" src="https://github.com/kivatmojo/econ_104/assets/137433466/4fddc440-f573-476f-a627-cc603c1651b9">

```r
attStats(Bor.res)
```
<img width="468" alt="Screen Shot 2024-03-15 at 18 33 21" src="https://github.com/kivatmojo/econ_104/assets/137433466/8ee34462-ecbc-4c33-83d0-9541a013f79b">

Predictors we are keeping with screened data:  
- artist_count  
- in_apple_charts  
- in_apple_playlists  
- in_spotify_charts  
- in_spotify_playlists  
- valence  
- speechiness

The Boruta algorithm suggested the model same except to replace instrumentalness with speechiness, which is the recommendation we decided to pursue.  

```r
# Make New Model
regmod2 <- lm(streams_thousands~artist_count + in_apple_charts + in_apple_playlists  
              + in_spotify_charts + in_spotify_playlists + valence + speechiness
              , data=music_data)
```

***
## Multicollinearity
```r
tab <- tidy(vif(regmod2))
kable(tab, col.names=c("variables","VIF"))
```
<img width="185" alt="Screen Shot 2024-03-15 at 18 34 33" src="https://github.com/kivatmojo/econ_104/assets/137433466/b6d35abb-e929-4cbb-bed5-2d8867f88372">

Variance inflation factor test (VIF) determines if any regressors are correlated with each other. Colinearity between explanatory variables inflates the variance of the residuals and standard errors. A score of above 5 in the VIF test indicates that the variable is colinear.   

In our test of the explanatory variables, all displayed scores of less than 5, revealing there are no problems with collinearity. I found this slightly surprising because I imagine that Spotify and Apple playlist data provide the same prediction power. However, because there is significant variation within our dataset, these two variables do provide substantial power to estimate the parameters precisely.  
***
## Residuals
```r
res <- residuals(regmod2)
yhat <- fitted(regmod2)

par(mfrow=c(2,4))

plot(yhat,res,xlab="fitted values"
     , ylab="residuals")
plot(music_data$artist_count,res,xlab="Artist Count"
     , ylab="residuals")
plot(music_data$in_spotify_playlists,res,xlab="Spotify Playlists"
     , ylab="residuals")
plot(music_data$in_apple_playlists,res,xlab="Apple Playlists"
     , ylab="residuals")
plot(music_data$in_spotify_charts,res,xlab="Spotify Charts"
     , ylab="residuals")
plot(music_data$in_apple_charts,res,xlab="Apple Charts"
     , ylab="residuals")
plot(music_data$speechiness,res,xlab="Speechiness"
     , ylab="residuals")
plot(music_data$valence,res,xlab="Valence"
     , ylab="residuals")
```
<img width="564" alt="Screen Shot 2024-03-15 at 18 35 02" src="https://github.com/kivatmojo/econ_104/assets/137433466/b27ccccc-c678-4d69-9df7-355b89dbc2f3">

After plotting the residuals against the fitted values, we noticed an increase in the variance of error terms in songs ranging from 0 to 50,000 predicted streams, then slowly tapering down after.  

This makes sense as it’s harder to predict streams of songs that are not very popular. As such, the model will over predict certain songs and under-predict others. Overall, this suggests a trend in the errors and a sign of heteroskedasticity.  

```r
spreadLevelPlot(regmod2)
```
<img width="545" alt="Screen Shot 2024-03-15 at 18 35 15" src="https://github.com/kivatmojo/econ_104/assets/137433466/c7c9b4e6-07bf-4cdb-b73b-b44fe3c76331">

Looking at the spread-level plot that regresses the fitted values of Y on the residuals, we noticed an upward trend. This means that when the predicted number of streams a song has based on the model, the larger the error there is, thus heteroskedasticity is present.  

***
## RESET Test
```r
resettest(regmod2, power=2:3, type="fitted")
```
<img width="401" alt="Screen Shot 2024-03-15 at 18 36 07" src="https://github.com/kivatmojo/econ_104/assets/137433466/85364d3a-0e8a-4ebf-9a89-d89642d73037">

When running our reset test, the P-values are insignificant and the F-statistic exceeds the critical value, so we reject the null hypothesis that the coefficients of the new terms are zero. In other words, a zero p-value indicates that adding a higher power or interaction does not improve the model.  

```r
regmod3 <- lm(streams_thousands~artist_count 
              + in_spotify_playlists + I(in_spotify_playlists^2) 
              + in_apple_playlists  
              + in_spotify_charts + I(in_spotify_charts^2) 
              + in_apple_charts
              + speechiness + valence
              , data=music_data)

resettest(regmod3, power=2:3, type="fitted")
```
<img width="358" alt="Screen Shot 2024-03-15 at 18 36 18" src="https://github.com/kivatmojo/econ_104/assets/137433466/89e678d3-eae7-452e-949a-af6ccce929d3">

Through intuition, as well as trial and error, we concluded that "in_spotify_playlists" and "in_spotify_charts" are better predictors of streams when we transformed them into an exponential function. The RESET test results on the new model also confirm that the model is now better specified.

The choice of variables to transform makes sense because the more playlists and charts a song is in, the more it gets into other playlists and charts, which ultimately leads to more streams.  
***
## Test for Heteroskedasticity
```r
# NCV Test
ncvTest(regmod3)
```
<img width="387" alt="Screen Shot 2024-03-15 at 18 37 17" src="https://github.com/kivatmojo/econ_104/assets/137433466/ffb93f74-c6a8-41fd-abba-577028d35bea">

After running the NCV test, since the p-value is less than the 5% significance level, we reject the null hypothesis that the variance is constant. Thus, our model has non-constant variance.  

```r
# BP Test
bptest(regmod3)
```
<img width="346" alt="Screen Shot 2024-03-15 at 18 37 28" src="https://github.com/kivatmojo/econ_104/assets/137433466/30db187b-a7fa-4f9e-add4-e77b52f131d0">

The BP test results signify heteroskedasticity.  

```r
# GQ Test
gqtest(regmod3)
```
<img width="430" alt="Screen Shot 2024-03-15 at 18 37 46" src="https://github.com/kivatmojo/econ_104/assets/137433466/a8a80d30-aee9-4310-a82f-367afc4518f2">

The GQ test says no heteroskedasticity is present.  

Since the GQ test and the BP test disagree, we need to perform the White test.

```r
alpha <- 0.05
ressq <- resid(regmod3)^2

modres <- lm(ressq~artist_count + in_spotify_playlists + I(in_spotify_playlists^2) 
              + in_apple_playlists  
              + in_spotify_charts + I(in_spotify_charts^2) 
              + in_apple_charts
              + instrumentalness + speechiness
              , data=music_data)

N <- nobs(modres)
gmodres <-glance(modres)
S <- gmodres$df
chisqcr <- qchisq(1-alpha, S-1)
Rsqres <- gmodres$r.squared
chisq <- N*Rsqres
pval <- 1-pchisq(chisq,S-1)
print(pval)
```
<img width="83" alt="Screen Shot 2024-03-15 at 18 38 16" src="https://github.com/kivatmojo/econ_104/assets/137433466/5cf62ed6-291b-4756-8090-11c522c52fb1">

The white test confirms the presence of heteroskedasticity.  

***
## Adjusting for Heteroskedasticity

```r
#Using white/robust standard errors
cov1 <- hccm(regmod2, type="hc1")
coeftest(regmod2, vcov.=cov1)
```
<img width="523" alt="Screen Shot 2024-03-15 at 18 38 38" src="https://github.com/kivatmojo/econ_104/assets/137433466/d8212d97-809a-42fd-8673-cfff62beaaff">

```r
# Standard SE
coeftest(regmod2)
```
<img width="521" alt="Screen Shot 2024-03-15 at 18 38 55" src="https://github.com/kivatmojo/econ_104/assets/137433466/88e3a458-2922-4b09-9a14-c968a2f770b8">

These results show that there is an increase in Std. Error when trying to fix the model with the White Robust Std. Errors.  

Thus, we are going to attempt to use GLS.

```r
music_data[is.na(music_data) | music_data=="Inf"] = NA

music_nozero <- data.frame(music_data$streams_thousands,music_data$artist_count,
                           music_data$in_spotify_playlists, music_data$in_apple_playlists, 
                           music_data$in_spotify_charts, music_data$in_apple_charts, 
                           music_data$speechiness, music_data$valence)
colnames(music_nozero) <- c("streams_thousands", "artist_count", "in_spotify_playlists", 
                            "in_apple_playlists", "in_spotify_charts","in_apple_charts", 
                            "speechiness","valence")

music_nozero <- filter(music_data, streams_thousands > 0, artist_count > 0, 
                       in_spotify_playlists > 0, in_apple_playlists > 0, 
                       in_spotify_charts > 0, in_apple_charts > 0, speechiness > 0, 
                       valence >0) 

regmod3_b <- lm(streams_thousands~artist_count + in_spotify_playlists + I(in_spotify_playlists^2) 
              + in_apple_playlists  
              + in_spotify_charts + I(in_spotify_charts^2) 
              + in_apple_charts
              + speechiness + valence
              , data=music_nozero)

# Feasible GLS
ehatsq <- resid(regmod3_b)^2
sighatsq.ols <- lm(log(ehatsq)~log(artist_count) 
             + log(in_spotify_playlists) + log(in_spotify_playlists^2)
             + log(in_apple_playlists)
             + log(in_spotify_charts) + log(in_spotify_charts^2) 
             + log(in_apple_charts)
             + log(speechiness) + log(valence)
             , data=music_nozero)
vari <- exp(fitted(sighatsq.ols)) #undo log
regmod.fgls <- lm(streams_thousands~artist_count 
              + in_spotify_playlists + I(in_spotify_playlists^2) 
              + in_apple_playlists
              + in_spotify_charts + I(in_spotify_charts^2) 
              + in_apple_charts
              + speechiness + valence
              , weights=1/vari, data=music_nozero)

tidy(regmod.fgls)
```
<img width="494" alt="Screen Shot 2024-03-15 at 18 39 15" src="https://github.com/kivatmojo/econ_104/assets/137433466/33898814-9483-470f-b1af-511dfdc32237">

Feasible GLS gives us a significantly lower SE.

***
## Model Selection and Comparison
```r
# Running BIC using model with FGLS
ss2test = regsubsets(streams_thousands~artist_count
              + in_spotify_playlists + I(in_spotify_playlists^2) 
              + in_apple_playlists  
              + in_spotify_charts + I(in_spotify_charts^2) 
              + in_apple_charts
              + speechiness + valence
              , weights=1/vari, data=music_nozero) 

ss2_sumtest <- summary(ss2test)
plot(ss2_sumtest$bic)
ss2_sumtest$bic
ss2_sumtest
```
<img width="433" alt="Screen Shot 2024-03-15 at 18 39 32" src="https://github.com/kivatmojo/econ_104/assets/137433466/9a1e796b-16eb-4b04-af6b-fdc21f113516">
<img width="458" alt="Screen Shot 2024-03-15 at 18 40 21" src="https://github.com/kivatmojo/econ_104/assets/137433466/170be9eb-a4bd-45b3-a70d-089b448156ff">

We ran the BIC test on our model with FGLS, and the result concluded that the best model uses 4 variables:   
  
artist_count, in_spotify_playlists, in_spotify_playlists^2 and in_apple_playlists

```r
# Make new model
music_mod <- data.frame(music_nozero$streams_thousands,music_nozero$artist_count, 
                        music_nozero$in_spotify_playlists,I(music_nozero$in_spotify_playlists^2), 
                        music_nozero$in_apple_playlists)
colnames(music_mod) <- c("streams_thousands", "artist_count", "in_spotify_playlists",
                         "in_spotify_playlists^2", "in_apple_playlists")
```
***
## Estimate Model Performance
```r
# k-Fold Cross Validation

set.seed(1)
train_control <- trainControl(method="cv", number = 10)
model_train <- train(streams_thousands~., data=music_mod,
                     method = "lm",trControl=train_control, weights=1/vari)
print(model_train)
```
<img width="458" alt="Screen Shot 2024-03-15 at 18 40 35" src="https://github.com/kivatmojo/econ_104/assets/137433466/ecbb373c-0e3e-47ec-a50b-00448e06657f">

Once we have found our final model, the final step is to determine how accurate our model is. Due to this, we applied the K-fold cross-validation test with 10 folds and an 80-20 split for training and testing, yielding an RMSE of 174,046. This represents 0.00008% of the data range (2,135,150,000). Our interpretation is reinforced by our model's R squared of 80% which is precise by industry standards.
***
## Conclusions and Findings
```r
regmod.fgls <- lm(streams_thousands~artist_count 
              + in_spotify_playlists + I(in_spotify_playlists^2) 
              + in_apple_playlists
              , weights=1/vari, data=music_mod)
              
summary(regmod.fgls)
```

<img width="487" alt="Screen Shot 2024-03-15 at 18 41 10" src="https://github.com/kivatmojo/econ_104/assets/137433466/69ffa6f2-35f1-4f95-8034-5df4c14ae7d8">

In our project, we used spotify data to create a model that best predicts the number of streams a song will have based on its music characteristics and how many playlists and charts it lands in.  

After doing a constant loop of cleaning and analyzing the data through data manipulations and visualizations, we ran that data and its regression through a series of tests to test for variable importance, heteroskedasticity, and model specification, then adjusted our model to be better fit through using FGLS and higher order terms in our model. Lastly, we used the resulting model and ran it on the AIC and BIC test for model selection, and it gave us the desired model for predicting streams.  

To evaluate our final model, we tested the model using k-Fold Cross Validation with k=10. The test concluded for our model to have an RMSE of 174,990.7 which means our model in average misses the likely number of streams a song has by 174,990.7 streams.   

We deem this to be reasonable considering our dataset has a lot of unmitigated outliers that scales up to 2 billion streams.  

Our final model to predict a song's number of streams is: $STREAMS = 129110.8402712 - 15378.0140630*ARTISTCOUNT + 94.0297995*SPOTIFYPLAYLISTS - 0.0016709*SPOTIFYPLAYLISTS^2 + 748.5154211*APPLEPLAYLISTS$  

This aligns with real-world theory where aside from virality, the biggest reason of a song's success is who the song is by and if it has features to reach a broader audience, and if the song is in people's personal playlists.   

A note on playlists is that the behavior of playlists is an exponential function because the more a song is in a playlist, the more people find the song or hear of the song from a friend and then adding that song to their own playlists, and so on.  

