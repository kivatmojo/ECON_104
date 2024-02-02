# Multiple Regression Modelling

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
13. Conclusions and Findings

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

```r
music_character <- gather(music_data, key = "Characteristics", value = "Percentage", 
                          danceability, valence, energy, acousticness, instrumentalness, 
                          liveness, speechiness)
ggplot(music_character, aes(x = Characteristics, y = Percentage)) + geom_boxplot()
```

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

When looking at the histogram of streams, we see a skewed distribution to the right - meaning there are many low-streamed songs and relatively low high-streamed songs. This makes sense as it’s harder to create popular songs.  

“Acousticness”, “instrumentalness”, and “liveliness” is skewed to the left - showing that most songs exhibit low “liveliness”, “instrumentalness”, and “acousticness”. The fact that many popular songs displayed low “liveliness” scores was surprising to us as we thought this was a strong music preference.  

Speechiness - the amount of spoken words in a song, is also skewed to the left. This means many consumers prefer songs with fewer words. This was an interesting finding as our group tends to enjoy songs with lyrics. However, there are also many genres that don’t include lyrics - such as EDM and binaural beats.  

The spotify playlist - the amount of playlist each song is in, and spotify charts variable, are as expected. It's harder to create well-produced and popular songs.  

### 4. Correlation Plot
```r
cor.table = cor(music_data)
corrplot(cor.table, method = "square", tl.col="black")
```

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

plot(streams_thousands~energy, data=music_data, ylab="Streams", xlab="Energy %")
plot(streams_thousands~danceability, data=music_data, ylab="Streams", xlab="Danceability %")
plot(streams_thousands~liveness, data=music_data, ylab="Streams", xlab="Liveness %")
plot(streams_thousands~speechiness, data=music_data, ylab="Streams", xlab="Speechiness %")
plot(streams_thousands~acousticness, data=music_data, ylab="Streams", xlab="Acousticness %")
plot(streams_thousands~instrumentalness, data=music_data, ylab="Streams", xlab="Instrumentalness %")
```
In efforts to further understand our data, we created scatter plots for each explanatory variable and its relationship to the number of streams a song earns. We noticed that both Apple and Spotify charts and playlist variables had a similar distribution - which validates the significance of their prediction power on number of streams. Additionally, we found the instrulmentalness data distribution to be very different, with a steep drop off when songs become more instrumental. This makes sense as consumers listen to much less classical music compared to several hundred years ago.  

Lastly, one variable I found interesting was the liveliness because the data shows that the more lively the song is, the more likely the song will earn less streams. This may pose an interesting reflection in consumer preferences where we prefer more relaxing than energetic songs. 
***
## Regression Model  
```{r}
```
***
## Analysis of Observations
```{r}
```
***
## Variable Selection
```{r}
```
***
## Multicollinearity
```{r}
```
***
## Residuals
```{r}
```
***
## RESET Test
```{r}
```
***
## Test for Heteroskedasticity
```{r}
```
***
## Adjusting for Heteroskedasticity
```{r}
```
***
## Model Selection and Comparison
```{r}
```
***
## Estimate Model Performance
```{r}
```
***
## Conclusions and Findings
```{r}
```
