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
```{r}
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
```{r}
```
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
