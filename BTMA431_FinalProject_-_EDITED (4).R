library(httr)
library(jsonlite)
library(spotifyr)
library(MASS)
library(ggfortify)
library(car)
library(dplyr)
library(plyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(sqldf)


### DATA RETRIEVAL AND PARSING ####

# Defining the ID, secret key and access token function to access the Spotify API
id <- 'eaa90c94062042b6be1d4a695369b045'
secret <- '9c16de7debc24bf380fa920fdb9526c8'
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

# Accessing Spotify's API & getting an access token to pass through API requests
response = POST( 
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(id, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)
mytoken = content(response)$access_token
HeaderValue = paste0('Bearer ', mytoken)

# Creating a function to get information about an artist's top tracks
getTopTracks <- function(url, artist.name) {
  top.tracks = GET(url, add_headers(Authorization = HeaderValue))
  artist.name = content(top.tracks)
  tracks <- artist.name$tracks 
  return(tracks)
}

# Retrieving the top tracks for selected artists
BadBunnyTracks <- getTopTracks("https://api.spotify.com/v1/artists/4q3ewBCX7sLwd24euuV69X/top-tracks?market=CA", BadBunny)
DrakeTracks <- getTopTracks("https://api.spotify.com/v1/artists/3TVXtAsR1Inumwj472S9r4/top-tracks?market=CA", Drake)
JBalvinTracks <- getTopTracks("https://api.spotify.com/v1/artists/1vyhD5VmyZ7KMfW5gqLgo5/top-tracks?market=CA", JBalvin)
JuiceWRLDTracks <- getTopTracks("https://api.spotify.com/v1/artists/4MCBfE4596Uoi2O4DtmEMz/top-tracks?market=CA", JuiceWRLD)
TheWeekndTracks <- getTopTracks("https://api.spotify.com/v1/artists/1Xyo4u8uXC1ZmMpatF05PJ/top-tracks?market=CA", TheWeeknd)
BTSTracks <- getTopTracks("https://api.spotify.com/v1/artists/3Nrfpe0tUJi4K4DXYWgMUX/top-tracks?market=CA", BTS)
BillieEilishTracks <- getTopTracks("https://api.spotify.com/v1/artists/6qqNVTkY8uBg9cP3Jd7DAH/top-tracks?market=CA", BillieEilish)
TaylorSwiftTracks <- getTopTracks("https://api.spotify.com/v1/artists/06HL4z0CvFAxyc27GXpf02/top-tracks?market=CA", TaylorSwift)
PostMaloneTracks <- getTopTracks("https://api.spotify.com/v1/artists/246dkjvS1zLTtiykXe5h60/top-tracks?market=CA", PostMalone)
TravisScottTracks <- getTopTracks("https://api.spotify.com/v1/artists/0Y5tJX1MQlPlqiwlOH1tJY/top-tracks?market=CA", TravisScott)

# Creating a function that extracts specific information for an artist's top # track
getTrackInfo <- function(x, artistName){
  Artist.ID <- artistName[[x]]$album$artists[[1]]$id
  Artist.Name <- artistName[[x]]$album$artists[[1]]$name
  Album.Name <- artistName[[x]]$album$name
  Release.Date <- artistName[[x]]$album$release_date
  Total.Tracks <- artistName[[x]]$album$total_tracks
  Duration <- artistName[[x]]$duration_ms
  Explicit <- artistName[[x]]$explicit
  Song.Name <- artistName[[x]]$name
  Popularity <- artistName[[x]]$popularity
  Track.Number <- artistName[[x]]$track_number
  Track.ID <- artistName[[x]]$id
  Track.Data <- data.frame(Artist.ID, Artist.Name, Album.Name, Release.Date, Total.Tracks, Song.Name, Popularity, Track.Number, Track.ID, 
                            Duration, Explicit)
  return(Track.Data)
}

# Getting the above information for all the selected artists' top tracks
for (i in 1:10) {
  eval(parse(text = paste0("DrakeTrack", i, "<- getTrackInfo(", i, ", DrakeTracks)")))
}

for (i in 1:10) {
  eval(parse(text = paste0("BadBunnyTrack", i, "<- getTrackInfo(", i, ", BadBunnyTracks)")))
}

for (i in 1:10) {
  eval(parse(text = paste0("JBalvinTrack", i, "<- getTrackInfo(", i, ", JBalvinTracks)")))
}

for (i in 1:10) {
  eval(parse(text = paste0("JuiceWRLDTrack", i, "<- getTrackInfo(", i, ", JuiceWRLDTracks)")))
}

for (i in 1:10) {
  eval(parse(text = paste0("TheWeekndTrack", i, "<- getTrackInfo(", i, ", TheWeekndTracks)")))
}

for (i in 1:10) {
  eval(parse(text = paste0("BTSTrack", i, "<- getTrackInfo(", i, ", BTSTracks)")))
}

for (i in 1:10) {
  eval(parse(text = paste0("BillieEilishTrack", i, "<- getTrackInfo(", i, ", BillieEilishTracks)")))
}

for (i in 1:10) {
  eval(parse(text = paste0("TaylorSwiftTrack", i, "<- getTrackInfo(", i, ", TaylorSwiftTracks)")))
}

for (i in 1:10) {
  eval(parse(text = paste0("PostMaloneTrack", i, "<- getTrackInfo(", i, ", PostMaloneTracks)")))
}

for (i in 1:10) {
  eval(parse(text = paste0("TravisScottTrack", i, "<- getTrackInfo(", i, ", TravisScottTracks)")))
}

#Combining info for all 10 artists into a data frame: 
Drake.Data <- rbind(DrakeTrack1, DrakeTrack2, DrakeTrack3, DrakeTrack4, DrakeTrack5, DrakeTrack6, DrakeTrack7, DrakeTrack8, DrakeTrack9, DrakeTrack10)
BadBunny.Data <- rbind(BadBunnyTrack1, BadBunnyTrack2, BadBunnyTrack3, BadBunnyTrack4, BadBunnyTrack5, BadBunnyTrack6, BadBunnyTrack7, BadBunnyTrack8, BadBunnyTrack9, BadBunnyTrack10)
JBalvin.Data <- rbind(JBalvinTrack1, JBalvinTrack2, JBalvinTrack3, JBalvinTrack4, JBalvinTrack5, JBalvinTrack6, JBalvinTrack7, JBalvinTrack8, JBalvinTrack9, JBalvinTrack10)
JuiceWRLD.Data <- rbind(JuiceWRLDTrack1, JuiceWRLDTrack2, JuiceWRLDTrack3, JuiceWRLDTrack4, JuiceWRLDTrack5, JuiceWRLDTrack6, JuiceWRLDTrack7, JuiceWRLDTrack8, JuiceWRLDTrack9, JuiceWRLDTrack10)
TheWeeknd.Data <- rbind(TheWeekndTrack1, TheWeekndTrack2, TheWeekndTrack3, TheWeekndTrack4, TheWeekndTrack5, TheWeekndTrack6, TheWeekndTrack7, TheWeekndTrack8, TheWeekndTrack9, TheWeekndTrack10)
BTS.Data <- rbind(BTSTrack1, BTSTrack2, BTSTrack3, BTSTrack4, BTSTrack5, BTSTrack6, BTSTrack7, BTSTrack8, BTSTrack9, BTSTrack10)
BillieEilish.Data <- rbind(BillieEilishTrack1, BillieEilishTrack2, BillieEilishTrack3, BillieEilishTrack4, BillieEilishTrack5, BillieEilishTrack6, BillieEilishTrack7, BillieEilishTrack8, BillieEilishTrack9, BillieEilishTrack10)
TaylorSwift.Data <- rbind(TaylorSwiftTrack1, TaylorSwiftTrack2, TaylorSwiftTrack3, TaylorSwiftTrack4, TaylorSwiftTrack5, TaylorSwiftTrack6, TaylorSwiftTrack7, TaylorSwiftTrack8, TaylorSwiftTrack9, TaylorSwiftTrack10)
PostMalone.Data <- rbind(PostMaloneTrack1, PostMaloneTrack2, PostMaloneTrack3, PostMaloneTrack4, PostMaloneTrack5, PostMaloneTrack6, PostMaloneTrack7, PostMaloneTrack8, PostMaloneTrack9, PostMaloneTrack10)
TravisScott.Data <- rbind(TravisScottTrack1, TravisScottTrack2, TravisScottTrack3, TravisScottTrack4, TravisScottTrack5, TravisScottTrack6, TravisScottTrack7, TravisScottTrack8, TravisScottTrack9, TravisScottTrack10)

Top.Tracks <- rbind(Drake.Data, BadBunny.Data, JBalvin.Data, JuiceWRLD.Data, TheWeeknd.Data, BTS.Data, BillieEilish.Data, TaylorSwift.Data, PostMalone.Data, TravisScott.Data)

# Getting each track's audio features
for(i in 1:nrow(Top.Tracks)){
  Sys.sleep(0.10)
  track_URI = paste0('https://api.spotify.com/v1/audio-features/',   
                      Top.Tracks$Track.ID[i])
  track_response = GET(url = track_URI, 
                        add_headers(Authorization = HeaderValue))
  tracks2 = content(track_response)
  
  Top.Tracks$Key[i] <- tracks2$key
  Top.Tracks$Mode[i] <- tracks2$mode
  Top.Tracks$Acousticness[i] <- tracks2$acousticness
  Top.Tracks$Danceability[i] <- tracks2$danceability
  Top.Tracks$Energy[i] <- tracks2$energy
  Top.Tracks$Instrumentalness[i] <- tracks2$instrumentalness
  Top.Tracks$Liveliness[i] <- tracks2$liveness
  Top.Tracks$Loudness[i] <- tracks2$loudness
  Top.Tracks$Valence[i] <- tracks2$valence
  Top.Tracks$Tempo[i] <- tracks2$tempo
}
  
# Getting additional data about each artist such as total followers, primary genre, etc.
artist.data <- GET("https://api.spotify.com/v1/artists?ids=4q3ewBCX7sLwd24euuV69X,3TVXtAsR1Inumwj472S9r4,1vyhD5VmyZ7KMfW5gqLgo5,4MCBfE4596Uoi2O4DtmEMz,1Xyo4u8uXC1ZmMpatF05PJ,3Nrfpe0tUJi4K4DXYWgMUX,6qqNVTkY8uBg9cP3Jd7DAH,06HL4z0CvFAxyc27GXpf02,246dkjvS1zLTtiykXe5h60,0Y5tJX1MQlPlqiwlOH1tJY", 
                   add_headers(Authorization = HeaderValue))
artists.info = content(artist.data)
artist <-artists.info$artists 

getArtistInfo <- function(x){
  Total.Followers <- artist[[x]]$followers$total
  Primary.Genre <- artist[[x]]$genres[[1]]
  Artist.Name <- artist[[x]]$name
  Artist.Popularity <- artist[[x]]$popularity
  Artist.Data <- data.frame(Artist.Name, Artist.Popularity, Total.Followers, Primary.Genre)
  return(Artist.Data)
}

# Getting the above information for our selected artists and combining it into the overall Spotify.Data data frame
for (i in 1:10) {
  eval(parse(text = paste0("Info.Artists", i, "<- getArtistInfo(", i, ")")))
}

Artist.Info <- rbind(Info.Artists1, Info.Artists2, Info.Artists3, Info.Artists4, Info.Artists5, Info.Artists6, Info.Artists7, Info.Artists8, Info.Artists9, Info.Artists10)
Spotify.Data <- merge(Top.Tracks, Artist.Info, by = "Artist.Name", all.x=TRUE) 

# Reading the CSV data file and storing in Recent.Charts
# Retrieved from https://www.kaggle.com/dhruvildave/spotify-charts 
Recent.Charts <- read.csv("data1.csv", header=TRUE, stringsAsFactors = FALSE)

# Picking a specific date for analysis
Recent.Charts <- Recent.Charts %>%
  filter(Recent.Charts$date == as.Date("2021-09-16"))

# Narrowing down the countries and adding them to a Countries data frame
filtered.countries <- c("Australia","New Zealand","Argentina","Brazil","Colombia","Canada","United States","United Kingdom","Russia","Italy","Spain","Indonesia","Pakistan","India","Malaysia","Japan","South Africa","Nigeria")
trial.list <- list()

for (i in 1:length(filtered.countries)){
  trial.list[[i]] <- Recent.Charts %>% filter(region == filtered.countries[i])
}

Countries <- data.frame(index = c(1:length(filtered.countries)), countries = filtered.countries) 


#### QUESTION 1: Which audio features are significant predictors of a track's danceability score? ####

danceability.data <- Spotify.Data[, c("Danceability", "Valence", "Acousticness", "Key", "Instrumentalness", "Liveliness", "Loudness", "Tempo", "Mode", "Energy")]
danceability_model <- lm(Danceability ~ Acousticness + Valence + Energy + Instrumentalness + Liveliness + Loudness + Tempo + Mode + Key, data = Spotify.Data) 
summary(danceability_model) # As per the p-values for each variable, we note that Acousticness, Valence and Key are significant predictor's of Danceability.

# In order to use the p-values to determine which variables to include in our model, we need to check the Regression assumptions: 
# a) Errors have mean zero & errors look Normally Distributed
Residuals <- danceability_model$residuals
hist(Residuals,main = "Distribution of Residuals",xlim=c(-0.3,0.3)) # The distribution is roughly centered at zero
autoplot(danceability_model, which = 2, ncol = 1) # We can see that the bulk of the points fall on the dashed line, meaning the distribution of the residuals matches the Normal distribution (dashed lines).

# b) Errors have common variance & should be uncorrelated with each predictor 
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(danceability_model,3) # Based on this plot, we conclude homogeneity of variance since our plot shows that the residuals are spread equally along the ranges of predictors.

# Checking the multicollinearity between variables 
vif(danceability_model) # None of the variables have a VIF greater than 5 meaning that they are not correlated/linearly dependent with the others

# As another check to ensure we are including the right predictors in our model, we use the AIC and BIC model selection techniques 
null.model <- lm (Danceability ~ 1, data = danceability.data)
full.model <- lm(Danceability ~ ., data = danceability.data)
step.both.AIC <- step(null.model, scope = list(lower=null.model, upper=full.model), direction="both")
summary(step.both.AIC)
step.both.BIC <- step(null.model, scope = list(lower=null.model, upper=full.model), direction="both", k=log(NROW(danceability.data)))
summary(step.both.BIC)
# The AIC approach selected Valence, Acousticness, Key and Energy as predictors 
# The BIC approach selected Valence, Acousticness and Key
# Therefore, both these approaches suggest that Valence, Acousticness and Key should indeed be included in our model to predict the Danceability of a song 

# Only including the significant predictors we get the following for our model: 
danceability_model_sig_predictors <- lm(Danceability ~ Acousticness + Valence + Key, data = Spotify.Data)
summary(danceability_model_sig_predictors)

# Creating a data frame to only include values for which danceability is higher than 60%
high.danceability.data <- Spotify.Data[which(Spotify.Data$Danceability >= 0.60), ]
# This table indicates that the most frequent keys are 0 (C) and 1 (C#). 
# Note: a legend of pitch class and its associated key can be found here: https://en.wikipedia.org/wiki/Pitch_class 
table(high.danceability.data$Key)

# The average valence score is determined to be 49%
average.valence <- mean(high.danceability.data$Valence)
average.valence

# The average acousticness score is determined to be 19%
average.acousticness <- mean(high.danceability.data$Acousticness)
average.acousticness


#### QUESTION 1 FOLLOW-UP: This finding led to us wondering whether the regression model we created suggests that a particular genre does better in terms of Danceability than other genres? ####

# Using our regression model to predict a song's Danceability rating based on Acousticness, Valence and Key
coef.est <- summary(danceability_model_sig_predictors)$coefficients[,  1] 
intercept <- coef.est[['(Intercept)']]
acousticness <- coef.est[['Acousticness']]
valence <- coef.est[['Valence']]
key <- coef.est[['Key']]

for(i in 1:nrow(Spotify.Data)){
  Spotify.Data$Prediction[i] <- acousticness*Spotify.Data$Acousticness[i]  + valence*Spotify.Data$Valence[i] + key*Spotify.Data$Key[i] + intercept
}

# Determining the average Danceability rating for each genre 
Genre.Means <- aggregate(Spotify.Data$Prediction, list(Spotify.Data$Primary.Genre), FUN=mean)
Genre.Means <- Genre.Means %>% rename(Avg.Danceability = x, Genre = Group.1)
Genre.Means$Genre[which.max(Genre.Means$Avg.Danceability)] 
# After applying the regression model to our dataset, it looks like the K-Pop Genre has the highest Danceability rating compared to the other genres

# Comparing the Danceability ratings across genres
Danceability.Genres <- ggplot(data=Genre.Means, aes(x= reorder(Genre, Avg.Danceability), y=Avg.Danceability)) + geom_bar(stat="identity", fill="#2AC831") + labs(x = "Genre", y = "Danceability Rating") + ggtitle("Danceability Ratings Across Different Genres") + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.text.x=element_text(angle=45, hjust=1)) + aes(text=paste('<br>Genre:', Genre, '<br>Avg. Danceability:', round(Avg.Danceability, digits=3)))
ggplotly(Danceability.Genres, tooltip="text") %>% layout(hoverlabel=list(bgcolor="white"))
# As shown in the interactive bar graph, K-pop has the highest Danceability rating at 0.749 and Art Pop has the lowest at 0.543
# However, all of the genres associated with the top 10 artists score high in Danceability 


#### Q1: ADDITIONAL ANALYSIS ####

# Finding the average danceability score in the Spotify.Data data frame
average.danceability <- mean(Spotify.Data$Danceability)
# The average danceability score is found to be 67%
average.danceability
# As the Spotify.Data data frame consisted of the top 10 artists, it can be said that a high danceability score among
# top artists is a common trend


#### QUESTION 2: Is there a significant difference in the streaming rates for top songs in Canada vs. the United States? ####

# Finding Canada's data frame from the list of country info
Canada <- trial.list[[6]]

# Removing rows in the data frame that have the same title (remixes typically) to avoid confusion when matching
noduplicates <- count(Canada$title)[count(Canada$title)$freq == 1, ]
Canada <- select(sqldf('SELECT * FROM noduplicates nd LEFT JOIN Canada c WHERE nd.x = c.title'), -c(x,freq))

# Finding USA data frame from the list of country info
US <- trial.list[[7]] 

# Merging the Canada data frame with the US data frame in order to perform paired hypothesis test, and renaming columns
# a is defined as country on the left of the join, and b as on the right
Canada.and.US <- sqldf('SELECT * FROM Canada c INNER JOIN US u ON c.title = u.title')
colnames(Canada.and.US) <- c("title.a","rank.a","date.a","artist.a","region.a","chart.a","trend.a","streams.a","title.b","rank.b","date.b","artist.b","region.b","chart.b","trend.b","streams.b")
Canada.and.US <- Canada.and.US[,c("title.a","rank.a","date.a","artist.a","region.a","chart.a","trend.a","streams.a","title.b","rank.b","date.b","artist.b","region.b","chart.b","trend.b","streams.b")]

# Creating a column of differences in stream numbers

# As of November 30, 2021, Canada's population is ~38.5 million (https://www.statcan.gc.ca/en/subjects-start/population_and_demography)
# As of November 30, 2021, USA's population is ~333 million (https://www.census.gov/topics/population.html)
# USA population / Canada population = 8.65
# In order to get a more meaningful difference, we divide the US stream number by 8.7 when we calculate difference.
Canada.and.US$streams.diff <- (Canada.and.US$streams.b/8.65) - Canada.and.US$streams.a
Canada.and.US <- Canada.and.US[order(Canada.and.US$rank.a), ]

# Here we will test if the differences in streams are Normally distributed
hist(Canada.and.US$streams.diff, breaks = 70, xlim = c(-120000,20000), xlab="Differences", col="#2AC831", main="Histogram of Streaming Differences") # This is left skewed
shapiro.test(Canada.and.US$streams.diff) # As there is a significant p-value, the differences are not Normal
# As the above are two strong indicators of non-Normality, we will use the Wilcoxon Signed Rank Sum Test 

# HO: the streaming rate for songs in the TOP200 chart are the same in Canada and the USA
# Ha: the streaming rate for songs in the TOP200 chart is greater in Canada than the USA

options(scipen = 999)
wilcox.test(Canada.and.US$streams.a, Canada.and.US$streams.b/8.65, paired=TRUE, alternative = "greater", conf.int = TRUE) 
# As the p-value of the test statistic lies in the rejection region and is significant at an alpha of 0.05, we have sufficient evidence to support the claim that songs in the Top200 chart are streamed at a higher rate by Canadians than Americans.

Canada.and.US$rank.diff <- Canada.and.US$rank.a - Canada.and.US$rank.b
hist(Canada.and.US$rank.diff, breaks = 100, xlab="Rank Differences", main="Histogram of the Differences in Ranking", col="#2AC831", xlim = c(-150,150))
shapiro.test(Canada.and.US$rank.diff)
# Assessing Normality of rank differences, we conclude it is not Normally distributed due to the p-value from the Shapiro test
# As the above are two strong indicators of non-Normality, we will use the Wilcoxon Signed Rank Sum Test 


#### QUESTION 2 FOLLOW-UP: Is there a difference in ranking of top songs between Canadians and Americans? ####

#H0: There is no difference in ranking of TOP songs between Canadians and Americans
#Ha: There is a difference in ranking of TOP songs between Canadians and Americans

wilcox.test(Canada.and.US$rank.a, Canada.and.US$rank.b, paired=TRUE, alternative = "two.sided", conf.int = TRUE)
# As the p-value of the test statistic (0.8407) does not lie in the rejection region, we failed to reject the null hypothesis. We have insufficient evidence to support the claim that songs in the TOP200 are ranked differently between Canadians and Americans. 

Canada.and.US$abs.rank.diff <- abs(Canada.and.US$rank.a - Canada.and.US$rank.b)

# Creating visuals to help a user determine the type of songs that do better in each country rank-wise
Song.Title <- reorder(Canada.and.US$title.a, - Canada.and.US$abs.rank.diff)
Rank.Deviation <- Canada.and.US$abs.rank.diff
abs.diff.plot <- ggplot(data = Canada.and.US, aes(x = Song.Title, y = Rank.Deviation)) + geom_col(col="#2AC831") + xlab("Songs in the Top200") + scale_y_continuous(breaks = c(seq(0,150,10))) + ylab("Absolute Deviation") + ggtitle("Absolute Deviation in Ranking of Songs") + theme(axis.text.x = element_blank()) + annotate("text", x = 0.5*nrow(Canada.and.US), y = round(max(Canada.and.US$abs.rank.diff)), label = "Hover over the bars for tooltip or use toolbar on top right", size= 3)

# Calling first plot
ggplotly(abs.diff.plot)

Song.Title <- reorder(Canada.and.US$title.a, - Canada.and.US$rank.diff)
Rank.Difference <- Canada.and.US$rank.diff 

diff.plot <- ggplot(data = Canada.and.US, aes(x = Song.Title, y = Rank.Difference)) + geom_col(col="#2AC831") + xlab("Songs in the Top200") + scale_y_continuous(breaks = c(seq(-150,150,10))) + ylab("Difference in Ranking (Canada-US)") + ggtitle("Difference in Ranking of Each Song CANADA-US") + theme(axis.text.x = element_blank()) + annotate("text", x = 0.5*nrow(Canada.and.US), y = round(max(Canada.and.US$rank.diff)), label = "Hover over the bars for tooltip or use toolbar on top right", size= 3) 

# Calling second plot
ggplotly(diff.plot)

sum(Canada.and.US$rank.diff) 
# As we can see, the left and right side above are nearly symmetrical, hence the low sum. This shows that absolute deviation is high, but in terms of total difference in ranking summed up, it's a very small value and both charts (US and Canada) can be said to have the same/similar ranking


#### Q2: ADDITIONAL ANALYSES ####

first.display.for.pq2 <- Canada.and.US[Canada.and.US$rank.diff > 100 | Canada.and.US$rank.diff < -100 ,c("title.a","artist.a","rank.diff","abs.rank.diff")]
colnames(first.display.for.pq2) <- c("Song Title", "Main Artist","Rank Difference(CAN-US)","Absolute Rank Difference")
first.display.for.pq2

second.display.for.pq2 <- Canada.and.US[Canada.and.US$rank.diff == 0 ,c("title.a","artist.a","rank.diff","abs.rank.diff")]
colnames(second.display.for.pq2) <- c("Song Title", "Main Artist","Rank Difference(CAN-US)","Absolute Rank Difference")
second.display.for.pq2

# Manually adding genre to the artists in the TOP200 of Canada by searching on Spotify - no easy table or API endpoint to help with this
Top.Artists <- data.frame(count(Canada, "artist"))
genres <- c("Hip-Hop/Rap","Hip-Hop/Rap","Hip-Hop/Rap","Hip-Hop/Rap","Hip-Hop/Rap","Alternative/Indie","Hip-Hop/Rap","Pop R&B","K-Pop","Rock","Pop","Pop","Country","Afrobeats/R&B","Pop","Hip-Hop/Rap","Rock/Alternative/Indie","Pop","Hip-Hop/Rap","Hip-Hop/Rap","Hip-Hop/Rap","Country","Pop/R&B","Pop/R&B","Hip-Hop/Rap","Hip-Hop/Rap","Hip-Hop/Rap","Hip-Hop/Rap","Pop","Country","Rock","Pop","Soft-Rock/Pop","Hip-Hop/Rap","Hip-Hop/Rap","Latin/Reggae/Pop","Rock","Hip-Hop/Rap","R&B/Soul","Alternative/Indie","Alternative/Indie","Rock/Pop","Hip-Hop/Rap","Latin/Reggae/Pop/Electronic","Hip-Hop/Rap","Hip-Hop/Rap","Hip-Hop/Rap","Rock/Pop","Pop/R&B","Electronic","Electronic","Country","Rock","Pop","Hip-Hop/Rap","Pop/Electronic","Pop","Alternative/Indie","Country","Country","Country","Hip-Hop/Rap","Pop/Dance/Electronic","Country","Alternative/Indie","Hip-Hop/Rap","Hip-Hop/Rap/R&B","Hip-Hop/Rap","Hip-Hop/Rap","Rock/Dance/Electronic","Pop/R&B","Country","Rock","Hip-Hop/Rap","Hip-Hop/Rap","Dance/Electronic","Rock/Pop","Dance/Electronic","Hip-Hop/Rap","Dance/Electronic","Hip-Hop/Rap","Hip-Hop/Rap","Hip-Hop/Rap","Rock/Pop","Hip-Hop/Rap","Alternative/Indie","Pop","Pop/Rock","Pop","Hip-Hop/Rap","Hip-Hop/Rap","Hip-Hop/Rap","Hip-Hop/Rap","Rock","Dance/Electronic","Hip-Hop/Rap","Hip-Hop/Rap","Country","Pop","Hip-Hop/Rap","Pop","Dance/Electronic","Hip-Hop/Rap","Reggae/R&B","Alternative/R&B/Electronic","Hip-Hop/Rap","Rock","R&B","Dance/Electronic","Pop","Hip-Hop/Rap","Alternative/Indie","Hip-Hop/Rap/Pop","Rock/Alternative/Indie","Alternative/Indie","Rock","R&B","Electronic/Dance","Electronic/Dance","Pop/Indie","Hip-Hop/Rap","Hip-Hop/Rap","Hip-Hop/Rap","Alternative/Indie","Country","Alternative/Indie","Afrobeats/R&B")

Top.Artists$genre <- genres

Hip.Hop.Rap.RnB <- round(sum(grepl("rap|r&b", Top.Artists$genre, ignore.case = TRUE)) / nrow(Top.Artists), 2)
Dance.EDM <- round(sum(grepl("dance|electr", Top.Artists$genre, ignore.case = TRUE)) / nrow(Top.Artists), 2)
Pop <- round(sum(grepl("pop", Top.Artists$genre, ignore.case = TRUE)) / nrow(Top.Artists), 2)
Country <- round(sum(grepl("country", Top.Artists$genre, ignore.case = TRUE)) / nrow(Top.Artists), 2)
Rock <-round(sum(grepl("rock", Top.Artists$genre, ignore.case = TRUE)) / nrow(Top.Artists), 2)
Alt.Indie <-round(sum(grepl("alter|indie", Top.Artists$genre, ignore.case = TRUE)) / nrow(Top.Artists), 2)

Genre.Plot <- data.frame(Type.Of.Music = c("Hip-Hop/Rap/RnB","Dance/EDM","Pop","Country","Rock","Alternative/Indie"), Proportions = c(Hip.Hop.Rap.RnB, Dance.EDM, Pop, Country, Rock, Alt.Indie))

Genre <- reorder(Genre.Plot$Type.Of.Music, + Genre.Plot$Proportions)
VisualForReport <- ggplot(data = Genre.Plot, aes(x = Genre, y = Proportions)) + geom_col(col="black", fill="#2AC831") + annotate("text", x = 0.6, y = 0.37, label = "Proportions will not sum to 1 due to artists in more than 1 genre", size="2.25") + coord_flip() + scale_y_continuous(breaks = c(seq(0,1,0.1)), labels = c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")) + ylab("Proportion") + xlab("Genre") + ggtitle("Proportion of Artists in the TOP200 Producing Music in 6 Main Genres") + theme(axis.title.x = element_text(hjust = 1))

Genre.Plot
ggplotly(VisualForReport)


#### QUESTION 3 - Does song duration have an effect on a song's popularity? ####

popularity_model <- lm(Top.Tracks$Popularity~ Top.Tracks$Duration+Top.Tracks$Energy+Top.Tracks$Danceability+
                         Top.Tracks$Acousticness+Top.Tracks$Valence+Top.Tracks$Key+Top.Tracks$Instrumentalness+Top.Tracks$Liveliness
                       +Top.Tracks$Loudness+Top.Tracks$Liveliness+Top.Tracks$Mode+Top.Tracks$Key, data = Top.Tracks)
summary(popularity_model)

#Plotting the residuals
plot(popularity_model$residuals, geom = 'histogram', bins = 30)
autoplot(popularity_model,which =2, ncol = 1)


#Errors have common variance & should be uncorrelated with each predictor 
par(mfrow=c(2,2))
plot(popularity_model,3)


#AIC/BIC Tests
null.pop <- lm(Top.Tracks$Popularity~1, data = Top.Tracks)

#AIC Test indicates that the best predictors are Liveliness, Valence, and Duration
step.pop <- step(null.pop, scope = list(lower = null.pop, upper = popularity_model), direction = 'both')
summary(step.pop)


#Check for multicollinearity. We can confirm that Duration has no multicolinearity with the other predictors.
vif(popularity_model)





