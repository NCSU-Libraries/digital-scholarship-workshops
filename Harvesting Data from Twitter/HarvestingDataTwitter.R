## This is a tutorial on how to get Twitter data using R. Suitable for beginners.
# by Alison Blaine, NCSU Libraries
# For More more help or to schedule a consultation, email me: ablaine@ncsu.edu


# Step 1. Install and load necessary R libraries. Only do this once.

install.packages("rtweet") # More info on this package: http://rtweet.info/
install.packages("SentimentAnalysis") # More infor on this package: http://goo.gl/Pqyv86
install.packages("httpuv") # May be required for authentication, depending on your machine

# Load the libraries into the current session. Do this every time you want to run this script.

library(rtweet)
library(SentimentAnalysis)
library(httpuv)

# Step 2. Get your App Keys from Your Twitter App and save them into R.

consumer_key <- 'YOURCONSUMERKEY'

consumer_secret <- 'YOURCONSUMERSECRET'


# Step 3. Create a token to connect to Twitter's API using your key and secret

token <- create_token(app="YOURAPPNAME", consumer_key, consumer_secret, set_renv = TRUE)


# Step 4. Search for some tweets.

tweets <- search_tweets("#futureofclothing OR #textiles", n = 500, include_rts = FALSE)

tweets  # See the tweets you just harvested in the console


# Step 5. Do a bigger search to get more tweets.

tweets <- search_tweets("#futureofclothing OR #textiles", n = 25000, retryonratelimit = TRUE, include_rts = FALSE)


# Step 6. Plot tweet locations on a simple map (the tweets that are geotagged)

tweets <- lat_lng(tweets) # create latitude and longitude columns in the dataset

## plot world boundaries

maps::map("world", lwd = .25)

## plot lat and lng points onto world map
with(tweets, points(lng, lat, pch = 20, col = rgb(0, .3, .7, .75)))


# Step 7. Let's see who the tweeters are in our dataset.

tweeters <- tweets$screen_name

tweeters


# Step 8. Let's remove duplicates from that tweeters list.

tweeters <- unique(tweeters)

tweeters


# Step 9. Let's see the hashtags people are using.

hashtags <- tweets$hashtags

hashtags # This is a list of lists... this is a problem for analysis.

hashtags <- unlist(hashtags)  # Collapse the lists

hashtags <- tolower(hashtags) # Convert to lowercase (only if needed)

hashtags_count <- table(hashtags) # Do a word count and make a table

hashtags_count # You can see that this is an odd format. Let's make it into a data frame.


# Step 10. Turn hashtags_count table into a data frame for easier analysis.

hashtags_count <- cbind.data.frame(hashtags=names(hashtags_count),count=as.integer(hashtags_count))  # cbind() build a data frame by combining vectors, existing data frames, or matrices.


# Step 11. Sort hashtags_count dataset in descending order by count

hashtags_count <- hashtags_count[order(-hashtags_count$count),]


# Step 12. Let's find out who the most influencial tweeters who have #textiles in their user profiles.

textiles_tweeters <- search_users("#textiles", n=500, parse=TRUE, verbose=TRUE)

# Order the tweeters by number of followers, descending order.
textiles_tweeters <- textiles_tweeters[order(-textiles_tweeters$followers_count),]

# Take the top 100 influential tweeters
top_textiles_tweeters <- head(textiles_tweeters, n=100)


# Step 13. Pick one of those tweeters. Get their timeline. Default is 100 tweets.

user_timeline <- get_timeline("textileinst")

tweet_text <- user_timeline$text

tweet_text


# Step 14. Run some functions to clean the text.

text <- iconv(tweet_text, to="ASCII", sub="") # uses iconv function (base package) to strip non-ASCII characters from tweets.

text <- tolower(text) # converts tweets to lower case

text <- gsub('http.*', '', text) # takes out url from the tweets

text <- gsub('[[:punct:]]', '', text) # removes punctuation

text


# Step 15. Calculate the sentiment of the tweet text to determine where it's positive or negative.
# Note: this process is not 100% accurate. The analyzeSentiment() function calculates
# sentiments based on 4 different dictionaries. See more information here: https://cran.r-project.org/web/packages/SentimentAnalysis/SentimentAnalysis.pdf

text_sentiment <- analyzeSentiment(text)

text_sentiment_binary <- convertToBinaryResponse(text_sentiment) #label numeric values as positive or negative


# Step 16. Merge the text data and the sentiment data together into one data frame.

sentiment_data <- cbind(text, text_sentiment_binary) #puts text and sentiment stats side-by-side.

sentiment_data # see the data

# Create a CSV file

write.csv(sentiment_data, file= "tweetsentiment.csv")

