## This is a tutorial to collect Twitter data and create a wordcloud of most frequent hashtags
## from tweets based on a hashtag search

# Step 1. Install and load necessary R libraries. Only do this once.

#install.packages("dplyr") # Packages for working with text
#install.packages("rtweet") # More info on this package: http://rtweet.info/
#install.packages("httpuv") # May be required for authentication, depending on your machine
#install.packages("slam") # Required by wordcloud2
#install.packages("wordcloud2") #Package for creating a wordlcoud
install.packages("shiny")
install.packages("shinydashboard")

# Load the libraries into the current session. Do this every time you want to run this script.
library(dplyr)
library(rtweet) 
library(httpuv)
library(slam)
library(wordcloud2)
library(shiny)
library(shinydashboard)

# Step 2. Get your App Keys from Your Twitter App and save them into R. 

consumer_key <- 'YOURCONSUMERKEY'  

consumer_secret <- 'YOURCONSUMERSECRET'

# Step 3. Create a token to connect to Twitter's API using your key and secret

token <- create_token(app="twarclet", consumer_key, consumer_secret, set_renv = TRUE)

# Step 4. Let's search for some tweets. 

# Type in a hashtag that interests you. 

hashtag <- "#data"

tweets <- search_tweets(hashtag, n = 200, include_rts = FALSE)

# Step 5. Let's make a wordcloud of the hashtags

hashtags <- tweets$hashtags

hashtags <- hashtags %>% 
  unlist()  %>%   #Collapse the hashtag lists into one long list
  tolower()  #Convert text to lowercase


# Step 6. Make a frequency table of hashtags and sort it.

hashtags_count <- table(hashtags)

# Step 7. Make a wordcloud from the hashtags_count table

# This function is for taking out the '#' from the hashtag
substrLeft <- function(x, n){
  substr(x, n+1, nchar(x))
}
keyword <- substrLeft(hashtag, length(hashtag)) #strip pound sign from hashtag

# Create the wordcloud from the hashtag

hashtags_count %>%  
  cbind.data.frame(tags=names(hashtags_count),count=as.integer(hashtags_count)) %>%  #create a dataframe from the hashtags table
  filter(hashtags != keyword) %>% #filter out the hashtag term itself from the wordcloud
  wordcloud2(.)  #make the wordcloud


#Step 6. Let's see the text of the tweets. 

text <- tweets$text 
text

