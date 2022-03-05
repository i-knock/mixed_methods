setwd("/Users/antoniosanchezmartin/OneDrive/Masters/Q3 Mixed Methods/tutorial_code")

install.packages(c("readtext", "dplyr", "quanteda", "webshot"))
library(readtext)
library(dplyr)
library(quanteda)
library(webshot)

rt <- readtext('all_hashtags.csv', text_field = 'tweet') #import the csv data
rt2 <- filter(rt, language == "en") #get only tweets in english

# First let's create the corpus to maintain the metadata
r2d2 <- corpus(rt2)

# Create the document-feature matrix from quanteda. Quite convenient preprocessing and why the fuck not.
dtm <- dfm(r2d2, tolower = TRUE, stem = TRUE, remove = stopwords("english"))
dtm
