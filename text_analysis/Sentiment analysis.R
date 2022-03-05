setwd("C:/Users/marij/Documents/MADE/Jaar_1/Electives/Mixed research methods/Data")

install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("tidyr")
#install.packages("bing")
#install.packages("AFINN")
install.packages("readtext")
install.packages("quanteda")
install.packages("stopwords")
install.packages("stringr")
install.packages("topicmodels")
install.packages("reshape2")
install.packages("ldatuning")
install.packages("stm")

library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
#library(bing)
#library(AFINN)
library(readtext)
library(quanteda)
library(stopwords)
library(stringr)
#library(tidyverse)  
library(ldatuning)
library(stm)

###### Retrieve data and filter for English language

data_AllHashtags <- readtext("C:/Users/marij/Documents/MADE/Jaar_1/Electives/Mixed research methods/Data/Mixed Research Methods/all_hashtags.csv")
data_AllHashtags <- filter(data_AllHashtags, language == "en")


###### Tokenize and count frequency
token_AllHashtags <- tokens(data_AllHashtags$tweet)
dtm <- dfm(token_AllHashtags)

wordcount <- docfreq(dtm)
wordcount <- sort(wordcount, decreasing = T)


###### Remove punctuation and stopwords
token_AllHashtags <- tokens(data_AllHashtags$tweet, remove_punct = T)
token_AllHashtags <- dfm(token_AllHashtags, tolower = T, wordstem = TRUE, remove = stopwords(language = "en"))

m <- as.matrix(token_AllHashtags)
v <- sort(colSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(007)
wordcloud::wordcloud(words = d$word, freq = d$freq, max.words=200)
wordcloud::wordcloud(words = d$word, freq = d$freq, max.words=50)

####### Tidy tweets

clean_tweets <- function(x) {
  x %>%
    # Remove URLs, $ and €
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove all weird characters
    str_remove_all("(€)(™)(â)(ðÿ)(¸)($)(ï)¬¦˜|¤²ª£ž¾ãƒœ°ðÿ±") %>%  ### doesn't work somehow
    # Remove all weird characters
    str_remove_all("â") %>%
    str_remove_all("ð") %>%
    str_remove_all("ÿ") %>%      ### doesn't work
    str_remove_all("$") %>%      ### doesn't work
    str_remove_all("€") %>%
    str_remove_all("™") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Remove hashtags
    str_remove_all("#[[:alnum:]_]+") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove puntucation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", "N/A") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}

data_AllHashtags <- data_AllHashtags %>%
  mutate(data_AllHashtags, tidy_tweet = clean_tweets(data_AllHashtags$tweet))

data_tidytweets <- data_AllHashtags
data_tidytweets <- data_tidytweets %>%             # making an errorless database by removing 0-values
  filter(data_tidytweets$tidy_tweet != "")

######## Wordcloud tidytweet
token_tidy <- tokens(data_tidytweets$tidy_tweet)
token_tidy <- tokens(data_tidytweets$tidy_tweet, remove_punct = T)
token_tidy <- dfm(token_tidy, tolower = T, wordstem = TRUE, remove = stopwords(language = "en"))

dtm_tidy <- dfm(token_tidy, tolower = T, wordstem = TRUE, remove = stopwords(language = "en"))

wordcount_tidy <- docfreq(dtm_tidy)
wordcount_tidy <- sort(wordcount_tidy, decreasing = T)

m_tidy <- as.matrix(token_tidy)
v_tidy <- sort(colSums(m_tidy),decreasing = TRUE)
d_tidy <- data.frame(word = names(v_tidy),freq=v_tidy)

set.seed(007)
wordcloud::wordcloud(words = d_tidy$word, freq = d$freq, max.words=200)
wordcloud::wordcloud(words = d_tidy$word, freq = d$freq, max.words=50)


####### Corpus
corpus_AllHashtags <- corpus(data_AllHashtags$tweet)
corpus_tidy <- corpus(data_tidytweets$tidy_tweet)


####### Topic probability
pr_lda <- topicmodels::LDA(dtm_tidy, k = 11)        ### k = number of topics
pr_topics <- tidy(pr_lda, matrix = "beta")

pr_top_terms <- pr_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

pr_documents <- tidy(pr_lda, matrix = "gamma")

pr_documents %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))    ### topic probability per tweet

k_search <- FindTopicsNumber(dtm, topics = seq(from = 2, to = 15, by = 1),
                             metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                             method = "Gibbs", control = list(seed = 2021), verbose = T)

FindTopicsNumber_plot(k_search)

##########################  STM WHOOPSIE  #####################################
#
##Preprocess the data using the STM library function
#stmInput <- textProcessor(data_AllHashtags$tweet, metadata = data_AllHashtags)
##Prepare documents for analysis; only terms with at least two occurrences will be selected because of lower.thresh = 2
#stmData  <- prepDocuments(stmInput$documents, stmInput$vocab, stmInput$meta, lower.thresh = 2)
#
#stm_k_3_20 <- searchK(stmData$documents, stmData$vocab, K = seq(3, 20, by = 1), data = stmData$meta, init.type = "Spectral")
#plot(stm_k_3_20)
#
#k <- 8   ### number of topics 
#stmOutput <- stm::stm(stmData$documents, stmData$vocab, K = k, data = stmData$meta, init.type = "Spectral")
#
#labelTopics(stmOutput, c(1:k)) ### find top words per topic
#
#
###############################################################################
