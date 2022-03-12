setwd("/Users/antoniosanchezmartin/OneDrive/Masters/Q3 Mixed Methods/tutorial_code/text_analysis")


package_list <- c("dplyr", "dplyr","tidytext","ggplot2","tidyr","bing","AFINN","readtext","quanteda","stopwords","stringr","topicmodels","reshape2","ldatuning", "stm", "tidyverse", "textdata")

install.packages(package_list)

library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
# library(bing)
# library(AFINN)
library(readtext)
library(quanteda)
library(stopwords)
library(stringr)
# library(tidyverse)
library(ldatuning)
library(textdata)
library(stm)

###### Retrieve data and filter for English language
data_AllHashtags <- readtext("port_only.csv")
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

data_tidytweets <- data_AllHashtags %>%
  mutate(data_AllHashtags, tidy_tweet = clean_tweets(data_AllHashtags$tweet))

print(data_tidytweets$tidy_tweet)
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

# set.seed(007)
wordcloud::wordcloud(words = d_tidy$word, freq = d$freq, max.words=200)
wordcloud::wordcloud(words = d_tidy$word, freq = d$freq, max.words=50)


####### Corpus
corpus_tidy <- corpus(data_tidytweets$tidy_tweet)
token_tidy <- tokens(data_tidytweets$tidy_tweet)
dtm_tidy <- dfm(token_tidy, tolower = T, wordstem = TRUE, remove = stopwords(language = "en"))

repeat{
  pr_lda <- topicmodels::LDA(dtm_tidy, k = 10)        ### k = number of topics
  pr_topics <- tidy(pr_lda, matrix = "beta")

  pr_top_terms <- pr_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

  if(any(c("earth", "tree", "sustain", "pollution") %in% pr_top_terms$term) == T){
    break
  }
}

pr_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# sentiment analysis
bing_dict <- as.dictionary(get_sentiments("bing"))
is.dictionary(bing_dict)
bing_sent <- dfm(corpus_tidy, dictionary = bing_dict)
bing_sent_summary <- as.data.frame(dfm_lookup(bing_sent, bing_dict, valuetype = "glob"))
barplot(bing_sent_summary$negative)
barplot(bing_sent_summary$positive)
print(colSums(bing_sent_summary[,-1]))
