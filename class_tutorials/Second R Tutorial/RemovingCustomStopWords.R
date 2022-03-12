###My directory structure
###Project (In this case: Demo)
###Sub-folders: Code, Data, Result, Literature, Manuscript


###First, I change the working directory to the overall project folder
setwd("~/surfdrive/teaching/SEN1231/2021-22/Demo")


###Read the input file
###On my system, the file is stored in the sub-folder data 
?readtext
coronanet <- readtext::readtext("data/policy_responses.csv", text_field = "description")                                                ###coronanet <- readtext::readtext("data/coronanet_release_20201125.csv", text_field = "description")
coronanet


###View the dataframe
?View
View(coronanet)


###Check if the data has any duplicates
###In this example, the function "distinct" also removes all columns except country and text from the dataset
###Later on in this file, we see how to retain all columns in the dataset 
?distinct
coronanet <- dplyr::distinct(coronanet, country, text)
coronanet


###Check the first entry in the dataframe
coronanet$text[1]


#Remove standalone numbers
coronanet$text <- stringi::stri_replace_all_regex(coronanet$text, "\\b[0-9]+(st|nd|rd|th)?\\b", "")

#Remove one or two letter words
coronanet$text <- stringi::stri_replace_all_regex(coronanet$text, "\\b[[:alpha:]]{1,2}\\b", "")

#Remove pre-defined stop words
dtm <- quanteda::dfm(coronanet$text, tolower = T, stem = F, remove = stopwords::stopwords("English"), remove_punct = T)

m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud::wordcloud(words = d$word, freq = d$freq, max.words=50)
#The word cloud contains other words that might not be interesting for our analysis


###
###Removing custom stop words from the data
###

basicStopWords <- stopwords::stopwords("English")

#Other words we want to remove after examining the data; please provide a reason for your choice
customStopWords <- c("announced", "march", "can")

allStopWords <- c(basicStopWords, customStopWords)
dtm <- quanteda::dfm(coronanet$text, tolower = T, stem = F, remove = allStopWords, remove_punct = T)

m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud::wordcloud(words = d$word, freq = d$freq, max.words=50)
#The 'words 'customStopWords' are no longer present in the data;
#however, the warning messages indicate that the word cloud does not display several words in the data.
#Usually, this issue might be addressed if you write the output to a pdf
#This is also useful for storing and presenting your outputs

#I write the output of the word cloud to a pdf document in order to create a high-resolution version for a presentation or write-up
#For this, I use the Cairo library
?cairo_pdf
cairo_pdf("Result/wordcloud.pdf")
wordcloud::wordcloud(words = d$word, freq = d$freq, max.words=50)
dev.off()


###
###The same idea can be used when working with tidytext as well 
###Removing custom stop words from tidy format

library(dplyr)
library(tidytext)

tidy_pr <- coronanet %>%
  unnest_tokens(word, text)

tidy_pr %>%
  count(word, sort = TRUE) 

tidy_pr <- tidy_pr %>%
  anti_join(stop_words)

tidy_pr %>%
  count(word, sort = TRUE) 

#Only retain words that are not in the vector 'customStopWords' (created on line 58)
tidy_pr <- tidy_pr %>%
  filter(!word %in% customStopWords)


library(ggplot2)

tidy_pr %>%
  count(word, sort = TRUE) %>%
  head(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col()

