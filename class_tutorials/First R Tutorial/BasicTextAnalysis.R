###My directory structure
###Project (In this case: Demo)
###Sub-folders: Code, Data, Result, Literature, Manuscript


###First, I change the working directory to the overall project folder
setwd("/Users/antoniosanchezmartin/OneDrive/Masters/Q3 Mixed Methods/tutorial_code/First R Tutorial")


###Read the input file
###On my system, the file is stored in the sub-folder data
?readtext
coronanet <- readtext::readtext("policy_responses.csv", text_field = "description")                                                ###coronanet <- readtext::readtext("data/coronanet_release_20201125.csv", text_field = "description")
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


###Tokenize the input text
tokens <- quanteda::tokens(coronanet$text)
tokens

###Create a document-term matrix
dtm <- quanteda::dfm(tokens)
dtm

###Count term frequencies
doc_freq <- quanteda::docfreq(dtm)
doc_freq
doc_freq <- sort(doc_freq, decreasing = T)
doc_freq

###Create dataframe of terms and their frequencies
m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)

###Create a wordcloud
###The argument max.words should be set to the maximum number of words we want to represent in the word cloud
wordcloud::wordcloud(words = d$word, freq = d$freq, max.words=50)

###The word cloud has too much noise. Remove stop words and punctuation and repeat the process
tokens <- quanteda::tokens(coronanet$text, remove_punct = T)
dtm <- quanteda::dfm(tokens, tolower = T, remove = stopwords::stopwords("English"))

m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(007)
wordcloud::wordcloud(words = d$word, freq = d$freq, max.words=200)

###The second word cloud looks better but it still has numbers and some extraneous characters

#Remove standalone numbers
coronanet$text <- stringi::stri_replace_all_regex(coronanet$text, "\\b[0-9]+(st|nd|rd|th)?\\b", "")

#Remove one or two letter words
coronanet$text <- stringi::stri_replace_all_regex(coronanet$text, "\\b[[:alpha:]]{1,2}\\b", "")

tokens <- quanteda::tokens(coronanet$text, remove_punct = T)
dtm <- quanteda::dfm(tokens, tolower = T, remove = stopwords::stopwords("English"))

m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)

#I write the output of the word cloud to a pdf document in order to create a high-resolution version for a presentation or write-up
#For this, I use the Cairo library
pdf("wordcloud.pdf")
wordcloud::wordcloud(words = d$word, freq = d$freq, max.words=50)
dev.off()


###Create a corpus from the input dataset as an alternative. The corpus has additional information on the country
coronanet_corpus <- quanteda::corpus(coronanet)
tokens <- quanteda::tokens(coronanet_corpus, remove_punct = TRUE)
tokens <- quanteda::tokens_remove(tokens, pattern = stopwords::stopwords("en"))
tokens <- quanteda::tokens_group(tokens, groups = country)

dtm <- quanteda::dfm(tokens, tolower = T, stem = F, remove = stopwords::stopwords("English"))

###Use information on country for the keyness plot
??textstat_keyness
keyness <- quanteda.textstats::textstat_keyness(dtm, target = "Netherlands")
quanteda.textplots::textplot_keyness(keyness)

keyness <- quanteda.textstats::textstat_keyness(dtm, target = "United States of America")
quanteda.textplots::textplot_keyness(keyness)




###Exercise 1: Compare the Netherlands with the United States on the keyness plot
keyness <- quanteda.textstats::textstat_keyness(dtm, target = "Netherlands")
quanteda.textplots::textplot_keyness(keyness)



###Exercise 2: Compare European countries with China for the keyness metric

###Exercise 3: Compare countries with GDP per capita less than USD 20,000 with those with GDP per capita equal to or more than USD 20,000



###Using tidy format
library(dplyr)
library(tidytext)


  # Tokenize the text at the word level
tidy_pr <- coronanet %>%
  unnest_tokens(word, text)

  # Count words and show by most occuring
tidy_pr %>%
  count(word, sort = TRUE)

  # Remove stop words
tidy_pr <- tidy_pr %>%
  anti_join(stop_words)

tidy_pr %>%
  count(word, sort = TRUE)

# Visualization
library(ggplot2)

# Bar chart
tidy_pr %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col()

# Choose only top 15
tidy_pr %>%
  count(word, sort = TRUE) %>%
  head(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col()


###Create frequency comparison plot
frequency <- coronanet %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(country, word) %>%
  group_by(country) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  tidyr::spread(country, proportion) %>%
  tidyr::gather(country, proportion, `China`:`Russia`)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `United States of America`,
                      color = abs(`United States of America` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_text(aes(label = word), check_overlap = T) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  facet_wrap(~country, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "United States of America", x = NULL)


###Replace NAs with a low value
frequency <- frequency %>%
  mutate(proportion = ifelse(is.na(proportion), 0.001, proportion)) %>%
  mutate(`United States of America` = ifelse(is.na(`United States of America`), 0.001, `United States of America`))


ggplot(frequency, aes(x = proportion, y = `United States of America`,
                      color = abs(`United States of America` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_text(aes(label = word), check_overlap = T) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  facet_wrap(~country, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "United States of America", x = NULL)


###Create bigrams
pr_bigrams <- coronanet %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#Remove stop words and re-create bigrams
pr_bigrams <- pr_bigrams %>%
  tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


###Create bigram graph
pr_bigrams <- pr_bigrams %>%
  count(word1, word2, sort = T)

pr_bigram_graph <- pr_bigrams %>%
  filter(n > 1) %>%
  igraph::graph_from_data_frame()

set.seed(2017)

ggraph::ggraph(pr_bigram_graph, layout = "fr") +
  ggraph::geom_edge_link() +
  ggraph::geom_node_point() +
  ggraph::geom_node_text(aes(label = name), vjust = 1, hjust = 1)


###Add information on directionality
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph::ggraph(pr_bigram_graph, layout = "fr") +
  ggraph::geom_edge_link(arrow = a) +
  ggraph::geom_node_point() +
  ggraph::geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()



###Exercise 4: Conduct word co-occurrence analysis using tidy text mining

#The code below has been only slightly adapted from the sample(s) on the website Text Mining with R: A tidy Approach
#I would highly recommend going through the website and trying out various code samples on it
