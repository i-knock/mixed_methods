###My directory structure
###Project (In this case: Demo)
###Sub-folders: Code, Data, Result, Literature, Manuscript


###First, I change the working directory to the overall project folder
setwd("~/surfdrive/teaching/SEN1231/2021-22/Demo")


###Read the input file
###On my system, the file is stored in the sub-folder data
coronanet <- readtext::readtext("data/policy_responses.csv", text_field = "description", encoding = "UTF-8")                                                
coronanet <- dplyr::distinct(coronanet, country, text)


coronanet$text[[1]]
#To remove unwanted characters, modify the following function
coronanet$text <- stringi::stri_replace_all_regex(coronanet$text, "[�]", " ")
coronanet$text[[1]]


coronanet$text
###Check and correct critical spelling errors
spelling_error <- hunspell::hunspell(coronanet$text, dict = "en_GB")
spelling_error <- sort(unique(unlist(spelling_error)))
write.csv(spelling_error, "spelling_error.csv")

###Go through the above file manually, correct errors, and save as a new file.
###See the structure of the file  "spelling_correction.csv" to understand what format you can save it in
spelling_correction <- read.csv("spelling_correction.csv", stringsAsFactors = F, header = F)
coronanet$text <- qdap::mgsub(paste0('\\b', spelling_correction[,1], '\\b'), 
                              spelling_correction[,2], coronanet$text, fixed = F)

coronanet$text


###Use parts of speech (POS) tagging to understand the data; Install 'udpipe' package first
library(lattice)
library(udpipe)

#Download the UDPipe model for your language
udmodel <- udpipe_download_model(language = "english")
#Check the current working directory for the model file name to be used below
udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.5-191206.udpipe')

#Annotate the data
?udpipe_annotate
coronanet_model <- udpipe_annotate(udmodel_english, coronanet$text)
#Convert to a data frame
coronanet_annotated <- data.frame(coronanet_model)

#Use the txt_freq function in udpipe library to check counts
pos_freq <- txt_freq(coronanet_annotated$upos)
#Sort in decreasing order
pos_freq$key <- factor(pos_freq$key, levels = rev(pos_freq$key))
#Plot on a bar chart
barchart(key ~ freq, data = pos_freq, main = "Parts of Speech", xlab = "Freq")


#Examine nouns in the data
noun_freq <- subset(coronanet_annotated, upos %in% c("NOUN")) 
noun_freq <- txt_freq(noun_freq$token)
noun_freq$key <- factor(noun_freq$key, levels = rev(noun_freq$key))
barchart(key ~ freq, data = head(noun_freq, 20), main = "Most occurring nouns", xlab = "Freq")


#Examine adjectives in the data
adj_freq <- subset(coronanet_annotated, upos %in% c("ADJ")) 
adj_freq <- txt_freq(adj_freq$token)
adj_freq$key <- factor(adj_freq$key, levels = rev(adj_freq$key))
barchart(key ~ freq, data = head(adj_freq, 20), main = "Most occurring adjectives", xlab = "Freq")


#Examine verbs in the data
verb_freq <- subset(coronanet_annotated, upos %in% c("VERB")) 
verb_freq <- txt_freq(verb_freq$token)
verb_freq$key <- factor(verb_freq$key, levels = rev(verb_freq$key))
barchart(key ~ freq, data = head(verb_freq, 20), main = "Most occurring Verbs", xlab = "Freq")


#Examine proper nouns in the data
proper_noun <- coronanet_annotated$lemma[coronanet_annotated$upos == "PROPN"]
proper_noun <- unique(proper_noun)
#write.csv(phrase, "proper_noun.csv")


###Co-occurrence for adjectives and nouns only
?cooccurrence
coronanet_cooc <- cooccurrence(subset(coronanet_annotated, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(coronanet_cooc, 15)


library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(coronanet_cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(edge_alpha = cooc)) +
  geom_node_text(aes(label = name), size = 4) +
 theme(legend.position = "none")



#Identify keywords using RAKE (Rapid Automatic Keyword Extraction technique)
kw_rake <- keywords_rake(x = coronanet_annotated, 
                         term = "lemma", 
                         group = c("doc_id", "paragraph_id", "sentence_id"), 
                         relevant = coronanet_annotated$upos %in% c("NOUN", "ADJ"), 
                         ngram_max = 5, n_min = 2)

#Select key terms consisting of more than one word
kw_rake <- kw_rake[kw_rake$ngram > 1,]

#Create factor variable for bar chart
kw_rake$key <- factor(kw_rake$keyword, levels = rev(kw_rake$keyword))
barchart(key ~ rake, data = head(kw_rake, 15), main = "Keywords identified by RAKE", xlab = "Rake")


#Identify commonly occurring noun phrases
coronanet_annotated$phrase_tag <- as_phrasemachine(coronanet_annotated$upos, type = "upos")
kw_phrase <- keywords_phrases(x = coronanet_annotated$phrase_tag,
                              term = coronanet_annotated$lemma,
                              pattern = "(A|N)*N(P+D*(A|N)*N)*",
                              is_regex = TRUE, detailed = FALSE)

#Select key terms consisting of more than one word
kw_phrase <- kw_phrase[kw_phrase$ngram > 1 & kw_phrase$freq > 1,]

#Create factor variable for bar chart
kw_phrase$key <- factor(kw_phrase$keyword, levels = rev(kw_phrase$keyword))
barchart(key ~ freq, data = head(kw_phrase, 15), main = "Keywords - simple noun phrases", xlab = "Frequency")


coronanet_phrase <- bind_rows(kw_phrase, kw_rake)
###If you want to customize the phrases used in the analysis,
###you can write the data to a file,
###edit it in Excel and then read it back into the variable 'coronanet_phrase'
#write.csv(coronanet_phrase, "coronanet_phrase.csv")
#coronanet_phrase <- read.csv("coronanet_phrase.csv")


## Recode terms to phrases
coronanet_annotated$term <- coronanet_annotated$token

coronanet_annotated$term <- txt_recode_ngram(coronanet_annotated$term,
                                             compound = coronanet_phrase$keyword, 
                                             ngram = coronanet_phrase$ngram)

coronanet_annotated <- coronanet_annotated[!is.na(coronanet_annotated$term),  ]

#Delete verb, punct, adp, det...
## Keep keyword or just plain nouns
coronanet_annotated$term <- ifelse(coronanet_annotated$upos %in% c("NOUN"), coronanet_annotated$lemma,
                                   ifelse(coronanet_annotated$term %in% coronanet_phrase$keyword, 
                                          coronanet_annotated$term, NA))

coronanet_annotated <- coronanet_annotated[!is.na(coronanet_annotated$term),  ]


## Build document/term/matrix

library(dplyr)
library(tidytext)

?count
coronanet_dtm <- 
  coronanet_annotated  %>%
  select(doc_id, term) %>%
  group_by(doc_id) %>%
  count(term, sort = TRUE) %>%
  tidytext::cast_dtm(doc_id, term, n)

m <- as.matrix(coronanet_dtm)
v <- sort(colSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
#write.csv(as.matrix(d), "coronanet_dtm.csv")

set.seed(007)
wordcloud::wordcloud(words = d$word, freq = d$freq, max.words=50)


###Run a topic model for select terms

library(stm)

#Earlier, we had used the function 'textprocessor' as the first step for the structural topic model 
#To use the DTM created using UDPipe (rather than raw text) as an input to structural topic model,
#we use the function 'readCorpus' in the package stm
stmInput <- readCorpus(coronanet_dtm, type="slam")

#Prepare documents for analysis; only terms which occurr in at least two documents will be selected because of lower.thresh = 1
stmData  <- prepDocuments(stmInput$documents, stmInput$vocab, stmInput$meta, lower.thresh = 1)

#See how different values of k affect information criteria
stm_k_3_20 <- searchK(stmData$documents, stmData$vocab, K = seq(3, 20, by = 1), data = stmData$meta, init.type = "Spectral")
plot(stm_k_3_20)



k <- 7
set.seed(2021)
stmOutput <- stm::stm(stmData$documents, stmData$vocab, K = k, data = stmData$meta, init.type = "Spectral")

#See the output of the model
plot.STM(stmOutput, type = "summary", xlim = c(0.0, 1.0), n = 8)

#See the top words for each topic. The highest probability and FREX words are usually most interesting
#Lift and Score are two other algorithms for determining the key words in each topic
labelTopics(stmOutput, c(1:k))

#Check topic correlation
stmTopicCorr <- topicCorr(stmOutput)
plot.topicCorr(stmTopicCorr)

