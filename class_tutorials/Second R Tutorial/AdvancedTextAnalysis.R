###My directory structure
###Project (In this case: Demo)
###Sub-folders: Code, Data, Result, Literature, Manuscript


###First, I change the working directory to the overall project folder
setwd("~/Documents/teaching/SEN1231 Mixed research methods/Demo")


###Read the input file
###On my system, the file is stored in the sub-folder data
coronanet <- readtext::readtext("data/policy_responses.csv", text_field = "description")                                                ###coronanet <- readtext::readtext("data/coronanet_release_20201125.csv", text_field = "description")
coronanet <- dplyr::distinct(coronanet, country, text)

#Create the document-term matrix
dtm <- quanteda::dfm(coronanet$text, tolower = T, remove = stopwords::stopwords(source = "smart"), remove_numbers = T, remove_punct = T, stem = T)

#Estimate the LDA topic model for two topics
pr_lda <- topicmodels::LDA(dtm, k = 2, control = list(seed = 2021))
pr_lda

#Obtain the probability distribution of each word in each topic 
library(tidytext)
pr_topics <- tidy(pr_lda, matrix = "beta")

pr_topics

#Store the terms with the highest probability in each topic 
library(ggplot2)
library(dplyr)
library(tidyr)

pr_top_terms <- pr_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Create a plot of the terms in each topic
pr_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


#Obtain the probability of each topic in each document
pr_documents <- tidy(pr_lda, matrix = "gamma")
pr_documents

#Plot 'gamma'
pr_documents %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))


#Install the package ldatuning to explore some metrc associated with 'k'
library(ldatuning)

k_search <- FindTopicsNumber(dtm, topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs", control = list(seed = 2021), verbose = T)

FindTopicsNumber_plot(k_search)

###Exercise 1: Also estimate the topic models for the number of topics (k) = 5
###Create the chart for top terms per topic and the topic prevalence per text
###Go through the policy responses to check the coherence of the topics
###Assign a label to each topic



###Structural topic model

#Here I am using the read.csv function instead of the readtext function
coronanet <- read.csv("data/policy_responses.csv", header = T, stringsAsFactors = F)

library(stm)

#Preprocess the data using the STM library function
stmInput <- textProcessor(coronanet$description, metadata = coronanet)
#Prepare documents for analysis; only terms with at least two occurrences will be selected because of lower.thresh = 2
stmData  <- prepDocuments(stmInput$documents, stmInput$vocab, stmInput$meta, lower.thresh = 2)

#See how different values of k affect information criteria
stm_k_3_20 <- searchK(stmData$documents, stmData$vocab, K = seq(3, 20, by = 1), data = stmData$meta, init.type = "Spectral")
plot(stm_k_3_20)

#Select k = 4 for now
k <- 4
set.seed(2021)
stmOutput <- stm::stm(stmData$documents, stmData$vocab, K = k, data = stmData$meta, init.type = "Spectral")

#See the output of the model
plot.STM(stmOutput, type = "summary", xlim = c(0.0, 1.0), n = 12)

#See the top words for each topic. The highest probability and FREX words are usually most interesting
#Lift and Score are two other algorithms for determining the key words in each topic
labelTopics(stmOutput, c(1:k))

#Check topic correlation
stmTopicCorr <- topicCorr(stmOutput)
plot.topicCorr(stmTopicCorr)

#Compare topics 3 and 4
plot(stmOutput, type = "perspectives", topics = c(3, 4))

#See two policy responses corresponding to each topic
findThoughts(stmOutput, texts = stmData$meta$description, topics = seq(1:k), n = 2)



###Exercise 2: Re-estimate the STM with country as a covariate for prevalence
###Compare topic prevalence in China versus Russia and Germany versus the Netherlands
###See STM Vignette for the syntax (pg. 11 and 17-18)

set.seed(2021)
stmOutput_prevalence <- stm::stm(stmData$documents, stmData$vocab, K = k, data = stmData$meta, prevalence =~ country,  init.type = "Spectral")

plot.STM(stmOutput_prevalence, type = "summary", xlim = c(0.0, 1.0), n = 12)

stmTopicCorr_prevalence <- topicCorr(stmOutput_prevalence)
plot.topicCorr(stmTopicCorr_prevalence)

#Estimate the effect of country on topic prevalence
stmEffect_prevalence <- estimateEffect(formula = 1:k ~ country, stmobj = stmOutput_prevalence,
                                       metadata = stmData$meta, uncertainty = "Global")
plot(stmEffect_prevalence, covariate = "country", topics = c(3, 4), model = stmOutput_prevalence)

#Compare topic prevalence in China and Russia
plot(stmEffect_prevalence, covariate = "country", topics = c(3, 4), model = stmOutput_prevalence, method = "difference",
     cov.value1 = "China", cov.value2 = "Russia")

#Compare topic prevalence in Germany and the Netherlands
plot(stmEffect_prevalence, covariate = "country", topics = c(1:4), model = stmOutput_prevalence, method = "difference",
     cov.value1 = "Germany", cov.value2 = "Netherlands")