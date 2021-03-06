library(tidytuesdayR)
library(dplyr)
library(tm)
library(wordcloud)
library(topicmodels)
library(tidytext)
library(ggplot2)

source("R/clean_corpus.R")

# Set constants for analysis ----------------------------------------------

set.seed(1234)

names_six <- c('Monica Geller', 'Joey Tribbiani', 'Chandler Bing',
               'Phoebe Buffay', 'Ross Geller', 'Rachel Green')

custom_stopwords <- c("yeah", "hey", "um", "uh", "ah", "umm",
                      "y'know", "ya", "ohh", "gonna", "wanna",
                      "ross", "rachel", "monica", "chandler", "joey",
                      "phoebe", "huh", "ooh", "uhm", "wow", "woah", "whoa",
                      "guys", "god", "fine", "gotta", "pheebs", "rach",
                      "just", "okay", "well", "like", "can", "get", "see",
                      "get", "know", "right", "tell", "look", "got", "come",
                      "go", "want", "really", "yes", "no", "maybe", "going",
                      "make", "now", "back",
                      stopwords("en"))




# Get the data ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2020-09-08')

lines <- tuesdata$friends
info <- tuesdata$friends_info
emotions <- tuesdata$friends_emotions


# Prepare data for analysis, add ID fields --------------------------------

lines_clean <- lines %>%
  filter(speaker %in% names_six) %>%
  mutate(
    ep_id = paste(season, episode, sep = '-'),
    ep_id = factor(ep_id, levels = unique(ep_id)),
    ep_no = cummax(as.numeric(ep_id)),
    line_id = paste(season, episode, scene, utterance, sep = '-')
  )


# Create corpus where each document is a character ------------------------

char_corpus <- lines_clean %>% 
  group_by(speaker) %>%
  summarise(text = paste(text, collapse = " ")) %>%
  ungroup() %>%
  rename(doc_id = speaker) %>%
  DataframeSource() %>%
  VCorpus()

corpus_clean <- clean_corpus(char_corpus,
                             stops = custom_stopwords)


# Create document term matrix ---------------------------------------------

char_dtm <- DocumentTermMatrix(corpus_clean)

char_dtm_m <- as.matrix(char_dtm)

# Get overall term frequencies and produce wordcloud

word_freqs <- colSums(char_dtm_m)

wordcloud(words = names(word_freqs), freq = word_freqs, max.words = 100)


# Topic modelling ---------------------------------------------------------

# Can we create topic groups that correspond well to the six main characters?

char_lda <- LDA(char_dtm, k = 3,
                control = list(
                  seed = 1234
                ))

topics_beta <- tidy(char_lda, matrix = "beta")

topics_top_terms <- topics_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup()

topics_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ topic, scales = "free_y")


