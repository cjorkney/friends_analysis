library(tidytuesdayR)
library(dplyr)
library(tm)

# Set constants for analysis ----------------------------------------------

names_six <- c('Monica Geller', 'Joey Tribbiani', 'Chandler Bing',
               'Phoebe Buffay', 'Ross Geller', 'Rachel Green')


# Get the data ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2020-09-08')

lines <- tuesdata$friends
info <- tuesdata$friends_info
emotions <- tuesdata$friends_emotions


# Prepare data for analysis, add ID fields --------------------------------

lines_clean <- lines %>%
  filter(!(speaker == 'Scene Directions')) %>%
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