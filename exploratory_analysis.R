library(tidytuesdayR)
library(dplyr)
library(forcats)
library(tidytext)
library(stringr)

# Get data

tuesdata <- tidytuesdayR::tt_load('2020-09-08')

lines <- tuesdata$friends
info <- tuesdata$friends_info
emotions <- tuesdata$friends_emotions

# Clean up lines data

lines_clean <- lines %>%
  filter(!(speaker == 'Scene Directions')) %>%
  mutate(
    speaker = as_factor(speaker),
    ep_id = paste(season, episode, sep = '-'),
    ep_id = factor(ep_id, levels = unique(ep_id)),
    ep_no = cummax(as.numeric(ep_id)),
    line_id = paste(season, episode, scene, utterance, sep = '-')
    )
