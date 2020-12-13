library(tidytuesdayR)
library(dplyr)
library(forcats)
library(tidytext)
library(stringr)
library(ggplot2)
library(RcppRoll)

# Set constants
names_six <- c('Monica Geller', 'Joey Tribbiani', 'Chandler Bing',
               'Phoebe Buffay', 'Ross Geller', 'Rachel Green')

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

# Summarise number of lines by character, by episode (limit to six main characters)
lines_by_character <- lines_clean %>%
  filter(speaker %in% names_six) %>%
  group_by(season, ep_id, ep_no, speaker) %>% 
  summarise(lines = n()) %>%
  ungroup() %>% 
  group_by(speaker) %>%
  mutate(movav = roll_meanr(lines, 30)) %>%
  ungroup()

# Line plot showing moving average of lines per episode by character over time
ggplot(lines_by_character, aes(x = ep_no, y = movav, colour = speaker)) +
  geom_line() +
  labs(
    title = 'Number of lines per episode by main character',
    subtitle = '30-episode moving averages',
    x = 'Episode number', y = 'Lines spoken per episode',
    colour = 'Character'
  )

# Density plot comparing characters' numbers of lines, faceted by season
ggplot(lines_by_character, aes(x = lines, colour = speaker)) +
  geom_line(stat = "density") +
  facet_wrap(~ season)

# Same information shown as a boxplot
ggplot(lines_by_character, aes(x = lines, y = speaker, colour = speaker)) +
  geom_boxplot() +
  facet_wrap(~ season) +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

# Boxplot for all seasons combined
ggplot(lines_by_character,
       aes(x = lines, y = reorder(speaker, lines, median), colour = speaker)) +
  geom_boxplot() +
  theme(
    legend.position = "none"
  )





