library(tidytuesdayR)
library(dplyr)
library(forcats)
library(tidytext)
library(stringr)
library(ggplot2)
library(RcppRoll)
library(purrr)

# Set constants
names_six <- c('Monica Geller', 'Joey Tribbiani', 'Chandler Bing',
               'Phoebe Buffay', 'Ross Geller', 'Rachel Green')

movav_eps <- 30

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
  mutate(movav = roll_meanr(lines, movav_eps)) %>%
  ungroup()

# Take first and last episodes from lines_by_character, to use as points and labels

lines_first <- lines_by_character %>%
  filter(ep_no == movav_eps)

lines_last <- lines_by_character %>%
  group_by(speaker) %>%
  slice(max(ep_no)) %>%
  ungroup()

# Line plot showing moving average of lines per episode by character over time
ggplot(lines_by_character, aes(x = ep_no, y = movav, colour = speaker)) +
  geom_line() +
  labs(
    title = 'Number of lines per episode by main character',
    subtitle = paste0(movav_eps, '-episode moving averages'),
    x = 'Episode number', y = 'Lines spoken per episode',
    colour = 'Character'
  )

# Define partially-filled versions of geom_point and geom_text to reduce
# duplicated code in labelling the end-points of the faceted plot

geom_point_movav_labels <- partial(geom_point,
                                   aes(x = ep_no, y = movav),
                                   shape = 21,
                                   fill = 'black')

geom_text_movav_labels <- partial(geom_text,
                                  aes(x = ep_no, y = movav,
                                      label = sprintf('%0.2f', round(movav, 1))),
                                  colour = 'black',)

# Facet the above plot by character
ggplot(lines_by_character, aes(x = ep_no, y = movav, colour = speaker)) +
  geom_line(show.legend = FALSE, size = 1) +
  geom_point_movav_labels(data = lines_first) +
  geom_text_movav_labels(data = lines_first,
                         hjust = 0.8, vjust = -0.5) +
  geom_point_movav_labels(data = lines_last) +
  geom_text_movav_labels(data = lines_last,
                         hjust = -0.01, vjust = -0.5) +
  geom_line(aes(y = lines), alpha = 0.4, show.legend = FALSE) +
  coord_cartesian(xlim = c(0, 260), ylim = c(0, 80)) +
  facet_wrap(~ speaker) +
  labs(
    title = 'Number of lines per episode by main character',
    subtitle = paste0(movav_eps, '-episode moving averages'),
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





