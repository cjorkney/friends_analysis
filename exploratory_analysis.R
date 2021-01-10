library(tidytuesdayR)
library(dplyr)
library(forcats)
library(tidytext)
library(stringr)
library(ggplot2)
library(RcppRoll)
library(purrr)


# Set constants for analysis ----------------------------------------------

names_six <- c('Monica Geller', 'Joey Tribbiani', 'Chandler Bing',
               'Phoebe Buffay', 'Ross Geller', 'Rachel Green')

movav_eps <- 30

top_words_n_season <- 10
top_words_n_char <- 10
top_ngrams_n_char <- 10
ngram_words <- 2
ngram_min_count <- 3
word_min_count <- 5

stop_words_plus <- stop_words %>%
  rbind(
    tibble(
      word = c('yeah', 'hey', 'um', 'uh', 'ah', 'umm',
               "y'know", 'ya', 'ohh', 'gonna', 'wanna',
               'ross', 'rachel', 'monica', 'chandler', 'joey',
               'phoebe', 'huh', 'ooh', 'uhm', 'wow', 'woah', 'whoa',
               'guys', 'god', 'fine', 'gotta', 'pheebs', 'rach'),
      lexicon = 'custom'
    )
  )

# Get the data ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2020-09-08')

lines <- tuesdata$friends
info <- tuesdata$friends_info
emotions <- tuesdata$friends_emotions



# Prepare data for analysis, add ID fields --------------------------------

lines_clean <- lines %>%
  filter(!(speaker == 'Scene Directions')) %>%
  mutate(
    speaker = as_factor(speaker),
    ep_id = paste(season, episode, sep = '-'),
    ep_id = factor(ep_id, levels = unique(ep_id)),
    ep_no = cummax(as.numeric(ep_id)),
    line_id = paste(season, episode, scene, utterance, sep = '-')
    )



# Summarise to show number of lines per char. per ep. ---------------------

# Summarise number of lines by character, by episode (limit to six main characters)
lines_by_character <- lines_clean %>%
  filter(speaker %in% names_six) %>%
  group_by(season, ep_id, ep_no, speaker) %>% 
  summarise(lines = n()) %>%
  ungroup() %>% 
  group_by(speaker) %>%
  mutate(movav = roll_meanr(lines, movav_eps)) %>%
  ungroup()



# Create plots showing lines per character per episode --------------------

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
                                   fill = 'black',
                                   show.legend = FALSE)

geom_text_movav_labels <- partial(geom_text,
                                  aes(x = ep_no, y = movav,
                                      label = sprintf('%0.1f', round(movav, 1))),
                                  colour = 'black')

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



# Further analysis of lines per character, no longer by ep. ---------------

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


# Tokenise all lines to words and do some analysis on them -----------------

words_full <- lines_clean %>%
  unnest_tokens(output = word, input = text)

words_neat <- words_full %>%
  anti_join(stop_words_plus)

# Word count by season
words_by_season <- words_neat %>% 
  count(season, word)

# Top 10 words by season
top_words_by_season <- words_by_season %>% 
  group_by(season) %>% 
  top_n(top_words_n_season, n) %>%
  ungroup()

ggplot(top_words_by_season, aes(x = reorder_within(word, n, season), y = n)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ season, scales = "free_y")

# Word count by character (main six)
words_by_char <- words_neat %>%
  filter(speaker %in% names_six) %>% 
  count(speaker, word)
  
# Top 10 words by character
top_words_by_char <- words_by_char %>% 
  group_by(speaker) %>% 
  top_n(top_words_n_char, n) %>%
  ungroup()

ggplot(top_words_by_char, aes(x = reorder_within(word, n, speaker), y = n, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ speaker, scales = "free_y")


# TFIDF analysis of ngrams by character -----------------------------------

# Unnest full lines into ngrams, count and add tfidf (main characters only)
ngrams_tfidf <- lines_clean %>%
  filter(speaker %in% names_six) %>% 
  unnest_tokens(output = ngram, input = text, token = "ngrams", n = ngram_words) %>%
  count(speaker, ngram) %>% 
  bind_tf_idf(term = ngram, document = speaker, n = n)

# Get top (by tfidf) ngrams for each character and plot - require used min. number of times

top_ngrams <- ngrams_tfidf %>%
  filter(n >= ngram_min_count) %>% 
  group_by(speaker) %>% 
  top_n(top_ngrams_n_char, tf_idf) %>%
  ungroup()

ggplot(top_ngrams, aes(x = reorder_within(ngram, tf_idf, speaker), y = tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ speaker, scales = 'free_y')


# TFIDF analysis of words by character ------------------------------------

words_tfidf <- words_full %>%
  filter(speaker %in% names_six) %>% 
  count(speaker, word) %>% 
  bind_tf_idf(term = word, document = speaker, n = n)

top_words_tfidf <- words_tfidf %>%
  filter(n >= word_min_count) %>%
  group_by(speaker) %>% 
  top_n(top_words_n_char, tf_idf) %>%
  ungroup()

ggplot(top_words_tfidf, aes(x = reorder_within(word, tf_idf, speaker), y = tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ speaker, scales = 'free_y')
  
