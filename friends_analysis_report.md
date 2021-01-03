Friends Analysis Report
================
Callum Orkney
13/12/2020

    ## 
    ##  Downloading file 1 of 3: `friends.csv`
    ##  Downloading file 2 of 3: `friends_info.csv`
    ##  Downloading file 3 of 3: `friends_emotions.csv`

## Analysis of number of lines

Summarising the data by episode and character, we can see how the number
of lines per episode for each character has changed over time. To avoid
an unreadable mess, let’s calculate a 30-episode moving average:

``` r
lines_by_character <- lines_clean %>%
  filter(speaker %in% names_six) %>%
  group_by(ep_id, ep_no, speaker) %>% 
  summarise(lines = n()) %>%
  ungroup() %>% 
  group_by(speaker) %>%
  mutate(movav = roll_meanr(lines, 30)) %>%
  ungroup()
```

Let’s plot a line graph of this, with one line representing each
character’s moving average lines-per-episode over time:

![](friends_analysis_report_files/figure-gfm/line-plot-1.png)<!-- -->
