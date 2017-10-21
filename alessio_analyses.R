library(anytime)
library(tidyverse)
library(lubridate)
library(RJSONIO)
library(tm)
library(SnowballC)
library(wordcloud)
library(igraph)
library(reshape)
library(broom)
library(plotly)

# reading data according to anil_metrics.Rmd
df_raw <- read_csv("input/ted_main.csv")
names(df_raw)
df <- df_raw %>%
  select(
    name, title, description, main_speaker, speaker_occupation,
    num_speaker, duration, event, film_date, published_date, comments, 
    tags, languages, ratings, related_talks, url, views) %>%
  mutate_at(funs(anydate(., tz = 'UTC')), .vars = c('film_date', 'published_date'))

# The data set is until September 21st, 2017.
final_date <- as.Date('2017-09-21')
df <- df %>%
  mutate(days_since_release = final_date - published_date)
df_filt_date <- df %>%
  filter(days_since_release > 356) %>%
  mutate(
    popularity = views / as.integer(days_since_release),
    engagement = comments / views
  )



## Analyses

rat_levels <- c("Beautiful", "Confusing", "Courageous", "Funny", "Informative",
                "Ingenious", "Inspiring", "Longwinded", "Unconvincing", 
                "Fascinating", "Jaw-dropping", "Persuasive", "OK", "Obnoxious")

get_max_rating <- function(s){
  x <- fromJSON(as.character(s))
  x[[which.max(sapply(x, function(x) x$count))]][["id"]]
}

df$max_ratid <- factor(sapply(df$ratings, get_max_rating),
                       labels = rat_levels)
df %>%
  group_by(max_ratid) %>%
  count()

ggplot(df, aes(x = max_ratid, y = views, fill = max_ratid)) + 
  geom_boxplot() + guides(fill=FALSE) + 
  labs(x = "", y = ("Views x 1000")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(trans = "log", breaks = c(100, 250, 1000, 5000, 15000)*10^3, 
                     labels = c(100, 250, 1000, 5000, 15000))

df %>%
  group_by(max_ratid) %>%
  do(tidy(summary(.$views))) %>%
  arrange(desc(mean))

# As expected, the most viewed talks are most rated as 'Inspiring', followed by
#  'Funny', 'Informative', and 'Courageouos'
#
# On average, talks most rated as 'Jaw-dropping' have a higher mean number of views,
#  followed by 'Funny', 'Inspiring', and 'Courageous'

#ggplotly(
ggplot(df, aes(x = max_ratid, y = comments, fill = max_ratid)) + 
  geom_boxplot() + guides(fill=FALSE) + 
  labs(x = "", y = ("Comments")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(trans = "log", breaks = c(5, 20, 100, 200, 600, 2000))  
#)

df %>%
  group_by(max_ratid) %>%
  do(tidy(summary(.$comments))) %>%
  arrange((minimum))

# Talks with the maximum number of comments was most rated as 'Courageous'.
#  Other most commented talks was instead most rated as 'Inspiring', 'Persuasive',
#  and 'Fascinating'
#
# The average number of comments followed a pattern similar to the maximum, with
#   the exception of 'Unconvincing'


df$question <- grepl("\\?", df$title)
ggplot(df, aes(x = question, y = views)) +
  geom_boxplot()
ggplot(df, aes(x = question, y = comments)) +
  geom_boxplot()



## Analyses with Anil's variable


df_filt_date$max_ratid <- factor(sapply(df_filt_date$ratings, get_max_rating),
                                 labels = rat_levels)
df_filt_date %>%
  group_by(max_ratid) %>%
  count()

ggplot(df_filt_date, aes(x = max_ratid, y = popularity, fill = max_ratid)) + 
  geom_boxplot() + guides(fill=FALSE) + 
  labs(x = "", y = ("Views x 1000")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(trans = "log")

df_filt_date %>%
  group_by(max_ratid) %>%
  do(tidy(summary(.$popularity))) %>%
  arrange(desc(mean))

#ggplotly(
ggplot(df_filt_date, aes(x = max_ratid, y = engagement, fill = max_ratid)) + 
  geom_boxplot() + guides(fill=FALSE) + 
  labs(x = "", y = ("Comments")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(trans = "log")  
#)

df_filt_date %>%
  group_by(max_ratid) %>%
  do(tidy(summary(.$engagement))) %>%
  arrange((minimum))

