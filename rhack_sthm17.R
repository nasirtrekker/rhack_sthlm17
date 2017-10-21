
install.packages("anytime","dplyr","ggplot2","dplyr","plyr","JSNONIO","tm","SnowballC","wordcloud","igraph","reshape")
suppressPackageStartupMessages(library(anytime)) # For converting UNIX timestamps
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RJSONIO))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(SnowballC))
suppressPackageStartupMessages(library(wordcloud))
install.packages("igraph")
library(igraph)
install.packages("reshape")
library("reshape")
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(reshape))

## read the data
df <- read.csv("ted_main.csv")
colnames(df)


# reorganize
df = df[, c('name', 'title', 'description', 'main_speaker', 'speaker_occupation', 'num_speaker', 'duration', 'event', 'film_date', 'published_date', 'comments', 'tags', 'languages', 'ratings', 'related_talks', 'url', 'views')]
# make it human readable format
df$film_date = anydate(df$film_date)
df$published_date = anydate(df$published_date)
head(df)

## dplyr and plyr
install.packages("dplyr")
library("dplyr")
install.packages("plyr")
library("plyr")
## magritter
install.packages("magrittr")
library(magrittr)
## speaker
speaker_df <- data.frame(table(df$main_speaker))
colnames(speaker_df) <- c("main_speaker", "appearances")
speaker_df <- speaker_df %>% arrange(desc(appearances))
head(speaker_df, 10)
### other

## install ggplot
install.packages("ggplot")
library("ggplot")
install.packages("ggplot2")
library("ggplot2")


## which occupation is popular
occupation_df <- data.frame(table(df$speaker_occupation))
colnames(occupation_df) <- c("occupation", "appearances")
occupation_df <- occupation_df %>% arrange(desc(appearances))
head(occupation_df, 10)

ggplot(head(occupation_df,10), aes(x=reorder(occupation, appearances), 
                                   y=appearances, fill=occupation)) + 
        geom_bar(stat="identity") + guides(fill=FALSE)
df_common_occ <- df[df$speaker_occupation %in% head(occupation_df$occupation, 10), ]
ggplot(df_common_occ, aes(x=speaker_occupation, y=views, fill=speaker_occupation)) + geom_boxplot() +
        guides(fill=FALSE)

## relationshipt of 10 most professions
df_common_occ <- df[df$speaker_occupation %in% head(occupation_df$occupation, 10), ]
ggplot(df_common_occ, aes(x=speaker_occupation, y=views, fill=speaker_occupation)) + geom_boxplot() +
        guides(fill=FALSE)

## no of talks more than one speaker
table(df$num_speaker)
# which performance
df[df[,'num_speaker'] == 5, c('title', 'description', 'main_speaker', 'event')]
## rating function
# These function are just quick hacks.

get_funny <- function(s){
        x <- fromJSON(as.character(s))
        for(i in 1:length(x)){
                if(x[[i]]$id==7){
                        return(x[[i]]$count)
                }
        }
        return(-1)
}

get_jawdrop <- function(s){
        x <- fromJSON(as.character(s))
        for(i in 1:length(x)){
                if(x[[i]]$id==23){
                        return(x[[i]]$count)
                }
        }
        return(-1)
}

get_beautiful <- function(s){
        x <- fromJSON(as.character(s))
        for(i in 1:length(x)){
                if(x[[i]]$id==1){
                        return(x[[i]]$count)
                }
        }
        return(-1)
}

get_confusing <- function(s){
        x <- fromJSON(as.character(s))
        for(i in 1:length(x)){
                if(x[[i]]$id==2){
                        return(x[[i]]$count)
                }
        }
        return(-1)
}

## fromJSON
install.packages("fromJSON")
install.packages("rjson")
library("rjson")
library("fromJSON")
install.packages("RJSONIO")
library("RJSONIO")
df$funny <- sapply(df$ratings, get_funny)
df$beautiful <- sapply(df$ratings, get_beautiful)
df$jawdrop <- sapply(df$ratings, get_jawdrop)
df$confusing <- sapply(df$ratings, get_confusing)
head(df)

## word cloud
texts <- df3$transcript
#texts <- iconv(texts, to = "utf-8")
corpus <- Corpus(VectorSource(texts))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, c("and", "this", "there")) 
corpus <- Corpus(VectorSource(corpus))
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
d <- d[-which(d$word %in% c("and","this","that")),]
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
## install anytime packages
install.packages("anytime")
library(anytime)
## installa fromJson
install.packages("fromJSON")
install.packages("rjson")
library("rjson")
df$funny <- sapply(df$ratings, get_funny)
df$beautiful <- sapply(df$ratings, get_beautiful)
df$jawdrop <- sapply(df$ratings, get_jawdrop)
df$confusing <- sapply(df$ratings, get_confusing)
head(df)

## funiest talk of all time
funnytalk <- df[, c('title', 'main_speaker', 'views', 'published_date', 'funny')] %>% 
        arrange(desc(funny)) %>% 
        head(10)
# most beautiful talk of all time
beautifultalk <- df[, c('title', 'main_speaker', 'views', 'published_date', 'beautiful')] %>% 
        arrange(desc(beautiful)) %>% 
        head(10)
## most jawdropping 
mostjawdropping <- df[, c('title', 'main_speaker', 'views', 'published_date', 'jawdrop')] %>% 
        arrange(desc(jawdrop)) %>% 
        head(10)
## most confusing
confusetalk <- df[, c('title', 'main_speaker', 'views', 'published_date', 'confusing')] %>% 
        arrange(desc(confusing)) %>% 
        head(10)
## get related pairs of all talks
get_related_pairs <- function(ix, df){
        row <- df[ix, ]
        self_title <- as.character(row$title)
        related <- row$related_talks
        s <- as.character(related)
        bits <- strsplit(s, ",")
        rel <- c()
        for(b in bits[[1]]){
                #print(b)
                if(length(grep("'title'", b))>0){
                        b <- strsplit(b, ": ")[[1]]
                        title <- noquote(b[[2]])
                        title <- gsub("'", "", title)
                        title <- gsub("\"", "", title)
                        rel <- c(rel, noquote(title))
                }
        }
        return(data.frame(source=rep(self_title, length(rel)), target=rel))
}
# Get the related pairs for all talks
related <- lapply(1:nrow(df), function(x) get_related_pairs(x, df))
# Flatten into a big data frame with pairs
all_related <- Reduce(function(a, b) rbind(a, b), related)
# Remove duplicates
all_related <- all_related[-which(duplicated(all_related)),]
# Make igraph graph
graph <- graph_from_edgelist(as.matrix(all_related), directed=FALSE)
graph_df <- graph.data.frame(layout_nicely(graph))
plot(graph_df, vertex.label=NA)

## word cloud
texts <- df3$transcript
#texts <- iconv(texts, to = "utf-8")
corpus <- Corpus(VectorSource(texts))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, c("and", "this", "there")) 
corpus <- Corpus(VectorSource(corpus))
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
d <- d[-which(d$word %in% c("and","this","that")),]
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

### speaker 
speaker_df <- data.frame(table(df$main_speaker))
colnames(speaker_df) <- c("main_speaker", "appearances")
speaker_df <- speaker_df %>% arrange(desc(appearances))
head(speaker_df, 10)
## speakers occupation
occupation_df <- data.frame(table(df$speaker_occupation))
colnames(occupation_df) <- c("occupation", "appearances")
occupation_df <- occupation_df %>% arrange(desc(appearances))
head(occupation_df, 10)

## 
df[df[,'num_speaker'] == 5, c('title', 'description', 'main_speaker', 'event')]

ggplot(head(occupation_df,10), aes(x=reorder(occupation, appearances), 
                                   y=appearances, fill=occupation)) + 
        geom_bar(stat="identity") + guides(fill=FALSE)


table(df$num_speaker)


