library(gdata) 
library(tm)
library(dplyr)


#set a working directory
setwd("~/documents/ted-talks/")

#read the csv file with all TED main data, top 50 rows
ted_data_main <- read.csv(file ="~/documents/ted-talks/ted_main.csv", nrows= 50)
ted_tags <- as.character(ted_data_main$tags)
ted_tags_str<- toString(ted_tags)


# Preprocess tags
ted_tags_str <- Corpus(VectorSource(ted_tags_str),
                        readerControl = list(reader = readPlain,
                                             language = "en",
                                             load = T))

mystem<- function( doc){
  library(stringr)
  sdoc <- system('bin/mystem -nl -e utf-8 ', intern=T, input=doc)
  sdoc <- str_replace(sdoc, '\\|.*$', '')
  sdoc <- str_replace(sdoc, '\\?', '')
  sdoc <- paste(sdoc, collapse=" ")
  attributes(sdoc) <- attributes(doc)
  sdoc
}

ted_tags_str <- tm_map(ted_tags_str, stripWhitespace)
ted_tags_str <- tm_map(ted_tags_str, tolower)
ted_tags_str <- tm_map(ted_tags_str, removeNumbers)
ted_tags_str <- tm_map(ted_tags_str, removeWords, stopwords("english"))
ted_tags_str <- tm_map(ted_tags_str, removePunctuation)

#make a Word Cloud from them 
library(wordcloud)
wordcloud(ted_tags, random.order=F, max.words=100, 
          colors=brewer.pal(6,"Blues")) 

#Count words frequency
myTdm <- as.matrix(TermDocumentMatrix(ted_tags_str))
FreqMat <- data.frame(ST = rownames(myTdm), 
                      Freq = rowSums(myTdm), 
                      row.names = NULL)
colnames(FreqMat)<- c("Word", "Amount")
frequency<- order(-FreqMat$Amount)
FreqMat[frequency, ]
most_popular <- subset(FreqMat, Amount>=10)
print(most_popular)
#visualization of results
ggplot(most_popular, 
       main = "Most Popular Tags in Most Popular TED videos", aes(x = reorder(Word, -Amount), y = Amount)) +
  geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

