# required pakacges
library(plyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(stringr)
rm(list = ls())

score.sentiment = function (sentence, stronger.words, strongest.words)
{
  require(stringr)
  scores = laply(sentence, function (sentence, stronger.words, strongest.words) {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub ('\\d+', '', sentence)
    
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    stronger.matches = match(words, stronger.words)
    strongest.matches = match(words, strongest.words)
    
    stronger.matches = !is.na(stronger.matches)
    strongest.matches = !is.na(strongest.matches)
    score = sum(stronger.matches) - sum(strongest.matches)
    print(words)
    return(score)
  }, stronger.words, strongest.words)
  
  return(scores)
}

stronger.words = scan('C:\\Users\\srirams\\Downloads\\test\\Stronger.txt', what='character', comment.char=';')
strongest.words = scan('C:\\Users\\srirams\\Downloads\\test\\Strongest.txt', what='character', comment.char=';')

sentence <- read.csv ('C:\\Users\\srirams\\Downloads\\test\\Messages.csv')
#Messages$x<- as.factor(Messages$x)

MessageResult.scores = score.sentiment(sentence, stronger.words, strongest.words)
path<-"C:\\Users\\srirams\\Downloads\\test\\"
write.csv(MessageResult.scores, file=paste(path, "MessageResult.csv",sep=""), row.names=TRUE)
