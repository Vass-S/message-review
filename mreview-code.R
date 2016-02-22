# required pakacges
library(plyr)
library(stringr)
library(ggplot2)
library(plyr)
library(stringr)
rm(list = ls())

score.sentiment = function (sentence, stronger.words, strongest.words)
{
  #scores = laply(sentences, function (sentence, stronger.words, strongest.words) {
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
  score = sum(stronger.matches) + 2*sum(strongest.matches)
  return(score)
  # }, stronger.words, strongest.words)
  #print(scores)
  #return(scores)
}

#stronger.words = scan('C:\\Users\\srirams\\Downloads\\test\\Stronger.txt', what='character', comment.char=';')
#strongest.words = scan('C:\\Users\\srirams\\Downloads\\test\\Strongest.txt', what='character', comment.char=';')
stronger.words = scan('/Users/sriniwassriram/Documents/DataScienceProjects/Message-review/message-review/Data/Stronger.txt', what='character', comment.char=';')
strongest.words = scan('/Users/sriniwassriram/Documents/DataScienceProjects/Message-review/message-review/Data/Strongest.txt', what='character', comment.char=';')

#Messages <- read.csv ('C:\\Users\\srirams\\Downloads\\test\\Messages.csv')
Messages <- scan('/Users/sriniwassriram/Documents/DataScienceProjects/Message-review/message-review/Data/Messages.txt', what='character', comment.char=';')
#Messages$x<- as.factor(Messages$x)

MessageResult.scores = score.sentiment(Messages, stronger.words, strongest.words)
#path<-"C:\\Users\\srirams\\Downloads\\test\\"
#path<-"/Users/sriniwassriram/Documents/DataScienceProjects/Message-review/message-review/Data/Results"
#write.csv(MessageResult.scores, file=paste(path, "MessageResult.csv",sep=""), row.names=TRUE)
MessageResult.scores
