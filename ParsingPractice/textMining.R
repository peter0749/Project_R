rm(list=ls(all=TRUE))
# install.packages("jiebaR")
# install.packages("tm")
# install.packages("slam")
#install.packages("RColorBrewer")
# install.packages("wordcloud")
# install.packages("topicmodels")
# install.packages("igraph")

library(jiebaRD)
library(jiebaR)       # ?·θ©ε©?¨
library(NLP)
library(tm)           # ??ε?θ?ε?η©?£??η??
library(slam)         # η¨??η©?£??η??
library(RColorBrewer)
library(wordcloud)    # ??ε?ι²
library(topicmodels)  # δΈ»ι?ζ¨‘???
library(igraph)       # δΈ»ι?ζ¨‘??ι?θ―
library(plyr)

orgPath = "./yahooText"
Wordlen = 3
text = Corpus(DirSource(orgPath), list(language = NA))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, function(word)
{ gsub("[A-Za-z0-9]", "", word) })

# ?²θ?δΈ­??ζ·θ©?
mixseg = worker()
mat <- matrix( unlist(text), nrow=length(text) )
totalSegment = data.frame()
reducedSegment = data.frame()
for( j in 1:length(text) )
{
  for( i in 1:length(mat[j,]) )
  {
    result = segment(as.character(mat[j,i]), jiebar=mixseg)
  }
  totalSegment = rbind(totalSegment, data.frame(result))
}

reducedSegment = rbind(reducedSegment,data.frame(totalSegment$result[nchar(as.vector(totalSegment[,1]))>=Wordlen]))
names(reducedSegment) = c("result")

countMat = count(reducedSegment)
names(countMat) = c("totaldiff", "freq")
print(countMat)
totaldiff = sum(countMat[,2])
countMat[,2] = (countMat[,2] / sum(countMat[,2]))

wordcloud(countMat$totaldiff, countMat$freq, min.freq = 1, random.order = F, ordered.colors = T, 
          colors = rainbow(length(countMat$totaldiff)))