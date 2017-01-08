rm(list=ls(all=TRUE))
library(XML)
library(RCurl)
library(bitops)
library(httr)
library(jiebaRD)
library(jiebaR)       
library(NLP)
library(tm)           
library(slam)         
library(RColorBrewer)
library(wordcloud)    
library(topicmodels) 
library(igraph)       
library(plyr)
library(Rfacebook)
toSeg <- function(X,mixseg) {
  return(segment(as.character(X), jiebar=mixseg))
}
tok = '' # <- Here your Facebook token
#getPage(page = '1510948885828736',token = tok)
text = getPage(page = '1510948885828736',token = tok, n=300)
text = Corpus(VectorSource(text$message))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)

text <- tm_map(text, function(word)
{ gsub("靠北|民雄|大鳳梨|鳳梨", "", word) })

text <- tm_map(text, function(word) {
  gsub("[^\u4e00-\u9fa5]","",word)
})
mixseg = worker()
mat <- matrix( unlist(text), nrow=length(text))

totalSegment = unlist(apply( mat, c(1,2), toSeg, mixseg ))
totalSegment = totalSegment[nchar(totalSegment)>=3]
countMat = count(totalSegment)
names(countMat) = c("totaldiff", "freq")
print(countMat)
totaldiff = sum(countMat[,2])
countMat[,2] = (countMat[,2] / sum(countMat[,2]))

wordcloud(countMat$totaldiff, countMat$freq, min.freq = 1.3, random.order = F, ordered.colors = T, 
          colors = rainbow(length(countMat$totaldiff)))
