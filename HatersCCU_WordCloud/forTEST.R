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
plot_wordcloud <- function(text,mixseg) {
  text = Corpus(VectorSource(text))
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeNumbers)
  
  text <- tm_map(text, function(word) {
    gsub("[^\u4e00-\u9fa5]","",word)
  })
  
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
}
topn = 100
pagen = 1000

tok = '' # <- Here your Facebook token
text = getPage(page = '',token = tok, n=pagen)
postActive = as.vector(text[order(text$likes,decreasing=TRUE)[1:topn],7])
postList = sapply(postActive, getPost, token=tok, n=topn)[[3]]
mostRep = as.vector(text[order(text$comments_count,decreasing=TRUE)[1:topn],7])
repList = sapply(mostRep, getPost, token=tok, n=topn)[[3]]
mixseg = worker()
plot_wordcloud(text$message,mixseg)
plot_wordcloud(postList$message,mixseg)
plot_wordcloud(repList$message,mixseg)
