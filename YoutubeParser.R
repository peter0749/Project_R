rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(NLP)
library(httr)

Sys.setlocale("LC_ALL", "cht")

#Load and process the data from webs
alldata = read.csv('Fulllist.csv')
alldata$Box = as.character(alldata$Box)
alldata$Box = substring(alldata$Box,2)
alldata$Box = gsub(",","",alldata$Box)
alldata$Box = as.numeric(alldata$Box)
alldata$Runtime = gsub(" hrs. ",":",alldata$Runtime)
alldata$Runtime = gsub(" min.",":00",alldata$Runtime)
alldata$Runtime = times(alldata$Runtime)
alldata$Runtime = hours(alldata$Runtime)*60 + minutes(alldata$Runtime)
alldata$Release.Date = gsub(",","",alldata$Release.Date)
alldata$Release.Date = gsub(" |[0-9]","",alldata$Release.Date)
alldata$Budget = substring(alldata$Budget,2)
alldata$Budget = gsub(" million","",alldata$Budget)
alldata$Budget = as.numeric(alldata$Budget)

youtubeSRC = 'https://www.youtube.com/results?q='
yAppendURL = '%20trailer&sp=CAA%253D'
youtubeURL = ''

fulldata = data.frame()

for( i in 1:length(alldata$X))
{
  youtubeURL <- paste(youtubeSRC, alldata$Title[i], yAppendURL, sep='')
  youtubeURL <- gsub(" ","%20",youtubeURL)
  youtubeURL <- iconv(youtubeURL, "big5", "utf8")
  #Encoding(youtubeURL) = "UTF-8"
  print(youtubeURL)
  URLExist = url.exists(youtubeURL)
  print(URLExist)
  if(URLExist)
  {
    html = getURL(youtubeURL, ssl.verifypeer = FALSE, encoding='UTF-8')
    xml = htmlParse(html, encoding='UTF-8')
    text = xpathSApply(xml,'//li/div/div/div[2]/div[2]/ul/li[2]/text()', sessionEncoding='utf8', xmlValue)
    text = substring(text,6)
    text <- gsub(",","",text)
    text <- as.numeric(text)
    testframe = as.data.frame(t(text)[1])
    names(testframe) = c("Youtube Views")
    testframe = cbind(alldata[i,-1],testframe)
    fulldata = rbind(fulldata, testframe)
  }
  
}

write.csv(fulldata,"Youtubelist.csv")