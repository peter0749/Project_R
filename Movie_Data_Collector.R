rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(NLP)
library(httr)

Sys.setlocale("LC_ALL", "cht")

alldata = read.csv('testcsv.csv')
orgURL = 'http://www.boxofficemojo.com'
fulldata = data.frame()

for( i in 1:length(alldata$X))
{
  yahooURL <- paste(orgURL, alldata$Path[i], sep='')
  yahooURL <- iconv(yahooURL, "big5", "utf8")
  #Encoding(yahooURL) = "UTF-8"
  print(yahooURL)
  URLExist = url.exists(yahooURL)
  print(URLExist)
  if(URLExist)
  {
    html = getURL(yahooURL, ssl.verifypeer = FALSE, encoding='UTF-8')
    xml = htmlParse(html, encoding='UTF-8')
    text = xpathSApply(xml, '//tr[@bgcolor="#ffffff"]/td[@valign="top"]/b', sessionEncoding='UTF-8', xmlValue)
    testframe = as.data.frame(t(text))
    names(testframe) = c("Director","Release Date","Genre","Runtime","MPAA","Budget")
    testframe = cbind(alldata[i,-1],testframe)
    fulldata = rbind(fulldata, testframe)
  }
  write.csv(fulldata,"Fulllist.csv")
}