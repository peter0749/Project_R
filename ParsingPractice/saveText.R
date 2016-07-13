rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(NLP)
library(httr)

Sys.setlocale("LC_ALL", "cht")

alldata = read.csv('testcsv.csv')
orgURL = 'https://tw.news.yahoo.com'
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
    text = xpathSApply(xml, '//div[@itemscope and @itemtype="https://schema.org/Article"]', sessionEncoding='UTF-8', xmlValue)
    name <- paste('./yahooText/text', i, '.txt', sep='')
    write(text, name)
  }
}