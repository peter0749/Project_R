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

myHttpheader<- c(
  "User-Agent"="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
  "Connection"="keep-alive",
  "Accept-Charset"="big5,GB2312,utf-8;q=0.7,*;q=0.7",
  "Accept-Encoding"="gzip, deflate, sdch",
  "Accept-Language"="zh-TW,zh;q=0.8,en-US;q=0.6,en;q=0.4",
  "Upgrade-Insecure-Requests"="1",
  "Cache-Control"="max-age=0",
  "Cookie"="__utmt=1; __utma=137419939.1443367072.1468586224.1468720076.1468727749.10; __utmb=137419939.3.10.1468727749; __utmc=137419939; __utmz=137419939.1468600072.4.2.utmcsr=google|utmccn=(organic)|utmcmd=organic|utmctr=(not%20provided)",
  "Host"="www.boxofficemojo.com",
  "Referer"="http://www.boxofficemojo.com/yearly/"
)

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
    html = getURL(yahooURL, ssl.verifypeer = FALSE, encoding='UTF-8', httpheader = myHttpheader)
    xml = htmlParse(html, encoding='UTF-8')
    text = xpathSApply(xml, '//tr[@bgcolor="#ffffff"]/td[@valign="top"]/b', sessionEncoding='UTF-8', xmlValue)
    if(length(text)<6) next
    testframe = as.data.frame(t(text))
    names(testframe) = c("Distrubutor","Release Date","Genre","Runtime","MPAA","Budget")
    testframe = cbind(alldata[i,-1],testframe)
    fulldata = rbind(fulldata, testframe)
  }
}

#Post-processing
fulldata$Runtime = gsub(" hrs. ",":",fulldata$Runtime)
fulldata$Runtime = gsub(" min.",":00",fulldata$Runtime)
fulldata$Runtime = times(fulldata$Runtime)
fulldata$Runtime = hours(fulldata$Runtime)*60 + minutes(fulldata$Runtime)
fulldata$'Release Date' = gsub(",","",fulldata$'Release Date')
fulldata$'Release Date' = gsub(" |[0-9]","",fulldata$'Release Date')
fulldata$Budget = substring(fulldata$Budget,2)
fulldata$Budget = gsub(" million","",fulldata$Budget)
fulldata$Budget = as.numeric(fulldata$Budget)

write.csv(fulldata,"Fulllist.csv")