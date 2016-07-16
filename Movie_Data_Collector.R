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