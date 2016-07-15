library(XML)
library(bitops)
library(RCurl)
library(httr)

rm(list=ls(all=TRUE))
Sys.setlocale("LC_ALL", "cht")
yahoourl = "http://www.boxofficemojo.com/alltime/world/?pagenum="
appendurl = "&p=.htm"
testurl = ""
testvector = c()
testframe = data.frame()


for(i in 1:5)
{
  testurl = paste(yahoourl,i,appendurl,sep='')
  testexist = url.exists(testurl)
  print(testurl)
  print(testexist)
  if(testexist)
  {
    html = getURL(testurl, ssl.verifypeer = FALSE, encoding='UTF-8')
    xml = htmlParse(html, encoding='UTF-8')
    path = xpathSApply(xml,'//table//td[2]/font[@size="2"]/a[@href]/b/../@href',sessionEncoding='UTF-8')
    if(length(path)<1) next
    title = xpathSApply(xml,'//table//td[2]/font[@size="2"]/a[@href]/b',sessionEncoding='UTF-8',xmlValue)
    if(length(title)<1) next
    box = xpathSApply(xml,'//table//td[4]/font/b',sessionEncoding='UTF-8',xmlValue)
    
    if(length(path)!=length(title)) next
    tempframe = data.frame(title,path,box)
    testframe = rbind(testframe,tempframe)
  }
  else print(paste("assert: URL:",testurl,"not exist!"))
}
names(testframe) = c("Title","Path","Box")
write.csv(testframe,"testcsv.csv")
