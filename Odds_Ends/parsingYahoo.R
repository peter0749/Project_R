library(XML)
library(bitops)
library(RCurl)
library(httr)

rm(list=ls(all=TRUE))
Sys.setlocale("LC_ALL", "cht")
yahoourl = "https://tw.news.yahoo.com/world/archive/"
appendurl = ".html"
testurl = ""
testvector = c()
testframe = data.frame()


for(i in 1:20)
{
  testurl = paste(yahoourl,i,appendurl,sep='')
  testexist = url.exists(testurl)
  if(testexist)
  {
    html = getURL(testurl, ssl.verifypeer = FALSE, encoding='UTF-8')
    xml = htmlParse(html, encoding='UTF-8')
    path = xpathSApply(xml,'//div/div[@class="txt"]/h4/a//@href',sessionEncoding='UTF-8')
	if(length(path)<1) next
    title = xpathSApply(xml,'//div/div[@class="txt"]/h4/a//text()',sessionEncoding='UTF-8',xmlValue)
	if(length(title)<1) next
	
    if(length(path)!=length(title)) next
    tempframe = data.frame(title,path)
    testframe = rbind(testframe,tempframe)
  }
  else print(paste("assert: URL:",testurl,"not exist!"))
}
names(testframe) = c("Title","Path")
write.csv(testframe,"testcsv.csv")