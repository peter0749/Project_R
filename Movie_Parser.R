library(XML)
library(bitops)
library(RCurl)
library(httr)

rm(list=ls(all=TRUE))
Sys.setlocale("LC_ALL", "cht")
yahoourl = "http://www.boxofficemojo.com/yearly/chart/?page="
appendurl_1 = "&view=releasedate&view2=domestic&yr="
appendurl_2 = "&p=.htm"
testurl = ""
testvector = c()
testframe = data.frame()

for(j in 1980:2016)
{
  for(i in 1:10)
  {
    testurl = paste(yahoourl,i,appendurl_1,j,appendurl_2,sep='')
    testexist = url.exists(testurl)
    print(testurl)
    print(testexist)
    if(testexist)
    {
      html = getURL(testurl, ssl.verifypeer = FALSE, encoding='UTF-8')
      xml = htmlParse(html, encoding='UTF-8')
      path = xpathSApply(xml,'//table//td[2]/b/font[@size="2"]/a/@href',sessionEncoding='UTF-8')
      if(length(path)<1) break
      title = xpathSApply(xml,'//table//td[2]/b/font[@size="2"]/a[@href]',sessionEncoding='UTF-8',xmlValue)
      if(length(title)<1) break
      box = xpathSApply(xml,'//table//td[4]/font[@size="2"]/b',sessionEncoding='UTF-8',xmlValue)
      if(length(box)<1) break
      
      testlen = length(path)
      
      if(length(title)!=testlen || length(box)!=testlen) next
      tempframe = data.frame(title,path,box)
      testframe = rbind(testframe,tempframe)
    }
    else print(paste("assert: URL:",testurl,"not exist!"))
  }
}

names(testframe) = c("Title","Path","Box")

#Post-processing and sorting
testframe$Box = substring(testframe$Box,2)
testframe$Box = gsub(",","",testframe$Box)
testframe$Box = as.numeric(testframe$Box)
testframe = testframe[order(testframe$Box,decreasing=TRUE),]

write.csv(testframe,"testcsv.csv")
