rm(list=ls(all=TRUE))
yahoourl = "https://tw.news.yahoo.com/world/archive/"
appendurl = ".html"
testurl = ""
testvector = c()

for(i in 1:10)
{
  testurl = paste(yahoourl,i,appendurl,sep='')
  testexist = url.exists(testurl)
  if(testexist)
  {
    html = getURL(testurl, ssl.verifypeer = FALSE)
    xml = htmlParse(html, encoding ='utf-8')
    title = xpathSApply(xml,'//div[2]/h4/a//text()',xmlValue)
    testvector = rbind(testvector, as.vector(title))
  }
}

write.csv(testvector,"testcsv.csv")