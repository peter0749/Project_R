rm(list=ls(all=TRUE))
gc()
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

URL_ERROR_HANDLE = function(X, MAX_TRI){
  if(MAX_TRI==0) return(NULL)
  tries = tryCatch(getURL(X,encoding='UTF-8', followlocation=TRUE, ssl.verifypeer = FALSE, .opts=list(timeout=3)), error = function(e) { Sys.sleep(5); return(URL_ERROR_HANDLE(X,MAX_TRI-1)) })
  return(tries)
}

RATING_PREFIX = "http://codeforces.com/ratings/page/"
RATING_NAME_XPATH = '//tr/td[4]/../td[2]/a'
RATING_SCORE = '//tr/td[4]'
PAGE_NUM = 7
CONTEST_LIM = 1
SUB_LIM = 2

MEMBER_URL_PREFIX = "http://codeforces.com/contests/with/"
CONTEST_XPATH = "//tr/td[4]/a/@href"

SRC_URL_FIX = "http://codeforces.com"
CODE_URL = c("http://codeforces.com/contest/","/submission/" )
SUBMIT_XPATH = "//tr/td[8]/../td[1]/a"
SRC_XPATH = '//pre[@class="prettyprint program-source"]//text()'

member_data = data.frame()

for(i in 1:PAGE_NUM)
{
  RAT_URL = paste(RATING_PREFIX,i,sep='',collapse='')
  if(!url.exists(RAT_URL)) next
  rat_html = URL_ERROR_HANDLE(RAT_URL,5)
  rat_xml = htmlParse(rat_html,encoding = 'UTF-8',asText=TRUE)
  sig_member = xpathSApply(rat_xml,RATING_NAME_XPATH,sessionEncoding='UTF-8',xmlValue)
  sig_member = sig_member[!grepl(pattern = 'Detailed|View all',x = sig_member)]
  sig_score = xpathSApply(rat_xml,RATING_SCORE,sessionEncoding='UTF-8',xmlValue)
  sig_score = gsub(pattern = '[\r\n \t]', x=sig_score,replacement = '')
  member = data.frame(sig_member,sig_score)
  member_data = rbind.fill(member_data, member)
  print(i)
}

member_num = nrow(member_data)
#member_num = 2
nalist = c()
con_list = data.frame()

for(i in 1:member_num)
{
  contest_url = paste(MEMBER_URL_PREFIX,member_data[i,1],sep='',collapse = '')
  if(!url.exists(contest_url))
  {
    nalist = c(nalist, i)
    next
  }
  cont_html = URL_ERROR_HANDLE(contest_url,5)
  cont_xml = htmlParse(cont_html,encoding='UTF-8',asText=TRUE)
  sig_cont = xpathSApply(cont_xml,CONTEST_XPATH,sessionEncoding='UTF-8')[1:CONTEST_LIM]
  for(j in 1:length(sig_cont))
  {
    sig_cont[[j]] = paste(SRC_URL_FIX, sig_cont[[j]], sep='', collapse = '')
  }
  con_list = rbind.fill(con_list,data.frame(t(unlist(sig_cont))))
  print(i)
}
if(!is.null(nalist))
  member_data = member_data[-nalist,]
nalist = c()
id_list = data.frame()

for(i in 1:nrow(con_list))
#for(i in 1:1)
{
  temp_list = list()
  for(j in 1:ncol(con_list))
  {
    if(!url.exists(as.character(con_list[i,j]))) next
    temphtml = URL_ERROR_HANDLE(con_list[i,j],5)
    tempxml  = htmlParse(temphtml, encoding = 'UTF-8', asText=TRUE)
    sub_id = xpathSApply(tempxml, SUBMIT_XPATH, sessionEncoding='UTF-8',xmlValue)[1:SUB_LIM]
    for(k in 1:length(sub_id))
    {
      sub_id[[k]] = paste(CODE_URL[1], substring(con_list[i,j],regexpr(text =con_list[i,j],pattern = '/contest/[0-9]+$')[[1]]+9), CODE_URL[2], sub_id[[k]], sep='', collapse = '')
    }
    temp_list = list(temp_list, t(sub_id))
  }
  id_list = rbind.fill(id_list, data.frame(t(unlist(temp_list))))
  print(i)
}

dir.create('D_CODE')
i=1
for(i in i:nrow(id_list))
{
  dname = paste('./D_CODE/',member_data[i,1],sep='',collapse='')
  dir.create(dname)
  row_code = list()
  for(j in 1:ncol(id_list))
  {
    re = regexpr(text =id_list[i,j],pattern = '/submission/[0-9]+$')[[1]]
    if(!url.exists(as.character(id_list[i,j])) || re==-1) next
    temphtml = URL_ERROR_HANDLE(id_list[i,j],5)
    tempxml  = htmlParse(temphtml, encoding='UTF-8', asText=TRUE)
    content_src = xpathSApply(tempxml, SRC_XPATH, sessionEncoding='UTF-8',xmlValue)
    write(content_src,paste(dname,'/',substring(id_list[i,j],re+12),'.txt',sep=''))
  }
  print(i)
}

#Create Word Cloud
text = Corpus(DirSource('./D_CODE/',recursive = TRUE), list(language=NA))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeNumbers)
mixseg = worker()
mat <- matrix( unlist(text), nrow=length(text) )
totalSegment = data.frame()
for( j in 1:length(text) )
{
  for( i in 1:length(mat[j,]) )
  {
    result = segment(as.character(mat[j,i]), jiebar=mixseg)
  }
  totalSegment = rbind(totalSegment, data.frame(result))
  print(j)
}

names(totalSegment) = c("result")
countMat = count(totalSegment)
names(countMat) = c("totaldiff", "freq")
print(countMat)
totaldiff = sum(countMat[,2])
countMat[,2] = (countMat[,2] / sum(countMat[,2]))
countMat = countMat[order(countMat$freq),]
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud.png", width=1280,height=800)
DISCARD_NUM = 7
INDEX = DISCARD_NUM:(nrow(countMat)-DISCARD_NUM)
wordcloud(countMat$totaldiff[INDEX], countMat$freq[INDEX], random.order = F,
          colors = pal2, vfont=c("sans serif","plain"), scale=c(7.2,1),min.freq = 3,max.words = Inf, rot.per = .15)
dev.off()
saveRDS(member_data,"member.RDS")
saveRDS(con_list,"con_list.RDS")
saveRDS(id_list,"id_list.RDS")