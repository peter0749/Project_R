rm(list=ls(all=TRUE))
library(Rfacebook)

parser_id <- function(X) {
  s = substring(X,first=regexpr('id=',X)+3)
  s = substring(s,first=1,last=regexpr('&',s)-1)
  return(s)
}

build_all_query <- function(X, token='') {
  return(getUsers(X,token, private_info = TRUE))
}

download_all_pic <- function(X) {
  print(X[,11])
  curl::curl_download(X[,11],paste('./FaceImg/', X[,1],'-', X[,2], '.jpg', sep='', collapse=''))
}

if(!dir.exists('./FaceImg')){
  dir.create('./FaceImg')
}

#token = ''
friend_ajax = read.table('friend_list.txt',header=FALSE)
friend_ids = sapply(friend_ajax, parser_id)
friend_status = data.frame()
for(i in 1:nrow(friend_ids)) {
  friend_status = rbind(friend_status, build_all_query(friend_ids[i,],token))
  download_all_pic(friend_status[i,])
}
write.csv(x = friend_status, file = 'myfriends.csv', fileEncoding = 'UTF-8')
