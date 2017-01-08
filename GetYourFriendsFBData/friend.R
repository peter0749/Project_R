rm(list=ls(all=TRUE))
library(Rfacebook)

parser_id <- function(X) {
  s = substring(X,first=regexpr('id=',X)+3)
  s = substring(s,first=1,last=regexpr('&',s)-1)
  return(s)
}

download_all_pic <- function(X) {
  if(!is.na(X[,11])) {
    print(X[,11])
    try(curl::curl_download(X[,11],paste('./FaceImg/', X[,1],'-', X[,2], '.jpg', sep='', collapse='')),silent=TRUE)
  }
}

if(!dir.exists('./FaceImg')){
  dir.create('./FaceImg')
}

token = ''
friend_ajax = read.table('friend_list.txt',header=FALSE)
friend_ids = sapply(friend_ajax, parser_id)
friend_status = data.frame()
for(i in 1:nrow(friend_ids)) {
  sig <- try((temp = getUsers(friend_ids[i],token, private_info = TRUE)), silent=FALSE)
  if(class(sig)=='try-error') next
  friend_status = rbind(friend_status, temp)
  download_all_pic(temp)
}
write.csv(x = friend_status, file = 'myfriends.csv', fileEncoding = 'UTF-8')
