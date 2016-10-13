#rm(list=ls(all=TRUE))
library('e1071')
library(plyr)
library(rgl)
library(scatterplot3d)

AAPL = read.csv('Youtubelist.csv')
AAPL = AAPL[,-c(1,2,3,10)]#Range to process

for(i in 1:7)
  AAPL = AAPL[!is.na(AAPL[,i]),]

#AAPL = AAPL[sample(nrow(AAPL),3000),]#Sampling
#AAPL = AAPL[order(AAPL$Box),]

AAPL$Youtube.Views = scale(AAPL$Youtube.Views)
#AAPL$Box = scale(AAPL$Box)
#AAPL$Budget[is.na(AAPL$Budget)] = mean(AAPL$Budget[!is.na(AAPL$Budget)])
#AAPL$Youtube.Views = scale(AAPL$Youtube.Views)
#plot(AAPL$Box)

AAPL = as.data.frame(AAPL)

np = ceiling(0.2 * nrow(AAPL))

#Y = ifelse(AAPL$Box>40000000,1,0)

AAPL$Y = c(rep(0,length(AAPL$Box)))
#ranklist = c(-1,1e7L,5e7L,1e7L,2e8L)
ranklist = c(-1,7e6L,3e7L,7e7L,1e8L)
ranklist = as.numeric(ranklist)
for(i in 2:length(ranklist))
{
  AAPL$Y[AAPL$Box<=ranklist[i] & AAPL$Box>ranklist[i-1]] = i-1
}
AAPL$Y[AAPL$Box>ranklist[length(ranklist)]] = length(ranklist)

AAPL = AAPL[,-1]

test.index = sample(1:nrow(AAPL), np)
#test.index = 1:np

AAPL.test = AAPL[test.index, ]
AAPL.train = AAPL[-test.index, ]

tuned = tune.svm(Y ~ ., data = AAPL.train, gamma = 2^(-7:-5), cost = 2^(2:4))
#summary(tuned)
if(nrow(count(AAPL.train$Y))>1)
{
  svm.model = svm(Y ~ ., data = AAPL.train, kernal='radial', type = 'C-classification', cost = 16, gamma = 0.03125)
  #svm.model = readRDS("movie.svmodel")
  svm.pred = predict(svm.model, AAPL.test[, -7])
  table.svm.test = table(pred = svm.pred, true = AAPL.test[, 7])
  correct.svm = sum(diag(table.svm.test) / sum(table.svm.test)) * 100
  result = cbind(AAPL.test, svm.pred)
  
  result = result[order(result$Y),]#Sorting
  plot(result$svm.pred, result$Y)
  for(i in 1:5)
  {
    plot(table(result$Genre[result$Y==i]),col='green', xlab="Genre", ylab="Number", main="Pred Box(red) vs Box(green)")
    points(table(result$Genre[result$svm.pred==i]),col='red')
  }
  for(i in 1:5)
  {
    plot(table(result$MPAA[result$Y==i]),col='green', xlab="MPAA", ylab="Number", main="Pred Box(red) vs Box(green)")
    points(table(result$MPAA[result$svm.pred==i]),col='red')
  }
  plot3d(result$Genre, result$MPAA, result$Y, main="3D scatterplot", pch=16, highlight.3d = TRUE, type="h", col=c('red','green','blue'))
  plot3d(result$Genre, result$MPAA[result$MPAA=="PG-13"], result$Y, main="3D scatterplot", pch=16, highlight.3d = TRUE, type="h", col=c('red','green','blue'))
  
}else{
  print("Assert: There is only one label.")
}
