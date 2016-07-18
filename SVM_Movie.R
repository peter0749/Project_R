rm(list=ls(all=TRUE))
library('e1071')
library(plyr)
tolevel <- function(X)
{
  X = as.character(X)
  X[is.na(X)] = "NULL"
  testlevel = count(X)[1]
  testlevel$x = as.character(testlevel$x)
  for(i in 1:length(testlevel$x))
  {
    X[X==testlevel$x[i]] = as.character(i)
  }
  X = as.numeric(X)
  return(X)
}

AAPL = read.csv('Youtubelist.csv')
AAPL = AAPL[60:13820,-c(1,2,3,10)]#Range to process
AAPL = AAPL[sample(nrow(AAPL),5000),]#Sampling

#rownames(AAPL) <- seq(length=nrow(AAPL))
AAPL$Genre = tolevel(AAPL$Genre)
AAPL$Distrubutor = tolevel(AAPL$Distrubutor)
AAPL$Release.Date = tolevel(AAPL$Release.Date)
AAPL$MPAA = tolevel(AAPL$MPAA)
AAPL$MPAA = as.numeric(AAPL$MPAA)
AAPL$Box[is.na(AAPL$Box)] = 0
AAPL$Runtime[is.na(AAPL$Runtime)] = mean(AAPL$Runtime[!is.na(AAPL$Runtime)])
AAPL$Youtube.Views[is.na(AAPL$Youtube.Views)] = 0
#AAPL$Budget[is.na(AAPL$Budget)] = mean(AAPL$Budget[!is.na(AAPL$Budget)])
#AAPL$Youtube.Views = scale(AAPL$Youtube.Views)

AAPL = as.data.frame(AAPL)


#AAPL[,2:5] = log(AAPL[,2:5])
np = ceiling(0.05 * nrow(AAPL))

Y = AAPL[, 1]
#return = AAPL[2:nrow(AAPL), 5] - AAPL[1:(nrow(AAPL)-1), 5]
#return = append(return, 0, after=0)
#label = ifelse(return > 400000000, 1, 0)

#AAPL = cbind(AAPL, label)
AAPL = cbind(AAPL, Y)

test.index = sample(1:nrow(AAPL), np)
#test.index = 1:np

AAPL.test = AAPL[test.index, ]
AAPL.train = AAPL[-test.index, ]

tuned = tune.svm(Y ~ ., data = AAPL.train, gamma = 10^(-3:-1), cost = 10^(-1:1))
summary(tuned)
svm.model = svm(Y ~ ., data = AAPL.train, kernal='radial', type = 'nu-regression', cost = tuned[[1]]$cost, gamma = tuned[[1]]$gamma)
svm.pred = predict(svm.model, AAPL.test[, -8])
table.svm.test = table(pred = svm.pred, true = AAPL.test[, 8])
correct.svm = sum(diag(table.svm.test) / sum(table.svm.test)) * 100
result = cbind(AAPL.test, svm.pred)