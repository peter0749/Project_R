library(shiny)

tryNinstall <- function(PakName=''){
  if(require(PakName, character.only=T, quietly=T)){
    print(paste('Okay, ',PakName,' is installed.',sep='',collapse=''))
  }else{
    print(paste('Trying to install ',PakName,'...',sep='',collapse=''))
    install.packages(PakName)
  }
}
tryNinstall('e1071')
tryNinstall('plyr')
#library(rgl)
tryNinstall('scatterplot3d')
source("./svm_pred.R")

npval = 0.2

AAPL = LOAD_AAPL(np=npval)

chifun <- function(X)
{
  testlist = as.list(t(count(X)[1]))
  names(testlist) = t(count(X)[1])
  return(testlist)
}
