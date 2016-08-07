rm(list=ls(all=TRUE))
library(plyr)
library(ggplot2)
library(ggmap)
library(mapproj)
library(grid)
library(shiny)
library(bitops)
library(plotrix)
source('func.R')

ctabel_preserve = read.csv("001.csv",fileEncoding='big5')
ManPower <- read.csv("ManPower.csv")

chiTPS <- function(X)
{
  temp = substring(X,first=0,last=3)
  temp = t(count(temp)[1])
  testlist = as.list(temp)
  names(testlist) = temp
  testlist = testlist[order(names(testlist))]
  return(testlist)
}

download.file("http://data.taipower.com.tw/opendata/apply/file/d006007/%E5%8F%B0%E7%81%A3%E9%9B%BB%E5%8A%9B%E5%85%AC%E5%8F%B8_%E6%9C%AA%E4%BE%86%E4%B8%80%E9%80%B1%E9%9B%BB%E5%8A%9B%E4%BE%9B%E9%9C%80%E9%A0%90%E6%B8%AC.txt",destfile="future.txt")
future_pred = read.csv("future.txt", header=F)

cordData = import_cordData()
#saveRDS(cordData,"cordData.RDS")
#cordData = readRDS("cordData.RDS")
regData = import_regData()
#saveRDS(regData,"regData.RDS")
#regData = readRDS("regData.RDS")
regTypeLev = get_Levs(regData[,3])