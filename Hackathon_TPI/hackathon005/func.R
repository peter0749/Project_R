
chifun <- function(X)
{
  temp = t(count(X)[1])
  testlist = as.list(temp)
  names(testlist) = gsub("[0-9]","",temp)
  testlist = testlist[order(names(testlist))]
  return(testlist)
}

find_ZIPCODE <- function(cordMAP)
{
  temp = as.list(cordMAP[,2])
  names(temp) = cordMAP[,1]
  temp = temp[order(names(temp))]
  return(temp)
}

#Import cordinary data
import_cordData <- function()
{
  cordData = read.table("1050429BIG5.TXT", header=T, sep="\t", fileEncoding = "big5")
  cordData = cordData[,-5]
  names(cordData) = c("Reg","Post","cN","cE")
  return(cordData)
}
#End of import

#Import main data
import_regData <- function()
{
  main_Data_dir = "PowerData"
  mainData = data.frame()
  
  from_y = 104
  to_y = 105
  from_m = 1
  to_m = 12
  
  for(i in from_y:to_y)
  {
    idy = sprintf("%03d", i)
    for(j in from_m:to_m)
    {
      idm = sprintf("%02d", j)
      f_Path = paste(main_Data_dir,"/test-",idy,idm,".txt",sep="")
      if(file.exists(f_Path))
      {
        temp = data.frame(read.table(f_Path, header=T, sep="\t", fileEncoding = "big5"))
        temp = temp[-nrow(temp),1:6]
        mainData = rbind(mainData, temp)
      }
    }
  }
  mainData = mainData[order(mainData[,1]),]
  mainData = mainData[!is.na(mainData[,6]),]
  mainData = mainData[nchar(as.character(mainData[,1]))==3,]
  mainData[,3] = gsub("\t|\u3000| ","", mainData[,3])
  return(mainData)
}
#End of import

get_Levs <- function(X)
{
  return(count(X)[1])
}

get_Cost <- function(mainData, postLevs, cordMap)
{
  totalCost = data.frame()
  for(i in 1:nrow(postLevs))
  {
    temp = data.frame(postLevs[i,],sum(mainData[mainData[,1]==postLevs[i,],6]))
    totalCost = rbind(totalCost, temp)
  }
  totalCost = cbind(totalCost, data.frame(rep(0,nrow(totalCost)), rep(0,nrow(totalCost))))
  names(totalCost) = c("ZIPCODE", "COST", "lon", "lat")
  
  for(i in 1:nrow(totalCost))
  {
    testindex = cordMap[,2]==totalCost[i,1]
    totalCost[i,3] = cordMap[testindex,3][1]
    totalCost[i,4] = cordMap[testindex,4][1]
  }
  totalCost = totalCost[!is.na(totalCost$lon)&!is.na(totalCost$lat)&!is.na(totalCost$COST),]
  return(totalCost)
}
#End of import