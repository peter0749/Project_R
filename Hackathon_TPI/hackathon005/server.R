
shinyServer(function(input, output) {
  
  CostMap <- eventReactive(input$goButton1, {
    index = input$ZIPCODE==cordData[,2]
    input_center_lon = cordData[index,3][1]
    input_center_lat = cordData[index,4][1]
    
    selectData = regData[regData[,3]==input$checkType,]
    postLevs = get_Levs(selectData[,1])
    totalCost = get_Cost(selectData,postLevs,cordData)
    map <- get_map(location = c(input_center_lon,input_center_lat), zoom = input$center_zoom, maptype = "roadmap", language = "zh-TW")
    return(list(map,totalCost))
  })
  
  output$scatterMap <- renderPlot({
    ggmap(CostMap()[[1]]) + geom_point(aes(x = lon, y = lat, size = COST), data = CostMap()[[2]], colour = "red", alpha=0.6)
  }, height = 600, width = 800)
  
  output$denMap <- renderPlot({
    ggmap(CostMap()[[1]]) + stat_density2d(
      aes(x = lon, y = lat),
      data = CostMap()[[2]]
    )
  }, height = 600, width = 800)
  
  output$pie_message <- renderUI({
    HTML(
      paste(
        "Note:",
        "\u4f4e\u58d3\u96fb\u529b: \u5c0f\u578b\u88fd\u9020\u696d",
        "\u9ad8\uff06\u7279\u9ad8\u58d3\u96fb\u529b: \u5de5\u696d",
        "\u8868\u71c8\u975e\u71df\u696d\u7528: \u6c11\u751f\u7528\u96fb",
        "\u81e8\u6642\u7528\u96fb: \u81e8\u6642\u7528\u96fb",
        "\u8868\u71c8\u71df\u696d\u7528: \u5546\u5bb6\u7528\u96fb",
        "\u4f4e\u58d3\u7d9c\u5408\u975e\u71df\u696d\u7528\u96fb: \u516c\u5171\u8a2d\u65bd",
        "\u4f4e\u58d3\u7d9c\u5408\u71df\u696d\u7528\u96fb: \u5546\u5bb6\u7528\u96fb",
        sep="<br/>"
      )
    )
  })
  
  output$future <- renderPlot({
    data = cbind(future_pred, future_pred[,2]-future_pred[,3])
    data[,4] = scale(data[,4])
    ggplot(data, aes(x=data[,1],y=data[,4])) + geom_bar(stat='identity',position='dodge') + xlab("Date") + ylab("Variance(scaled)")
    
  })
  
  output$checkType_pie <- renderPlot({
    pieData = data.frame(
      sum(regData[grepl("13\u4f4e\u58d3\u96fb\u529b\u5c0f\u8a08",regData[,3]),6]),
      sum(regData[grepl("16\u9ad8\uff06\u7279\u9ad8\u58d3\u96fb\u529b\u5c0f\u8a08",regData[,3]),6]),
      sum(regData[grepl("1\u8868\u71c8\u975e\u71df\u696d\u7528",regData[,3]),6]),
      sum(regData[grepl("22\u81e8\u6642\u7528\u96fb\u5c0f\u8a08",regData[,3]),6]),
      sum(regData[grepl("2\u8868\u71c8\u71df\u696d\u7528",regData[,3]),6]),
      sum(regData[grepl("6\u4f4e\u58d3\u7d9c\u5408\u975e\u71df\u696d\u7528\u96fb\u5c0f\u8a08",regData[,3]),6]),
      sum(regData[grepl("9\u4f4e\u58d3\u7d9c\u5408\u71df\u696d\u7528\u96fb\u5c0f\u8a08",regData[,3]),6])
    )
    names(pieData) = c(
      "\u5c0f\u578b\u88fd\u9020\u696d",
      "\u5de5\u696d",
      "\u6c11\u751f\u7528\u96fb",
      "\u81e8\u6642\u7528\u96fb",
      "\u5546\u5bb6\u7528\u96fb",
      "\u516c\u5171\u8a2d\u65bd",
      "\u5546\u5bb6\u7528\u96fb"
    )
    pie3D(t(pieData), labels=names(pieData), explode=0.05, main="\u7528\u96fb\u6bd4\u4f8b\u5713\u9905\u5716",
          labelcex=1.2,col=c("blue","red","yellow","lightgray","darkgrey","black","green")
    )
  })
  
  ##Merge from Sam
  get_ctabel <- eventReactive(input$goButton2,{
    ctabel = ctabel_preserve
    index = ( grepl(paste(input$Area,collapse="|",sep=""), ctabel[,2]) )
    #ctabel = ctabel[index,]
    sctabel = ctabel[index,]
    x<-names(ctabel)
    
    y<-x[1]  # names\u5f9e\u4e00\u958b\u59cb
    column_count <- length(ctabel) # \u8a08\u7b97 column \u8207 count \u500b\u6578\u3002
    row_count<-dim(ctabel)  #\u8a08\u7b97\u591a\u5c11\u7b46
    Total_record<-row_count[1]
    
    real_Record<-as.numeric(Total_record)
    # \u6e2c\u8a66\u8cc7\u6599\u75282015-06-01  aa<-ctabel[1:real_aab,2]  # [\u7b46\u6578,\u6b04\u4f4d],4\u8868\u793a\u5206\u56db\u985e
    
    return(sctabel)
    
  })
  
  # output$address <- renderDataTable({
  #   
  #   
  # })
  
  output$address <- renderTable({
    #data.frame(ctabel)
    temp = get_ctabel()[,1:3]
    names(temp) = c("\u55ae\u4f4d","\u5730\u5740","\u96fb\u8a71")
    rownames(temp) = c(1:nrow(temp))
    temp
  })
  
  output$Servicelocal <- renderPlot({
    #uv <- read.csv("001.csv")
    # map<-map + geom_point(aes(x = lon, y = lat),data=ctabel)
    dmap = get_map(location = 'Taiwan', zoom = 8, language="zh-TW", maptype="roadmap")
    ggmap(dmap) + geom_point(aes(x = lon, y = lat),data=get_ctabel(), colour="red", alpha="0.6")
    
  })
  
  
  #Tab 2
  output$ManPower <- renderPlot({
    
    Gov_Man<-c(4478,5416,2627,287,4,0)
    Normal_Man<-c(458,3598,3778,5610,392,108)
    databar=rbind(Gov_Man,Normal_Man)
    par(family=('Heiti TC Light'))
    barplot(databar,names.arg = c("\u78a9\u58eb","\u5927\u5b78","\u5c08\u79d1","\u9ad8\u4e2d","\u4e2d\u5b78","\u5c0f\u5b78"),beside = TRUE,
            main="\u53f0\u96fb\u4eba\u54e1\u5b78\u6b77\u7d71\u8a08\u8868",xlab = "\u7d05\u8272\u8868\u793a\u516c\u52d9\u4eba\u54e1/\u85cd\u8272\u50f1\u54e1",ylab="\u4eba\u6578",col=c("red","blue"))
    legend(x="topleft",rownames(ManPower),fill=c("red","blue"))
    
  })
  
  output$ManPower3D <- renderPlot({
    ManTotal<-c(4936,9014,6405,5897,396,108)
    par(family=('Heiti TC Light'))
    pie3D(ManTotal,labels=c("\u78a9\u58eb","\u5927\u5b78","\u5c08\u79d1","\u9ad8\u4e2d","\u4e2d\u5b78","\u5c0f\u5b78"),explode=0.05,
          main="3D \u53f0\u96fb\u4eba\u54e1\u5b78\u6b77\u6bd4\u4f8b\u5716",labelcex=0.8,col=c("blue","red","yellow","lightgray","darkgrey","black"))
    
  })
  
  #Tab 3
  
  output$DistMRT <- renderPlot({
    A_select <- input$Area1
    tarr <- target
    tarr3 <- target[target$Area==A_select,]
    if (A_select==0){tarr3 <- target}
    
    NearCrit <- input$Distnce
    tarr3$DistNear <- ifelse(tarr3$distMRT<NearCrit,TRUE,FALSE)
    TT <- t.test(MetPr~DistNear,tarr3, paired=FALSE)
    box <- ggplot(data=tarr3 ,aes(x=DistNear , y=MetPr,fill = DistNear)) 
    box <- box + geom_boxplot(outlier.size = 2, outlier.shape = 1,outlier.colour = "gray",position = "dodge",colour = "#666666",width = 0.5)
    box <- box + labs(x = "Near MRT or Not", y = "Per Ping price (10K)", title = paste0(ifelse(A_select==0,"Taipei",AreaList[as.numeric(A_select)])," Near MRT vs. Far: 1033 to 1053"))
    box
  })
  ##End merge
  
})
