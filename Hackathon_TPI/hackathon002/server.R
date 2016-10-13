
shinyServer(function(input, output) {
  
  CostMap <- reactive({
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
  
})
