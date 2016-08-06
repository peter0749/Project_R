
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
  
})
