
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyUI(navbarPage(
  "\u53f0\u7063104\u5e74~105\u5e74\u7528\u96fb\u6982\u6cc1",
  tabPanel("\u5730\u5340\u7528\u96fb\u6982\u6cc1\u5716",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("checkType", label = h3("\u9078\u53d6\u6b32\u8a08\u7b97\u985e\u578b"), 
                                  choices = (ch_type=chifun(regTypeLev)),
                                  selected = ch_type[c(1,2,4)]
               ),
               selectInput("ZIPCODE", label = h3("\u9078\u64c7\u5340\u57df"), 
                           choices = (ch_zip = find_ZIPCODE(cordData)), 
                           selected = ch_zip[1]),
               sliderInput("center_zoom",
                           "\u653e\u5927\u500d\u7387",
                           min = 3,
                           max = 21,
                           value = 7
               )
             ),
             mainPanel(
               plotOutput("scatterMap", width = "100%", height = 650),
               plotOutput("denMap", width = "100%", height = 650)
             )
           )
  ),
  tabPanel(
    "\u7528\u96fb\u7a2e\u985e\u6bd4\u4f8b",
      mainPanel(
        plotOutput("checkType_pie",width="100%"),
        htmlOutput("pie_message")
      )
  ),
  tabPanel(
    "\u672a\u4f86\u4e00\u5468\u88ab\u8f09\u91cf\u6a19\u6e96\u5316",
    mainPanel(
      plotOutput("future",width="100%")
    )
  )
)
)