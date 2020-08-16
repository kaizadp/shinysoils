### SOIL WATER SUCTION UNITS

library(shiny)
library(shinythemes)


ui = (fluidPage(
  theme = shinytheme("united"),
  #shinythemes::themeSelector(),
  
  titlePanel("Soil Water Suction"),
  h5("Use these tools to convert between units of soil water suction"),
  br(),
  navlistPanel(
    "converting units",
    tabPanel("converting from pF",
             h3("input pF"),
             textInput("in_pf", "", value = "0.66"),
             br(),
             h3("output cm"),
             textOutput("out_cm"),
             br(),
             h3("output kPa"),
             textOutput("out_kpa")
             ),
    
    tabPanel("converting from kPa",
             h3("input pF"),
             textInput("in_kpa", "", value = "100"),
             br(),
             h3("output cm"),
             textOutput("out_cm2"),
             br(),
             h3("output pF"),
             textOutput("out_pf")
             ),
    
    tabPanel("calculations",
             "pF = log10 (suction as cm)",
             br(),
             "cm = kPa/10"
             )
  )))

server = function(input, output) {
  
  conv1 <- reactive({
    wtInput <- as.numeric(input$in_pf)
    wtInput
  })
  
  output$out_cm <- renderText({
    round(10^conv1(),2)
  })
  output$out_kpa <- renderText({
    round((10^conv1())/10,2)
  })
  
  conv2 <- reactive({
    wtInput <- as.numeric(input$in_kpa)
    wtInput
  })
  
  output$out_cm2 <- renderText({
    round(conv2()*10,2)
  })
  output$out_pf <- renderText({
    round(log10(conv2()*10),2)
  })
}
