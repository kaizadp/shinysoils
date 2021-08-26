# ISOTOPES

library(shiny)


# ui/server ---------------------------------------------------------------


ui = (fluidPage(
  theme = shinytheme("united"),
  
  titlePanel("Stable Isotopes"),
  h5("Use these tools for basic calculations for stable isotopes"),
  br(),
  navlistPanel(
    # 1. convert R to delta --------------------------------------------------------------
    "convert R to δ",
    tabPanel("13C-carbon",
             h3("input enrichment (R)"),
             textInput("enrich_c", " ", value = "0.10"),
             br(),
             h3("&delta;-13C"),
             textOutput("delta_c")),
    tabPanel("15N-nitrogen",
             h3("input enrichment (R)"),
             textInput("enrich_n", " ", value = "0.10"),
             br(),
             h3("δ-15N"),
             textOutput("delta_n")),
    tabPanel("calculations",
             h3("different notations for stable isotopes"),
             br(),
             h4("fractional abundance of 13C (13F)"),
             "F = 13C/(12C + 13C)",
             br(),
             br(),
             h4("atom percent of 13C (at%)"),
             "at% = [13C/(12C + 13C)] * 100",
             br(),
             br(),
             h4("carbon isotope ratio (R)"),
             "R = 13C/12C",
             br(),
             br(),
             h4("converting units"),
             "13F = 13R/(1 + 13R)",
             br(),
             "δ = (R-sample/R-standard - 1) * 1000",
             br(),
             br(),
             h4("to calculate δ value of a mixture"),
             "Mass-mixture * R-mixture = M1 * R1 + M2 * R2"),    
    
    # 2. target enrichment ----------------------------------------------------------------------
    "labelled mixtures",
    tabPanel("enrichment of a mixture",
             h5("Enter enrichment/mass for each compound to calculate the enrichment of the mixture"),
             sidebarLayout(position = "left",
                           sidebarPanel(
                             h3("compound 1"),
                             textInput("R1", "enrichment (R1)", value = "0.010"),
                             textInput("M1", "mass (M1)", value = "100"),
                             
                             h3("compound 2"),
                             textInput("R2", "enrichment (R2)", value = "0.99"),
                             textInput("M2", "mass (M2)", value = "1")
                           ),
                           
                           mainPanel(
                             h2("mass (M)"),
                             textOutput("mass_total"),
                             h2("enrichment (R)"),
                             textOutput("enrich_total")
                           )
             ))
  )))
                           
  
  # server ----
  server = function(input, output) {
    # 1. R to delta --------------------------------------------------------------
    ## inputs ----
    
    Cenrich <- reactive({
      wtInput <- as.numeric(input$enrich_c)
      wtInput
    })
    
    Nenrich <- reactive({
      wtInput <- as.numeric(input$enrich_n)
      wtInput
    })
    
    
    ## outputs ----
    
    output$delta_c <- renderText({
      ## REFERENCE VALUES FOR STANDARDIZATION
      C_VPDB = 0.011237 # 13C/12C ratio
      N_AIR = 0.003676 # 15N/14N ratio
      ((Cenrich()/C_VPDB) - 1) * 1000
    })
    
    output$delta_n <- renderText({
      ## REFERENCE VALUES FOR STANDARDIZATION
      C_VPDB = 0.011237 # 13C/12C ratio
      N_AIR = 0.003676 # 15N/14N ratio
      ((Nenrich()/N_AIR) - 1) * 1000
    })
    
    # 2. ----------------------------------------------------------------------
    ## inputs ----
    
    mass1 <- reactive({
      wtInput <- as.numeric(input$M1)
      wtInput
    })
    
    mass2 <- reactive({
      wtInput <- as.numeric(input$M2)
      wtInput
    })
    
    enrich1 <- reactive({
      wtInput <- as.numeric(input$R1)
      wtInput
    })
    
    enrich2 <- reactive({
      wtInput <- as.numeric(input$R2)
      wtInput
    })
    
    ## outputs ----
    
    output$mass_total <- renderText({
      round(mass1()+mass2(),2)
    })
    
    output$enrich_total <- renderText({
      round((mass1()*enrich1() + mass2()*enrich2())/ (mass1()+mass2()), 4)
    })
  }
  

# functions ---------------------------------------------------------------

carbon_from_r = function(){

  ui = (fluidPage(
    #theme = shinytheme("united"),
    br(),
    ("input isotope ratio (R)"), 
    textInput("enrich_c", " ", value = "0.10"),
    br(),
    h5(("fractional abundance-13C")), 
    textOutput("fraction_c"),
    br(),
    h5(("delta-13C")), 
    textOutput("delta_c"),
  ))
  
  # server ----
  server = function(input, output) {
    ## inputs
    Cenrich <- reactive({
      wtInput <- as.numeric(input$enrich_c)
      wtInput
    })
  
    ## outputs
    output$fraction_c <- renderText({
      Cenrich()/(1 + Cenrich())
    })
    
    output$delta_c <- renderText({
      ## REFERENCE VALUES FOR STANDARDIZATION
      C_VPDB = 0.011237 # 13C/12C ratio
      N_AIR = 0.003676 # 15N/14N ratio
      ((Cenrich()/C_VPDB) - 1) * 1000
    })
  }
  
  list(ui = ui,
       server = server)
  
  
}

carbon_from_f = function(){
  
  ui = (fluidPage(
    #theme = shinytheme("united"),
    br(),
    ("input fractional abundance (F)"), 
    textInput("fraction_c", " ", value = "0.0909"),
    br(),
    h5(("isotope ratio, R")), 
    textOutput("ratio_c"),
    br(),
    h5(("delta-13C")), 
    textOutput("delta_c"),
  ))
  
  # server ----
  server = function(input, output) {
    ## inputs
    Cenrich <- reactive({
      wtInput <- as.numeric(input$fraction_c)
      wtInput
    })
    
    ## outputs
    output$ratio_c <- renderText({
      Cenrich()/(1 - Cenrich())
    })
    
    output$delta_c <- renderText({
      ## REFERENCE VALUES FOR STANDARDIZATION
      C_VPDB = 0.011237 # 13C/12C ratio
      N_AIR = 0.003676 # 15N/14N ratio
      C_ratio = Cenrich()/(1 - Cenrich())
      ((C_ratio/C_VPDB) - 1) * 1000
    })
  }
  
  list(ui = ui,
       server = server)
  
  
}

carbon_from_d = function(){
  
  ui = (fluidPage(
    #theme = shinytheme("united"),
    br(),
    ("input delta"), 
    textInput("delta_c", " ", value = "50"),
    br(),
    h5(("isotope ratio, R")), 
    textOutput("ratio_c"),
    br(),
    h5(("fractional abundance, F")), 
    textOutput("fraction_c"),
  ))
  
  # server ----
  server = function(input, output) {
    ## inputs
    Cenrich <- reactive({
      wtInput <- as.numeric(input$delta_c)
      wtInput
    })
    
    ## outputs
    output$ratio_c <- renderText({
      C_VPDB = 0.011237
      ((Cenrich()/1000) + 1) * C_VPDB
    })
    
    output$fraction_c <- renderText({
      ## REFERENCE VALUES FOR STANDARDIZATION
      C_VPDB = 0.011237 # 13C/12C ratio
      C_ratio = ((Cenrich()/1000) + 1) * C_VPDB
      C_ratio/(1 + C_ratio)
    })
  }
  
  list(ui = ui,
       server = server)
  
  
}

##

nitrogen_from_r = function(){
  
  ui = (fluidPage(
    #theme = shinytheme("united"),
    br(),
    ("input isotope ratio (R)"), 
    textInput("enrich_n", " ", value = "0.10"),
    br(),
    h5(("fractional abundance, F")), 
    textOutput("fraction_n"),
    br(),
    h5(("delta-15N")), 
    textOutput("delta_n"),
  ))
  
  # server ----
  server = function(input, output) {
    ## inputs
    Nenrich <- reactive({
      wtInput <- as.numeric(input$enrich_n)
      wtInput
    })
    
    ## outputs
    output$fraction_n <- renderText({
      Nenrich()/(1 + Nenrich())
    })
    
    output$delta_n <- renderText({
      ## REFERENCE VALUES FOR STANDARDIZATION
      N_AIR = 0.003676 # 15N/14N ratio
      ((Nenrich()/N_AIR) - 1) * 1000
    })
  }
  
  list(ui = ui,
       server = server)
  
  
}

nitrogen_from_f = function(){
  
  ui = (fluidPage(
    #theme = shinytheme("united"),
    br(),
    ("input fractional abundance (F)"), 
    textInput("fraction_n", " ", value = "0.0909"),
    br(),
    h5(("isotope ratio, R")), 
    textOutput("ratio_n"),
    br(),
    h5(("delta-15N")), 
    textOutput("delta_n"),
  ))
  
  # server ----
  server = function(input, output) {
    ## inputs
    Nenrich <- reactive({
      wtInput <- as.numeric(input$fraction_n)
      wtInput
    })
    
    ## outputs
    output$ratio_n <- renderText({
      Nenrich()/(1 - Nenrich())
    })
    
    output$delta_n <- renderText({
      ## REFERENCE VALUES FOR STANDARDIZATION
      N_AIR = 0.003676 # 15N/14N ratio
      N_ratio = Nenrich()/(1 - Nenrich())
      ((N_ratio/N_AIR) - 1) * 1000
    })
  }
  
  list(ui = ui,
       server = server)
  
  
}

nitrogen_from_d = function(){
  
  ui = (fluidPage(
    #theme = shinytheme("united"),
    br(),
    ("input delta"), 
    textInput("delta_n", " ", value = "50"),
    br(),
    h5(("isotope ratio, R")), 
    textOutput("ratio_n"),
    br(),
    h5(("fractional abundance, F")), 
    textOutput("fraction_n"),
  ))
  
  # server ----
  server = function(input, output) {
    ## inputs
    Nenrich <- reactive({
      wtInput <- as.numeric(input$delta_n)
      wtInput
    })
    
    ## outputs
    output$ratio_n <- renderText({
      N_AIR = 0.003676 # 15N/14N ratio
      ((Nenrich()/1000) + 1) * N_AIR
    })
    
    output$fraction_n <- renderText({
      ## REFERENCE VALUES FOR STANDARDIZATION
      N_AIR = 0.003676 # 15N/14N ratio
      N_ratio = ((Nenrich()/1000) + 1) * N_AIR
      N_ratio/(1 + N_ratio)
    })
  }
  
  list(ui = ui,
       server = server)
  
  
}

##

mixing_model = function(){
  ui = (fluidPage(
    theme = shinytheme("united"),
    br(),
    sidebarLayout(position = "left",
                  sidebarPanel(
                    h3("compound 1"),
                    textInput("R1", "enrichment", value = "0.010"),
                    textInput("M1", "mass (M1)", value = "100"),
                    
                    h3("compound 2"),
                    textInput("R2", "enrichment", value = "0.99"),
                    textInput("M2", "mass (M2)", value = "1")
                    ),
                    
                  mainPanel(
                    h3("mixture"),
                    br(),
                    h4("enrichment"),
                    textOutput("enrich_total"),
                    br(),
                    h4("mass (M)"),
                    textOutput("mass_total")
                  ))))
  
  # server
  server = function(input, output) {
    ## inputs ----
    
    mass1 <- reactive({
      wtInput <- as.numeric(input$M1)
      wtInput
    })
    
    mass2 <- reactive({
      wtInput <- as.numeric(input$M2)
      wtInput
    })
    
    enrich1 <- reactive({
      wtInput <- as.numeric(input$R1)
      wtInput
    })
    
    enrich2 <- reactive({
      wtInput <- as.numeric(input$R2)
      wtInput
    })
    
    ## outputs ----
    
    output$mass_total <- renderText({
      round(mass1()+mass2(),2)
    })
    
    output$enrich_total <- renderText({
      round((mass1()*enrich1() + mass2()*enrich2())/ (mass1()+mass2()), 4)
    })
  }
 
  list(ui = ui,
       server = server) 
}






