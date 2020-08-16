# ISOTOPES

library(shiny)

ui = (fluidPage(
  titlePanel("Stable Isotopes"),
  h5("Use these tools to for basic calculations for stable isotopes"),
  br(),
  # 1. R to delta --------------------------------------------------------------
  navlistPanel(
    "R to delta",
    tabPanel("C",
             h3("input enrichment"),
             textInput("enrich_c", " ", value = "0.10"),
             br(),
             h3("δ-13C"),
             textOutput("delta_c")),
    tabPanel("N",
             h3("input enrichment"),
             textInput("enrich_n", " ", value = "0.10"),
             br(),
             h3("δ-15N"),
             textOutput("delta_n")),
  
  # 2. target enrichment ----------------------------------------------------------------------
    "target enrichment",
    tabPanel("calc",
             h5("Use this tool to calculate the enrichment of a mixture"),
             sidebarLayout(position = "left",
                           sidebarPanel(
                             h3("compound 1"),
                             textInput("R1", "enrichment", value = "0.010"),
                             textInput("M1", "mass", value = "100"),
                             
                             h3("compound 2"),
                             textInput("R2", "enrichment", value = "0.99"),
                             textInput("M2", "mass", value = "1")
                           ),
                           
                           position = "right",
                           sidebarPanel(
                             h3("compound 1"),
                             textInput("R1", "enrichment", value = "0.010"),
                             textInput("M1", "mass", value = "100"),
                             
                             h3("compound 2"),
                             textInput("R2", "enrichment", value = "0.99"),
                             textInput("M2", "mass", value = "1")
                           ),
                           
                           mainPanel(
                             h2("mass"),
                             textOutput("mass_total"),
                             h2("enrichment"),
                             textOutput("enrich_total")
                           )
             )))))
                           
  
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
      round((mass1()*enrich1() + mass2()*enrich2())/ (mass1()+mass2()), 2)
    })
  }
  
  