# ISOTOPES

library(shiny)

ui = (fluidPage(
  theme = shinytheme("united"),
  
  titlePanel("Stable Isotopes"),
  h5("Use these tools for basic calculations for stable isotopes"),
  br(),
  navlistPanel(
    # 1. convert R to delta --------------------------------------------------------------
    "convert R to δ",
    tabPanel("13C-carbon",
             h3("input enrichment"),
             textInput("enrich_c", " ", value = "0.10"),
             br(),
             h3("δ-13C"),
             textOutput("delta_c")),
    tabPanel("15N-nitrogen",
             h3("input enrichment"),
             textInput("enrich_n", " ", value = "0.10"),
             br(),
             h3("δ-15N"),
             textOutput("delta_n")),
    tabPanel("calculations",
             "atom percent 13C = [13C/(12C + 13C)]100",
             br(),
             br(),
             "fractional abundance of 13C ≡ 13F 13F = 13C/(12C + 13C)",
             br(),
             br(),
             "carbon isotope ratio” = 13C/12C ≡ 13R",
             br(),
             br(),
             "13F = 13R/(1 + 13R)",
             br(),
             br(),
             "δ = (R-sample/R-standard - 1) * 1000"),
    
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
      round((mass1()*enrich1() + mass2()*enrich2())/ (mass1()+mass2()), 2)
    })
  }
  
  