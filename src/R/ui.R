library(shiny)

shinyUI(fluidPage(
  titlePanel("Anomaly Detective!"),
  sidebarLayout(
    sidebarPanel(
      numericInput("Cond", "Enter Concept ID", 312664),
      br(),
      br(),
      selectInput("DB", "Select Database", choices=c("JMDC" = "JMDC", "CPRD" = "CPRD",
                                                     "Optum" = "Optum", "Truven CCAE" = "Truven CCAE",
                                                     "Truven MDCD" = "Truven MDCD",
                                                     "Truven MDCR" = "Truven MDCR"),
                  multiple = F ),
      br(),
      br(),
      sliderInput("multse", "Multiplier of SE", min=1, max=5, value=2, step=0.25),
      br()
    ),
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Year against Typical Year",plotOutput("mainplot", height = 1200)),
                  tabPanel("Table Preview", verbatimTextOutput("table")),
                  tabPanel("Time Series Analysis", plotOutput("tsplot", height = 1200)),
                  tabPanel("Concept by Database",plotOutput("condplot", height = 500))
      )
      
    )
  )
))