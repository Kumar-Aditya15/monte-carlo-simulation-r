library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(triangle)
library(dplyr)

#############################
# CONSTANTS
#############################

REV0 <- 50
TAX <- 0.21
DA <- 0.04
CAPEX <- 0.08
NWC <- 0.03

RF <- 0.042
MRP <- 0.055
RD <- 0.07
WD <- 0.15
WE <- 0.85

NET_DEBT <- 15
SHARES <- 5

#############################
# UI
#############################

ui <- dashboardPage(
  
  dashboardHeader(title = "PROJECT VITALITY"),
  
  dashboardSidebar(
    
    sliderInput("runs","Monte Carlo Iterations",1000,50000,10000,step=1000),
    
    sliderInput("growth","Revenue Growth Mean",0.10,0.30,0.20),
    
    sliderInput("vol","Growth Volatility",0.01,0.10,0.05),
    
    sliderInput("beta","Equity Beta",1.2,1.7,1.45),
    
    sliderInput("pgr","Terminal Growth Rate",0.01,0.05,0.03),
    
    br(),
    
    actionButton("run","Run Simulation",width="100%")
    
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("

body{
background:#0b1320;
color:white;
}

.content-wrapper{
background:#0b1320;
}

.box{
background:#111827;
border-radius:8px;
border:none;
}

.main-header .logo{
background:#0b1320;
color:white;
}

.main-header .navbar{
background:#0b1320;
}

.sidebar{
background:#111827;
}

"))),
    
    fluidRow(
      
      valueBoxOutput("mean",3),
      valueBoxOutput("median",3),
      valueBoxOutput("range",3),
      valueBoxOutput("ev",3)
      
    ),
    
    tabBox(width=12,
           
           tabPanel("Distribution",
                    
                    fluidRow(
                      
                      box(width=6,
                          plotlyOutput("hist",height=350)
                      ),
                      
                      box(width=6,
                          plotlyOutput("cdf",height=350)
                      )
                      
                    )
                    
           ),
           
           tabPanel("Simulation Paths",
                    
                    box(width=12,
                        plotlyOutput("paths",height=400)
                    )
                    
           ),
           
           tabPanel("Valuation Bridge",
                    
                    box(width=12,
                        plotlyOutput("bridge",height=400)
                    )
                    
           ),
           
           tabPanel("Cash Flow",
                    
                    box(width=12,
                        DTOutput("table")
                    )
                    
           )
           
    )
    
  )
  
)

#############################
# SERVER
#############################

server <- function(input, output){
  
  sim <- eventReactive(input$run,{
    
    n <- input$runs
    
    growth <- rnorm(n,input$growth,input$vol)
    
    margin <- rtriangle(n,0.10,0.22,0.15)
    
    beta <- runif(n,1.2,1.7)
    
    pgr <- rnorm(n,input$pgr,0.006)
    
    ke <- RF + beta*MRP
    wacc <- WE*ke + WD*RD*(1-TAX)
    
    price <- numeric(n)
    ev <- numeric(n)
    
    for(i in 1:n){
      
      rev <- REV0
      pv <- 0
      
      for(t in 1:5){
        
        rev <- rev*(1+growth[i])
        
        ebit <- rev*margin[i]
        
        nopat <- ebit*(1-TAX)
        
        fcff <- nopat + rev*DA - rev*CAPEX - rev*NWC
        
        pv <- pv + fcff/(1+wacc[i])^t
        
      }
      
      tv <- fcff*(1+pgr[i])/(wacc[i]-pgr[i])
      
      pv_tv <- tv/(1+wacc[i])^5
      
      ev[i] <- pv + pv_tv
      
      eq <- ev[i] - NET_DEBT
      
      price[i] <- eq/SHARES
      
    }
    
    data.frame(price,ev)
    
  })
  
  #############################
  # KPI
  #############################
  
  output$mean <- renderValueBox({
    
    req(sim())
    
    valueBox(
      paste0("$",round(mean(sim()$price),2)),
      "Mean Share Price",
      color="blue")
    
  })
  
  output$median <- renderValueBox({
    
    req(sim())
    
    valueBox(
      paste0("$",round(median(sim()$price),2)),
      "Median Share Price",
      color="green")
    
  })
  
  output$range <- renderValueBox({
    
    req(sim())
    
    p10 <- quantile(sim()$price,0.10)
    p90 <- quantile(sim()$price,0.90)
    
    valueBox(
      paste0("$",round(p10,2)," - $",round(p90,2)),
      "P10 / P90 Range",
      color="orange")
    
  })
  
  output$ev <- renderValueBox({
    
    req(sim())
    
    valueBox(
      paste0("$",round(mean(sim()$ev),1),"M"),
      "Enterprise Value",
      color="purple")
    
  })
  
  #############################
  # HISTOGRAM
  #############################
  
  output$hist <- renderPlotly({
    
    req(sim())
    
    plot_ly(sim(),
            x = ~price,
            type = "histogram",
            marker = list(color = "#4f8dd6")
    ) %>%
      layout(
        plot_bgcolor="#0b1320",
        paper_bgcolor="#0b1320",
        font=list(color="white"),
        xaxis=list(title="Share Price"),
        yaxis=list(title="Frequency")
      )
    
  })
  
  #############################
  # CDF
  #############################
  
  output$cdf <- renderPlotly({
    
    req(sim())
    
    x <- sort(sim()$price)
    y <- seq_along(x)/length(x)
    
    plot_ly(
      x=x,
      y=y,
      type="scatter",
      mode="lines",
      line=list(color="#ff8c42",width=3)
    ) %>%
      layout(
        plot_bgcolor="#0b1320",
        paper_bgcolor="#0b1320",
        font=list(color="white"),
        xaxis=list(title="Price"),
        yaxis=list(title="Probability")
      )
    
  })
  
  #############################
  # PATHS
  #############################
  
  output$paths <- renderPlotly({
    
    paths <- matrix(0,5,30)
    
    for(i in 1:30){
      
      rev <- REV0
      
      for(t in 1:5){
        
        rev <- rev*(1+rnorm(1,0.20,0.05))
        
        paths[t,i] <- rev
        
      }
      
    }
    
    fig <- plot_ly()
    
    for(i in 1:30){
      
      fig <- fig %>% add_lines(
        x=1:5,
        y=paths[,i],
        line=list(color="#4f8dd6",width=1)
      )
      
    }
    
    fig %>%
      layout(
        plot_bgcolor="#0b1320",
        paper_bgcolor="#0b1320",
        font=list(color="white")
      )
    
  })
  
  #############################
  # VALUATION BRIDGE
  #############################
  
  output$bridge <- renderPlotly({
    
    req(sim())
    
    ev <- mean(sim()$ev)
    eq <- ev - NET_DEBT
    
    plot_ly(
      x=c("Enterprise Value","Net Debt","Equity Value"),
      y=c(ev,-NET_DEBT,eq),
      type="bar",
      marker=list(color=c("#4f8dd6","#ff8c42","#22c55e"))
    ) %>%
      layout(
        plot_bgcolor="#0b1320",
        paper_bgcolor="#0b1320",
        font=list(color="white")
      )
    
  })
  
  #############################
  # TABLE
  #############################
  
  output$table <- renderDT({
    
    req(sim())
    
    datatable(sim(),
              options=list(pageLength=10))
    
  })
  
}

shinyApp(ui, server)