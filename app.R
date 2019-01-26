##load the xts objects for the dygraphs

attach("llau.RData")

library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(tidyr)
library(plotly)
library(dygraphs)
library(xts)
library(forecast)
library(ggfortify)
library(tidyverse)
library(shinythemes)
library(lubridate)
library(ggfortify)
library(tseries)
library(tibbletime)


ui <- fluidPage(theme = shinytheme("flatly"),
  
  (titlePanel("App | Energy management", windowTitle = "App")),
  br(),
  #Range input
  sidebarLayout(
    sidebarPanel(
      helpText(strong("Long-term forecast (Tab 1-2)")),
      numericInput("months", label = "Weeks to Predict", 
                   value = 72, min = 12, max = 144, step = 12),
      
      sliderInput(inputId = "range",
                   label = "Set confidence interval",
                    min = 0, max = 100, value = c(65, 90)),
      
    
      helpText("Click and drag to zoom in on the lower graph (double click to zoom back out)."),
      
      
      hr(),
      
      helpText(strong("Short-term forecast(Tab 3)")),
      
      
      numericInput("days", label = "Days to Predict", 
                   value = 7, min = 1, max = 14, step = 1),
      
      hr(),
      
      helpText(strong("Energy density by sub-meter(Tab 4)")),
      
      selectizeInput(inputId = "factor", 
                     label = "Choose Submeter",
                     choices = c("Kitchen",
                                 "Laundry_room",
                                 "Heaters",
                                 "S4")),
      
      sliderInput(inputId = "bins",
                   label = "Granularity", 0, 100, 50),
      
      hr(),
      
      helpText(strong("Dig your own (Tab 5)")),
      
      
      
      sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(hourly),
                  value = 9000, step = 500, round = 0),
      selectInput('x', 'X', choices = nms, selected = "`date(DateTime)"),
      selectInput('y', 'Y', choices = nms, selected = "Global_active_power"),
      selectInput('color', 'Color', choices = nms, selected = "Global_reactive_power"),
      
      selectInput('facet_row', 'Facet Row', c(None = '.', nms), selected = "wday(DateTime)"),
      selectInput('facet_col', 'Facet Column', c(None = '.', nms)),
      sliderInput('plotHeight', 'Height of plot (in pixels)', 
                  min = 100, max = 2000, value = 1250)
      

      
    ),
  #checbox input
  mainPanel(
    tabsetPanel(type = "pills",
                tabPanel("Consumption", 
                             plotOutput("plot1"),
                             hr(),
                             dygraphOutput("dygraph")),
                             
                           
                           
                tabPanel("Loss",           
                             plotOutput("plot2"),
                             hr(),
                             dygraphOutput("dygraph2")),
                
                tabPanel("New Behavior",
                             plotOutput("plot5"),
                             hr(),
                             plotOutput("plot6")),
                
                tabPanel("Energy Distribution",
                             plotOutput("plot3"),
                             hr(),
                             plotOutput("plot4")),
                
                tabPanel("Explore", 
                         plotlyOutput("da.plot.man"))
                
                         )
                         
                )
    ))
  


server <- function(input, output) {
  
  output$dygraph <- renderDygraph({
   all.xts %>% dygraph() %>% 
      dySeries("actual", label = "Consumption", color = "darkgreen") %>% 
      dySeries(c("lwr", "fit", "upr"), label = "Predictions", color = "gray") %>% 
      dyRangeSelector(height = 30)
    
  })
  
  output$dygraph2 <- renderDygraph({
    all.xts2 %>% dygraph() %>%
      dySeries("actual", label = "Loss", color = "darkred") %>%
      dySeries(c("Lo 80", "Point Forecast", "Hi 80"), label = "Predictions", color = "gray") %>%
      dyRangeSelector(height = 30) 
    
  })
  
  output$plot1 <- renderPlot({
    autoplot(forecast(week.ts, 
                  level = c(input$range), h = input$months),
         main = "Weekly", col = "darkgreen", lty = 3) + theme_minimal() + ylab("Global consumption")
  })
  
  
  output$plot2 <- renderPlot({
    autoplot(forecast(tbatsFit, 
                  level = c(input$range), h = input$months), ylab = "Global losses",
         main = "Forecast on losses", col = "darkred", lty = 3) + theme_minimal()
  })
  
  selectedData <- reactive({
    
    test %>% dplyr:: filter(submeters == input$factor)
    
  })
  
  output$plot3 <- renderPlot({
    
    ggplot(selectedData(), aes(x = consumption, color = submeters, fill = quarter(`date(DateTime)`))) + 
      geom_histogram(bins = input$bins, fill = "lightblue") + 
      facet_grid(.~quarter(`date(DateTime)`)) + theme_minimal() + 
      xlab("Consumption") + ylab("Frequency") + ggtitle("Individual submeters usage frequency")
    
  })
  
  
  output$plot4 <- renderPlot(
    ggplot(test, aes(Global_active_power, col = submeters)) +
      geom_histogram(bins = input$bins) + facet_grid(.~quarter(`date(DateTime)`)) + theme_minimal() + xlab("Global Consumption") + ylab("Frequency")
  )
  
  output$plot5 <- renderPlot(
    autoplot(forecast(fit, 
                  h = input$days), 
         main = "Based on last month - Short-term predictions of losses", 
         xlab = "Two weeks intervals", 
         col = "lightgreen", 
         lty = 3) + theme_minimal() + autolayer(o))
    
    output$plot6 <- renderPlot(
      autoplot(forecast(fit.loss, 
                        h = input$days), 
               main = "Based on last month - Short-term predictions of losses", 
               xlab = "Two weeks intervals", 
               col = "red", 
               lty = 3) + theme_minimal()) 
    
    

  
  
  
  
  dataset <- reactive({
    hourly[sample(nrow(hourly), input$sampleSize),]
  })
  
  output$da.plot.man <- renderPlotly({
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_point() + scale_color_continuous(low = "pink", high = "blue") + theme_minimal()
    
    # if at least one facet column/row is specified, add it
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') 
    p <- p + facet_grid(facets)
    
    ggplotly(p) %>% 
      layout(height = input$plotHeight, autosize=TRUE)
    
  })
  

  
}

shinyApp(ui, server)

















