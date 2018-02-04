#https://deanattali.com/blog/building-shiny-apps-tutorial/#4-load-the-dataset

library(shiny)
library(ggplot2) 
library(DT)
df <- read.csv("C:/Users/testadmin/Desktop/Udemy/02. Shiny/Project/course-proj-data.csv",sep = ";")

server <- function(input, output, session) {
  data <- reactive({
    df$points <- df$G1 * input$myslider1 + df$G2 * input$myslider2 + df$G3 * input$myslider3
    df
  })
  
  output$plot <- renderPlot({
    ggplot(data(),aes(x=points,y=MarketCap.in.M)) + 
      geom_point() +
      geom_smooth(method='lm')
    
  })
  
  diam <- reactive({
    user_brush <- input$user_brush
    sel <- brushedPoints(data(), user_brush)
    return(sel)
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable(diam()))
  
}

ui <- fluidPage(
  
  titlePanel("The Mining Stock Scale"), # our title
  
  mainPanel(
    tabsetPanel(
      tabPanel("Tab1",
        fluidRow(
          column(12,
                 wellPanel(
                   sliderInput(inputId = "myslider1",
                               label = "Weight on Grade 1",
                               value = 7, min = 0, max = 20, step = 2),
                   sliderInput(inputId = "myslider2",
                               label = "Weight on Grade 2",
                               value = 6, min = 0, max = 20, step = 2),
                   sliderInput(inputId = "myslider3",
                               label = "Weight on Grade 3",
                               value = 0.8, min = 0, max = 6, step = 0.6),
                   plotOutput("plot",brush = "user_brush"),
                   dataTableOutput("table")
                          )
                 )
              )       
        ),
      
      tabPanel("Tab2", "Second Tab"), 
      
      tabPanel("Tab3", "Third Tab")
    )
  )
)

shinyApp(ui = ui, server = server)