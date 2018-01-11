#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#s

library(mixtools)  
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Level curves"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       textInput("sigma", "Coefficients for sigma", "10,0,0,1"),
       textInput("mu", "Coefficients for mu", "1,0")
     ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$plot <- renderPlot({
    mu <- as.numeric(unlist(strsplit(input$mu, ",")))
    sigma <-  matrix(as.numeric(unlist(strsplit(input$sigma, ","))),2,2)
    
    ellipse(mu, sigma, npoints = 200, newplot = TRUE, col = 'white', xlab = 'X', ylab = 'Y', asp=1)
    
    for (i in 1:10) {
      ellipse(mu, sigma, alpha = .09*i, npoints = 100, newplot = FALSE)
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

