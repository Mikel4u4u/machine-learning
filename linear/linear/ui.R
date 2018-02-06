library(shiny)
library(kernlab)

shinyUI(fluidPage(
  tabsetPanel(     
    tabPanel(title = "ADALINE",
             sidebarPanel(
               sliderInput("obj1",
                           "The number of objects of each class",
                           min = 10,  max = 200, value = 100),
               textInput("sigma1_adaline", "Coefficients for sigma1", "2,0,0,10"),
               textInput("mu1_adaline", "Coefficients for mu1", "0,0"),
               textInput("sigma2_adaline", "Coefficients for sigma2", "4,1,1,2"),
               textInput("mu2_adaline", "Coefficients for mu2", "10,-10")
             ),
             mainPanel(
               plotOutput('plot1')
             )
    ),
    tabPanel(title = "Hebb's rule",
             sidebarPanel(
               sliderInput("obj2",
                           "The number of objects of each class",
                           min = 10,  max = 200, value = 100),
               textInput("sigma1_hebb", "Coefficients for sigma1", "2,0,0,10"),
               textInput("mu1_hebb", "Coefficients for mu1", "0,0"),
               textInput("sigma2_hebb", "Coefficients for sigma2", "4,1,1,2"),
               textInput("mu2_hebb", "Coefficients for mu2", "10,-10")
             ),
             mainPanel(
               plotOutput('plot2')
             )
    )
  ))
)