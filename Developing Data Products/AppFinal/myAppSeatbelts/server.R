#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- Seatbelts[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)


    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'red', border = 'black')
    #plot(density(Seatbelts[, 2])*1)
    #curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=T)


  })

})
