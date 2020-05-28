#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        df <- data.frame(Gender=c("Male", "Female"),
                         Number=c(200, 108))
        head(df)
        p<-ggplot(data=df, aes(x=Gender, y=Number)) +
            geom_bar(stat="identity")
        p
    })

})
