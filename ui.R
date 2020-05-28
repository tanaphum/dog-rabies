#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tags$div(class="header bg-primary",
             tags$h1(class="text-center","monitoring dog sterilization program")
    ),
    
    tabsetPanel(
        tabPanel("Introduction",
                 tags$h2("Introduction"),
                 ),
        tabPanel("Mathematical model",
                 tags$h2("Mathematical model"),
                 ),
        tabPanel("Scenarios",
                 tags$div(
                     radioButtons("screening", "Screening type :",
                                  c("Sterilization only" = 1,
                                    "Vaccination only " = 2,
                                    "Both" = 3 ),
                                  inline = T)
                 ),
                 tags$div(class="col-sm-12",
                          tags$h2("Sterilization"),
                          tags$div(class="col-sm-4",
                                   sliderInput("coverage_str",
                                               "Coverage of Sterilization (%)",
                                               min = 0,
                                               max = 100,
                                               step = 1,
                                               value = 50
                                   )
                          ),
                          tags$div(class="col-sm-4",
                                   radioButtons("gender", "Gender : ",
                                                c("Male" = 1,
                                                  "Female" = 2))
                          ),
                          tags$div(class="col-sm-4",
                                   radioButtons("status", "Status : ",
                                                c("Owned" = 1,
                                                  "Strayed" = 2))
                          ),
                 ),
                 tags$div(class="col-sm-12",
                          tags$h2("Vaccination"),
                          tags$div(class="col-sm-6",
                                   sliderInput("coverage_Vac",
                                               "Coverage of Vaccination (%)",
                                               min = 0,
                                               max = 100,
                                               step = 1,
                                               value = 50
                                   )
                          ),
                          tags$div(class="col-sm-6",
                                   sliderInput("efficacy",
                                               "Efficacy of Vaccination (%)",
                                               min = 0,
                                               max = 100,
                                               step = 1,
                                               value = 50
                                   )
                          ),
                          
                 ),
                 ),
        tabPanel("Model predictions",
                 tabsetPanel(
                     tabPanel("Dog population",
                              plotOutput("distPlot")
                              ),
                     tabPanel("Biting rate"),
                     tabPanel("Rabies")
                 ),
                 ),
        tabPanel("Cost",
                 tabsetPanel(
                     tabPanel("interventions"),
                     tabPanel("cost of dogs"),
                     tabPanel("cost of biting"),
                     tabPanel("burdens")
                 ),),
        tabPanel("Economic analysis",
                 tags$h1("scenario comparison")
                 )
        
    ),


))
