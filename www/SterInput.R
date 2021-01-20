tags$div(
  tags$div(class="col-sm-12 boxInput",
           tags$div(class="col-sm-4",
                    sliderInput("SterIndoor",
                                "Sterilization Indoor dogs",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterOutdoor",
                                "Sterilization Outdoor dogs",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 33
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterStray",
                                "Sterilization Stray dogss",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 33
                    )
           ),
           
  )
)