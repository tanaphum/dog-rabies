tags$div(
  tags$div(class="col-sm-12 boxInput",
           tags$div(class="col-sm-4",
                    sliderInput("VacIndoor",
                                "Vaccine Indoor dogs",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("VacOutdoor",
                                "Vaccine Outdoor dogs",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 33
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("VacStray",
                                "Vaccine Stray dogss",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 33
                    )
           ),
           
  )
)
