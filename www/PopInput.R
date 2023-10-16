tags$div(
  tags$div(class="col-sm-12 boxInput",
           uiOutput("Indoor_pop"),
           tags$hr(),
           tags$div(class="col-sm-6",
                    sliderInput("FemaleIndoorRate",
                                "Female Indoor(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 50
                    )
           ),
           tags$div(class="col-sm-6",
                    sliderInput("MaleIndoorRate",
                                "Male Indoor(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 50
                    )
           ),
           tags$div(class="col-sm-6",
                    sliderInput("BirthAdultIndoor",
                                "Adult Indoor Give Birth rate (per year)",
                                min = 0,
                                max = 3,
                                step = 0.05,
                                value = 1
                    )
           ),
           tags$div(class="col-sm-6",
                    sliderInput("BirthOlderIndoor",
                                "Older Indoor Give Birth rate (per year)",
                                min = 0,
                                max = 3,
                                step = 0.05,
                                value = 1
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("PuppyIndoor",
                                "Puppy Indoor dogs(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 40
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("AdultIndoor",
                                "Adult Indoor dogs(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 30
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("OlderIndoor",
                                "Older Indoor dogs(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 30
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathPuppyIndoor",
                                "Puppy Indoor Death rate (per year)",
                                min = 0,
                                max = 0.5,
                                step = 0.05,
                                value = 0.2
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathAdultIndoor",
                                "Adult Indoor Death rate (per year)",
                                min = 0,
                                max = 0.5,
                                step = 0.05,
                                value = 0.2
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathOlderIndoor",
                                "Older Indoor Death rate (per year)",
                                min = 0,
                                max = 0.5,
                                step = 0.05,
                                value = 0.2
                    )
           ),
           uiOutput("Indoor_pop_type")
           
  ),
  
  tags$div(class="col-sm-12 boxInput",
           uiOutput("Outdoor_pop"),
           tags$hr(),
           tags$div(class="col-sm-6",
                    sliderInput("FemaleOutdoorRate",
                                "Female Outdoor(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 50
                    )
           ),
           tags$div(class="col-sm-6",
                    sliderInput("MaleOutdoorRate",
                                "Male Outdoor(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 50
                    )
           ),
           tags$div(class="col-sm-6",
                    sliderInput("BirthAdultOutdoor",
                                "Adult Outdoor Give Birth rate (per year)",
                                min = 0,
                                max = 3,
                                step = 0.05,
                                value = 1
                    )
           ),
           tags$div(class="col-sm-6",
                    sliderInput("BirthOlderOutdoor",
                                "Older Outdoor Give Birth rate (per year)",
                                min = 0,
                                max = 3,
                                step = 0.05,
                                value = 1
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("PuppyOutdoor",
                                "Puppy Outdoor dogs(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 40
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("AdultOutdoor",
                                "Adult Outdoor dogs(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 30
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("OlderOutdoor",
                                "Older Outdoor dogs(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 30
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathPuppyOutdoor",
                                "Puppy Outdoor Death rate (per year)",
                                min = 0,
                                max = 0.5,
                                step = 0.05,
                                value = 0.2
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathAdultOutdoor",
                                "Adult Outdoor Death rate (per year)",
                                min = 0,
                                max = 0.5,
                                step = 0.05,
                                value = 0.2
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathOlderOutdoor",
                                "Older Outdoor Death rate (per year)",
                                min = 0,
                                max = 0.5,
                                step = 0.05,
                                value = 0.2
                    )
           ),
           uiOutput("Outdoor_pop_type")
           
  ),
  tags$div(class="col-sm-12 boxInput",
             uiOutput("Stray_pop"),
             tags$hr(),
             tags$div(class="col-sm-6",
                      sliderInput("FemaleStrayRate",
                                  "Female Stray(%)",
                                  min = 0,
                                  max = 100,
                                  step = 1,
                                  value = 50
                      )
             ),
             tags$div(class="col-sm-6",
                      sliderInput("MaleStrayRate",
                                  "Male Stray(%)",
                                  min = 0,
                                  max = 100,
                                  step = 1,
                                  value = 50
                      )
             ),
           tags$div(class="col-sm-6",
                    sliderInput("BirthAdultStray",
                                "Adult Stray Give Birth rate (per year)",
                                min = 0,
                                max = 3,
                                step = 0.05,
                                value = 1
                    )
           ),
           tags$div(class="col-sm-6",
                    sliderInput("BirthOlderStray",
                                "Older Stray Give Birth rate (per year)",
                                min = 0,
                                max = 3,
                                step = 0.05,
                                value = 1
                    )
           ),
             tags$div(class="col-sm-4",
                      sliderInput("PuppyStray",
                                  "Puppy Stray dogs",
                                  min = 0,
                                  max = 100,
                                  step = 1,
                                  value = 40
                      )
             ),
             tags$div(class="col-sm-4",
                      sliderInput("AdultStray",
                                  "Adult Stray dogs",
                                  min = 0,
                                  max = 100,
                                  step = 1,
                                  value = 30
                      )
             ),
             tags$div(class="col-sm-4",
                      sliderInput("OlderStray",
                                  "Older Stray dogs",
                                  min = 0,
                                  max = 100,
                                  step = 1,
                                  value = 30
                      )
             ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathPuppyStray",
                                "Puppy Stray Death rate (per year)",
                                min = 0,
                                max = 0.5,
                                step = 0.05,
                                value = 0.2
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathAdultStray",
                                "Adult Stray Death rate (per year)",
                                min = 0,
                                max = 0.5,
                                step = 0.05,
                                value = 0.2
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathOlderStray",
                                "Older Stray Death rate (per year)",
                                min = 0,
                                max = 0.5,
                                step = 0.05,
                                value = 0.2
                    )
           ),
           tags$div(class="col-sm-6",
                    sliderInput("Strayrate",
                                "Stray rate(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 50
                    )
           ),
           tags$div(class="col-sm-6",
                    sliderInput("Adoptedrate",
                                "Adopted rate(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 50
                    )
           ),
           uiOutput("Stray_pop_type")
  ),
  

)


