tags$div(
  tags$div(class="col-sm-12 boxInput",
           tags$h2("Population dog"),
           tags$hr(),
           numericInput("Pop", "Number of dogs:", 2000,
                        min = 100, max = 1000000),
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
  ),
  tags$div(class="col-sm-12 boxInput",
           tags$h2("Indoor"),
           tags$hr(),
           tags$div(class="col-sm-4",
                    sliderInput("FemaleIndoorRate",
                                "Female Indoor(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 50
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("MaleIndoorRate",
                                "Male Indoor(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 50
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("BirthIndoor",
                                "Indoor Birth rate (per year)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("PuppyIndoor",
                                "Puppy Indoor dogs",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("AdultIndoor",
                                "Adult Indoor dogs",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 33
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("OlderIndoor",
                                "Older Indoor dogs",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 33
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathPuppyIndoor",
                                "Puppy Indoor Death rate (per year)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathAdultIndoor",
                                "Adult Indoor Death rate (per year)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathOlderIndoor",
                                "Older Indoor Death rate (per year)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           
  ),
  
  tags$div(class="col-sm-12 boxInput",
           tags$h2("Outdoor"),
           tags$hr(),
           tags$div(class="col-sm-4",
                    sliderInput("FemaleOutdoorRate",
                                "Female Outdoor(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 50
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("MaleOutdoorRate",
                                "Male Outdoor(%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 50
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("BirthOutdoor",
                                "Outdoor Birth rate (per year)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("PuppyOutdoor",
                                "Puppy Outdoor dogs",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("AdultOutdoor",
                                "Adult Outdoor dogs",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 33
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("OlderOutdoor",
                                "Older Outdoor dogs",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 33
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathPuppyOutdoor",
                                "Puppy Outdoor Death rate (per year)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathAdultOutdoor",
                                "Adult Outdoor Death rate (per year)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathOlderOutdoor",
                                "Older Outdoor Death rate (per year)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
           
           
  ),
  tags$div(class="col-sm-12 boxInput",
             tags$h2("Stray"),
             tags$hr(),
             tags$div(class="col-sm-4",
                      sliderInput("FemaleStrayRate",
                                  "Female Stray(%)",
                                  min = 0,
                                  max = 100,
                                  step = 1,
                                  value = 50
                      )
             ),
             tags$div(class="col-sm-4",
                      sliderInput("MaleStrayRate",
                                  "Male Stray(%)",
                                  min = 0,
                                  max = 100,
                                  step = 1,
                                  value = 50
                      )
             ),
           tags$div(class="col-sm-4",
                    sliderInput("BirthStray",
                                "Stray Birth rate (per year)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
             tags$div(class="col-sm-4",
                      sliderInput("PuppyStray",
                                  "Puppy Stray dogs",
                                  min = 0,
                                  max = 100,
                                  step = 1,
                                  value = 34
                      )
             ),
             tags$div(class="col-sm-4",
                      sliderInput("AdultStray",
                                  "Adult Stray dogs",
                                  min = 0,
                                  max = 100,
                                  step = 1,
                                  value = 33
                      )
             ),
             tags$div(class="col-sm-4",
                      sliderInput("OlderStray",
                                  "Older Stray dogs",
                                  min = 0,
                                  max = 100,
                                  step = 1,
                                  value = 33
                      )
             ),
           tags$div(class="col-sm-4",
                    sliderInput("DeathStray",
                                "Stray Death rate (per year)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 34
                    )
           ),
  ),
  

)

