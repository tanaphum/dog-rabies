tags$div(
  tags$div(class="col-sm-12 boxInput",
           tags$h2("Indoor"),
           tags$hr(),
           tags$div(class="col-sm-4",
                    sliderInput("SterFemalePuppyIndoor",
                                "Sterilization coverage Female Puppy Indoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterFemaleAdultIndoor",
                                "Sterilization coverage Female Adult Indoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterFemaleOlderIndoor",
                                "Sterilization coverage Female Older Indoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterMalePuppyIndoor",
                                "Sterilization coverage Male Puppy Indoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterMaleAdultIndoor",
                                "Sterilization coverage Male Adult Indoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterMaleOlderIndoor",
                                "Sterilization coverage Male Older Indoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
  ),
  tags$div(class="col-sm-12 boxInput",
           tags$h2("Outdoor"),
           tags$hr(),
           tags$div(class="col-sm-4",
                    sliderInput("SterFemalePuppyOutdoor",
                                "Sterilization coverage Female Puppy Outdoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterFemaleAdultOutdoor",
                                "Sterilization coverage Female Adult Outdoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterFemaleOlderOutdoor",
                                "Sterilization coverage Female Older Outdoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterMalePuppyOutdoor",
                                "Sterilization coverage Male Puppy Outdoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterMaleAdultOutdoor",
                                  "Sterilization coverage Male Adult Outdoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterMaleOlderOutdoor",
                                "Sterilization coverage Male Older Outdoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
  ),
  tags$div(class="col-sm-12 boxInput",
           tags$h2("Stray"),
           tags$hr(),
           tags$div(class="col-sm-4",
                    sliderInput("SterFemalePuppyStray",
                                "Sterilization coverage Female Puppy Stray dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterFemaleAdultStray",
                                "Sterilization coverage Female Adult Stray dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterFemaleOlderStray",
                                "Sterilization coverage Female Older Stray dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterMalePuppyStray",
                                "Sterilization coverage Male Puppy Stray dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterMaleAdultStray",
                                "Sterilization coverage Male Adult Stray dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("SterMaleOlderStray",
                                "Sterilization coverage Male Older Stray dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           
  )
)
