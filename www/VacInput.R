tags$div(
  tags$div(class="col-sm-12 boxInput",
           tags$div(class="col-sm-4",
                    sliderInput("VacPuppyIndoor",
                                "Vaccine coverage Puppy Indoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("VacAdultIndoor",
                                "Vaccine coverage Adult Indoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("VacOlderIndoor",
                                "Vaccine coverage Older Indoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("VacPuppyOutdoor",
                                "Vaccine coverage Puppy Outdoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("VacAdultOutdoor",
                                "Vaccine coverage Adult Outdoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("VacOlderOutdoor",
                                "Vaccine coverage Older Outdoor dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("VacPuppyStray",
                                "Vaccine coverage Puppy Stray dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("VacAdultStray",
                                "Vaccine coverage Adult Stray dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           tags$div(class="col-sm-4",
                    sliderInput("VacOlderStray",
                                "Vaccine coverage Older Stray dogs (%)",
                                min = 0,
                                max = 100,
                                step = 1,
                                value = 0
                    )
           ),
           
  )
)
