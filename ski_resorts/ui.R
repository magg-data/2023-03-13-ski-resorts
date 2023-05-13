#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("ski_resorts/helpers.R")

# RANGES #####


# find out the range of prices
PRICE <- c( min(ski_data$Price),  max(ski_data$Price))

# find out available range of longest run in km
LONGEST_RUN <- c(min(ski_data$`Longest run`), max(ski_data$`Longest run`))

# lowest and highest points  in meters
LOWEST <- c(min(ski_data$`Lowest point`), max(ski_data$`Lowest point`))
HIGHEST <- c(min(ski_data$`Highest point`), max(ski_data$`Highest point`))

# beginners
BEGINNER <- c(min(ski_data$`Beginner slopes`), max(ski_data$`Beginner slopes`))
INTERMEDIATE <- c(min(ski_data$`Intermediate slopes`), max(ski_data$`Intermediate slopes`))
DIFFICULT <- c(min(ski_data$`Difficult slopes`), max(ski_data$`Difficult slopes`))

# padding style
PADDING1 <- "padding-left:1px; padding-right:1px; padding-top:10px; padding-bottom:1px"
PADDING2 <- "padding-left:2px; padding-right:1px; padding-top:1px; padding-bottom:1px"




fluidPage(
  titlePanel(h2(HTML("<b>Choose Your Next Ski Resort</b>"), align = "center")),
  p("If you want to enable a date filter, please uncheck the checkbox. 
  If you want to get a little info about a particular resort, please
  hover over the resort marker on the map. To get more info, you can 
  click on the marker or you can take a look at the table below the map."),
  p("The data is provided by ",
    a("Maven Data Analytics Ski Resorts Challenge.", href = "https://app.mavenanalytics.io/datasets"),
    "The exact timespan of data is unknown to me, although I reckon that",
    " it regards 2022 year. ", "Maven states that the data source is ", 
    a("Ski resort stats.com", href="https://ski-resort-stats.com"),
    " and NASA Earth Observations with License ", 
    em("Public Domain.")),
  shinyjs::useShinyjs(),
  # WHEN, WHERE, PRICE ################################
  fluidRow(style=PADDING1,
    column(
      4,
      wellPanel(
        h5(HTML("<b>When?</b>")),
        checkboxInput("inDatesDisabled", "Date Filter Disabled", TRUE), 
        dateRangeInput("inDates", NULL, min=Sys.Date())
      ),
      wellPanel(
        sliderInput(
          inputId = "inPricePerDay",
          label = "Price per Day for an Adult",
          min = PRICE[1],
          max = PRICE[2],
          value = PRICE,
          step = 1
        )
      ),
      
      wellPanel(
        selectInput(
          "inSelectChild",
          label = "Child Friendly",
          choices = THREE_SELECT,
          selected = -1
        ),
        
        selectInput(
          "inSelectSnowparks",
          label = "Snowparks",
          choices = THREE_SELECT,
          selected = -1
        ),
        
        selectInput(
          "inSelectNight",
          label = "Night Skiing",
          choices = THREE_SELECT,
          selected = -1
        ),
        
        selectInput(
          "inSelectSummer",
          label = "Summer Skiing",
          choices = THREE_SELECT,
          selected = -1
        )
      ), 
    ),
    
    # create a space to display  the map
    
    column(
      8,
      leafletOutput("my_leaf", width = "100%"),
      fluidRow(
        style="padding-left:2px; padding-right:1px; padding-top:1px; padding-bottom:1px",
        fluidRow(column(4, style = PADDING1,
                        wellPanel(style = PADDING1,
                          sliderInput(
                            "inMinLongestRun",
                            "Min Longest Run [km]",
                            min = LONGEST_RUN[1],
                            max = LONGEST_RUN[2],
                            value = LONGEST_RUN[1],
                            step = 1
                          )
                        )),
                 column(8, style = PADDING1,
                        wellPanel(style = PADDING1,
                                  fluidRow(
                                    column(
                                      6,
                                      # h4("Select Minimum Lowest and Highest Points:"),
                                      sliderInput(
                                        inputId = "inLowestPoint",
                                        label = "Lowest Point [m]",
                                        min = LOWEST[1],
                                        max = LOWEST[2],
                                        value = LOWEST,
                                        step = 100
                                      )
                                    ),
                                    
                                    column(
                                      6,
                                      sliderInput(
                                        inputId = "inHighestPoint",
                                        label = "Highest Point [m]",
                                        min = HIGHEST[1],
                                        max = HIGHEST[2],
                                        value = HIGHEST,
                                        step = 100
                                      )
                                    )
                                  )))), 
        
        fluidRow(
          style = PADDING2,
                 wellPanel(fluidRow(
                   column(
                     4,
                     sliderInput(
                       inputId = "inSliBeginner",
                       label = "Min # of Beginner Trails",
                       min = BEGINNER[1],
                       max = BEGINNER[2],
                       value = BEGINNER[1],
                       step = 1
                     )),
                   
                   column(
                     4,
                     sliderInput(
                       inputId = "inSliInter",
                       label = "Min # of Intermed Trails",
                       min = INTERMEDIATE[1],
                       max = INTERMEDIATE[2],
                       value = INTERMEDIATE[1],
                       step = 1
                     )),
                   
                   column(
                     4,
                     sliderInput(
                       inputId = "inSliDifficult",
                       label = "Min # of Difficult Trails",
                       min = DIFFICULT[1],
                       max = DIFFICULT[2],
                       value = DIFFICULT[1],
                       step = 1
                     ))
                 )))
        ), 
    )
  ),
  
  # Table ####################
  fluidRow(
    hr(),
    h4(HTML(paste("<b>Skiing resorts of your choice.",
                  "Highest.point and Lowest.point in meters.",
                  "Longest.run in kilometers.</b>"))),
    hr(),
    dataTableOutput("outResortsTab"),
  )
)

