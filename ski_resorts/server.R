#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("ski_resorts/helpers.R")



# add the marker labels to the main data
# it suppose to work with html but apparently it does not
# html works with the popup option
ski_data$MarkerLabel <- unlist(
  sprintf("%s, $%s, %s", ski_data$Resort, ski_data$Price, ski_data$Season) %>%
  lapply(htmltools::HTML))

# if set to 1, then the debug in session
DEBUG <- 1

# Debug message ####
# in: msg What you want to display
#     tag Specifies what it is
#
debMsg <- function(msg, tag = "DEB") {
  print(paste0(tag, ": ", msg))
}

# Popup message ##################################################
# in: data Hold the data to be displayed
displayPopup <- function(data) {
  paste0(data$Resort, 
         "<BR/>", "Price per Day for Adult $", data$Price,
         "<BR/> Season: ", data$Season, 
         "<BR/> Child friendly: ", data$Child.friendly.Org,
         "<BR/> Snowparks: ", data$Snowparks.Org,
         "<BR/> Night skiing: ", data$Nightskiing.Org,
         "<BR/> Summer skiing: ", data$Summer.skiing.Org,
         "<BR/> Longest run: ", data$Longest.run, " km",
         "<BR/> Lowest point: ", data$Lowest.point, " m",
         "<BR/> Highest point: ", data$Highest.point, " m",
         "<BR/> Trails count:",
         "<BR/> * Beginner: ", data$Beginner,
         "<BR/> * Intermediate: ", data$Intermediate, 
         "<BR/> * Difficult: ", data$Difficult
         )
}

# renderAgain ####################################################
# in: data - it should be data that you want to render - the filtered
#            data
#     rea_vals - reactive values (like a pointer to the reactive values that 
#                need to be checked for the update)
#     deb_msg -  debug message that will be displayed
renderAgain <- function(data,  deb_msg) {
  
  if (DEBUG == 1) {
    debMsg(deb_msg)
  }
  
  # check for empty dataframe
  # leaflet struggles when empty dataframe so here we are
  if (nrow(data) == 0){
      leafletProxy(mapId = "my_leaf", data = data) %>% 
      clearMarkers()
  } else {
    leafletProxy(mapId = "my_leaf", data = data) %>% 
      clearMarkers() %>%
      addMarkers(label = ~ MarkerLabel,
                 popup = displayPopup(data))   
  }
}

# selectRowsBasedOnBool ############################
# Selects rows based on the bool expression
# in: menu_val Character name of the menu
#     data Data frame that is used to filter
#     field Which field in the data frame we check
# out: data Returns filtered data frame
selectRowsBasedOnBool <- function(val, data, field) {
  # ignore if menu_val is "N/A"
  if (val == THREE_SELECT["Yes"]) {
    data <- data[field == 1, ]
  } else if (val == THREE_SELECT["No"]) {
    data <- data[field == 0, ]
  }

  return (data)
}

# genMonthsInYear #############################
#
# Generates 12 dates with the beginning of the month
# this is a helper function for filtering the dates for skiing
# 
# like 2023-01-01, 2023-02-01, 2023-03-01, ..., 2023-12-01
# but there are constraints because the start date can start in any month
# 
# in: month - the month you generate the date
#     first_month the month of the start date from the range of a date (start;end)
#     first_year the year of the start date from the range of a date (start;end)
# out: a date like "first_year-month-01" or "++first_year-month-01" (as a year,
# following the start date)
genFirstDaysInMonth <- function(month, first_month, first_year) {
  
  if (DEBUG == 1) {
    TAG <- "genFirstDaysInMonth(): "
    debMsg(paste0(TAG, "month=", month, 
                  " first_month=", first_month, 
                  " first_year=", first_year))
  }
  
  s <- paste0("-", month, "-01")
  if (month < first_month) {
    s <- paste0(as.numeric(first_year) + 1,s)  
  } else {
    s <- paste0(first_year, s)
  }
  
  if (DEBUG == 1) {
    debMsg(paste0("genFirstDaysInMonth: s=", s))
  }
  return (s)
}

# filterDates ##################################
# Selects rows based on the dates. There are two cases in
# fact: 
# 1. when the timespan < 1 year - check month by month 
# 2. when the timespace >= 1 year - filter if anything open year around
#
# in: data - the data to filter
#  start - start date 
#  end - end date
# out: filtered data
filterDates <- function (data, start, end) {
  
  # the numerical field indicating the month in ski_data
  JAN_IDX <- which (names(data) == "January")
  
  ss <- as.Date(start)
  ee <- as.Date(end)
  
  months <-  c(as.numeric(format(ss, "%m")), as.numeric(format(ee, "%m")))
  year_s <- format(ss, "%Y")
  
  # we are only interested in one year at most
  m <- as.Date(sapply(1:12, genFirstDaysInMonth, first_month=months[1], first_year=year_s))

  # do the actual filtering, check which months of the year overlap
  # with the resort operational season
  
  next_m <- months[1]
  idx <- JAN_IDX - 1 # to have JAN_IDX + next_m - 1
  for( i in 1:12 ){
     if ( m[next_m] <= ee ) {
       data <- data[data[idx + next_m] == 1, ]
       if (DEBUG == 1) {
         debMsg(paste0("filterDates: next_m=", next_m, " nrow(data)=", nrow(data)))
       }
     } else {
       break
     }
    next_m <- (next_m + 1) %% 12
    if (next_m == 0) {
      next_m <- 1
    }
  }
  
  return (data)
}

# Main server function ################################
function(input, output, session) {
  # our data
  df <- data.frame(ski_data)
  
  ## create static element
  output$my_leaf <- renderLeaflet({
    leaflet(df) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lat = 0, lng = 0, zoom = 0) %>% 
      addMiniMap(width=150, height=150)
  })
  
  output$outResortsTab <-  renderDataTable({
    if (DEBUG == 1) {
      
      debMsg(paste0("renderDataTable(): nrow(reactive_vals$resort_df)=",
                    nrow(reactive_vals$resort_df)))
      debMsg(paste0("renderDataTable(): is.null(resort_df)=",
                    is.null(reactive_vals$resort_df)))
    
      debMsg("renderdataTable(): Before select()  ")
    }
    
    if (nrow(reactive_vals$resort_df) == 0) {
      if (DEBUG == 1) {
        debMsg("renderDataTable(): table empty. returning NULL")
      }
      shiny::showNotification("The table is empty", type = "warning")
      NULL
    } else {
        if (DEBUG == 1) {
          debMsg("renderDataTable(): The table is non empty")
        }
      
        select(reactive_vals$resort_df, Resort, Country, Continent, Price, 
           Season, Seasons.Count, Highest.point, Lowest.point, 
           Beginner.slopes, Intermediate.slopes, Difficult.slopes,
           Total.slopes, Longest.run, Snow.cannons, 
           Surface.lifts, Chair.lifts, Gondola.lifts, Total.lifts, Lift.capacity,
           Child.friendly.Org, Snowparks.Org, Nightskiing.Org, Summer.skiing.Org)
    }
  })
  

  # create a reactive object with initial value of the number
  # of resorts
  reactive_vals <- reactiveValues(
    # it seems that '=' needs to be here and not '<-'
    resort_df = df
  )
  
  ## filter data
  df_filtered <- reactive({
    
    # whenever input$* values change the df_filtered expression got
    # invalidated
    if (DEBUG == 1) {
      debMsg("reactive: df_filtered")
      debMsg("df=")
      str(df)
    }
    
    # that should be probably made smarter, right now it always changes
    # but probably it should so ... as the filters change
    df_tmp <- df
    
    if (input$inDatesDisabled != TRUE) {
      df_tmp <- filterDates(df_tmp, input$inDates[1], input$inDates[2])
    }
      
    df_tmp <- df_tmp[between(df_tmp$Price, input$inPricePerDay[1], input$inPricePerDay[2])
                 & df_tmp$Longest.run >= input$inMinLongestRun
                 & between(df_tmp$Lowest.point, input$inLowestPoint[1], input$inLowestPoint[2])
                 & between(df_tmp$Highest.point, input$inHighestPoint[1], input$inHighestPoint[2])
                 & df_tmp$Beginner.slopes >= input$inSliBeginner
                 & df_tmp$Intermediate.slopes >= input$inSliInter
                 & df_tmp$Difficult.slopes >= input$inSliDifficult
                 ,]

    df_tmp <- selectRowsBasedOnBool(input$inSelectChild, df_tmp, df_tmp$Child.friendly)    
    df_tmp <- selectRowsBasedOnBool(input$inSelectSnowparks, df_tmp, df_tmp$Snowparks)
    df_tmp <- selectRowsBasedOnBool(input$inSelectNight, df_tmp, df_tmp$Nightskiing)
    df_tmp <- selectRowsBasedOnBool(input$inSelectSummer, df_tmp, df_tmp$Summer.skiing)
    
    # update the data for the table
    reactive_vals$resort_df <- df_tmp
    
    if (DEBUG == 1) {
      row_count <- nrow(df_tmp)
      debMsg(paste0("df_filtered(): nrow(df_tmp)=", row_count))
      
      if (row_count == 0) {
        debMsg("df_filtered(): df_tmp=")
        print(df_tmp)
      }
    }
    
    df_tmp
  })

    
  ## respond to the filtered data
  observeEvent(input$inDatesDisabled, {
    
    if(input$inDatesDisabled){
      shinyjs::disable("inDates")
    }else{
      shinyjs::enable("inDates")
    }
    
    renderAgain(df_filtered(),
                paste("observeEvent: inDatesDisabled changed to ...", input$inDatesDisabled))  
  })
  
  # invalidate everything when the inPricePerDay changes
  # then update the reactive values with the new row counts
  # and render updated text
  observeEvent(input$inPricePerDay, {
    renderAgain(df_filtered(),
                "observeEvent: inPricePerDay changed ..")  
  })
  
  observeEvent(input$inDates, {
    # my validation is to swap the dates if they are out of order
    # i.e. start with end and vice versa
    
    if (input$inDates[1] > input$inDates[2]) {
      if (DEBUG == 1) {
        debMsg("observeEvent: inDates ranges incorrect ... swapping start with end")
      }
      updateDateRangeInput(session, "inDates", start = min(input$inDates),
                           end = max(input$inDates))
    }
    
    renderAgain(df_filtered(),
              paste0("observeEvent: inDates changed ...[", 
                       input$inDates[1], ", ", input$inDates[2], "]"))
  })

  observeEvent(input$inSliBeginner, {
    renderAgain(df_filtered(), 
                paste0("observeEvent: inSliBeginner changed to ...", input$inSliBeginner))
  })
  
  observeEvent(input$inSliInter, {
    renderAgain(df_filtered(), 
                paste0("observeEvent: inSliInter changed to ...", input$inSliInter))
  })
  
  observeEvent(input$inSliDifficult, {
    renderAgain(df_filtered(), 
                paste0("observeEvent: inSliDifficult changed to ...", input$inSliDifficult))
  })
  
  observeEvent(input$inSelectChild, {
    renderAgain(df_filtered(),
                paste0("observeEvent: inSelectChild changed to...", 
                       input$inSelectChild))  
  })

  observeEvent(input$inSelectSnowparks, {
    renderAgain(df_filtered(), 
                paste0("observeEvent: inSelectSnowparks changed to...", 
                       input$inSelectSnowparks))  
  })
  
  observeEvent(input$inSelectNight, {
    renderAgain(df_filtered(), 
                paste0("observeEvent: inSelectNight changed to...", 
                       input$inSelectNight))  
  })
  
  observeEvent(input$inSelectSummer, {
    renderAgain(df_filtered(), 
                paste0("observeEvent: inSelectSummer changed to...", 
                       input$inSelectSummer))  
  })
  
  observeEvent(input$inLowestPoint, {
    renderAgain(df_filtered(), 
                paste0("observeEvent: inSelectLowestPoint changed to... [", 
                       input$inLowestPoint[1], ",", input$inLowestPoint[2], "]"))   
  })
  
  observeEvent(input$inHighestPoint, {
    renderAgain(df_filtered(), 
                paste0("observeEvent: inSelectHighestPoint changed to... [", 
                       input$inHighestPoint[1], ",", input$inHighestPoint[2], "]"))  
  })
        
  observeEvent(input$inMinLongestRun, {
    renderAgain(df_filtered(), 
        paste0("observeEvent: inMinLongestRun changed ... inMinLongestRun=", input$inMinLongestRun))
                
  })
}
