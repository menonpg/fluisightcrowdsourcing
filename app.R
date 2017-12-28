## app.R ##
# R/Shiny app for crowd-sourcing votes on the top-team to trust with regard to predictions of influenza risk, scored based on the CDC's Weekly U.S. Influenza Surveillance Report.
# 12/28/2017
# Jeff Morgan, Prahlad G Menon, PhD

## Clear Summary data before serving this app up! 
    # rm(list=c("responses"))

library(shinydashboard)
require(dplyr)
require(FluSight)
library(stringr)
library(plotly)
library(shinyjs)

## Global functions
# dir_2018 <- "../../FluSight-forecastsX/2017-2018/Delphi-Epicast/"
dir_2018 <- "Forecasts/2017-2018/Delphi-Epicast/"

source('readFluViewGTdata.R')

import_forecasts <- function(this_dir, this_week) {
  file_names <- list.files(this_dir, recursive=T, pattern = "*.csv")
  if (length(file_names) == 0) stop("No files found; check directory.")
  
  # Only take files of week of interest
  these_files <- grep(paste0("EW", this_week), file_names, value = TRUE)
  if (length(these_files) == 0) stop("No files found for that week; check week number")
  
  # Remove historical average predictions - only want to average submitted models
  if (any(grepl("Hist-Avg", these_files))) these_files <- these_files[-(grep("Hist-Avg", these_files))]
  
  # these_files <- file_names
  forecast_data <- data.frame()
  
  for (this_file in these_files) {
    
    this_sub <- read_entry(paste0(this_dir, this_file)) %>%
      filter(type == "Bin")
    
    this_sub$forecast_week <- NULL
    
    forecast_data <- rbind(forecast_data, this_sub)
  }
  
  return(forecast_data)
}

get_forecast_date <- function(this_dir, this_week) {
  file_names <- list.files(this_dir, recursive=T)
  if (length(file_names) == 0) stop("No files found; check directory.")
  
  # Take first file from week of interest
  this_file <- grep(paste0("EW", this_week), file_names, value = TRUE)[1]
  forecast_date <- regmatches(this_file, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",
                                                 this_file))
  return(forecast_date)
}


saveData <- function(data) {
  data <- as.data.frame(t(data))
  
  AllTEAMS <- returnList[[1]]
  TeamNames <- returnList[[2]]
  
  selection1 <- which (TeamNames %in% data$TopTeam) 
  SCORESELECTION <- round(COMPARISON_GT_POINTS[selection1],4)
  
  if (exists("responses")) {
    responses <<- rbind(responses, data.frame("Forecast Week" = forecastWeek, data, 
                                              "REGION" = inputREGION, "TARGET" = inputTARGET, "Score" = SCORESELECTION,
                                              "Date Submitted" = Sys.time()))
    
  } else {
    responses <- data.frame(data, "REGION" = inputREGION, "TARGET" = inputTARGET, "Score" = SCORESELECTION,
                            "Date Submitted" = Sys.time())
    responses <<- data.frame("Forecast Week" = forecastWeek, responses)
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}


## App UI
ui <- dashboardPage(
  dashboardHeader(title = "Crowd Wisdom: Flu"),
  dashboardSidebar(
    # box(
    title = "Data Load Controls:",
    textInput("forecastWeek", "Enter Forecast Week", "50"),
    
    title = "Plot Controls:",
    selectInput(inputId = "REGION", label = "Region for Analysis:", choices = c( "HHS Region 1",  "HHS Region 10", "HHS Region 2" , "HHS Region 3" , "HHS Region 4",  "HHS Region 5" , "HHS Region 6" ,
                                                                                 "HHS Region 7" , "HHS Region 8" , "HHS Region 9",  "US National" ) , selected = "US National"),
    selectInput(inputId = "TARGET", "Target Plot:", choices = c("1 wk ahead"      ,       "2 wk ahead"    ,         "3 wk ahead",             "4 wk ahead" ,            "Season onset"  ,        
                                                                     "Season peak percentage" ,"Season peak week" )),
    
    selectInput(inputId = "PlotType", "Plot Type: ", choices = c("Bar chart", "Line chart"))
    
    # )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow( 
      useShinyjs(), 
      # box(
      # plotOutput("plot0", height = 550, width = 550)
      
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", 
                   plotlyOutput("plot1", width = 650),
                   textInput("name", "Your Name", ""),
                   textInput("TopTeam", "Top Team Selection", ""),
                   textInput("Remarks", "Any comments ?", ""),
                   
                   # verbatimTextOutput("verbatimReport"),
                   hidden(
                     div(id='text_div',
                         verbatimTextOutput("verbatimReport")
                     )
                   ),
                   
                   actionButton("submit", "Submit")
                   # selectInput("TopTeam", choices = c("CU_Vixen" ,       "FluSightNetwork", "HumNat" ,         "NEU-GLEAM"      )),
          ),
          tabPanel("Summary", 
                   
                   textInput("DownloadPassword", "Enter Password to download data: ", ""),
                   
                   actionButton(inputId = "Load", label = "Load Submissons"),
                   downloadButton('downloadVotes', 'Download data in this table CSV') ,
                   
                   # uiOutput(outputId = "responses"), tags$hr() 
                   DT::dataTableOutput("responses", width = 300), tags$hr()
                   
          ),
          tabPanel("About",
                   h2 ("Data Sources: "),
                   p("The models compared and evaluated in this app were downloaded from: https://github.com/cdcepi/FluSight-forecasts.  Data on CDC outbreaks was downloaded from: https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html"),
                   p("These models are submitted to CDC as part of the Influenza Challenge on a weekly basis.  Votes are compiled and submitted as part of a 'Crowd' for a 'Wisdom of Crowds' approach to assigning weights to the various models."),
                   
                   h2("Directions for Use:  "),
                   p("Please enter your top-team selections for each prediction target and Regions 3 and 4 only.  Please enter your selection for other regions if time permits.  Please refresh the app webpage in the event that any error messages are noted!  The latter are likely owing to download errors for data residing on github."),
                   
                   h2("Contact: "),
                   p("This app was developed by Jeff Morgan and Prahlad G Menon.  Please submit comments to Jeff Morgan by contacting 540-845-7249 or emailing 19morgan@cua.edu .")
                   
                   
                                    
          )
        )
      )
      
      
    )
  )
)

# Define the fields we want to save from the form
fields <- c("name", "TopTeam", "Remarks")

server <- function(input, output,session) {
  
  output$downloadVotes <- downloadHandler(
    filename = function() { 
      paste("Votes_",Sys.Date(), '.csv', sep='')
       
    },
    
    content = function(file) {
      withProgress(message = "Downloading Votes!", value = 0, {
        ToWrite <- responses[,c(1,2,5,6,3,4,7,8)] # This is a global variable created by the plotting container responsible for visualizing the data of interest
        colnames(ToWrite) <- c("Forecast Week", "Respondent Name", "REGION","TARGET", "Voted Team", "Remarks", "Score", "Submission Date/Time")
      })
      
      if (input$DownloadPassword == "PasswordForThisApp") {
        write.csv(ToWrite, file, row.names = F)
      } else {
        write.csv(as.data.frame("Error" = "PASSWORD INCORECT!"), file, row.names = F)
      }
    }       
  )
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
    
    toggle('text_div')
    output$verbatimReport <- renderText({"Submitted!"})
    
    Sys.sleep(1)
    toggle('text_div')
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    # input$submit
    input$Load
    isolate(if (input$DownloadPassword == "JeffsPassword") {
        loadData()
    })
  })     
  
  
  
  myData <- reactive({
    ## Compare 1 week ahead forecasts for all teams 
    # AllTEAMS <- list.dirs(path = "../../FluSight-forecastsX/2017-2018/", full.names = TRUE, recursive = TRUE)
    AllTEAMS <- list.dirs(path = "Forecasts//2017-2018/", full.names = TRUE, recursive = TRUE)
    
    # TeamNames = 
    AllTEAMS <- AllTEAMS[-c(1)]
    
    # Select teams:
    # AllTEAMS <- AllTEAMS[c(7,12,15,22)]
    AllTEAMS <- AllTEAMS[c(11,12,10,9,18,19,21,4)] #(7,12,15,22)]
    # AllTEAMS <- AllTEAMS[c(5,7,8,9,10,11,12,15,16,17,18,19,21, 22, 23, 25, 32, 33)]
    
    TeamNames <- sub(".*//", "", AllTEAMS)
    myColorsRegions <- sample(colours(distinct = T), length(TeamNames))
    
    return(list(AllTEAMS, TeamNames, myColorsRegions))
  })
  
  output$plot0 <- renderPlot({
    returnList <<- myData()
    
    AllTEAMS <- returnList[[1]]
    TeamNames <- returnList[[2]]
    myColorsRegions <- returnList[[3]]
    
    forecast_week <- input$forecastWeek
    forecastWeek <<- forecast_week
    
    forecast_data <- import_forecasts(dir_2018, forecast_week)
    
    uniqueTarget <- unique(forecast_data$target)
    uniqueRegions <- unique(forecast_data$location)
    
    inputREGION <<- input$REGION
    inputTARGET <<- input$TARGET
    
    for (j in input$REGION) {
      for (i in input$TARGET) {
        TeamCount = 0
        for (dir_2018 in AllTEAMS) {
          TeamCount = TeamCount + 1
          forecast_data <- import_forecasts(paste0(dir_2018, "/"), forecast_week)
          
          if (dir_2018==AllTEAMS[1]) { 
            fctemp <- forecast_data[(forecast_data$target==i) & (forecast_data$location==j),]
            plot( y =fctemp$value[order(as.numeric(fctemp$bin_start_incl))], 
                  x=as.numeric(fctemp$bin_start_incl[order(as.numeric(fctemp$bin_start_incl))]), type="l",
                  xlab = "Bin", ylab = "Value", main = paste(j, i, ": Kernel Density Plots"),
                  # xlim = c(0,8), 
                  # ylim = c(0,1.1*max(fctemp$value)))
                  ylim = c(0, 0.75))
          } else {
            fctemp <- forecast_data[(forecast_data$target==i) & (forecast_data$location==j),]
            lines(y=fctemp$value[order(as.numeric(fctemp$bin_start_incl))], 
                  x=as.numeric(fctemp$bin_start_incl[order(as.numeric(fctemp$bin_start_incl))]), type="l", 
                  col=myColorsRegions[TeamCount])
          }
        }
        
        legend("topright",col = myColorsRegions, legend = TeamNames, cex=1, lty=1)
      }
    }
    
  })
  
  
  # renderPlotly() version of the above
  output$plot1 <- renderPlotly({
    returnList <<- myData()
    
    AllTEAMS <- returnList[[1]]
    TeamNames <- returnList[[2]]
    myColorsRegions <- returnList[[3]]
    
    forecast_week <- input$forecastWeek
    forecastWeek <<- forecast_week
    forecast_data <- import_forecasts(dir_2018, forecast_week)
    
    ## GET GROUND TRUTH INFORMATION:
    if (!exists("ili_df_HHS")) {
      
      ili_df_National <- ilinet(region = c("national"))     
      ili_df_National <<- ili_df_National[ili_df_National$year %in% c(2017,2018), c("region","year", "week", "weighted_ili")]
      # View(ili_df_National)
      ili_df_HHS <- ilinet(region = c( "hhs" ))
      ili_df_HHS <<- ili_df_HHS[ili_df_HHS$year %in% c(2017,2018), c("region","year", "week", "weighted_ili")]
      # View(ili_df_HHS)
      
    }
    
    
    uniqueTarget <- unique(forecast_data$target)
    uniqueRegions <- unique(forecast_data$location)
    
    
    inputREGION <<- input$REGION
    inputTARGET <<- input$TARGET
    
    # Let's do a first plot
    p<-plot_ly()
    
    if (!exists("COMPARISON_GT_POINTS")) {
      COMPARISON_GT_POINTS = c()
    }
    
    for (j in input$REGION) {
      for (i in input$TARGET) {
        TeamCount = 0
        for (dir_2018 in AllTEAMS) {
          TeamCount = TeamCount + 1
          forecast_data <- import_forecasts(paste0(dir_2018, "/"), forecast_week)
          
          if (dir_2018==AllTEAMS[1]) { 
            fctemp <<- forecast_data[(forecast_data$target==i) & (forecast_data$location==j),]
            # plot( y =fctemp$value[order(as.numeric(fctemp$bin_start_incl))], 
            #       x=as.numeric(fctemp$bin_start_incl[order(as.numeric(fctemp$bin_start_incl))]), type="l",
            #       xlab = "Bin", ylab = "Value", main = paste(j, i, ": Kernel Density Plots"),
            #       # xlim = c(0,8), 
            #       # ylim = c(0,1.1*max(fctemp$value)))
            #       ylim = c(0, 0.75))
            
            if (input$TARGET %in% c( "Season onset" ,"Season peak week")) {
              if (input$PlotType %in% c("Line chart")) {
                xtemp <<- as.numeric(fctemp$bin_start_incl[order(as.numeric(fctemp$bin_start_incl))])
                ytemp <<- fctemp$value[order(as.numeric(fctemp$bin_start_incl))]
                #print(cbind(xtemp[c(21:33,1:20)],ytemp[c(21:33,1:20)]))
                p<-add_trace(p, y=ytemp[c(21:33,1:20)], 
                             x=as.data.frame(as.character(paste(xtemp[c(21:33,1:20)])),stringsAsFactors = F), #as.data.frame(1:length(ytemp)) , 
                             type="scatter", mode="lines",name = TeamNames[TeamCount] )
                
              } else {
                xtemp <<- as.numeric(fctemp$bin_start_incl[order(as.numeric(fctemp$bin_start_incl))])
                ytemp <<- fctemp$value[order(as.numeric(fctemp$bin_start_incl))]
                #print(cbind(xtemp[c(21:33,1:20)],ytemp[c(21:33,1:20)]))
                p<-add_trace(p, y=ytemp[c(21:33,1:20)], 
                             x=as.data.frame(as.character(paste(xtemp[c(21:33,1:20)])),stringsAsFactors = F), #as.data.frame(1:length(ytemp)), 
                             type="bar",name = TeamNames[TeamCount])
              }
            } else {
              
              # print(sub(".*? (.+)", "\\1", input$REGION))
              
              ## Compute Scoring by bin ranges
              if (input$REGION %in% "US National") {
                temp1 <- as.data.frame(ili_df_National[((ili_df_National$week==(as.numeric(forecast_week)+1))),])  ## GENERALIZE THIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                print(as.data.frame(ili_df_National[((ili_df_National$week==(as.numeric(forecast_week)+1))),]))
              } else {
                temp1 <- as.data.frame(ili_df_HHS[((ili_df_HHS$week==(as.numeric(forecast_week)+1))&(ili_df_HHS$region==sub(".*? (.+)", "\\1", input$REGION))),])  ## GENERALIZE THIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                print(as.data.frame(ili_df_HHS[((ili_df_HHS$week==(as.numeric(forecast_week)+1))&(ili_df_HHS$region==sub(".*? (.+)", "\\1", input$REGION))),]))
              } 
              
              targetRow <- which(((fctemp$bin_start_incl<temp1$weighted_ili)&(fctemp$bin_end_notincl>temp1$weighted_ili)))
              # print(targetRow)
              if (length(targetRow) == 0) {
                COMPARISON_GT_POINTS <<- NA
              } else {
                COMPARISON_GT_POINTS <<- sum(fctemp[(targetRow-5):(targetRow+5),]$value)
              }
              
              print(paste(  sub(".*? (.+)", "\\1", input$REGION) , ", Week", as.numeric(forecast_week)+1, "Score for 1st Team: ", COMPARISON_GT_POINTS[1]))
              
              if (input$PlotType %in% c("Line chart")) {
                p<-add_trace(p, y=fctemp$value[order(as.numeric(fctemp$bin_start_incl))], 
                             x=as.numeric(fctemp$bin_start_incl[order(as.numeric(fctemp$bin_start_incl))]) , 
                             type="scatter", mode="lines",name = TeamNames[TeamCount] )
                
              } else {
                p<-add_trace(p, y=fctemp$value[order(as.numeric(fctemp$bin_start_incl))], 
                             x=as.numeric(fctemp$bin_start_incl[order(as.numeric(fctemp$bin_start_incl))]) , 
                             type="bar",name = TeamNames[TeamCount] )
              }
            }
            
            
          } else {
            fctemp <- forecast_data[(forecast_data$target==i) & (forecast_data$location==j),]
            
            if (input$TARGET %in% c( "Season onset" ,"Season peak week")) { 
              if (input$PlotType %in% c("Line chart")) {
                xtemp <<- as.numeric(fctemp$bin_start_incl[order(as.numeric(fctemp$bin_start_incl))])
                ytemp <<- fctemp$value[order(as.numeric(fctemp$bin_start_incl))]
                
				p<-add_trace(p,  y=ytemp[c(21:33,1:20)], 
                             x=as.data.frame(as.character(paste(xtemp[c(21:33,1:20)])),stringsAsFactors = F), 
                             type="scatter", mode="lines", name = TeamNames[TeamCount])
              } else {
                xtemp <<- as.numeric(fctemp$bin_start_incl[order(as.numeric(fctemp$bin_start_incl))])
                ytemp <<- fctemp$value[order(as.numeric(fctemp$bin_start_incl))]
                
				p<-add_trace(p,  y= ytemp[c(21:33,1:20)], 
                             x=as.data.frame(as.character(paste(xtemp[c(21:33,1:20)])),stringsAsFactors = F), 
                             type="bar", name = TeamNames[TeamCount])
              }
            } else {
              
              ## Compute Scoring by bin ranges
              if (input$REGION %in% "US National") {
                temp1 <- as.data.frame(ili_df_National[((ili_df_National$week==(as.numeric(forecast_week)+1))),])  
              } else {
                temp1 <- as.data.frame(ili_df_HHS[((ili_df_HHS$week==(as.numeric(forecast_week)+1))&(ili_df_HHS$region==sub(".*? (.+)", "\\1", input$REGION))),])  
				
              } 
              targetRow <- which(((fctemp$bin_start_incl<temp1$weighted_ili)&(fctemp$bin_end_notincl>temp1$weighted_ili)))
              
              if (length(targetRow) > 0) {
                SCORE <- sum(fctemp[(targetRow-5):(targetRow+5),]$value)
              } else {
                SCORE <- NA
              }
              COMPARISON_GT_POINTS <<- c(COMPARISON_GT_POINTS, ifelse(is.na(SCORE), 0, SCORE))
              print(paste(  sub(".*? (.+)", "\\1", input$REGION) , ", Week", as.numeric(forecast_week)+1, "Score for Team #", TeamCount," :", COMPARISON_GT_POINTS[TeamCount]))
              
              if (input$PlotType %in% c("Line chart")) {
                p<-add_trace(p, y=fctemp$value[order(as.numeric(fctemp$bin_start_incl))], 
                             x=as.numeric(fctemp$bin_start_incl[order(as.numeric(fctemp$bin_start_incl))]) , 
                             type="scatter", mode="lines", name = TeamNames[TeamCount])
              } else {
                p<-add_trace(p, y=fctemp$value[order(as.numeric(fctemp$bin_start_incl))], 
                             x=as.numeric(fctemp$bin_start_incl[order(as.numeric(fctemp$bin_start_incl))]) , 
                             type="bar", name = TeamNames[TeamCount])
              }
            }
          }
        }
        
      }
    }
    
    ax <- list(
      # zeroline = TRUE,
      # showline = TRUE,
      # mirror = "ticks",
      # gridcolor = toRGB("gray50"),
      # gridwidth = 10,
      # zerolinecolor = toRGB("red"),
      # zerolinewidth = 4,
      # linecolor = toRGB("black"),
      # linewidth = 1
      nticks=48
    )
    
    p <- p %>% layout(xaxis = list(nticks=48), yaxis = list(nticks=10))
   
    p
    
  })
  
  
}

shinyApp(ui, server)