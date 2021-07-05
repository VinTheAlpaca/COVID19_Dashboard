# Author: Shengzhe Feng

# This application contains three widgets and four displays:

# Three widgets: 1. Date selector, 
#                2. Country list(can be selected as an input),
#                3. SliderInput.

# Four displays: 1. Geograpich map,
#                2. Trend (line chart and bar chart),
#                3. Data table list,
#                4. Animated bar chart.

####################################################################################
##################################### The code #####################################
####################################################################################

# install the package which are required in this app
# Note that some of the package need to restart the rStudio,
# so if this part failed, please install all the packages manually
list.of.packages <- c("shiny", 
                      "shinydashboard", 
                      "RCurl", 
                      "tidyverse", 
                      "lubridate", 
                      "dplyr", 
                      "rlist", 
                      "rbokeh", 
                      "leaflet", 
                      "DT")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinydashboard)
library(RCurl)
library(tidyverse)
library(lubridate)
library(dplyr)
library(rlist)
#library(maps)
library(rbokeh)
library(leaflet)
#library(graphics)
library(DT)

# set up the workinig direct
setwd("C:/Users/44757/Desktop/Bath/50647_analytic_in_Context/Coursework/Final_Submition/")

# the functions that will be used to make the graphs
source("functions.r")

# set the seed so that in the animated bar chart, 
# the color for coutries will not change
set.seed(123)


##################################### Data Preparation ##################################### 
rawURL_comfirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
rawData_comfirmed <- getURL(rawURL_comfirmed)
covid_confirmed <- read_csv(rawData_comfirmed)
covid_confirmed[nrow(covid_confirmed) + 1, 1:4] = list(0, 'Worldwide', 0, 0)
covid_confirmed[is.na(covid_confirmed)] = 0
covid_confirmed[nrow(covid_confirmed), 5:ncol(covid_confirmed)] = c(colSums(covid_confirmed[,5:ncol(covid_confirmed)]))
covid_confirmed$`Province/State`[covid_confirmed$`Province/State` == 0] <- "NA"

# set the color for each country,
# which will be used in the animated bar chart
colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
agr_conf_list <- aggregate(covid_confirmed[,5:length(covid_confirmed)], by=list(covid_confirmed$`Country/Region`), FUN=sum)
agr_conf_list$colors = c(sample(colors, nrow(agr_conf_list)))


rawURL_death <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
rawData_death <- getURL(rawURL_death)
covid_death <- read_csv(rawData_death)
covid_death[nrow(covid_death) + 1, 1:4] = list(0, 'Worldwide', 0, 0)
covid_death[is.na(covid_death)] = 0
covid_death[nrow(covid_death), 5:ncol(covid_death)] = c(colSums(covid_death[,5:ncol(covid_death)]))
agr_died_list <- aggregate(covid_death[,5:length(covid_death)], by=list(covid_death$`Country/Region`), FUN=sum)
covid_death$`Province/State`[covid_death$`Province/State` == 0] <- "NA"

rawURL_recover <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
rawData_recover <- getURL(rawURL_recover)
covid_recover <- read_csv(rawData_recover)
covid_recover[is.na(covid_recover)] = 0
covid_recover[nrow(covid_recover) + 1, 1:4] = list(0, 'Worldwide', 0, 0)
covid_recover[is.na(covid_recover)] = 0
covid_recover[nrow(covid_recover), 5:ncol(covid_recover)] = c(colSums(covid_recover[,5:ncol(covid_recover)]))
agr_reco_list <- aggregate(covid_recover[,5:length(covid_recover)], by=list(covid_recover$`Country/Region`), FUN=sum)
covid_recover$`Province/State`[covid_recover$`Province/State` == 0] <- "NA"


#transfer all the data into daily data
covid_confirmed_daily = covid_confirmed
covid_death_daily = covid_death
covid_recover_daily = covid_recover

trans1 <-t(apply(covid_confirmed[,5:length(covid_confirmed)], 1, diff))
trans2 <- covid_confirmed[,1:5]
covid_confirmed_daily <- cbind(trans2,trans1)
covid_confirmed_daily[covid_confirmed_daily < 0] <- 0

trans1 <-t(apply(covid_death[,5:length(covid_death)], 1, diff))
trans2 <- covid_death[,1:5]
covid_death_daily <- cbind(trans2,trans1)
covid_death_daily[covid_death_daily < 0] <- 0

trans1 <-t(apply(covid_recover[,5:length(covid_recover)], 1, diff))
trans2 <- covid_recover[,1:5]
covid_recover_daily <- cbind(trans2,trans1)
covid_recover_daily[covid_recover_daily < 0] <- 0


# set the start date and latest date for the date selector 
# (note that the data has been used in this app starts from 1/22/20)
start_date_string = '1/22/20'
start_date = mdy(start_date_string)

newest_date = mdy(names(covid_confirmed[1,length(covid_confirmed)]))



#####################################  User interface ##################################### 
ui <- dashboardPage(
  
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    fluidRow(
      tags$head(
        tags$style(
          HTML(
            ".title {margin: auto; width: 800px}",
            "body {font-family: GillSans}",
            ".fluid {margin: 10 auto; width: 150%; height: 150%}",
            ".b {font color=red}",
            ".content-wrapper, .right-side {
            background-color: #edeff0;
            }"
 
          )
        )
      ),
      
      #The header
      column(8, align = "center",
             
             box(width = 13, height = 75, solidHeader = TRUE, #background = 'blue', #status = "primary",
                 
                 h3(htmlOutput("info", style = "font-family: GillSans; font-weight: bold;")),
                 
                 # tags$a(href="https://www.linkedin.com/in/shengzhe-feng-9515a1193/", 
                 #        "- by Shengzhe Feng",  style = "font-family: GillSans; margin-left: 40em;")
                 # 
             )
      ),
      
      # The date selector
      column(4, #align = "center",
             box(width = 12, height = 75, solidHeader = TRUE, #background = 'blue', #status = "primary",
                 dateInput("selected_date", 
                           width = 300,
                           label = tags$b("Date for Displaying", style = "font-family: GillSans;"),
                           value = newest_date,
                           min = start_date,
                           max = newest_date,
                           format = "yyyy-mm-dd",
                           startview = "day",
                           language = "en"
                           )
             )
             
      ),
      
      # The two instructions
      column(12,
             column(8, 
                    box(width = 11, status = "primary",
                        title = tags$b("Instruction for the Main Display Area",
                                       style = "font-family: GillSans;font-weight: bold;"),
                        collapsible = TRUE,
                        collapsed = TRUE,
                        solidHeader = TRUE,
                        
                        h5(tags$b("Map tab:" ), tags$b("1." ),  "The geographic map displays the 
                  distribution of the confirmed case according to the 
                  date and country that visitors picked;", tags$b("2." ), " Hoover over 
                  the red cycles to get textual information for that area;", tags$b("3." ), " 
                  Wheel in/out to zoom in/out the map or alternatively by 
                  clicking the +/- button; ", tags$b("4." ), " The focus of 
                  the map can also be moved by clicking on the map and 
                  holding and moving the mouse.", 
                           style = "font-family: GillSans;margin-left: 1em; "),
                        
                        h5(tags$b("Trend tab:" ), tags$b("1." ), "The line chart in the 
                  \"Cumulative Cases\" subtab shows the cumulative 
                  cases/deaths/recoveries tendency while the bar chart 
                  in the \"Daily Increase\" subtab indicates the daily 
                  increase of the cases/deaths/recoveries across time 
                  in the country that visitors picked; ", tags$b("2." ), " Hoover over 
                  the dots to get textual information; ", tags$b("3." ), " By clicking the 
                  symbol in the right of the chart, it allows visitors 
                  to use \"Pan\" and \"Wheel Zoom\" tools on these two charts.", 
                           style = "font-family: GillSans;margin-left: 1em; "),
                        
                        h5(tags$b("Table tab:" ), tags$b("1." ), "The table shows the detail of 
                  confirmed cases, deaths, recoveries as well as death 
                  rate and recovery rate in a specific country and date that visitors picked; ", tags$b("2." ), 
                           "This table list is in decreasing order of confirmed cases by default, 
                  it can also be reordered by clicking the button near each column name.", 
                           style = "font-family: GillSans;margin-left: 1em; "),
                        
                        h5(tags$b("Animated Bar tab:" ), tags$b("1." ), "The animated bar char 
                  demonstrate the top 10 countries with the most confirmed 
                  cases across time by using animated sliderinput.", 
                           style = "font-family: GillSans;margin-left: 1em; ")
                        
                    )
             ),
             
             column(4, 
                    box(width = 12, status = "primary",
                        title = tags$b("Instruction for the Widgets",
                                       style = "font-family: GillSans;font-weight: bold;"),
                        collapsible = TRUE,
                        collapsed = TRUE,
                        solidHeader = TRUE,
                        
                        h5(tags$b("Date Selector:" ), tags$b("1." ), "It allows visitor to choose a 
                         date and according to the date selected, the List of Countries
                         and the Main display area will show the information in that day.", 
                           style = "font-family: GillSans;margin-left: 1em; "),
                        
                        h5(tags$b("List of Countries:" ), tags$b("1." ),  "It shows the number of confirmed 
                         cases/deaths/recoveries of each country (note that the \"Worldwide\"
                         option counts for all the cases around the world); ", tags$b("2." ), " visitor can 
                         select a single country by clicking one of the options on the list; ", tags$b("3." ),  
                           "if nothing is selected, it will use \"Worldwide\" option which is also the default value; ", tags$b("4." ),
                           "The list is in decreasing order of confirmed cases by default, 
                         it can also be reordered by clicking the button near each column name.", 
                           style = "font-family: GillSans;margin-left: 1em; "),
                        
                        h5(tags$b("Date Sliderinput:" ), tags$b("1." ), "This widget is in the ", tags$b("Animated Bar" ), 
                           "tab as it is only for controling the date parameter of 
                         the animated bar chart; ", tags$b("2." ), " the animation can be actived by 
                         clicking the play button just under the slider or vistors 
                         can also manually choose the date.", 
                           style = "font-family: GillSans;margin-left: 1em; "),
                    )
                    
             )
      ),
      
      # The main display area
      column(8,
             
             box(width = 13, status = "primary", height = 470, solidHeader = TRUE, #background = 'navy',
                 
                 tabsetPanel(
                   
                   # The geographic map
                   tabPanel(title = "Map",
                            status = "primary",
                            leafletOutput("map", height = 410)),
                   
                   # The line chart and bar char
                   tabPanel(title = "Trend", 
                            status = "primary", 
                            tabsetPanel(type = "pills", 
                                        tabPanel(title = "Cumulative Cases", 
                                                 rbokehOutput("lines_cumulative", width = "650px")),
                                        tabPanel(title = "Daily Increase",
                                                 rbokehOutput("lines_daily", width = "650px")))
                   ),
                   
                   # The table list
                   tabPanel(title = "Table", 
                            status = "primary",
                            DT::dataTableOutput("table"),
                            style = "height:410px; width:740px; overflow-y: scroll;"
                   ),
                   
                   # The animated bar chart
                   tabPanel(title = "Animated Bar", 
                            status = "primary",
                            
                            sliderInput("slider_date",
                                        label = NULL,
                                        min = start_date,
                                        max = newest_date,
                                        value = start_date,
                                        animate = animationOptions(interval = 700, 
                                                                   loop = FALSE, 
                                                                   playButton = NULL,
                                                                   pauseButton = NULL),
                                        width = "700px"
                            ),
                            
                            plotOutput("animate", 
                                       width = "700px", 
                                       height = "310px"))
                 )
             )
      ),
      
      # The List of countries
      column(4, align = "center",
             box(width = 12, height = 470, solidHeader = TRUE, #background = 'navy', #status = "primary",
                 title = tags$b("Cases/Deaths/Recoveries by Country", 
                                style = "font-family: GillSans;"), 
                  
                 DT::dataTableOutput("trace_table"),
                 style = "height:425px; width:320px; overflow-y: scroll;"
             )
      ),
      
      column(8),
      
      # The data reference
      column(4, 
             tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data", 
                    "Covid-19 data are daily updated from Johns Hopkins CSSE.")
             
      )
    )
  )
)



#####################################  Server logic ##################################### 
server <- function(input, output) {
  
  # get the selected country from customer
  selected <- reactive({!is.null(input$trace_table_rows_selected)}) 
  
  val <- reactiveValues()
  
  # do some calculations which will be needed in mutiple outputs
  observe({
    
    # transfer the selected date into the column number
    val$date_diff <- as.integer(as.Date(input$selected_date) - as.Date(start_date))
    
    # combind the confirmed, died and recovery list by column
    aggregated_list = cbind(agr_conf_list[, c(1, val$date_diff + 2)], 
                            agr_died_list[, val$date_diff + 2],  
                            agr_reco_list[, val$date_diff + 2])
    
    # sort the combined aggregated list
    sorted_list <- aggregated_list[order(aggregated_list[, 2], decreasing = TRUE), ]
    names(sorted_list)[1] = "Countries"
    names(sorted_list)[2] = "Confirmed"
    names(sorted_list)[3] = "Died"
    names(sorted_list)[4] = "Recovered"
    
    val$sorted_list <- sorted_list
    
    # if visitor cancled his/her choice, set Worldwide as the default value
    # if visitor has chosen a country, get the selected country
    if (selected()){
      val$selected_country <- val$sorted_list[input$trace_table_rows_selected, 1]
    }else{
      val$selected_country = "Worldwide"
    }
    
  })
  
  # output the list of countries
  output$trace_table <- renderDataTable({
    
    # only single row can be selected and the default value is the first row
    datatable(val$sorted_list,
              options = list(paging = FALSE),
              rownames = FALSE, selection = list(mode = "single", selected = 1))
    
  })
  
  # output the header
  output$info <- renderText({
    
    cur_total_com = filter(covid_confirmed, covid_confirmed[,2] == "Worldwide")[, 5 + val$date_diff]
    cur_total_com = prettyNum(cur_total_com, big.mark = ",")
    paste("COVID-19 | The Global Confirmed Cases" ," at ", input$selected_date, ": <font color=\"#FF0000\">", cur_total_com, "</font>")
  })
  
  # call the leafmap function to draw the geographic map
  # see the leafMap function in the "functions.r" file
  output$map <- renderLeaflet({
    leafMap(covid_confirmed, input$selected_date, val$selected_country)
  })
  
  # call the lineGraph function to draw the line chart
  # see the lineGraph function in the "functions.r" file
  output$lines_cumulative <- renderRbokeh({
    lineGraph(covid_confirmed, covid_death, covid_recover, val$selected_country)
  })
  
  # call the lineGraph function with the parameter 
  # graphType = "bar" to draw the bar chart
  # see the lineGraph function in the "functions.r" file
  output$lines_daily <- renderRbokeh({
    lineGraph(covid_confirmed_daily, covid_death_daily, covid_recover_daily, val$selected_country, graphType = "bar")
  })
  
  # output the dataTable 
  output$table <- renderDataTable({
    
    # select rows with the same country as visitors picked  
    filter_conf_list <- filter(covid_confirmed[,c(1, val$date_diff + 5)], covid_confirmed[,2] == val$selected_country)
    filter_died_list <- filter(covid_death[,c(1, val$date_diff + 5)], covid_death[,2] == val$selected_country)
    filter_reco_list <- filter(covid_recover[,c(1, val$date_diff + 5)], covid_recover[,2] == val$selected_country)
    
    # some country has different number of rows for cases, 
    # deaths and recoveries
    if (nrow(filter_conf_list) == nrow(filter_died_list)) {
      filter_list <- cbind(filter_conf_list, filter_died_list[, 2])
      if (nrow(filter_conf_list) == nrow(filter_reco_list)){
        filter_list <- cbind(filter_list, filter_reco_list[, 2])
      }else{
        filter_list$Recovered <- NA
      }
    }else{
      filter_list$Died <- NA
      if (nrow(filter_conf_list) == nrow(filter_reco_list)){
        filter_list <- cbind(filter_list, filter_reco_list[, 2])
      }else{
        filter_list$Recovered <- NA
      }
    }
    
    # sort the list by the decreasing order of confirmed cases 
    sorted_list_country <- filter_list[order(filter_list[,2], decreasing = TRUE), ]
    names(sorted_list_country)[1] = "City/Region"
    names(sorted_list_country)[2] = "Confirmed"
    names(sorted_list_country)[3] = "Died"
    names(sorted_list_country)[4] = "Recovered"
    
    # calculate the death rate and recovery rate
    sorted_list_country$`Death_Rate(%)` <-round(sorted_list_country[,3]/sorted_list_country[,2]*100, 2)
    sorted_list_country$`Recovery_Rate(%)` <-round(sorted_list_country[,4]/sorted_list_country[,2]*100, 2)
    
    # draw the data table
    datatable(sorted_list_country,
              options = list(paging = FALSE),
              rownames = FALSE, selection = "single"
    )
    
  })
  
  # output the animated bar chart
  output$animate <- renderPlot({
    date_diff <- as.integer(as.Date(input$slider_date) - as.Date(start_date))
    
    # delete the worldwide row and select only the columns of country, 
    # selected date, and the color
    conf_list <- data.frame(
      "country" <- agr_conf_list[-which(agr_conf_list$Group.1 == "Worldwide"), 1],
      "number" <- agr_conf_list[-which(agr_conf_list$Group.1 == "Worldwide"), 2 + date_diff],
      "colors" <- agr_conf_list[-which(agr_conf_list$Group.1 == "Worldwide"), ncol(agr_conf_list)]
    )
    
    # rename the columns
    names(conf_list)[1] <- "country"
    names(conf_list)[2] <- "number"
    names(conf_list)[3] <- "colors"
    
    # order the list
    conf_list <- conf_list[order(conf_list$number, decreasing = TRUE),]
    
    # adjust the margin of the chart
    par(mar = c(5,8,4,2) + 0.1)
    
    # plot the bar chart
    bar <- barplot(height = conf_list[10:1,2], 
                   main = "Top 10 Countries with Most Cases",
                   names.arg = conf_list[10:1,1], 
                   horiz = TRUE, 
                   las=1,
                   col = c(as.character(conf_list[10:1,3])),
                   xlim = c(0, max(conf_list[10:1,2]*1.1))
    ) 
    # add textual label (the number of confirmed cases)
    text(conf_list[10:1,2]*1.05 +10, bar, labels=as.character(conf_list[10:1,2]))
    # adjust the margin again
    par(mar = c(5,8,4,2) + 0.1)
  })
  
}

#####################################  Run app ##################################### 
shinyApp(ui, server)