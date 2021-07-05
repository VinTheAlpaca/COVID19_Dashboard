#library(maps)
library(rbokeh)

# the leafMap function: draw the geographic map
leafMap <- function(covid_confirmed, selected_date, selected_country) {
  # calculate the difference between 
  # start date (1/22/20) and the selected date 
  start_date_string = '1/22/20'
  start_date = mdy(start_date_string)
  
  date_diff = as.integer(as.Date(selected_date) - as.Date(start_date))
  
  # declare a new data frame with only the 
  # information that we needed (according to the selected date)
  covid_comList <- data.frame(
    "Province" <- covid_confirmed[,1],
    "Country" <- covid_confirmed[,2],
    "Lat" <- covid_confirmed[,3],
    "Long" <- covid_confirmed[,4],
    "ComfNo" = covid_confirmed[,5 + date_diff]
  )
  
  # rename the columns
  names(covid_comList)[1] <- "Province"
  names(covid_comList)[2] <- "Country"
  names(covid_comList)[3] <- "Lat"
  names(covid_comList)[4] <- "Long"
  names(covid_comList)[5] <- "ComfNo"
  
  
  # delete the rows with country name as worldwide or the confirmed cases equal to 0
  confirmeds <- dplyr::filter(covid_comList[-which(covid_comList$Country == "Worldwide"),], ComfNo > 0)
  if ((selected_country != "Worldwide")){
    confirmeds <- dplyr::filter(confirmeds, Country == selected_country)
  }
  
  confirmeds$Number <- prettyNum(confirmeds$ComfNo, big.mark = ",")
  
  # add a new column to adjust the radius of cycles
  confirmeds$dotSize <- round(log(confirmeds$ComfNo, 1.3), 4)
  
  # assign the format for the textual information
  labels <- sprintf(
    "<strong>%s</strong> (%s)<br/>%s",
    # 
    confirmeds$Country, 
    confirmeds$Province,
    confirmeds$Number
  ) %>% lapply(htmltools::HTML)
  
  # draw the leaflet map
  map <- leaflet(data = confirmeds) %>%
    addProviderTiles(providers$CartoDB.Positron,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addTiles() %>%
    addCircles(lat = ~Lat, 
               lng = ~Long, 
               weight = 1, 
               radius = ~dotSize*10000, 
               color = "red",
               label = labels
    ) %>%
    addEasyButton(
      easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }")))
  
  # set the focus of the map according to the country that visitors picked
  if (selected_country == "Worldwide"){
    setView(map = map, 0, 0, zoom = 1)
  } else {
    flyTo(map = map, confirmeds[!duplicated(confirmeds$Country),4], 
          confirmeds[!duplicated(confirmeds$Country),3], 
          zoom = 4)
  }
  
}


# lineGraph function: draw the line chart and bar chart
# this function currently only draws line chart or bar chart
# if the graphType = "line", it draws line chart. otherwise,
# it will draw a bar chart
lineGraph <- function(covid_confirmed, covid_death, covid_recover, selected_country, graphType = "line"){
  
  # prepare the data
  # combined required data by row
  if (selected_country == "Worldwide"){
    
    lines_data <- data.frame(
      "Date" = mdy(names(covid_confirmed[1, 5:length(covid_confirmed)])),
      "Number" = colSums(covid_confirmed[, 5:length(covid_confirmed)]),
      "Type" = "Confirmed"
    )
    
    lines_data <- rbind(lines_data, data.frame(
      "Date" = mdy(names(covid_death[1, 5:length(covid_death)])),
      "Number" = colSums(covid_death[, 5:length(covid_death)]),
      "Type" = "Died"
    ))
    
    lines_data <- rbind(lines_data, data.frame(
      "Date" = mdy(names(covid_recover[1, 5:length(covid_recover)])),
      "Number" = colSums(covid_recover[, 5:length(covid_recover)]),
      "Type" = "Recorverd"
    ))

  }else{
    lines_data <- data.frame(
      "Date" = mdy(names(covid_confirmed[1, 5:length(covid_confirmed)])),
      "Number" = colSums(filter(covid_confirmed[, 5:length(covid_confirmed)], covid_confirmed[, 2] == selected_country)),
      "Type" = "Confirmed"
    )
    
    lines_data <- rbind(lines_data, data.frame(
      "Date" = mdy(names(covid_death[1, 5:length(covid_death)])),
      "Number" = colSums(filter(covid_death[, 5:length(covid_death)], covid_death[, 2] == selected_country)),
      "Type" = "Died"
    ))
    
    lines_data <- rbind(lines_data, data.frame(
      "Date" = mdy(names(covid_recover[1, 5:length(covid_recover)])),
      "Number" = colSums(filter(covid_recover[, 5:length(covid_recover)], covid_recover[, 2] == selected_country)),
      "Type" = "Recorverd"
    ))
  }
  
  lines_data$Value <- prettyNum(lines_data$Number, big.mark = ",")
  
  # draw the chart according to the graphType parameter
  if (graphType == "line"){
    
    # draw a line chart
    suppressWarnings(figure(width = 400, height = 225,
                            padding_factor = 0.05,
                            xlab = "Date", 
                            ylab = "Number of People",
                            tools = c("pan", "wheel_zoom", "reset"),
                            legend_location = "top_left",
                            toolbar_location = "right") %>%
                       # add points chart layer into the figure
                       ly_points(Date, Number, data = lines_data, size = 5, color = Type, 
                                 hover = c(Date, Value)) %>%
                       # add line chart layer into the figure
                       ly_lines(Date, Number, data = lines_data, col = "grey", group = Type) %>%
                       theme_plot(outline_line_color = "#f8f9fa", 
                                  background_fill_color = "#f8f9fa",# 1F272D
                                  border_fill_color = "#f8f9fa"
                       ) %>%
                       # adjust the grid, axis, legend
                       theme_grid(grid_line_color = "#f8f9fa",
                                  grid_line_width = 0.5) %>%
                       theme_axis(major_label_text_color  = "black",
                                  axis_label_text_color = "black",
                                  axis_label_text_font_style ="bold"
                       ) %>%
                       theme_legend(background_fill_color = "#f8f9fa",
                                    label_standoff = 10,
                                    label_text_font_size = "10pt",
                                    legend_padding = 6.5,
                                    legend_spacing = 3
                       ) %>%
                       y_axis(number_formatter = "numeral", format = "0,000")
                     
    )
  }else{
    
    # draw a bar chart
    suppressWarnings(figure(width = 400, height = 225,
                            padding_factor = 0.05,
                            xlab = "Date", 
                            ylab = "Number of People",
                            tools = c("pan", "wheel_zoom", "reset"),
                            legend_location = "top_left",
                            toolbar_location = "right") %>%
                       
                       # add a bar chart layer into the figure
                       ly_bar(Date, Number,
                              data = lines_data,
                              color = Type,
                              position = "dodge",
                              hover = TRUE,
                              legend = "Type"
                              
                              
                       ) %>%
                       
                       # adjust the grid, axis, legend
                       theme_grid(grid_line_color = "#f8f9fa",
                                  grid_line_width = 0.5) %>%
                       theme_axis(major_label_text_color  = "black",
                                  axis_label_text_color = "black",
                                  axis_label_text_font_style ="bold",
                                  major_label_orientation = 90,
                                  major_label_text_font_size = "6pt"
                       ) %>%
                       theme_legend(background_fill_color = "#f8f9fa",
                                    label_standoff = 10,
                                    label_text_font_size = "10pt",
                                    legend_padding = 6.5,
                                    legend_spacing = 3
                       ) %>%
                       
                       y_axis(number_formatter = "numeral", format = "0,000")
                       
    )
  }
}

