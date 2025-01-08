# Title: Crash analysis 
# Description:
# Author: Yasna Rahmani 
# Date: 12/07/2024


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)    # data wrangling and graphics
library(lubridate)    # for working with dates
library(leaflet)      # web interactive maps
library(plotly)       # web interactive graphics


# =======================================================
# Import data
#
# For demo purposes, in this "template" we use storms data
# (but you will have to replace this with the crash data)
#
# Uncomment the lines below in order to import the data!!!
# We're assuming the CSV file is in the working directory of your app.
# =======================================================
all_crashes = read_csv("crashes_california_2014_2023.csv")

# export csv file (containing only a couple of years: 2021-2022)
all_crashes |> 
  filter(ACCIDENT_YEAR %in% 2021:2022) |>
  write_csv(file = "crashes_california_2021_2022.csv")

crashes = read_csv(
  file = "crashes_california_2021_2022.csv",
  col_types = list(
    col_double(),    #  1) CASE_ID
    col_double(),    #  2) ACCIDENT_YEAR
    col_date(),      #  3) COLLISION_DATE
    col_double(),    #  4) COLLISION_TIME
    col_double(),    #  5) HOUR
    col_integer(),   #  6) DAY_OF_WEEK
    col_character(), #  7) WEATHER_1
    col_character(), #  8) WEATHER_2
    col_character(), #  9) STATE_HWY_IND
    col_character(), # 10) COLLISION_SEVERITY
    col_integer(),   # 11) NUMBER_KILLED
    col_integer(),   # 12) NUMBER_INJURED
    col_integer(),   # 13) PARTY_COUNT
    col_character(), # 14) PCF_VIOL_CATEGORY
    col_character(), # 15) TYPE_OF_COLLISION
    col_character(), # 16) ROAD_SURFACE
    col_character(), # 17) ROAD_COND_1
    col_character(), # 18) ROAD_COND_2
    col_character(), # 19) LIGHTING
    col_character(), # 20) PEDESTRIAN_ACCIDENT
    col_character(), # 21) BICYCLE_ACCIDENT
    col_character(), # 22) MOTORCYCLE_ACCIDENT
    col_character(), # 23) TRUCK_ACCIDENT
    col_character(), # 24) NOT_PRIVATE_PROPERTY
    col_character(), # 25) ALCOHOL_INVOLVED
    col_character(), # 26) COUNTY
    col_character(), # 27) CITY
    col_character(), # 28) PO_NAME
    col_double(),    # 29) ZIP_CODE
    col_double(),    # 30) POINT_X
    col_double()     # 31) POINT_Y
  ))

county_names <- c("Alameda", "Alpine", "Amador", "Calaveras", "Inyo", 
                  "Mariposa", "Mono", "Tuolumne", "Butte", "Colusa", 
                  "Glenn", "Tehama", "Trinity", "Contra Costa", 
                  "Del Norte", "Lassen", "Modoc", "Plumas", 
                  "Siskiyou", "El Dorado", "Fresno", "Humboldt", 
                  "Imperial", "Kern", "Kings", "Lake", "Mendocino", 
                  "Los Angeles", "Madera", "Marin", "Merced", 
                  "Monterey", "San Benito", "Napa", "Nevada", 
                  "Sierra", "Orange", "Placer", "Riverside", 
                  "Sacramento", "San Bernardino", "San Diego", 
                  "San Francisco", "San Joaquin", "San Luis Obispo", 
                  "San Mateo", "Santa Barbara", "Santa Clara", 
                  "Santa Cruz", "Shasta", "Solano", "Sonoma", 
                  "Stanislaus", "Sutter", "Yuba", "Tulare", 
                  "Ventura", "Yolo")

poverty_rates <- c(15.4, 13.4, 13.4, 13.4, 13.4, 
                   13.4, 13.4, 13.4, 18.6, 17.9, 
                   17.9, 17.9, 17.9, 14.3, 15.6, 
                   15.6, 15.6, 15.6, 15.6, 10.7, 
                   18.0, 18.9, 18.8, 17.7, 14.5, 
                   19.3, 19.3, 23.0, 17.5, 17.9, 
                   16.8, 17.9, 17.9, 15.5, 18.6, 
                   18.6, 20.4, 12.5, 17.6, 16.4, 
                   17.2, 19.3, 18.4, 15.6, 17.2, 
                   16.5, 22.0, 16.0, 21.7, 18.3, 
                   14.6, 15.8, 14.2, 15.1, 15.1, 
                   19.0, 16.9, 19.9)

county_poverty <- tibble(
  COUNTY = toupper(county_names),
  PovertyRate = poverty_rates
)

# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Interactive exploration of Crashes in California"),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # ---------------------------------------------
      # input widgets of first tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Exploratory Analysis"),
        # replace with your widgets
        selectInput(
          inputId = "facet_by",
          label = "Facet by:",
          choices = c("Road Condition" = "ROAD_COND_1", "Road Surface" = "ROAD_SURFACE"),
          selected = "ROAD_COND_1"),
        dateInput("start_date", "Start Date", value = "2021-01-01"),
        dateInput("end_date", "End Date", value = "2021-12-31"),
        selectInput(
          inputId = "selected_county",     
          label = "Select a County:",      
          choices = unique(crashes$COUNTY), 
          selected = NULL                   
        ),
      ), # closes 1st panel
      
      # ---------------------------------------------
      # input widgets of second tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==2",  
        h4("Map"),
        
        sliderInput(
          inputId = "selected_year",
          label = "Select Year:",
          min = min(crashes$ACCIDENT_YEAR),  
          max = max(crashes$ACCIDENT_YEAR),  
          value = max(crashes$ACCIDENT_YEAR), 
          step = 1,
          sep = ""
        ),
        
        selectInput(
          inputId = "selected_cities",
          label = "Select a City:",
          choices = unique(crashes$PO_NAME), 
          selected = NULL,
          multiple = TRUE  
        ),
        
        selectInput(
          inputId = "selected_violations",
          label = "Select Violation Categories:",
          choices = unique(crashes$PCF_VIOL_CATEGORY), 
          selected = NULL,
          multiple = TRUE  
        ),
        
      
        radioButtons(
          inputId = "color_coding",
          label = "Color Code Crashes By:",
          choices = c("Type of Collision" = "TYPE_OF_COLLISION", 
                      "Collision Severity" = "COLLISION_SEVERITY"),
          selected = "TYPE_OF_COLLISION"
        )
      ),
      
      # ---------------------------------------------
      # input widgets of third tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==3",
        h4("More Analysis"),
        # replace with your widgets
        sliderInput(
          inputId = "selected_year2",
          label = "Select Year:",
          min = min(crashes$ACCIDENT_YEAR),  
          max = max(crashes$ACCIDENT_YEAR),  
          value = max(crashes$ACCIDENT_YEAR), 
          step = 1,
          sep = ""
        ),
      ) # closes 3rd panel
      
    ), # closes sidebarPanel
    
    
    # -------------------------------------------------------
    # Main Panel with 3 tabsets: 
    # tab1: exploratory analysis
    # tab2: map of crashes
    # tab3: table
    # -------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # first tab (graphic)
        tabPanel(title = "Explore",
                 value = 1,
                 plotlyOutput(outputId = "plot1"),
                 hr(),
                 plotOutput(outputId = "plot2"),
                 hr(),
                 plotOutput(outputId = "plot3")),
        # second tab (map)
        tabPanel(title = "Map",
                 value = 2,
                 leafletOutput("map", height = 600)),
        # third tab (other)
        tabPanel(title = "Poverty Rates and Crash Data",
                 value = 3,
                 plotlyOutput(outputId = "plot4"),
                 hr(),
                 plotOutput(outputId = "plot5")),
        # selected tab
        id = "tabselected"
        
      ) # closes tabsetPanel
    ) # closes mainPanel
    
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  # ------------------------------------------------
  # Output for first TAB (i.e. summary plot)
  # (adapt code to your analysis)
  # ------------------------------------------------
  output$plot1 <- renderPlotly({
    # the following code is for demo purposes only; adapt it!!!
    year_start = year(input$start_date)
    year_end = year(input$end_date)
    year_seq = seq(year_start,year_end)
    
    selected_years= crashes |> 
      filter(year(COLLISION_DATE) %in% year_seq,
             COUNTY==input$selected_county)
      
    top_crash_types= selected_years|>
      group_by(TYPE_OF_COLLISION) |>
      summarize(frequency= n())
    
    top_crash_types |> 
      slice_head(n=5)|>
      ggplot(aes(x= TYPE_OF_COLLISION, y=frequency, fill="orange")) +
      geom_col() +
      labs(title = paste0("Top 5 most common crash types in"," ", year_start ," ","through"," ", year_end),
           x= "Type of Collision",
           y= "Crash Counts") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7))
  
  })

  
  output$plot2 <- renderPlot({
    year_start = year(input$start_date)
    year_end = year(input$end_date)
    year_seq = seq(year_start, year_end)
    
    selected_years <- crashes |>
      filter(year(COLLISION_DATE) %in% year_seq,
             COUNTY==input$selected_county) |>
      group_by(ACCIDENT_YEAR) |>
      summarise(DUI_Collisions = n())
    

    ggplot(selected_years, aes(x = ACCIDENT_YEAR, y = DUI_Collisions)) +
      geom_line(color = "skyblue", size = 1) +
      geom_point(color = "purple", size = 2) +
      labs(
        title = paste0("DUI Collisions from ",year_start, " ", "to", " ", year_end),
        x = "Year",
        y = "Number of DUI Collisions"
      ) +
      theme_classic()
  })
  
  
  output$plot3 <- renderPlot({

    year_start = year(input$start_date)
    year_end = year(input$end_date)
    year_seq = seq(year_start, year_end)
    facet_type = input$facet_by
    
    selected_years <- crashes |>
      filter(year(COLLISION_DATE) %in% year_seq,
             COUNTY==input$selected_county)
    
    faceted_table <- selected_years |>
      group_by(ACCIDENT_YEAR, !!sym(facet_type)) |>  
      summarise(Collision_Count = n(), .groups = "drop") |> 
      rename(Facet_Var = !!sym(facet_type)) 

    ggplot(faceted_table, aes(x = ACCIDENT_YEAR, y = Collision_Count)) +
      geom_col(position = "dodge", fill= "purple") +
      facet_wrap(~Facet_Var, scales = "free_y") + 
      labs(
        title = paste("Collision Counts by", ifelse(facet_type == "ROAD_COND_1", "Road Condition", "Road Surface")),
        x = "Year",
        y = "Collision Count"
      ) +
      theme_classic() +
      theme(
        legend.position = "none", 
        strip.text = element_text(size = 10, face = "bold")
      )
  })
  
  
  # -----------------------------------------------
  # Output for second TAB (i.e. map)
  # (adapt code to make your map of crashes)
  # -----------------------------------------------
  output$map <- renderLeaflet({
    filtered_data <- crashes %>%
      filter(
        ACCIDENT_YEAR == input$selected_year,  
        PO_NAME %in% input$selected_cities,     
        PCF_VIOL_CATEGORY %in% input$selected_violations  
      )
    
  
    color_column <- if (input$color_coding == "TYPE_OF_COLLISION") {
      filtered_data$TYPE_OF_COLLISION
    } else {
      filtered_data$COLLISION_SEVERITY
    }
    
    palette <- colorFactor(palette = "Dark2" , domain = unique(color_column))
    
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~POINT_X,
        lat = ~POINT_Y,
        color = ~palette(color_column), 
        popup = ~paste0(
          "Date: ", COLLISION_DATE, "<br>",
          "Type: ", TYPE_OF_COLLISION, "<br>",
          "Severity: ", COLLISION_SEVERITY, "<br>",
          "City: ", PO_NAME
        ),
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.7
      ) %>%
      addLegend(
        "bottomright",
        pal = palette,
        values = color_column,
        title = if (input$color_coding == "TYPE_OF_COLLISION") "Type of Collision" else "Collision Severity",
        opacity = 0.7
      )
  })
  # -----------------------------------------------
  # Output for third TAB (i.e. table of fires)
  # (adapt code to show a table of fires in selected year)
  # -----------------------------------------------
  output$plot4 <- renderPlotly({
    # Ensure valid filtering
    selected_year= crashes |>
      filter(ACCIDENT_YEAR == input$selected_year2)
    
    pedestrians=selected_year |>
      filter(TYPE_OF_COLLISION=="vehicle/pedestrian")
    
    
    # Group pedestrian crashes by county
    avg_pedestrian_per_county <- pedestrians |>
      group_by(COUNTY) |>
      summarise(Pedestrian_Crashes = n())
    
    Poverty_added <- inner_join(avg_pedestrian_per_county, county_poverty, by = "COUNTY")
    
    plot <- ggplot(Poverty_added, aes(x = PovertyRate , y = Pedestrian_Crashes )) +
      geom_point(color = "purple", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(
        title = "Do counties with higher poverty rates experience more pedestrian crashes?",
        x = "Poverty Rate (%)",
        y = "Pedestrian Crashes"
      ) +
      theme_minimal()
    
    ggplotly(plot)  })
  

  
} # closes server



# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
