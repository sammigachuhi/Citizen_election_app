# create a simple app that allows citizens to view results of every ward

# first, populate empty columns with some reasonable values. This table will be used to render 
# some data in the app.

library(shiny)
library(leaflet)
library(dplyr)
library(htmltools)
library(rgeos)
library(rgdal)
library(sf)
library(sp)
library(DT)
library(tools)

# load the data
# load the csv file and save it as .RData

load('new_wards.RData')
attach(new_wards)

new_shapefile <- readOGR(dsn = 'wards_shapefile.shp')

# define the ui ----------------------------------------
# the app should show input user name
# should be reactive to show a table showing selected wards, and total voters, alongside votes all candidates
# he/she will choose from the checkboxgroupInput



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = 'user_name',
        label = 'Enter your name',
        value = '',
        placeholder = 'Jesse Otieno'
      ),
      
      # provide option to select wards
      selectizeInput(
        inputId = 'selected_wards',
        label = 'Select ward',
        choices = unique(wards$ward),
        multiple = T
      ),
      
      # provide the user option to select candidates they'd prefer to see
      checkboxGroupInput(
        inputId = 'selected_candidate',
        label = 'Selected candidate(s)',
        choices = c(
          "Total voters" = 'voters',
          "David Waihiga" = 'david_waih',
          "George Wajackoyah" = 'george_waj',
          "Raila Odinga" = 'raila_odin',
          "Reuben Kigame" = 'reuben_kig',
          "William Ruto" = 'william_ru'
        )
      ),
      
      # show a submit button which will be reactive. Pressing it will display map and table on screen
      actionButton(inputId = 'submit',
                   label = 'Submit choices'),
      
      br(),
      br(),
      # button to download selected choices
      toTitleCase("Download Wards data"),
      downloadButton(
        outputId = 'download',
        label = 'Download selected ward results'
      )
    ),
    
    #the mainpanel. This should show the map and the table with ward, voters, and selected candidates results
    mainPanel(leafletOutput(outputId = 'the_wards'),
              DTOutput(outputId = 'choices_data'),
              DTOutput(outputId = 'counties_data'),
              uiOutput(outputId = 'signed_user'))
  )
)

# define the server side -------------------------

server <- function(input, output, session){
  
  updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = 'selected_wards', 
                       choices = unique(wards$ward), server = T)
  
  factpal <- colorFactor(palette = rainbow(47), unique(new_shapefile@data$county))
  
  the_map <- map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = new_shapefile, stroke = T, weight = 0.5,
                fillColor = ~factpal(new_shapefile@data$county), 
                fillOpacity = 0.2, popup = paste0("County: ", new_shapefile$county, "<br>",
                                                  "Sub_county: ", new_shapefile$subcounty, "<br>",
                                                  "Wards: ", new_shapefile$ward
                                                  ))
  the_newmap <- reactive({
    
    if (input$submit) return(the_map)
  })
  
  output$the_wards <- renderLeaflet(the_newmap())
  
  new_table <- eventReactive(input$submit, {
    req(input$selected_wards)
    input$selected_candidate
    
    new_wards %>%
      select(ward, county, subcounty, input$selected_candidate) %>%
      filter(ward %in% input$selected_wards)
  })
  
  output$choices_data <- renderDT(new_table())
  
  my_name <- eventReactive(input$submit, {
    input$user_name
  })
  
  output$signed_user <- renderUI({
    HTML(paste0("My name is: ", "<br>", my_name()))
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("results-", Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(new_table(), file)
    }
  )
  
}


# call the shiny app project -------------------

shinyApp(ui, server)





