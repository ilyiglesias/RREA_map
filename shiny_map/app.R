# to run, select "Run App"
# will eventually need to set this up with shinyapps.io token (from account i set up at shinyapps.io)
# install.packages("rsconnect")


# question: I think I may wish to put the code to get all of the variables loaded, data input etc. here prior to adding it to the shiny code. 

library(shiny)

################ Define UI for application ######################
ui <- fluidPage(
   
   # Application title
       titlePanel(h1("Rockfish Recruitment and Ecosystem Assessment Catch Data", align= "center", style = "font-family: 'times'")),
  
  # Create select box -- species common names
      sidebarLayout(
          sidebarPanel(
            selectInput(inputId= "select", label = h3("Species:", style = "font-family: 'times'", 
            helpText(h6("Select species by common name- note that not all species were enumerated for each year", style= "font-family: 'times'" , align="center"))), 
            choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),  # will need to change these choices to reflect spp. common names
            selected = 1),
        
        # insert image--- cruise catch 
            img(src = "catch.jpg", width= 100, height= 100, align= "center"),
        
   # Sidebar- Slider --- survey year
            sliderInput(inputId = "slider",
                     label= h3("Survey Year:", style = "font-family: 'times'"),
                     min = 1985, #would like to make this based on data available for a given spp selection (ie. min(species_yr))
                     max = 2017, # again would like to make this general as database will be updated every year (ie max(species_yr))
                     value = 1),
                     helpText(h6("Survey began in 1983- but the spatial coverage has varied greatly since that time"), style= "font-family: 'times'" , align="center")), 
      
    # Show a plot of the generated distribution
      mainPanel(plotOutput(outputId= "map") # name output object
   )
)
)

#################### Define server logic #########################
server <- function(input, output) {
  
  # to access input from select box-- species
  output$value <- renderPrint({ input$select }) # note that inputId for selectInput (select box) was "select"
  
  
  # to access input from slider -- year
  output$value <- renderPrint({ input$slider }) # note inputId "slider"
  
  # create plot based on inputs from UI 
  
  output$map <- renderPlot({ 
      
    # this is where I eventually want to insert the code for constructing a map based on inputs in UI
   })
}



########################## Run the application ##################
shinyApp(ui = ui, server = server)

