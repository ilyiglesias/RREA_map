# Hello and welcome to this SHINY APP!! 
# The following app creates a map for a given selection of species and year using data collected as part of the Rockfish Recruitment and Ecosystem Assessment Cruise.
# This app was developed by ilysa.iglesias@noaa.gov Note to self: these files are also stored (and backed up on git) under rrea_map (rrea_map: rproject- for loop to export pdfs), rrea_map/shiny_map (rproject to run shiny app)

# In order to run this app, please select the "Run App" button to your upper right from within Rstudio- this should produce a separate window
  # there are 2 selection options on the left pane for species and year. Select your species of interest from the drop down menu, and then slide to the year of interest
  # a map should display at right once you have made your selection. If not, either there were none of that species collected for that year, or that spp was not enumerated or identified that year 

# The following code should run automatically, if for some reason it does not, check to ensure that you have all of the required packages installed (see run_before_shiny.R)

# First,To run code to upload data and create basemaps via:
   source("run_before_shiny.R")

# values for select box (species common names)
select_box_spp <- catch %>%
                    select(COMMON_NAME)%>%
                    distinct()%>%
                    arrange(COMMON_NAME)
  
  
# values for slider (years of survey)                 
slider_years<- catch %>%
                      select(year)%>%
                      distinct()%>%
                      arrange(year)



 ############################################################### S H I N Y #########################################################################################
library(shiny)

################ Define UI for application ######################
ui <- fluidPage(
   
   # Page title
       titlePanel(h1("Rockfish Recruitment and Ecosystem Assessment Catch Data", align= "center", style = "font-family: 'times'")),
  
  # Create select box -- species common names
      sidebarLayout(
          sidebarPanel(
            selectInput(inputId= "select", 
                    label = h3("Species:", style = "font-family: 'times'", 
                    helpText(h6("Select species by common name- note that not all species were enumerated for each year", style= "font-family: 'times'" , align="center"))), 
                    choices = (select_box_spp),  # list of unique spp names from catch df
                    selected = 1),
        
        # insert image--- cruise catch 
            #img(src = "catch.jpg", width= 100, height= 100, align= "center"),
           
   # Sidebar- Slider --- survey year
             sliderInput(inputId = "slider",
                     label= h3("Survey Year:", style = "font-family: 'times'"),
                     min = min(slider_years), #min year from catch df (selected only years in slider_years) survey started in 1983
                     max = max(slider_years), #max year from catch df (selected years in slider_years above) latest date 2017- note this is not spp specific 
                     value = 1, sep=""), #  note sep removes , from values so they look like years
                     helpText(h6("Survey began in 1983- but the spatial coverage has varied greatly since that time"), style= "font-family: 'times'" , align="center")), 
      
    # Show a plot of the generated distribution
      mainPanel(plotOutput(outputId= "map", width = "100%"),
        helpText(h6("If map does not display with selection, then species was not identified, enumerated or caught in that year. + represent sites that were trawled, but selected spp was not caught", style="font-family: 'times'" ,align= "center"))
   )
)
)

#################### Define server logic #########################
server <- function(input, output) {
  
  # create plot based on inputs from UI 
  
  output$map <- renderPlot({ 
      
    # input from UI :
    spp<- input$select # this is the spp selected from the select box
    years<- input$slider # this is the year selected on the year slider bar
    
    # select species
    for(i in 1:length(spp)) {
      spp.i<- spp[i] 
      species<-catch %>%
        filter(COMMON_NAME==spp.i) %>% # filter by a specific spp within our catch df
        group_by(year, STATION, COMMON_NAME)%>% # group by year, station, common_name (other columns maintained)         
        summarise(TOTAL_MEAN= mean(TOTAL_NO), se_station= sd(TOTAL_NO)/sqrt(n()))%>% # need to take the mean catch per station as some stations were sampled multiple times  per year. Also calculated the standard error for each station, which gave an NA if there was only one station per year
        left_join(select(stations, STATION, LATITUDE, LONGITUDE, STATION_BOTTOM_DEPTH), by= "STATION") # add lat long data for each station with particular spp
      
      # for given spp, select year          
      for(j in 1:length(years)){
        year.j<- years[j] 
        species_yr <- species %>%
          filter(year==year.j)
        
        # create df of stations that were trawled but didn't capture any spp.i in year.j 
        nocatch <- stations_yr %>% # selects from our station df which lists each station surveyed per year
          filter(year== year.j)%>% # filter this database for our specific year
          filter(!(STATION %in% species_yr$STATION)) # selects only those values from our station table that DONT match the stations within species_yr (those sations where a given species WERE found)
        
        
        # not plotting map for years where 0 of a spp were captured (could be because spp wasn't enumerated in that year, OR none were captured-- see notes for clarification)
        
        if(nrow(species_yr) == 0){
          print(paste("Zero", spp.i, "caught in", year.j, "or species not enumerated in this year",sep= " "))
        }else{
          species_yr_sf<- st_as_sf(species_yr, coords = c("LONGITUDE", "LATITUDE"), crs=4326)# note crs code is for WGS84 crs=4326
          
          nocatch_sf<- st_as_sf(nocatch, coords = c("LONGITUDE", "LATITUDE"), crs=4326) # convert no catch point layer into sf feature
          
          
    plot_map<- ggplot()+
            #geom_raster(data = basemap, mapping = aes(X,Y, fill=elevation), alpha=0.7, show.legend = FALSE)+ # add basemap layer of ocean and terrain
            #scale_fill_gradient(low = "black", high = "white")+  # ...with black and white color pattern    
            geom_sf(data=ca, aes(), fill="antiquewhite3", alpha=0.9, inherit.aes=FALSE)+ # plot simple .shp layer of states
            geom_sf(data= nocatch_sf,aes(), alpha= 0.7, shape=43, size=3, inherit.aes = FALSE)+ # add survey locations where no catch occured
            geom_sf(data = species_yr_sf, aes(size= TOTAL_MEAN), fill= "cyan", color= "black", pch=21, alpha=0.6, inherit.aes = FALSE, show.legend = "point") + #inhereit.aes removes an error message and this adds our species specific catch data for a given year-- note that the point size is relative and varies per year
            theme(legend.key = element_blank(), panel.grid.major = element_line(colour = "transparent"), panel.background = element_blank())+ # removes boxes around legend symbols, removes grid lines on map and background color for our polygon layer
            labs(size='Catch per trawl') + # legend title
            coord_sf(xlim = c(-126, -116), ylim = c(32, 42), crs=4326)+ # note: this sets the projection info to latlong, WGS84 (to check use st_crs) this also sets a static map zoom to better facilitate comparisons bw years but can be changed based on min, max values of coordinates in given querry
            xlab("Longitude")+
            ylab("Latitude")+
            ggtitle(paste0(spp.i, " ", year.j, sep=" ")) # main title
            print(plot_map)
        }  
      }
    }
   })
  }




########################## Run the application ##################
shinyApp(ui = ui, server = server)

############################ Publish on server ####################
#rsconnect::setAccountInfo(name='ilyiglesias',
 # token='C7D0B7F515311744426ED8A7D0F5D79A',
  #secret='YdDEQ0sH5CWwRMV8UKOal//Tlag/C0KbLtHTBBU0')

#rsconnect::showLogs()
