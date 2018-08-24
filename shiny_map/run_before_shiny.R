# run code before shiny app-- this should automatically load when you select "run app" from the app.R script
# currently running without raster baselayer
############################################# LOAD AND ORGANIZE DATA INPUTS ###################################################################################
# note the following code is not very annotated- please see final_forloop for detailed description of functions 
# Load libraries-- if you are missing any of these packages, please run the following code:
#install.packages(c("tidyverse", "sf", "ggplot2", "lubridate"))
library(tidyverse) # this isn't working on my pc for some reason- going to install packages seperately
library(sf)
library(ggplot2)
library(lubridate)
### Read in data files 
juv_catch<- read.csv("data/dbo_JUV_CATCH.csv", header=TRUE) # read in juvenile catch data-- note that for this app i am adding data/ because data is now stored in data folder
juv_haul <- read.csv("data/dbo_JUV_HAUL.csv", header=TRUE) # read in haul data- including station info
species_codes <- read.csv("data/dbo_SPECIES_CODES.csv", header=TRUE) # read in species code table
swfsc_stations<- read.csv("data/dbo_STANDARD_STATIONS.csv", header = TRUE) # read in station information including lat long data
nwfsc_stations<- read.csv("data/dbo_NWFSC_STANDARD_STATIONS.csv", header = TRUE) # NWFSC stations

# combine files and select info needed for querry
juv_haul <- juv_haul %>% # df that we are working with
  mutate(time= parse_date_time(juv_haul$HAUL_DATE, orders = "mdy HMS")) %>% # this creates a new column of converted dates- from HAUL_DATE
  mutate(year= year(time)) 
catch <- juv_catch %>%  # our main df that we want to add data to
  left_join(select(species_codes, -PACFIN_CODE, -MATURITY_CODES), by="SPECIES")%>% # joined specific columns from our second df, species_codes -col
  left_join(select(juv_haul, CRUISE, HAUL_NO, STATION, DEPTH_STRATA, BOTTOM_DEPTH, PROBLEM, STANDARD_STATION, time, year), by=c("CRUISE", "HAUL_NO")) %>% #  merge station information from juv_haul to juv_catch based on cruise and haul number
  filter(STANDARD_STATION==1) # this selects ONLY those stations that were "standard_stations" note: only those rows with standard_station==1, 0= problem!!!
stations_yr <- juv_haul %>% # creates new column of just years using the year() function- extracts year info from time column
  filter(STANDARD_STATION==1) %>% # only include standard stations (ie standard station=1, not 0)- otherwise problem or special tow- don't want
  group_by(year, STATION) %>% # group these data by year and station
  summarise() # col= year, station. For each year a list of (non-repeating) stations sampled and fished correctly

# combine station information and reformat as necessary
swfsc_stations<- swfsc_stations %>%
                  select(-ACTIVE)%>%
                  mutate(SCI_CENTER= "SWFSC")
nwfsc_stations$LATITUDE <- as.numeric(unlist(nwfsc_stations$LATITUDE)) # convert latitude values from integer to numeric- note need "unlist" to avoid error message
nwfsc_stations <-mutate(nwfsc_stations, SCI_CENTER="NWFSC")
stations<- rbind(swfsc_stations, nwfsc_stations) # combine into one df for stations

# convert lat long values to decimal degrees
stations <- stations %>% 
  rename(y = LATITUDE, x = LONGITUDE) %>% 
  mutate(lat.d  = as.numeric(str_sub(y, 1, 2)),
    lat.m  = as.numeric(str_sub(y, 3, 7)),
    LATITUDE    = lat.d + lat.m/60,
    long.d = as.numeric(str_sub(x, 1, 3)),
    long.m = as.numeric(str_sub(x, 4, 8)),
    LONGITUDE = -(long.d + long.m/60)) %>%
    select(-c(lat.d, long.d, lat.m, long.m))
stations_yr <- stations_yr %>%
  left_join(select(stations, STATION_BOTTOM_DEPTH, LATITUDE, LONGITUDE, STATION, AREA), by="STATION") # joined cols from stations to our list of stations trawled each year. 
rm(stations, nwfsc_stations, swfsc_stations, juv_haul, juv_catch, species_codes) # removed columns we no longer need
stations<-  stations_yr %>%# finally, need to create a simple table of station locations without the additional year information. Ie one value of lat and long per station (total of 170 stations surveyed)
  ungroup()%>%            
  group_by(STATION, STATION_BOTTOM_DEPTH, LATITUDE, LONGITUDE, AREA) %>%
  summarize()
# basemap-- states
ca_nad <- st_read(dsn =paste0(getwd(),"/data/states/cb_2017_us_state_5m.shp" )) # note that this is the path on my mac
ca <- st_transform(ca_nad, crs=4326) # transform to WGS84 or in this case the crs of the point df species_yr_sf- note have to run this before it will recognize necessary info from species_yr_sf-- run through at least one loop first 
rm(ca_nad)
# basemap-- terrain and ocean
#library(raster) # need to load {raster} package in order to upload geotif and convert to dataframe so it can be read into ggplot
#library(rgdal) #dependencies
#library(sp)# dependencies
#basemap_raster <- raster(paste0(getwd(),"/data/GRAY_50M_SR_OB/GRAY_50M_SR_OB.tif")) # create raster file from .tif of ocean and land layer- note datum is WGS84
# convert raster to df so it can be read by ggplot 
#basemap_c <- rasterToPoints(basemap_raster) # {raster} package function for converting to points
#basemap_df <- data.frame(basemap_c) # then convert to dataframe
#colnames(basemap_df) <- c("X","Y","elevation")
#basemap <- basemap_df %>%
 # filter(X>=-126 & X<=-114, Y>=32 & Y<= 44) # i selected for only those values within our maping region so as to limit the size of the file!! Note that this is still slightly larger than our map extent of x = c(-126, -116), y= c(32, 42)
#rm(basemap_raster, basemap_c, basemap_df)
#detach(package:raster) # because this package blocks functionality of dplyr "select", need to turn it off once created!
library(tidyverse) # because some objects were blocked with {raster} so reload tidyverse packages 
