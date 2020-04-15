### US COASTAL CITIES ANALYSIS
### Megan Davis
### Urban Ocean Lab

##This analysis is intended to determine the number of Americans living in coastal cities. While our definition of coastal
#is static - any area that falls within a census-designated coastal county - because the definition of what constitutes a
#city is so fluid, we employ three different definitions: an incoporated place within an Urbanized Area, an incorporated 
#place within an Urbanized Cluster, or a Metropolitan Area. Our published piece **INSERT TITLE HERE** adheres to the 
#definition of a city as an incorporated place within an Urbanized Area.

#----------------#
##### SET UP #####
#----------------#

##Prepare workspace for analysis. The Urban Area shapefile can be downloaded here: 
#https://catalog.data.gov/dataset/tiger-line-shapefile-2018-2010-nation-u-s-2010-census-urban-area-national. All state 
#level incorporated place shapefiles can be downloaded here: https://www2.census.gov/geo/tiger/TIGER2016/PLACE/.

#All data to be loaded can be accessed in `Megan Projects/Census Stuff`.

##Clear the workspace.
rm(list = ls())

##Load libraries.
library(rgdal)
library(dplyr)
library(stringr)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(leaflet)

##Set working directory.
setwd("/Users/MeganDavis/Documents/r_code/coastal_cities")

##Pull in Urban Area shapefile. This shapefile includes boundaries for Urbanized Clusters, places with 2.5 to 50 thousand
#people per census block, and Urbanized Areas, places with more than 50 thousand people per census block.
urban.shp <- readOGR("urban_area/tl_2018_us_uac10.shp")

##Create a shapefile with just Urbanized Areas.
ua.shp <- urban.shp[str_detect(urban.shp@data$NAMELSAD10, "Urbanized Area"), ]

#-----------------------------------------#
##### LOAD AND MANIPULATE CITIES DATA #####
#-----------------------------------------#

##Prior to this portion of the analysis, a list of all incorporated places with a total population of greater than 50
#thousand was pulled from the census website 
#(https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-cities-and-towns.html). This is equivalent to 
#780 incorporated places. Each of these incorporated places were manually matched to their corresponding county. If the 
#county is classified as coastal (https://www.census.gov/library/visualizations/2019/demo/coastline-america.html), the 
#incorporated place is considered coastal as well. You can access this spreadsheet here: 
#https://docs.google.com/spreadsheets/d/1XgDIfbgstbIpe9L-UdJsF8mZv0JbtJcyMnF8nGrkgVg/edit?usp=sharing.

##Read in the incorporated places csv. Convert State, City, County, and Region columns to characters for easy data
#manipulation.
inc.df <- read.csv(file = "incorporated_places.csv", header = TRUE, sep = ",") %>%
  mutate(State = as.character(State),
         City = as.character(City),
         County = as.character(County),
         Region = as.character(Region))

##Create a dataframe that is just unique state names and a row number (to be used for the purpose of looping).
state.df <- inc.df[!duplicated(inc.df[c(1)]),]

state.df$row_num <- seq(length(state.df$State))
state.df <- state.df[,c(1,17)]

##Create an urbanized cities dataframe to be populated
cities <- inc.df[1,c(1:2)] %>%
  mutate(State = NA,
         City = NA,
         Urbanized = NA)

#----------------------------------------------------#
##### REMOVE ALL CITIES NOT IN COASTAL DATAFRAME #####
#----------------------------------------------------#
for(i in 1:length(state.df$row_num)){
  ##Designate state name for current iteration of loop.
  state <- state.df$State[state.df$row_num==i]
  
  ##Filter the coastal incorporated places dataframe to only include results from the relevant state.
  state.c <- inc.df %>%
    filter(State %in% state)
  
  if((state %in% "Hawaii")==F){
    ##Set working directory to one that contains state shapefile.
    setwd(as.character(paste0("~/Google Drive/Megan Projects/Census Stuff/States/",state)))
  
    ##Load state shapefile.
    state.s <- readOGR(as.character(paste0(state,".shp")))
  
    ##Filter out all incorporated places in the shapefile that are not included in the relevant state's coastal city
    #dataframe.
    state.sc <- state.s[as.character(state.s@data$NAME) %in% state.c$City,]

    state.sc.df <- as(state.sc, "data.frame") %>%
      mutate(NAME = as.character(NAME))
    state.sc.df$row_num <- seq(length(state.sc.df$NAME))
  
    for(x in 1:length(state.sc.df$NAME)){
      nam <- state.sc.df$NAME[state.sc.df$row_num==x]
      shp <- state.sc[as.character(state.sc@data$NAME) %in% nam,]
      test <- as(shp, "data.frame")
      ##If there isn't an overlap this returns one row on NAs...
      ##At some point perhaps add something that looks at the percent overlap and have it so only places with a certain 
      #percent overlap can be included...
      ov <- over(ua.s, shp) %>%
        mutate(State = state,
              City = as.character(NAME),
              Urbanized = case_when(is.na(STATEFP)==F ~ TRUE,
                                    is.na(STATEFP)==T ~ FALSE)) %>%
        filter(Urbanized %in% TRUE) %>%
        select(State, City, Urbanized)
    
      if(length(ov)>=1){
        cities <- rbind(cities, ov)
      }
    }
  }else{
    ov <- state.c %>%
      mutate(State = as.character(State),
             City = as.character(City),
             Urbanized = TRUE) %>%
      select(State, City, Urbanized)
    
    cities <- rbind(cities, ov)
  } 
}  

##Join cities data to original incorporated places dataframe
inc.df <- left_join(inc.df, cities[!duplicated(cities[c(1:3)]),], by = c("State","City")) %>%
  filter(Urbanized %in% TRUE)

##Save final dataframe as csv
setwd("~/Google Drive/Megan Projects/Census Stuff")
write.csv(inc.df, "Coastal Cities Pop.csv")

##Calculate total population living in coastal cities.
coast.pop.2018 <- as.numeric(sum(inc.df$Population.Estimate..as.of.July.1....2018))

##Calculate proportion of Americans living in coastal cities given a U.S. population of 331,883,986.
prop.coast.2018 <- as.numeric(coast.pop.2018/331883986)

##Going to need to load in cities data then filter by what's coastal. Then we need to set up a for loop with a dummy 
#dataframe that has a list of all the states. One by one it will go through each of the states and only keep incorporated 
#places that match those included in the coastal incorporated places dataframe. Once this is completed we will have a list
#of incorporated places in coastal counties. We then need to go through and only keep those that are contained in an
#Urban Area.

##First calculate number of "cities by our new definition. Then what we should do since we're now doing city with both 
#versions of density and a version with metropolitan areas is we should produce a map with each one, showing how much area
#that definition encompasses.