### US COASTAL CITIES ANALYSIS
### Megan Davis
### Urban Ocean Lab

##This analysis is intended to determine the number of Americans living in coastal cities. While our definition of coastal
#is static - any area that falls within a census-designated coastal county - because the definition of what constitutes a
#city is so fluid, we employ five different definitions: an incoporated place within an Urbanized Area, an incorporated 
#place within an Urbanized Area or Cluster, an Urban Area, an Urban Cluster or Urban Area, and a Metropolitan Area. Our 
#published piece **INSERT TITLE HERE** adheres to the definition of a city as an incorporated place within an Urbanized 
#Area.

#----------------#
##### SET UP #####
#----------------#

##Prepare workspace for analysis. The Urban Area shapefile can be downloaded here: 
#https://catalog.data.gov/dataset/tiger-line-shapefile-2018-2010-nation-u-s-2010-census-urban-area-national. All state 
#level incorporated place shapefiles can be downloaded here: https://www2.census.gov/geo/tiger/TIGER2016/PLACE/.

##Clear the workspace.
rm(list = ls())

##Load libraries.
library(rgdal)
library(dplyr)
#library(plyr)
library(stringr)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(leaflet)

##Set working directory. I store all of my shapefiles in a separate folder called geographies to avoid shapefile 
#duplications across multiple projects.
setwd("/Users/MeganDavis/Documents/r_code/geographies")

##Pull in Urban Area shapefile. This shapefile includes boundaries for Urbanized Clusters, places with 2.5 to 50 thousand
#people per census block, and Urbanized Areas, places with more than 50 thousand people per census block.
urban.shp <- readOGR("urban_area/tl_2018_us_uac10.shp")

##Create a shapefile with just Urbanized Areas.
ua.shp <- urban.shp[str_detect(urban.shp@data$NAMELSAD10, "Urbanized Area"), ]

##Though I am not pulling in the incorporated place shapefiles yet, it is important to ensure that these shapefiles are 
#saved in a specific way in order for this program to run properly. Within my geographies folder I created another folder
#called states. In this folder I have stored all of the unzipped incorporated place files, which can be downloaded from 
#this site: https://www2.census.gov/geo/tiger/TIGER2016/PLACE/. DO NOT CHANGE THE NAME OF THE DOWNLOADED FILES.

##Create a dataframe of all states in alphabetical order, followed by all U.S. territories in alphabetical order.
files <- rbind(rbind(data.frame(state.name), data.frame("state.name" = c("District of Columbia"))) %>%
  arrange(as.character(state.name)), data.frame("state.name" = c("American Samoa", "Guam", "Northern Mariana Islands",
                                                                 "Puerto Rico", "U.S. Virgin Islands"))) %>%
  mutate(state.name = as.character(state.name))

##Retrieve the list of file names in the states folder. Put these files in alphabetical order and add row numbers. Merge 
#this dataframe to the dataframe containing the list of states and U.S. territories. We know know which files correspond
#to which states.
files <- cbind(files, data.frame("files" = list.files("/Users/MeganDavis/Documents/r_code/geographies/states")) %>%
  mutate(files = as.character(files),
         num = row_number()) %>%
  arrange(files))

##This if statement ensures that the file names have not already been converted.
if(list.files("/Users/MeganDavis/Documents/r_code/geographies/states")[[1]] %in% "Alabama" == FALSE){
  
  ##The following section of code renames all folders and and the files within them to be compatible with the rest of this
  #program.
  for(i in 1:nrow(files)){
    
    ##Set the working directory to be inside the relevant folder.
    setwd(paste0("/Users/MeganDavis/Documents/r_code/geographies/states/", files$files[i]))
    
    ##Generate a list of the files currently in the folder.
    old_files <- list.files(getwd())
    
    ##Using the relevant state name, create a list of new file names.
    new_files <- c(paste0(files$state.name[i],".cpg"), paste0(files$state.name[i],".dbf"), 
                   paste0(files$state.name[i],".prj"), paste0(files$state.name[i],".shp"), 
                   paste0(files$state.name[i],".shp.ea.iso.xml"), paste0(files$state.name[i],".shp.iso.xml"), 
                   paste0(files$state.name[i],".shp.xml"), paste0(files$state.name[i],".shx"))
    
    ##Copy the data from the old files and recreate the same files using the new names.
    file.copy(from = old_files, to = new_files)
    
    ##Remove all files under their old names.
    file.remove(old_files)
    
    ##Reset the working directory to the states folder.
    setwd("/Users/MeganDavis/Documents/r_code/geographies/states")
    
    ##Rename the relevant folder to its corresponding state name.
    file.rename(from = paste0(files$files[i]), to = paste0(files$state.name[i]))
  }
}

##Create the outline of a dataframe that will hold all of the different coastal city population estimates for easy 
#comparison.
coastal_cities <- data.frame("Definition" = c("Incorporated Urban Area", "Incorporated Urban Area or Cluster", 
                                              "Urban Area", "Urban Cluster", "Metropolitan Area"), 
                             Coastal_City_Population = NA, 
                             US_Population_Proportion = NA)

#-----------------------------------------#
##### LOAD AND MANIPULATE CITIES DATA #####
#-----------------------------------------#

##Prior to this portion of the analysis, a list of all incorporated places with a total population of greater than 50
#thousand was pulled from the census website 
#(https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-cities-and-towns.html). This is equivalent to 
#780 incorporated places. Each of these incorporated places were manually matched to their corresponding county. If the 
#county is classified as coastal (https://www.census.gov/library/visualizations/2019/demo/coastline-america.html), the 
#incorporated place is considered coastal as well. You can access this spreadsheet here: 
#https://docs.google.com/spreadsheets/d/1XgDIfbgstbIpe9L-UdJsF8mZv0JbtJcyMnF8nGrkgVg/edit?usp=sharing. The 
#incorporated_places spreadsheet on Github is the same as the U.S. Cities page in the master spreadsheet.

##Set the working directory to the coastal_cities project folder.
setwd("/Users/MeganDavis/Documents/r_code/coastal_cities")

##Read in the incorporated places csv. Convert State, City, County, and Region columns to characters for easy data
#manipulation.
inc.df <- read.csv(file = "incorporated_places.csv", header = TRUE, sep = ",") %>%
  mutate(State = as.character(State),
         City = as.character(City),
         County = as.character(County),
         Region = as.character(Region))

##Create a dataframe that is just unique state names and a row number (to be used for the purpose of looping).
state.df <- inc.df[!duplicated(inc.df[c(1)]),] %>%
  mutate(row_num = row_number()) %>%
  select(State, row_num)

##Create a cites dataframe to be populated. As the loop in the following section of code runs, incorporated places that 
#meet the criteria of falling in an Urbanized Area or an Urbanized Cluster will be added to this empty dataframe.
cities <- inc.df[1,c(1:2)] %>%
  mutate(State = NA,
         City = NA,
         Urbanized_Cluster = NA,
         Urbanized_Area = NA)

#-------------------------------------------------------#
##### DETERMINE URBANIZATION OF INCORPORATED PLACES #####
#-------------------------------------------------------#

##This section of code goes through each incorporated place in each state, one-by-one, and determines if it overlaps with 
#an Urbanized Area or an Urbanized Cluster. If the incorporated place meets either criteria, it is added to the cities 
#dataframe.

##This loop runs the following section of code for each state in the state.df dataframe.
for(i in 1:nrow(state.df)){
  
  ##Determine the state name for the current iteration of the loop.
  state <- state.df$State[state.df$row_num==i]
  
  ##Filter the incorporated places dataframe to only include results from the relevant state.
  state.c <- inc.df %>%
    filter(State %in% state)
  
  ##Set the working directory to the folder that contains the corresponding state's shapefile.
  setwd(paste0("/Users/MeganDavis/Documents/r_code/geographies/states/", state))
  
  ##Load the state shapefile.
  state.s <- readOGR(paste0(state, ".shp"))
  
  ##We need to perform some extra data manipulation at this point once the loop reaches Hawaii. Honolulu is a consolidated
  #city-county. What we would traditionally describe as the city of Honolulu is listed in the Hawaii shapefile as 
  #"Urban Honolulu." In order for the code to run properly, we need to replace that variable with "Honolulu" so it matches
  #the name in the incorporated places data.
  if(state %in% "Hawaii"){
    
    ##Revalue the NAME factor in the state.s shapefile data so Urban Honolulu is replaced with Honolulu. To avoid having
    #to use the plyr package (which would make you need to designate which commands come from the dplyr versus the plyr
    #package), I did this in base R.
    levels(state.s@data$NAME)[levels(state.s@data$NAME)=="Urban Honolulu"] <- "Honolulu"
  }
  
  ##Filter out all incorporated places included in the shapefile that are not included in the state's city dataframe. The
  #areas that will be removed include incorporated places with a total population of less than 50 thousand and all 
  #unincorporated places.
  state.sc <- state.s[as.character(state.s@data$NAME) %in% state.c$City,]
  
  ##Convert the shapefile of filtered incorporated places into a dataframe. Transform the NAME variable to a character for
  #easy data manipulation and add row numbers. Filter the columns to only include NAME and row_num. This dataframe
  #will be used to test each incorporated place, one-by-one, to see if it falls in an Urbanized Area or an Urbanized 
  #Cluster.
  state.sc.df <- as(state.sc, "data.frame") %>%
    mutate(NAME = as.character(NAME),
           row_num = row_number()) %>%
    select(NAME, row_num)
  
  ##This loop runs the following section of code for each incorporated place in the state.sc.df dataframe.
  for(x in 1:nrow(state.sc.df)){
    
    ##Create a variable that holds the name of the relevant incorporated place.
    nam <- state.sc.df$NAME[state.sc.df$row_num==x]
    
    ##Create a variable that holds a shapefile of the relevant incorporated place.
    shp <- state.sc[as.character(state.sc@data$NAME) %in% nam,]
    
    ##Test to see if the relevant incorporated place overlaps with any Urbanized Areas OR Urbanized Clusters. If the 
    #incorporated place does not fall within either of the urbanization classifications, this will create a dataframe that
    #consists entirely of NAs. At present, any level of overlap is considered acceptable to classify the incorporated place
    #as falling within an urbanization classification. Create a State column, populated by the state variable created in 
    #the state-level loop, and a City column using the name of the incorporated place in the ov dataframe. Using the first
    #column, test to see if any of the rows contain anything other than NA values. If a row is populated, that suggests the
    #incorporated place overlaps with an Urbanized Area OR an Urbanized Cluster. This fact is housed in the 
    #Urbanized_Cluster column. Filter the dataframe to only the State, City, and Urbanized_Cluster columns. Select only
    #distinct rows.
    ov <- over(urban.shp, shp) %>%
      mutate(State = state,
             City = as.character(NAME),
             Urbanized_Cluster = case_when(is.na(STATEFP)==F ~ TRUE,
                                           is.na(STATEFP)==T ~ FALSE)) %>%
      select(State, City, Urbanized_Cluster) %>%
      distinct()
    
    ##This section of code only runs if the ov.uac dataframe contains more than one row, meaning the incorporated place 
    #overlaped with an Urbanized Area or an Urbanized Cluster, and tests to see if the incorporated place overlaps with an
    #Urbanized Area specifically. If the ov.uac dataframe doesn't have more than one row, meaning no overlap, there is 
    #nothing to add to the cities dataframe and the loop will proceed to the next incorporated place.
    if(nrow(ov)>=1){
      
      ##Filter the ov dataframe to only include the row that represents an overlap between the incorporated place and an
      #Urbanized Area or Urbanized Cluster, then test to see if the incorporated place overlaps with an Urbanized Area
      #specifically. Join the two dataframes using the State and City columns (this ensures that only an incorporated 
      #place that overlaps with an Urbanized Area will be joined to the dataframe). If the incorporated place does not fall
      #in an urbanized area, the joined dataframe will have an NA in the Urbanized_Area column. The final chunk of this
      #code replaces any of those NAs with a FALSE response.
      ov <- left_join(ov %>%
                          filter(Urbanized_Cluster %in% TRUE), 
                        over(ua.shp, shp) %>%
                          mutate(State = state,
                                 City = as.character(NAME),
                                 Urbanized_Area = case_when(is.na(STATEFP)==F ~ TRUE,
                                                            is.na(STATEFP)==T ~ FALSE)) %>%
                          select(State, City, Urbanized_Area) %>%
                          distinct(), by = c("State", "City")) %>%
        mutate(Urbanized_Area = case_when(is.na(Urbanized_Area)==T ~ FALSE,
                                          is.na(Urbanized_Area)==F ~ Urbanized_Area))
      
      ##Add the results to the cities dataframe.
      cities <- rbind(cities, ov)
    }
  }
}

##Because the above section of code takes so long to run, I always save a backup of the unmanipulated cities dataframe 
#just in case I need to redo any data manipulation I perform later on.
cities_backup <- cities

#----------------------------------------------------#
##### REMOVE ALL CITIES NOT IN COASTAL DATAFRAME #####
#----------------------------------------------------#

##This section of code creates the coastal cities dataframe and calculates how many Americans are living in coastal cities
#if we define a coastal city as any incorporated place that falls within a coastal county and a Urbanized Cluster or 
#Urbanized Area. All coastal cities are located in areas that fall under the highest urban density classification: Urban 
#Areas. Therefore, all the following calculations apply to both definitions.

##Join cities data to original incorporated places dataframe
inc.df <- left_join(inc.df, cities[!duplicated(cities[c(1:4)]),], by = c("State","City")) %>%
  filter(Urbanized_Cluster %in% TRUE & Coastal %in% TRUE)

##Set the working directory to where we want to save the coastal cities dataframe.
setwd("/Users/MeganDavis/Documents/r_code/coastal_cities")

##Save the inc.df dataframe as a csv. The csv generated by this section of code is available at the U.S. Coastal Cities 
#(UA/UC) tab of our coastal cities master spreadsheet 
#(https://docs.google.com/spreadsheets/d/1XgDIfbgstbIpe9L-UdJsF8mZv0JbtJcyMnF8nGrkgVg/edit?usp=sharing).
write.csv(inc.df, "coastal_cities_pop.csv")

##Calculate total population living in coastal cities. This comes out to 49,297,913 people.
coastal_cities$Coastal_City_Population[coastal_cities$Definition %in% "Incorporated Urban Area" | 
                                         coastal_cities$Definition %in% "Incorporated Urban Area or Cluster"] <- 
  as.numeric(sum(inc.df$Population.Estimate..as.of.July.1....2018))






##SAVE THIS AT THE END

##Read in the U.S Population Total csv.
total_pop <- read.csv(file = "total_us_pop.csv", header = TRUE, sep = ",")

##Pull the U.S. population estimate from 2018 and use that to calculate the proportion of Americans living in coastal 
#cities in 2018. This comes out to about 15% of the U.S. population.
prop.coast.2018 <- coast.pop.2018/as.numeric(total_pop$POPESTIMATE2018[as.character(total_pop$NAME) %in% "United States"]).