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
library(stringr)
library(stringr)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(leaflet)
library(htmlwidgets)

##Set working directory. I store all of my shapefiles in a separate folder called geographies to avoid shapefile 
#duplications across multiple projects.
setwd("/Users/MeganDavis/Documents/r_code/geographies")

##Pull in Urban Area shapefile. This shapefile includes boundaries for Urban Clusters, places with 2.5 to 50 thousand
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
                                              "Urban Area", "Urban Area or Cluster", "Metropolitan Area",
                                              "Metropolitan or Micropolitan Area"), 
                             Coastal_City_Population = NA, 
                             US_Population_Proportion = NA,
                             People_of_Color = NA,
                             Proportion_of_Color = NA)

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
inc.df <- read.csv(file = "data/incorporated_places.csv", header = TRUE, sep = ",") %>%
  mutate(State = as.character(State),
         City = as.character(City),
         County = as.character(County),
         Region = as.character(Region))

##Create a dataframe that is just unique state names and a row number (to be used for the purpose of looping).
state.df <- inc.df[!duplicated(inc.df[c(1)]),] %>%
  mutate(row_num = row_number()) %>%
  select(State, row_num)

##Create a cites dataframe to be populated. As the loop in the following section of code runs, incorporated places that 
#meet the criteria of falling in an Urbanized Area or an Urban Cluster will be added to this empty dataframe.
cities <- inc.df[1,c(1:2)] %>%
  mutate(State = NA,
         City = NA,
         Urbanized_Cluster = NA,
         Urbanized_Area = NA)

#-------------------------------------------------------#
##### DETERMINE URBANIZATION OF INCORPORATED PLACES #####
#-------------------------------------------------------#

##This section of code goes through each incorporated place in each state, one-by-one, and determines if it overlaps with 
#an Urbanized Area or an Urban Cluster. If the incorporated place meets either criteria, it is added to the cities 
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
    
    ##Test to see if the relevant incorporated place overlaps with any Urbanized Areas OR Urban Clusters. If the 
    #incorporated place does not fall within either of the urbanization classifications, this will create a dataframe that
    #consists entirely of NAs. At present, any level of overlap is considered acceptable to classify the incorporated place
    #as falling within an urbanization classification. Create a State column, populated by the state variable created in 
    #the state-level loop, and a City column using the name of the incorporated place in the ov dataframe. Using the first
    #column, test to see if any of the rows contain anything other than NA values. If a row is populated, that suggests the
    #incorporated place overlaps with an Urbanized Area OR an Urban Cluster. This fact is housed in the 
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
    #overlaped with an Urbanized Area or an Urban Cluster, and tests to see if the incorporated place overlaps with an
    #Urbanized Area specifically. If the ov.uac dataframe doesn't have more than one row, meaning no overlap, there is 
    #nothing to add to the cities dataframe and the loop will proceed to the next incorporated place.
    if(nrow(ov)>=1){
      
      ##Filter the ov dataframe to only include the row that represents an overlap between the incorporated place and an
      #Urbanized Area or Urban Cluster, then test to see if the incorporated place overlaps with an Urbanized Area
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
#if we define a coastal city as any incorporated place that falls within a coastal county and a Urban Cluster or 
#Urbanized Area. All coastal cities are located in areas that fall under the highest urban density classification: Urban 
#Areas. Therefore, all the following calculations apply to both definitions.

##Join cities data to original incorporated places dataframe
inc.df <- left_join(inc.df, cities[!duplicated(cities[c(1:4)]),], by = c("State","City")) %>%
  filter(Urbanized_Cluster %in% TRUE & Coastal %in% TRUE)

##Set the working directory to where we want to save the coastal cities dataframe.
setwd("/Users/MeganDavis/Documents/r_code/coastal_cities")

##Save the inc.df dataframe as a csv. The csv generated by this section of code is available at the U.S. Coastal Cities 
#(IUA/IUC) tab of our coastal cities master spreadsheet 
#(https://docs.google.com/spreadsheets/d/1XgDIfbgstbIpe9L-UdJsF8mZv0JbtJcyMnF8nGrkgVg/edit?usp=sharing).
write.csv(inc.df, "coastal_cities_pop.csv")

##Calculate total population living in coastal cities. This comes out to 49,297,913 people.
coastal_cities$Coastal_City_Population[coastal_cities$Definition %in% "Incorporated Urban Area" | 
                                         coastal_cities$Definition %in% "Incorporated Urban Area or Cluster"] <- 
  as.numeric(sum(inc.df$Population.Estimate..as.of.July.1....2018))

##The caculation of how many people living in coastal cities are people of color was originally done directly in the 
#coastal cities master spreadsheet. I eventually plan to move that portion of the analysis into this code, but for now the 
#number of people living in coastal cities who are people of color came out to be 31,149,833. Load this number into the 
#coastal_cities dataframe.
coastal_cities$People_of_Color[coastal_cities$Definition %in% "Incorporated Urban Area" | 
                                 coastal_cities$Definition %in% "Incorporated Urban Area or Cluster"] <- 31149833

#-------------------------------------------#
##### CREATE COASTAL COUNTIES SHAPEFILE #####
#-------------------------------------------#

##The Census Bureau provides county-level shapefiles. However, they do not distinguish which of the counties are coastal in
#the shapefile data. This section of code pulls in a list of U.S. coastal counties and uses that information to filter the
#county shapefile to only include coastal counties.

##Set the working directory to the coastal cities project folder.
setwd("/Users/MeganDavis/Documents/r_code/coastal_cities")

##Read in the coastal counties dataframe. I created this spreadsheet manually using the list of U.S. coastal counties 
#available here: https://www.census.gov/library/visualizations/2019/demo/coastline-america.html. I edited the name of some
#counties in the spreadsheet. My edited version is available under the U.S. Coastal Counties tab of the master spreadsheet:
#https://docs.google.com/spreadsheets/d/1XgDIfbgstbIpe9L-UdJsF8mZv0JbtJcyMnF8nGrkgVg/edit?usp=sharing. The Coastal.County
#and State columns are renamed in order to bind this information to the county shapefile.
coast_county.df <- read.csv(file = "data/coastal_counties.csv", header = TRUE, sep = ",") %>%
  mutate(Coastal.County = as.character(Coastal.County),
         State = as.character(State),
         Region = as.character(Region)) %>%
  select("NAMELSAD" = Coastal.County, "Name" = State, Region)

##Set the working directory to the geographies folder.
setwd("/Users/MeganDavis/Documents/r_code/geographies")

##Load in the U.S. county shapefile. This shapefile can be downloaded here: 
#https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2019&layergroup=Counties+%28and+equivalent%29.
coast_county.shp <- readOGR("counties/tl_2019_us_county.shp")

##In order to merge the county shapefile to the coast_county.df dataframe, we need two columns to join on: the county name
#and the state name. This is because some counties of the same name may exist in different states. However, the shapefile
#does not list the states by name, it uses their FIPS code. Therefore, we first bind the shapefile to a dataframe that maps
#FIPS codes to state name. Once this is completed, we merge the coast_county.df dataframe to the county shapefile by state 
#name, using the Name column, and county name, using the NAMELSAD column.
coast_county.shp <- merge(merge(coast_county.shp, 
                                rbind(read.csv(file = "states_geocodes.csv", header = TRUE, sep = ",") %>%
                                        select("STATEFP" = State..FIPS., Name) %>%
                                        mutate(Name = as.character(Name),
                                               STATEFP = as.character(STATEFP),
                                               STATEFP = case_when(str_length(STATEFP) == 1 ~ paste0("0",STATEFP),
                                                                   str_length(STATEFP) != 1 ~ STATEFP)), 
                                      data.frame("STATEFP" = c("03", "07", "14", "43", "52", "60", "66", "69", "72", "78"), 
                                                 "Name" = c("American Samoa", "Canal Zone", "Guam", "Puerto Rico", 
                                                            "U.S. Virgin Islands", "American Samoa", "Guam", 
                                                            "Northern Mariana Islands", "Puerto Rico", 
                                                            "U.S. Virgin Islands"))), 
                                by = "STATEFP"), coast_county.df, by = c("NAMELSAD", "Name"))

##Filter the county shapefile (coast_county.shp) to only include coastal counties.
coast_county.shp <- coast_county.shp[is.na(coast_county.shp@data$Region)==F,]

#-------------------------------------------------------------#
##### IDENTIFY COASTAL URBANIZED AREAS AND URBAN CLUSTERS #####
#-------------------------------------------------------------#

##This section of code determines which Urbanized Areas and Urban Clusters fall in coastal counties. The urban shapefile 
#data was already generated at the beginning of this program. The urban population data is pulled from the census data 
#portal (https://data.census.gov/cedsci/) by doing an advanced search for people and population in urban areas. There are 
#two versions of this data: one created using data collected over the past five years and one created using data over the 
#past year. While the one year data is more accurate because it was collected more recently, not all areas will have data 
#collected in the past year. Therefore, for our  anaylsis, I use the five year data, so I can capture more areas in my 
#estimate. You can learn more about the difference here: https://www.census.gov/programs-surveys/acs/guidance/estimates.html
#If you decide to use the one year estimate instead, substitute "urban_pop.csv" with "urban_pop_1yr.csv."

##Set the working directory to the coastal cities project folder.
setwd("/Users/MeganDavis/Documents/r_code/coastal_cities")

##Read in the Urbanized Area and Urban Cluster population data. Remove the extra row of column labels and remove the 
#"(2010)" from the end of the Urbanized Area/Cluster name. There is also an issue where county names with accents did not
#load properly. The section of code with all of the case_when statements remedies this issue. Add row numbers. Convert
#all of the population stats to characters so we don't have to deal with factors.
urban.df <- read.csv(file = "data/urban_pop.csv", header = TRUE, sep = ",") %>%
  filter(!as.character(GEO_ID) %in% "id") %>%
  mutate(NAME = as.character(NAME),
         NAME = str_remove(NAME, " [(]2010[])]"),
         NAME = case_when(str_detect(NAME, "i�n|m�n")==T ~ str_replace(NAME, "�", "á"),
                          str_detect(NAME, "i�n|m�n")==F ~ NAME),
         NAME = case_when(str_detect(NAME, "a�o")==T ~ str_replace(NAME, "�", "ñ"),
                          str_detect(NAME, "a�o")==F ~ NAME),
         NAME = case_when(str_detect(NAME, "b�r")==T ~ str_replace(NAME, "�", "é"),
                          str_detect(NAME, "b�r")==F ~ NAME),
         NAME = case_when(str_detect(NAME, "D�a")==T ~ str_replace(NAME, "�", "í"),
                          str_detect(NAME, "D�a")==F ~ NAME),
         NAME = case_when(str_detect(NAME, "g�e")==T ~ str_replace(NAME, "�", "ü"),
                          str_detect(NAME, "g�e")==F ~ NAME),
         row_num = row_number()) %>%
  select(NAME, "pop_2018" = S0101_C01_001E, "pop_male" = S0101_C03_001E, "pop_female" = S0101_C05_001E, 
         row_num) %>%
  mutate(pop_2018 = as.character(pop_2018),
         pop_male = as.character(pop_male),
         pop_female = as.character(pop_female))

##Create an empty dataframe that will hold a list of all Urbanized Areas and Urban Clusters that fall in a coastal county.
coast_urban.df <- data.frame("Name" = NA, "Coastal" = NA)

##The following section of code checks each Urbanized Area and Urban Cluster, one-by-one, to see if it falls in a coastal
#county.
for(i in 1:nrow(urban.df)){
  
  ##Create a variable that holds the name of the relevant Urbanized Area or Urban Cluster.
  nam <- urban.df$NAME[urban.df$row_num==i]
  
  ##Create a variable that holds a shapefile of the relevant Urbanized Area or Urban Cluster.
  shp <- urban.shp[as.character(urban.shp@data$NAMELSAD10) %in% nam,]
  
  ##Check to see if the relevant Urbanized Area or Urban Cluster falls in a coastal county. If that is the case, the Coastal
  #column will be populated with a TRUE result. Filter this dataframe to only include TRUE results and remove all 
  #duplicates, as some Urbanized Areas or Urban Clusters may fall in multiple coastal counties.
  ov <- over(coast_county.shp, shp) %>%
    mutate(Name = as.character(NAMELSAD10),
           Coastal = case_when(is.na(UACE10)==F ~ TRUE,
                               is.na(UACE10)==T ~ FALSE)) %>%
    select(Name, Coastal) %>%
    filter(Coastal %in% TRUE) %>%
    distinct()
  
  ##Add the results of the above section of code to the coast_urban.df dataframe.
  coast_urban.df <- rbind(coast_urban.df, ov)
  
}

##Join the 2018 Urbanized Area and Urban Cluster population data to the dataframe of coastal Urbanized Areas and Urban 
#Clusters. Remove the dummy row of NA values. Convert all population stats to numeric variables.
coast_urban.df <- left_join(coast_urban.df, urban.df[1:4], by = c("Name" = "NAME")) %>%
  filter(is.na(Name)==F) %>%
  mutate(pop_2018 = as.numeric(pop_2018),
         pop_male = as.numeric(pop_male),
         pop_female = as.numeric(pop_female))

#----------------------------------------------#
##### DETERMINE PROPORTION PEOPLE OF COLOR #####
#----------------------------------------------#

##This section of code pulls in information on the number of people in Urbanized Areas and Urban Clusters in 2018 who can 
#be classified as white alone and uses that number to calculate the number of people living in coastal Urbanized Areas or
#Urban Clusters who identify as people of color. The Urbanized Area and Urban Cluster white alone data is pulled from the
#census data portal (https://data.census.gov/cedsci/) by doing an advanced search for race and ethnicity in urban areas.
#Be sure to select the table that's white alone, not Hispanic or Latino. There are two versions of this data: one created 
#using data collected over the past five years and one created using data over the past year. The table used should match 
#the table type used to pull the urban population data. In this case that is the five year data. If you decide to use the 
#one year estimate instead, substitute "urban_white.csv" with "urban_white_lyr.csv."

##Pull in the data with the number of people living in  Urbanized Areas and Urban Clusters that can be categorized as white
#alone. Manipulate the dataframe to be in the same format as the coast_urban.df dataframe and join it to that dataframe 
#using the Name/NAME columns. Subtract the white alone values from the total population values to determine the number of 
#people living in coastal Urbanized Areas or Urban Clusters who can be categorized as people of color.
coast_urban.df <- left_join(coast_urban.df,
                            read.csv(file = "data/urban_white.csv", header = TRUE, sep = ",") %>%
                              filter(!as.character(GEO_ID) %in% "id") %>%
                              mutate(NAME = as.character(NAME),
                                     NAME = str_remove(NAME, " [(]2010[])]"),
                                     NAME = case_when(str_detect(NAME, "i�n|m�n")==T ~ str_replace(NAME, "�", "á"),
                                                      str_detect(NAME, "i�n|m�n")==F ~ NAME),
                                     NAME = case_when(str_detect(NAME, "a�o")==T ~ str_replace(NAME, "�", "ñ"),
                                                      str_detect(NAME, "a�o")==F ~ NAME),
                                     NAME = case_when(str_detect(NAME, "b�r")==T ~ str_replace(NAME, "�", "é"),
                                                      str_detect(NAME, "b�r")==F ~ NAME),
                                     NAME = case_when(str_detect(NAME, "D�a")==T ~ str_replace(NAME, "�", "í"),
                                                      str_detect(NAME, "D�a")==F ~ NAME),
                                     NAME = case_when(str_detect(NAME, "g�e")==T ~ str_replace(NAME, "�", "ü"),
                                                      str_detect(NAME, "g�e")==F ~ NAME),
                                     row_num = row_number()) %>%
                              select(NAME, "white_2018" = B01001H_001E, "white_male" = B01001H_002E, 
                                     "white_female" = B01001H_017E) %>%
                              mutate(white_2018 = as.numeric(as.character(white_2018)),
                                     white_male = as.numeric(as.character(white_male)),
                                     white_female = as.numeric(as.character(white_female))), by = c("Name" = "NAME")) %>%
  mutate(non_white_2018 = pop_2018 - white_2018,
         non_white_male = pop_male - white_male,
         non_white_female = pop_female - white_female)

##Save the coast_urban.df dataframe as a csv. The csv generated by this section of code is available at the U.S. Coastal 
#Cities (UA/UC) tab of our coastal cities master spreadsheet
#(https://docs.google.com/spreadsheets/d/1XgDIfbgstbIpe9L-UdJsF8mZv0JbtJcyMnF8nGrkgVg/edit?usp=sharing).
write.csv(coast_urban.df, "coastal_cities_pop_v2.csv")

##Calculate the total population living in coastal Urbanized Areas or Urban Clusters. This comes out to 107,766,631 people.
coastal_cities$Coastal_City_Population[coastal_cities$Definition %in% "Urban Area or Cluster"] <- 
  sum(coast_urban.df$pop_2018)

##Calculate how many people of color are living in coastal Urbanized Areas or Urban Clusters. This comes out to 40,038,775
#people.
coastal_cities$People_of_Color[coastal_cities$Definition %in% "Urban Area or Cluster"] <-
  sum(coast_urban.df$non_white_2018)

##Calculate the total population living in coastal Urbanized Areas. This comes out to 104,413,413 people.
coastal_cities$Coastal_City_Population[coastal_cities$Definition %in% "Urban Area"] <-
  sum(coast_urban.df$pop_2018[str_detect(coast_urban.df$Name, "Urbanized Area")])

##Calculate how many people of color are living in coastal Urbanized Areas. This comes out to 
coastal_cities$People_of_Color[coastal_cities$Definition %in% "Urban Area"] <-
  sum(coast_urban.df$non_white_2018[str_detect(coast_urban.df$Name, "Urbanized Area")])

#---------------------------------------------------------#
##### IDENTIFY COASTAL METROPOLITAN STATISTICAL AREAS #####
#---------------------------------------------------------#

##This section of code determines which Metropolitan and Micropolitan Statistical Areas fall in coastal counties. The 2018 
#Statistical Area shapefile can be downloaded here: https://catalog.data.gov/dataset/tiger-line-shapefile-2018-nation-u-s-
#current-metropolitan-statistical-area-micropolitan-statist. The Metropolitan and Micropolitan Statistical Area data is 
#pulled from the census data portal (https://data.census.gov/cedsci/) by doing an advanced search for people and population 
#in Metropolitan Statistical Area/Micropolitan Statistical Area. There are two versions of this data: one created using data
#collected over the past five years and one created using data over the past year. While the one year data is more accurate 
#because it was collected more recently, not all areas will have data collected in teh past year. Therefore, for our 
#analysis, I use the five year data, so I can capture more areas in my estimate. You can learn more about the difference 
#between the two datasets here: https://www.census.gov/programs-surveys/acs/guidance/estimates.html. If you decide to use 
#the one year estimate instead, substitute "metropolitan_pop.csv" with "metropolitan_pop_1yr.csv."

##Set the working directory to the geographies folder.
setwd("/Users/MeganDavis/Documents/r_code/geographies/")

##Load in the U.S. Metropolitan and Micropolitan Statistical Area shapefile.
met.shp <- readOGR("metro_areas/tl_2018_us_cbsa.shp")

##Set the working directory to the coastal cities project folder.
setwd("/Users/MeganDavis/Documents/r_code/coastal_cities")

##Read in the Metropolitan/Micropolitan Statistical Area data. Remove the extra row of column labels. Select just the name
#of the Statistical Area with its 2018 population, male population and female population. Convert all columns to characters
#so we don't have to deal with factors. There is also an issue where county names with accents did not load properly. The 
#section of code with all of the case_when statements remedies this issue.
met.df <- read.csv(file = "data/metropolitan_pop.csv", header = TRUE, sep = ",") %>%
  filter(!as.character(GEO_ID) %in% "id") %>%
  select(NAME, "pop_2018" = S0101_C01_001E, "pop_male" = S0101_C03_001E, "pop_female" = S0101_C05_001E) %>%
  mutate(NAME = as.character(NAME),
         NAME = case_when(str_detect(NAME, "a�o")==T ~ str_replace(NAME, "�", "ñ"),
                          str_detect(NAME, "a�o")==F ~ NAME),
         NAME = case_when(str_detect(NAME, "g�e")==T ~ str_replace(NAME, "�", "ü"),
                          str_detect(NAME, "g�e")==F ~ NAME),
         NAME = case_when(str_detect(NAME, "m�n")==T ~ str_replace(NAME, "�", "á"),
                          str_detect(NAME, "m�n")==F ~ NAME),
         pop_2018 = as.character(pop_2018),
         pop_male = as.character(pop_male),
         pop_female = as.character(pop_female),
         row_num = row_number())

##Create an empty dataframe that will hold a list of Metropolitan Statistical Areas and all Micropolitan Statistical Areas
#that fall in a coastal county.
coast_met.df <- data.frame("Name" = NA, "Coastal" = NA)

##The following section of code checks each Metropolitan Statistical Area and Micropolitan Statistical Area, one-by-one, to
#see if it falls in a coastal county.
for(i in 1:nrow(met.df)){
  
  ##Create a variable that holds the name of the relevant Metropolitan/Micropolitan Statistical Area.
  nam <- met.df$NAME[met.df$row_num==i]
  
  ##Create a variable that holds a shapefile of the relevant Metropolitan/Micropolitan Statistical Area.
  shp <- met.shp[as.character(met.shp@data$NAMELSAD) %in% nam,]
  
  ##Check to see if the relevant Metropolitan/Micropolitan Statistical Area falls in a coastal county. If that is the case,
  #the Coastal column will be populated with a TRUE result. Filter this dataframe to only include TRUE results and remove
  #all duplicates, as some Metropolitan/Micropolitan Statistical Areas may fall in multiple coastal counties.
  ov <- over(coast_county.shp, shp) %>%
    mutate(Name = as.character(NAMELSAD),
           Coastal = case_when(is.na(GEOID)==F ~ TRUE,
                               is.na(GEOID)==T ~ FALSE)) %>%
    select(Name, Coastal) %>%
    filter(Coastal %in% TRUE) %>%
    distinct()
  
  ##Add the results of the above section of code to the coast_met.df dataframe.
  coast_met.df <- rbind(coast_met.df, ov)
}

##Join the 2018 Metropolitan Statistical Area and Micropolitan Statistical Area population data to the dataframe of coastal
#Metropolitan and Micropolitan Statistical Areas. Remove the dummy row of NA values. Convert all population stats to 
#numeric variables.
coast_met.df <- left_join(coast_met.df, met.df[1:4], by = c("Name" = "NAME")) %>%
  filter(is.na(Name)==F) %>%
  mutate(pop_2018 = as.numeric(pop_2018),
         pop_male = as.numeric(pop_male),
         pop_female = as.numeric(pop_female))

##Save the coast_met.df dataframe as a csv. The csv generated by this section of code is available at the U.S. Coastal
#Cities (MSA) tab of our coastal cities master spreadsheet
#(https://docs.google.com/spreadsheets/d/1XgDIfbgstbIpe9L-UdJsF8mZv0JbtJcyMnF8nGrkgVg/edit?usp=sharing).
write.csv(coast_met.df, "coastal_cities_pop_v3.csv")

##Calculate the total population living in coastal Metropolitan Statistical Areas or Micropolitan Statistical Areas. This 
#comes out to 140,025,326 people.
coastal_cities$Coastal_City_Population[coastal_cities$Definition %in% "Metropolitan or Micropolitan Area"] <-
  sum(coast_met.df$pop_2018)

##Calculate the total population living in coastal Metropolitan Statistical Areas. This comes out to 
coastal_cities$Coastal_City_Population[coastal_cities$Definition %in% "Metropolitan Area"] <-
  sum(coast_met.df$pop_2018[str_detect(coast_met.df$Name, "Metro Area")])

#-------------------------------------------------------------#
##### DETERMINE PROPORTION OF AMERICANS IN COASTAL CITIES #####
#-------------------------------------------------------------#

##This section of code determines the proportion of the U.S. population that lives in coastal cities according to each 
#definition of coastal city. It also determine what proportion of coastal city residents for each definition of coastal 
#city identify as people of color.

##Read in a csv with yearly U.S. population statistics. This csv can be downloaded here:
#https://www.census.gov/data/tables/time-series/demo/popest/2010s-national-total.html#par_textimage_2011805803
total_pop <- read.csv(file = "data/total_us_pop.csv", header = TRUE, sep = ",")

##Use the 2018 population estimate to calculate the proportion of Americans living in coastal cities under each definition.
coastal_cities <- coastal_cities %>%
  mutate(US_Population_Proportion = Coastal_City_Population /
           total_pop$POPESTIMATE2018[as.character(total_pop$NAME) %in% "United States"],
         Proportion_of_Color = People_of_Color / Coastal_City_Population)

##Save the coastal_cities dataframe as a csv. The csv generated by this section of code is available at the U.S. Coastal 
#Cities Population tab of our coastal cities master spreadsheet
#(https://docs.google.com/spreadsheets/d/1XgDIfbgstbIpe9L-UdJsF8mZv0JbtJcyMnF8nGrkgVg/edit?usp=sharing).
write.csv(coastal_cities, "coastal_cities_pop_comparison.csv")

#--------------------------------------#
##### CREATE MAP OF COASTAL CITIES #####
#--------------------------------------#

##This section of code creates a map of the area included in each definition of coastal cities.

##Set the working directory to the coastal cities project folder.
setwd("/Users/MeganDavis/Documents/r_code/coastal_cities")

##Create an outline of a coastal cities map to be populated with all coastal cities shapefiles.
coastal_cities.map <- leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") 

###METROPOLITAN OR MICROPOLITAN STATISTICAL AREAS

##Merge the coast_met.df dataframe to the Metropolitan and Micropolitan Statistical Area shapefile.
shp <- merge(met.shp, coast_met.df %>%
               mutate(NAMELSAD = Name), by = c("NAMELSAD"))

##Filter the shapefile to only include Metropolitan and Micropolitan Statistical Areas that are coastal.
shp <- shp[is.na(shp@data$Coastal)==F,]

##Add coastal Metropolitan and Micropolitan Statistical Areas to the coastal cities map.
coastal_cities.map <- coastal_cities.map %>%
  addPolygons(data = shp, fillColor = "yellow", fillOpacity = 1, stroke = FALSE)

##Create a map of just coastal Metropolitan and Micropolitan Statistical Areas.
save.map <- leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addPolygons(data = shp, fillColor = "lightblue", fillOpacity = 1, stroke = FALSE) %>%
  addControl("Coastal Metropolitan or Micropolitan Area", position = c("topright"))

##Save the map of coastal Metropolitan and Micropolitan Statistical Areas as an html widget.
#saveWidget(save.map, file = "met_mic_coast.html")

###METROPOLITAN STATISTICAL AREAS

##Filter the coastal Metropolitan and Micropolitan Statistical Area shapefile to only include Metropolitan Statistical 
#Areas.
shp <- shp[str_detect(shp@data$NAMELSAD, "Metro Area")==T,]

##Add coastal Metropolitan Statistical Areas to the coastal cities map.
coastal_cities.map <- coastal_cities.map %>%
  addPolygons(data = shp, fillColor = "gold", fillOpacity = 1, stroke = FALSE)

##Create a map of just coastal Metropolitan Statistical Areas.
save.map <- leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addPolygons(data = shp, fillColor = "lightblue", fillOpacity = 1, stroke = FALSE) %>%
  addControl("Coastal Metropolitan Area", position = c("topright"))

##Save the map of coastal Metropolitan Statistical Areas as an html widget.
#saveWidget(save.map, file = "met_coast.html")

###URBANIZED AREA OR URBAN CLUSTER

##Merge the coast_urban.df dataframe to the Urbanized Area and Urban Cluster shapefile.
shp <- merge(urban.shp, coast_urban.df %>%
               mutate(NAMELSAD10 = Name), by = c("NAMELSAD10"))

##Filter the shapefile to only include Urbanized Areas and Urban Clusters that are coastal.
shp <- shp[is.na(shp@data$Coastal)==F,]

##Add coastal Urbanized Areas and Urban Clusters to the coastal cities map.
coastal_cities.map <- coastal_cities.map %>%
  addPolygons(data = shp, fillColor = "orange", fillOpacity = 1, stroke = FALSE)

##Create a map of just coastal Urbanized Areas and Urban Clusters.
save.map <- leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addPolygons(data = shp, fillColor = "lightblue", fillOpacity = 1, stroke = FALSE) %>%
  addControl("Coastal Urban Area or Cluster", position = c("topright"))

##Save the map of coastal Urbanized Areas and Urban Clusters as an html widget.
#saveWidget(save.map, file = "urban_uac.html")

###URBANIZED AREA

##Merge the coast_urban.df dataframe to the Urbanized Area shapefile.
shp <- merge(ua.shp, coast_urban.df %>%
               mutate(NAMELSAD10 = Name), by = c("NAMELSAD10"))

##Filter the shapefile to only include Urbanized Areas that are coastal.
shp <- shp[is.na(shp@data$Coastal)==F,]

##Add coastal Urbanized Areas to the coastal cities map.
coastal_cities.map <- coastal_cities.map %>%
  addPolygons(data = shp, fillColor = "orangered", fillOpacity = 1, stroke = FALSE)

##Create a map of just coastal Urbanized Areas and Urban Clusters.
save.map <- leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addPolygons(data = shp, fillColor = "lightblue", fillOpacity = 1, stroke = FALSE) %>%
  addControl("Coastal Urban Area", position = c("topright"))

##Save the map of coastal Urbanized Areas as an html widget.
#saveWidget(save.map, file = "urban_ua.html")

###INCORPORATED PLACES

##Create the outline for the coastal Incorporated Place map to be populated.
save.map <- leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels")

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
  
  ##Merge the state shapefile to the filtered incorporated place dataframe.
  state.s <- merge(state.s, state.c %>%
                     mutate(NAME = City), by = c("NAME"))
  
  ##Filter the state shapefile to only include incorporated places that are included in the filtered incorporated place
  #dataframe.
  state.s <- state.s[is.na(state.s@data$State)==F,]
  
  ##Filter out any CDPs, we only want cities or villages.
  state.s <- state.s[!str_detect(state.s@data$NAMELSAD, "CDP"),]
  
  ##Add all coastal incorporated places for the relevant state to the coastal cities map.  
  coastal_cities.map <- coastal_cities.map %>%
    addPolygons(data = state.s, fillColor = "darkred", fillOpacity = 1, stroke = FALSE)
  
  ##Add all coastal incorporated places for the relevant state to the coastal incorporated place map.
  save.map <- save.map %>%
    addPolygons(data = state.s, fillColor = "lightblue", fillOpacity = 1, stroke = FALSE)
}

##Create a dataframe that will be used to create a legend.
legend.df <- data.frame("Name" = c("Incorporated Place", "Urbanized Area", "Urban Cluster", "Metropolitan Area", 
                                   "Micropolitan Area"), "Color" = c("darkred", "orangered", "orange", "gold", "yellow"))

##Add a label to the coastal incorporated place map
save.map <- save.map %>%
  addControl("Coastal Incorporated Places", position = c("topright"))

##Reset the working directory to the coastal cities project folder.
setwd("/Users/MeganDavis/Documents/r_code/coastal_cities")

##Save the coastal Incorporated Places map as an html widget.
#saveWidget(save.map, file = "incorp_coast.html")

##Add a legend and a label to the coastal cities map.
coastal_cities.map <- coastal_cities.map %>%
  addLegend(colors = legend.df$Color, labels = legend.df$Name, position = "bottomright") %>%
  addControl("Footprint of U.S. Coastal Cities", position = c("topright"))

##Save the coastal cities map as an html widget.
#saveWidget(coastal_cities.map, file = "coastal_cities.html")