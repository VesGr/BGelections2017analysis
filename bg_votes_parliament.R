### Analysis of the 2017 Parliamentary election results in voting sections outside Bulgaria
### Author: Vesela Grozeva
### Date: April 2017

require(tidyverse)

##############################################################################################
## 1-Read in the data
##############################################################################################

### Raw data downloaded in .txt from: https://results.cik.bg/pi2017/csv.html
### We will rely on the readme.txt file to extract the features of interest and create a single DF
### All files are linked by section id (V1)


### Read in the following files and keep the following variables:
# protocols.txt - "section_id", "tot_ballots", "invalid_ballots", "valid_ballots","no_sup_ballots"
# votes.txt - "section_id", followed by pairs of party id and number of votes for that party id
# sections.txt - "section_id", "country", and "city"

### Ballots per section (protocols) ###########

protocols.raw <- read.table("protocols.txt", sep=";", header=FALSE)
View(protocols.raw)
dim(protocols.raw)

# Extract foreign sections (V1=7) and their section id (V2), # ballots (V15), # invalid ballots (V16), # valid ballots (V17)
# and # ballots that do not support any party (V19)
protocols.df <- protocols.raw %>%
                filter(V1==7) %>%
                select(num_range("V", c(2,15:17, 19)))
colnames(protocols.df) <- c("section_id", "tot_ballots", "invalid_ballots", "valid_ballots","no_sup_ballots")

# The file structure is such that the first 12070 rows refer to sections in Bulgaria
# We can skip these rows when reading in data for the foreign sections only
rows_to_skip <- dim(protocols.raw)[1] - dim(protocols.df)[1]


### Votes per section ##########

votes.raw <- read.table("votes.txt", sep=";", header=FALSE, skip=rows_to_skip)
View(votes.raw)
# The votes file starts with the section id, followed by tri-variable sequences of party id, valid votes for this party, and invalid votes for this party
# The parties are identified by numbers 1-21 
# We will have to transform this data into a "tidy" format such that each row will be identified by section id and party id
# so that each row will represent a party result for a given section
# For now we will just remove the columns corresponding to the invalid votes (V5, V8, ...V65) as well as column 2
votes.df <- votes.raw %>%
            select(-seq(5, dim(votes.raw)[2], 3)) %>%
            select(-2)
View(votes.df)       
colnames(votes.df)[1] <- c("section_id")


### Section location ############

# The city and country names of the section location are encoded in Cyrillic 
# A couple of great resources about dealing with data in Cyrillic are:
# https://www.r-bloggers.com/r-and-foreign-characters/
# http://people.fas.harvard.edu/~izahn/posts/reading-data-with-non-native-encoding-in-r/
# The difficulty in our case comes with the fact that we cannot detect what was the original encoding
# and the documentation to the files does not disclose it either

# Will use read_delim() to specify encoding as UTF-8
require(readr)
section.raw <- read_delim("sections.txt", delim=";", col_names=FALSE, skip=rows_to_skip, locale = locale(encoding = "UTF-8"))
View(section.raw)

# We can view the proper names in Cyrillic using print.listof
print.listof(head(section.raw[ ,5])) 

# Output data in Excel  
write.csv(section.raw, file="section.csv", fileEncoding="UTF-8")

# Manual edit data in Excel: enter English names of cities and countries

##############################################################################################
## 2-Merge data into ballots.df
##############################################################################################

# Read in sections data with English names
sections.df <- read.table("~/Study projects/Visualization - BG elections/Parliament - 2017/section_english.csv", sep=",", header=FALSE)
View(sections.df)

# Keep only the columns with section_id, country name, and city name
sections.df <- sections.df[, c(1,3:4)]
colnames(sections.df) <- c("section_id", "country", "city")
View(sections.df)

# section_id is the primary key for all three tables
# dublecheck that it is indeed unique in all three tables

unique_key <- function(my.df) {
  count_sections <- sections.df %>% 
                    count(section_id) %>% 
                    filter(n > 1)
  ifelse(dim(count_sections)[1]==0, "unique", "not unique")   
}

df.list <- list(protocols.df, votes.df, sections.df)
sapply(df.list, unique_key)

# IF we needed to add a surrogate key, we could have done the following, which appends
# the row # at the end of the dataframe
# votes.df %>%
#  mutate(row_number())

# Join all three dataframes
ballots.df <- protocols.df %>% 
                left_join(sections.df, by="section_id") %>%
                left_join(votes.df, by="section_id")


##############################################################################################
## 3-Create "tidy" dataframe
##############################################################################################

# We will structure ballots.df to have one row corresponding to section location and votes for a given party 
# Therefore, we will have 21 rows for each section_id (there are 21 parties)

# Since the party data columns (V3, V4, V6, ...) are not named as such, we will rename them:
colnames(ballots.df)[8:49] <- paste0(rep(c("NA", "P"), 21), c(rep(1:21, each=2)))

# We do not need the NA1-NA21 columns because the party id's are 1 to 21, which is contained in the names of columns P1-P21
# Therefore, we will reshape to have two columns - one indicating the party id and another indicating the number of votes for this party

ballots.df <- ballots.df %>%
            select(-starts_with("NA"))  %>%
            gather(num_range("P", 1:21), key="party_id", value="party_votes") %>%
            arrange(section_id)

View(ballots.df)



##############################################################################################
## 5-Add party names and % votes
##############################################################################################
# Create locations id
locations <- as.character(paste0(ballots.df$city, ", ", ballots.df$country))
ballots.df <- mutate(ballots.df, locations)

# Create party names
party_id <- paste0("P", seq(1,21))
party_names <- c("DROM", "Bulgarska prolet", "Napred Bulgaria",
                 "Nova Republica", "Koalitzia Nedovolnite", "Volia",
                 "BDC", "ABV", "DPS",
                 "BSP", "GERB", "Zelenite", 
                 "DOST", "Vyzrazdane", "Prezaredi Bulgaria", 
                 "DA Bulgaria", "Patrioti", "KOI", 
                 "NRP", "BNO", "Reformatorski Bloc")
party.df <- data.frame(party_id, party_names)

ballots.df <- left_join(ballots.df, party.df)

# Create % votes per section
ballots.df <- mutate(ballots.df, perc_vote=party_votes/valid_ballots)

# Let's summarize the data
summary(ballots.df)

# Number of countries with voting sections
length(unique(ballots.df$country))
# Number of cities with voting sections
length(unique(ballots.df$city))
# Number of voting sections
length(unique(ballots.df$section_id))


##############################################################################################
## 6-Create global map of votes
##############################################################################################

# Geocode locations
library(ggmap)
locations <- unique(locations)
head(locations)
lonlat <- geocode(locations) 
geodata.df <- data.frame(locations, lonlat)

# Visually expect results
head(geodata.df)
tail(geodata.df)

# Manually populate lon and lat for NAs
filter(geodata.df, is.na(lon))
geodata.df$lon[which(locations=="Jönköping, Sweden")] <- 14.1618
geodata.df$lat[which(locations=="Jönköping, Sweden")] <- 57.7826

# Merge with ballots.df

ballots.df <- left_join(ballots.df, geodata.df)
save(ballots.df, file="ballots")


##############################################################################################
# 7-Aggregate voting sections by city and store in a wide format for use with leaflet
##############################################################################################

# load("ballots")

# Aggregate voting sections by city
ballots_city <- ballots.df %>%
  group_by(locations, party_id, lon, lat) %>% 
  summarise(tot_ballots_bycity = sum(tot_ballots), party_votes_bycity = sum(party_votes)) %>%
  arrange(locations) %>% 
  spread(key=party_id, value=party_votes_bycity)    # Transform to wide format

# Doublecheck
filter(ballots_city, locations=="Madrid, Spain") %>%
  select(locations, lon, P16)
ballots.df %>% filter(locations=="Madrid, Spain", party_id=="P16") %>%
  select(section_id, locations, party_id, party_votes)

# Create percentage results
ballots_city$tot_valid_bycity <- rowSums(ballots_city[, c(5:25)])

sapply(names(ballots_city)[-c(1:4,26)], function(x) {
  ballots_city[,paste0(x, "_pct")] <<- ballots_city[,x]/ballots_city$tot_valid_bycity
})

ballots_city[ ,c(27:47)] <- sapply(ballots_city[ ,c(27:47)], function(x) {
  sprintf("%.2f%%", 100*x)
})

# Save
save(ballots_city, file="ballots_city")


##############################################################################################
## 8-Create global map of votes
##############################################################################################

require(leaflet)
# http://rstudio.github.io/leaflet/
# http://spatioanalytics.com/ 

popup_text <- paste0("Location: ", ballots_city$locations, "<br>Total votes: ", ballots_city$tot_ballots_bycity, "<br>DA Bulgaria: ", ballots_city$P16, "<br>GERB: ", ballots_city$P11, "<br>BSP: ", ballots_city$P10, "<br>Patrioti: ", ballots_city$P17, "<br>Volia: ", ballots_city$P6, "<br>RB: ", ballots_city$P21, "<br>DPS: ", ballots_city$P9, "<br>DOST: ", ballots_city$P13)

leaflet(ballots_city) %>% addTiles() %>% addMarkers(popup=popup_text) 



##############################################################################################
## 9-Examining the performance of DA Bulgaria
##############################################################################################


# Create dataframes at different levels of aggregation for use in markdown file
# Since Turkey has historically been an outlier and this election is not an exception, we will exclude it from the analysis. 

votes_turkey <- ballots.df %>% 
  filter(country=="Turkey") %>%
  group_by(party_id, party_names, country) %>%
  summarize(agg_votes_byparty = sum(party_votes)) %>%
  select(country, agg_votes_byparty, party_id, party_names)
save(votes_turkey, file="votes_turkey")


### Create dataframe with aggregate votes excluding Turkey
ballots_agg <- ballots.df %>% 
           filter(country!="Turkey") %>%
           group_by(party_id, party_names) %>%
           mutate(agg_votes_byparty = sum(party_votes)) %>%
           select(agg_votes_byparty, party_id, party_names)

ballots_agg <- ballots_agg[1:21,]

ballots_agg_valid <- ballots.df %>% 
          filter(country!="Turkey" & party_id=="P1") %>%
          mutate(agg_validvotes = sum(valid_ballots)) %>%
          select(agg_validvotes)

ballots_agg$valid <- ballots_agg_valid[1,1]

ballots_agg <- select(agg_votes, valid, agg_votes_byparty, party_id, party_names)

# Calculate each party's share of the vote
ballots_agg$agg_votes_perc <- ballots_agg$agg_votes_byparty/ballots_agg$valid

save(ballots_agg, file="ballots_agg")


### Create dataframe with aggregate votes by country (exclude Turkey)
ballots_country <- ballots.df %>%
  group_by(country, party_id, party_names) %>% 
  summarise(tot_ballots_bycountry = sum(tot_ballots), valid_bycountry = sum(valid_ballots), party_votes_bycountry = sum(party_votes)) %>%
  arrange(country) %>%
  mutate(votes_perc_bycountry = party_votes_bycountry/valid_bycountry)
save(ballots_country, file="ballots_country")


### Create dataframe with aggregate votes by city (exclude Turkey) in long format
ballots_city2 <- ballots.df %>%
  group_by(city, country, party_id, party_names) %>% 
  summarise(tot_ballots_bycity = sum(tot_ballots), valid_bycity = sum(valid_ballots), party_votes_bycity = sum(party_votes)) %>%
  arrange(city) %>%
  mutate(votes_perc_bycity = party_votes_bycity/valid_bycity)
save(ballots_city2, file="ballots_city2")



 