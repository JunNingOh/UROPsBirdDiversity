##Information
#This script is the collation and cleaning of bird, plant, geographical data for use



#Libraries imported
library(readxl)
library(ggplot2)
library(reshape2)
library(readr)
library(dplyr)
library(FD)
library(rWCVP)
library(terra)
library(sf)
####################Import Data#########################################
#raw_bird_geog is a data with bird species matched with their
#level 3 botanical countries (BC)

#bird_geog_data is data of BC against species

#traits_data contains 18 measured traits of bird species

#we're focusing on frugivorous birds categorised in Trophic.Niche
#as fruits making up 60% of their diet
########################################################################
traits_data <- read_excel("Bird_traits_cleaned.xlsx", 
                          sheet = "AVONET1_BirdLife")
traits_data=traits_data[,-1]

frug_birds <- traits_data[traits_data$Trophic.Niche=='Frugivore',]
#Removed non-measurement related categories from Avonet1_Birdlife data
raw_bird_geog <- read.table("Occurrences_GlobalBirds_TDWG.txt", header=T,sep="")

bird_geog_data <- acast(data = raw_bird_geog,
                        formula = LEVEL_3_CO ~ SpecName,  
                        fill = 0,
                        fun.aggregate = length,
                        value.var = "SpecName")

###Data matching###
#proportion of bird with geog data with matching trait data
raw_bird_geog$SpecName = as.factor(raw_bird_geog$SpecName)

match_birdtrait <- semi_join(raw_bird_geog,traits_data,by = c('SpecName'='Species1'))
no_birdtrait <- anti_join(raw_bird_geog,traits_data,by = c('SpecName'='Species1'))
match_birdtrait_byBC <- acast(data=match_birdtrait,
                           formula = LEVEL_3_CO ~ SpecName,
                           fun.aggregate = length,
                           value.var = 'SpecName')

#####Visualise frugivore traits########
frug_trait_melt <- melt(frug_birds, id.vars = c("Species1"))
ggplot(data = frug_trait_melt) +
  geom_histogram(aes(x = value), stat = "count") +
  facet_wrap(~variable, scales = "free")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

###Frugivore species richness###
no_

####Species Richness###
Bird_data_BC = data.frame(Initial=c(rowSums(bird_geog_data)))

bot_countries <- st_read("lvl3/level3.shp")
areas<-st_area(bot_countries)
Bird_data_BC=cbind(Bird_data_BC,areas)
Bird_data_BC$`Yes data` <- rowSums(match_birdtrait_byBC)
Bird_data_BC$Percentage <- Bird_data_BC$`Yes data`/Bird_data_BC$Initial
Bird_data_BC$areacode <- row.names(Bird_data_BC)
Bird_data_BC$geometry <- st_geometry(bot_countries$geometry)

Data_availability <- Bird_data_BC[,c('Percentage','geometry')]


Data_availability_sf <- st_as_sf(Data_availability, wkt = 'geometry')
ggplot() +
  geom_sf(data = Data_availability, aes(fill = 'Percentage'), color = "white", size = 0.5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal()

summary(finalbirdfit)
#Call: S==d/(1+exp(-z*A+c))
#converge:true
#d = 67.709
#z = 0.021256
#c = 3.6780


Bird_data_BC$`species richness`= Bird_data_BC$Initial*(1+exp((-0.02)*Bird_data_BC$areas+3.6780))
remove_outlier=Bird_data_BC[-15,]
ggplot(data = remove_outlier,aes(y=`species richness`,x=areas)) +
  geom_point(aes(col=Initial))
ggplot(data = Bird_data_BC,aes(y=`species richness`,x=areas)) +
  geom_point(aes(col=Initial))
  
#####Functional Diversity########
FD = dbFD(x=traits_data, a = bird_geog_data, w.abun = F, stand.x = T)

