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
#######Import Data########
traits_data <- read_excel("Bird_traits_cleaned.xlsx", 
                          sheet = "AVONET1_BirdLife")
traits_data=traits_data[,-1]
#Removed non-measurement related categories from Avonet1_Birdlife data
raw_bird_geog <- read.table("Occurrences_GlobalBirds_TDWG.txt", header=T,sep="")

bird_geog_data <- acast(data = raw_bird_geog,
                        formula = LEVEL_3_CO ~ SpecName,  
                        fill = 0,
                        fun.aggregate = length,
                        value.var = "SpecName")


#####Visualise########
trait_data_melt <- melt(traits_data, id.vars = c("Species1"))
ggplot(data = trait_data_melt) +
  geom_histogram(aes(x = value), stat = "count") +
  facet_wrap(~variable, scales = "free")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

####Taxonomic Diversity/Species Richness####
Bird_data_BC = data.frame(Initial=c(rowSums(bird_geog_data)))

bot_countries <- st_read("lvl3/level3.shp")
areas<-st_area(bot_countries)
Bird_data_BC=cbind(Bird_data_BC,areas)

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

