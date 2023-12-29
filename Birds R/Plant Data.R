library(rWCVP)
library(rWCVPdata)
library(dplyr)
library(writexl)
library(readxl) 

install.packages("rWCVPdata", repos = c
                 ("https://matildabrown.github.io/drat","https://cloud.r-project.org"))
bot_countries <- st_read("Data/lvl3/level3.shp")

# taxonomy data
names <- rWCVPdata::wcvp_names #required!

# distribution data
distributions <- rWCVPdata::wcvp_distributions #required!

fruitplant_families <- read.csv('Data/Plant Data/Fruitplant_families.csv')

###Checked that 80 family names correspond to rWCVP data###

#TODO: append all 80 family
fruit_distri=data.frame(wcvp_occ_mat('Amaryllidaceae', taxon_rank = 'family'),introduced=F,extinct=F)
#only native needed
for (i in fruitplant_families$Confirmed.names){
  if (i == 'Amaryllidaceae'){
    next
  }
  list_in <- data.frame(wcvp_occ_mat(i, taxon_rank = 'family'),introduced=F,extinct=F)
  fruit_distri <- rbind(fruit_distri,list_in)
}

#write_xlsx(fruit_distri,'Data/Plant Data/fruit species distribution.xlsx')
###saved in excel file
#fruit_distri <- read_excel("Data/Plant Data/fruit species distribution.xlsx")

fruit_distri_copy <- fruit_distri[,-c(1,372,373)] #no run twice!

fruit_distri_df <- t(fruit_distri_copy)
fruit_distri_df_header <- fruit_distri_df[1,]
colnames(fruit_distri_df) <- fruit_distri_df_header
fruit_distri_df <- fruit_distri_df [-1,]
fruit_distri_df <- as.data.frame(fruit_distri_df)
fruit_distri_df <- as.data.frame(sapply(fruit_distri_df, as.numeric))

plant_BC <- data.frame(SpeciesRichness=c(rowSums(fruit_distri_df)))
plant_BC <- cbind(plant_BC,bot_countries$geometry)
plant_BC <- st_as_sf(plant_BC)
plant_BC <- cbind(Bird_data_BC$Frugivore,plant_BC)
plot(plant_BC)
