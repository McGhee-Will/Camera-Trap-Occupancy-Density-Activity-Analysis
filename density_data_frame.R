# DATA FRAM FOR REM MODELS

library(terra)
library(dplyr)
library(sf)
library(sfheaders)

setwd("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis")

# REM data
REMdata <- read.csv("REM_data.csv")
head(REMdata)
# remove squirrels
squREM <- subset(REMdata, REMdata$species == "squirrel")
REMdata <- subset(REMdata, species %in% c("deer", "fox"))

# light pollution data
light <- terra::rast("Openspacesites/GCC_light.tif")

# noise pollution data
noise <- terra::rast("Openspacesites/GCC_noise.tif")

# site polygons
spaces <- vect("Openspacesites/Openspacesites/Site_polygon/gdb_data_vectssnew.shp")

# land cover data
LC <- rast("Openspacesites/FME_35646466_1737655111641_7289/data/LCM.tif")

# camera locations
cam1.locs <- vect("Openspacesites/Openspacesites/Camera_locations/RandomPoints_round1_sampling.shp")
cam2.locs <- vect("Openspacesites/Openspacesites/Camera_locations/points_projected_round2.shp")
# add placement column
cam1.locs$placement <- 1
cam2.locs$placement <- 2
# remove extra point for audio at F1
cam2.locs$site
cam2.locs <- cam2.locs[-c(28),]
table(cam2.locs$site, cam2.locs$equip)
# fix column headings
cam1.locs <- cam1.locs[,c("site", "placement")]
cam2.locs <- cam2.locs[,c("site", "placement")]
head(cam1.locs)
head(cam2.locs)
# combine point objects
all.locs <- rbind(cam1.locs, cam2.locs)
table(all.locs$site, all.locs$placement)

####################################################
### LOCATE CENTROID OF CAMERA LOCATIONS PER SITE ###
####################################################

# new column with site names
all.locs$space <- NA
all.locs$space[all.locs$site == "A0"] <- "Blawarthill"
all.locs$space[all.locs$site == "A1"] <- "Trinley"
all.locs$space[all.locs$site == "A2"] <- "Dawsholm"
all.locs$space[all.locs$site == "A2/2"] <- "Dawsholm"
all.locs$space[all.locs$site == "B0"] <- "Fortingall"
all.locs$space[all.locs$site == "B2"] <- "Kelvingrove"
all.locs$space[all.locs$site == "B2/2"] <- "Kelvingrove"
all.locs$space[all.locs$site == "C0"] <- "Maryhill"
all.locs$space[all.locs$site == "C1"] <- "Ruchill"
all.locs$space[all.locs$site == "C1/2"] <- "Ruchill"
all.locs$space[all.locs$site == "C2"] <- "Woodside"
all.locs$space[all.locs$site == "D1"] <- "Cowlairs"
all.locs$space[all.locs$site == "D1/2"] <- "Cowlairs"
all.locs$space[all.locs$site == "D2"] <- "St_mungo"
all.locs$space[all.locs$site == "E0"] <- "G_Green"
all.locs$space[all.locs$site == "E0/2"] <- "G_Green"
all.locs$space[all.locs$site == "E1"] <- "Hogganfield"
all.locs$space[all.locs$site == "E1/2"] <- "Hogganfield"
all.locs$space[all.locs$site == "E2"] <- "Greenfield"
all.locs$space[all.locs$site == "F0"] <- "Lightburn"
all.locs$space[all.locs$site == "F1"] <- "Maxwell"
all.locs$space[all.locs$site == "F2"] <- "Balado"
all.locs$space[all.locs$site == "G0"] <- "Garrowhill"
all.locs$space[all.locs$site == "G1"] <- "Farmington"
all.locs$space[all.locs$site == "G2"] <- "Huntingtower"
all.locs$space[all.locs$site == "H0"] <- "Crookston"
all.locs$space[all.locs$site == "H1"] <- "Corkerhill"
all.locs$space[all.locs$site == "H2"] <- "King_G_V"
all.locs$space[all.locs$site == "I0"] <- "Pollock"
all.locs$space[all.locs$site == "I1"] <- "Pollock"
all.locs$space[all.locs$site == "I2"] <- "Pollock"
all.locs$space[all.locs$site == "J0"] <- "Cathkin"
all.locs$space[all.locs$site == "J1"] <- "Cathkin"
all.locs$space[all.locs$site == "J2"] <- "Cathkin"
levels(as.factor(all.locs$space))

# factor
all.locs$space <- as.factor(all.locs$space)

# identify unique site names
sites <- unique(all.locs$space)
# new data frame
site.centroids <- data.frame(sites = sites)
site.centroids$geometry <- NA

for (i in seq_along(sites)){
  # subset by site
  space <- sites[i] # select unique space
  site <- subset(all.locs, all.locs$space == space) # subset
  # extract geometry
  centroid.df <- geom(site)
  centroid.df <- as.data.frame(centroid.df)
  # calculate centroid
  centroid <- sf_multipoint(centroid.df[,c("x", "y")]) %>%
    st_centroid()
  # add centroid to data frame
  site.centroids$geometry[i] <- centroid$geometry
}

site.centroids$x <- 0
site.centroids$y <- 0

for (i in 1:nrow(site.centroids)) {
  # select geometry
  geometry <- site.centroids$geometry[[i]]
  # change to data frame
  geometry <- as.data.frame(geometry)
  # extract coordinates
  x.coord <- geometry[1,]
  y.coord <- geometry[2,]
  # add to data frame
  site.centroids$x[i] <- x.coord
  site.centroids$y[i] <- y.coord
}

# set site centroids as a spatial object
site.centroids <- vect(site.centroids, geom = c("x", "y"))

# save as shapefile
writeVector(site.centroids, "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/Openspacesites/centroids.shp", overwrite=T)

################################
### BUFFERS AROUND CENTROIDS ###
################################

buff250 <- terra::buffer(site.centroids, width = 250)
buff1km <- terra::buffer(site.centroids, width = 1000)

### 250m ###

# extract land cover in sites and buffers
int.list250 <- list()

for (i in 1:length(buff250)){
  int.list250[[i]] <- terra::extract(LC, buff250[i,])
  print(i)
}

head(int.list250[[1]]) 

# We're only interested in LC1
ss250 <- lapply(int.list250, "[", 2)

head(ss250[[1]]) 

# Create an empty dataframe to populate later
landcover250 <- data.frame(matrix(NA, nrow = length(buff250),
                                  ncol = 21))

# Add column names corresponding to landcover types
names(landcover250) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                          "11", "12", "13", "14", "15", "16", "17", "18",
                          "19", "20", "21")

head(landcover250)

# Run loop to add number of cells of each landcover type into dataframe
for (i in 1:length(buff250)){
  for ( j in 1:length(names(landcover250))) {
    landcover250[i,j] <- table(
      ss250[[i]])[as.character(names(landcover250)[j])]
  }}

# Name columns with landcover types
names(landcover250) <- c("Broadleaved woodland",
                         "Coniferous woodland",
                         "Arable",
                         "Improved grassland",
                         "Neutral grassland",
                         "Calcareous grassland",
                         "Acid grassland",
                         "Fen, marsh and swamp",
                         "Heather and shrub",
                         "Heather grassland",
                         "Bog",
                         "Inland rock",
                         "Saltwater",
                         "water",
                         "Supralittoral rock",
                         "Supralittoral sediment",
                         "Littoral rock",
                         "Littoral sediment",
                         "Saltmarsh",
                         "Urban",
                         "Suburban")

# Replace NAs with 0
landcover250[is.na(landcover250)] <- 0

# Calculate number of cells in each open space buffer
row_sum250 <- rowSums(landcover250)

# Calculate proportions by dividing number of cells of each landcover with number of cells in each open space buffer
prop250 <- landcover250/row_sum250

# Round the proportions
prop250 <- round(prop250, 3)

# Replace NA with 0
prop250[is.na(prop250)] <- 0

# new data frame to extract proportions into
df.prop250 <- as.data.frame(buff250[,c("sites")])

# group classes and add proportions
df.prop250$wood250 <- rowSums(prop250[,c(1,2)])# broadleaf and conifer
df.prop250$wet250 <- rowSums(prop250[,c(8, 11, 19)])# fen,marsh,swamp,bog, saltmarsh
df.prop250$urban250 <- rowSums(prop250[,c(20,21)])# urban suburban
df.prop250$water250 <- prop250[,c(14)] #water
df.prop250$grass250 <- rowSums(prop250[,c(4,5,6,7,9, 10)]) # improved, neutral, calcareous, acid, heather and shrub, heather grassland 
df.prop250$arable250 <- prop250[,c(3)] #arable


### 1km ###

# extract land cover in sites and buffers
int.list1km <- list()

for (i in 1:length(buff1km)){
  int.list1km[[i]] <- terra::extract(LC, buff1km[i,])
  print(i)
}

head(int.list1km[[1]]) 

# We're only interested in LC1
ss1km <- lapply(int.list1km, "[", 2)

head(ss1km[[1]]) 

# Create an empty dataframe to populate later
landcover1km <- data.frame(matrix(NA, nrow = length(buff1km),
                                  ncol = 21))

# Add column names corresponding to landcover types
names(landcover1km) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                          "11", "12", "13", "14", "15", "16", "17", "18",
                          "19", "20", "21")

head(landcover1km)

# Run loop to add number of cells of each landcover type into dataframe
for (i in 1:length(buff1km)){
  for ( j in 1:length(names(landcover1km))) {
    landcover1km[i,j] <- table(
      ss1km[[i]])[as.character(names(landcover1km)[j])]
  }}

# Name columns with landcover types
names(landcover1km) <- c("Broadleaved woodland",
                         "Coniferous woodland",
                         "Arable",
                         "Improved grassland",
                         "Neutral grassland",
                         "Calcareous grassland",
                         "Acid grassland",
                         "Fen, marsh and swamp",
                         "Heather and shrub",
                         "Heather grassland",
                         "Bog",
                         "Inland rock",
                         "Saltwater",
                         "water",
                         "Supralittoral rock",
                         "Supralittoral sediment",
                         "Littoral rock",
                         "Littoral sediment",
                         "Saltmarsh",
                         "Urban",
                         "Suburban")

# Replace NAs with 0
landcover1km[is.na(landcover1km)] <- 0

# Calculate number of cells in each open space buffer
row_sum1km <- rowSums(landcover1km)

# Calculate proportions by dividing number of cells of each landcover with number of cells in each open space buffer
prop1km <- landcover1km/row_sum1km

# Round the proportions
prop1km <- round(prop1km, 3)

# Replace NA with 0
prop1km[is.na(prop1km)] <- 0

# new data frame to extract proportions into
df.prop1km <- as.data.frame(buff1km[,c("sites")])

# group classes and add proportions
df.prop1km$wood1km <- rowSums(prop1km[,c(1,2)])# broadleaf and conifer
df.prop1km$wet1km <- rowSums(prop1km[,c(8, 11, 19)])# fen,marsh,swamp,bog, saltmarsh
df.prop1km$urban1km <- rowSums(prop1km[,c(20,21)])# urban suburban
df.prop1km$water1km <- prop1km[,c(14)] #water
df.prop1km$grass1km <- rowSums(prop1km[,c(4,5,6,7,9, 10)]) # improved, neutral, calcareous, acid, heather and shrub, heather grassland 
df.prop1km$arable1km <- prop1km[,c(3)] #arable

#########################################################
### ENVIRONMENTAL VALUES OF BUFFERS TO REM DATA FRAME ###
#########################################################

head(REMdata)
head(df.prop1km)

# new site ID column in buffer dfs
df.prop250$site_ID <- NA
df.prop1km$site_ID <- NA
# identify site IDs
df.prop250$site_ID[df.prop250$sites == "Blawarthill"] <- "A0"
df.prop250$site_ID[df.prop250$sites == "Trinley"] <- "A1"
df.prop250$site_ID[df.prop250$sites == "Dawsholm"] <- "A2"
df.prop250$site_ID[df.prop250$sites == "Fortingall"] <- "B0"
df.prop250$site_ID[df.prop250$sites == "Kelvingrove"] <- "B2"
df.prop250$site_ID[df.prop250$sites == "Maryhill"] <- "C0"
df.prop250$site_ID[df.prop250$sites == "Ruchill"] <- "C1"
df.prop250$site_ID[df.prop250$sites == "Woodside"] <- "C2"
df.prop250$site_ID[df.prop250$sites == "Cowlairs"] <- "D1"
df.prop250$site_ID[df.prop250$sites == "St_mungo"] <- "D2"
df.prop250$site_ID[df.prop250$sites == "G_Green"] <- "E0"
df.prop250$site_ID[df.prop250$sites == "Hogganfield"] <- "E1"
df.prop250$site_ID[df.prop250$sites == "Greenfield"] <- "E2"
df.prop250$site_ID[df.prop250$sites == "Lightburn"] <- "F0"
df.prop250$site_ID[df.prop250$sites == "Maxwell"] <- "F1"
df.prop250$site_ID[df.prop250$sites == "Balado"] <- "F2"
df.prop250$site_ID[df.prop250$sites == "Garrowhill"] <- "G0"
df.prop250$site_ID[df.prop250$sites == "Farmington"] <- "G1"
df.prop250$site_ID[df.prop250$sites == "Huntingtower"] <- "G2"
df.prop250$site_ID[df.prop250$sites == "Crookston"] <- "H0"
df.prop250$site_ID[df.prop250$sites == "Corkerhill"] <- "H1"
df.prop250$site_ID[df.prop250$sites == "King_G_V"] <- "H2"
df.prop250$site_ID[df.prop250$sites == "Pollock"] <- "I"
df.prop250$site_ID[df.prop250$sites == "Cathkin"] <- "J"
head(df.prop250, 24)
df.prop1km$site_ID[df.prop1km$sites == "Blawarthill"] <- "A0"
df.prop1km$site_ID[df.prop1km$sites == "Trinley"] <- "A1"
df.prop1km$site_ID[df.prop1km$sites == "Dawsholm"] <- "A2"
df.prop1km$site_ID[df.prop1km$sites == "Fortingall"] <- "B0"
df.prop1km$site_ID[df.prop1km$sites == "Kelvingrove"] <- "B2"
df.prop1km$site_ID[df.prop1km$sites == "Maryhill"] <- "C0"
df.prop1km$site_ID[df.prop1km$sites == "Ruchill"] <- "C1"
df.prop1km$site_ID[df.prop1km$sites == "Woodside"] <- "C2"
df.prop1km$site_ID[df.prop1km$sites == "Cowlairs"] <- "D1"
df.prop1km$site_ID[df.prop1km$sites == "St_mungo"] <- "D2"
df.prop1km$site_ID[df.prop1km$sites == "G_Green"] <- "E0"
df.prop1km$site_ID[df.prop1km$sites == "Hogganfield"] <- "E1"
df.prop1km$site_ID[df.prop1km$sites == "Greenfield"] <- "E2"
df.prop1km$site_ID[df.prop1km$sites == "Lightburn"] <- "F0"
df.prop1km$site_ID[df.prop1km$sites == "Maxwell"] <- "F1"
df.prop1km$site_ID[df.prop1km$sites == "Balado"] <- "F2"
df.prop1km$site_ID[df.prop1km$sites == "Garrowhill"] <- "G0"
df.prop1km$site_ID[df.prop1km$sites == "Farmington"] <- "G1"
df.prop1km$site_ID[df.prop1km$sites == "Huntingtower"] <- "G2"
df.prop1km$site_ID[df.prop1km$sites == "Crookston"] <- "H0"
df.prop1km$site_ID[df.prop1km$sites == "Corkerhill"] <- "H1"
df.prop1km$site_ID[df.prop1km$sites == "King_G_V"] <- "H2"
df.prop1km$site_ID[df.prop1km$sites == "Pollock"] <- "I"
df.prop1km$site_ID[df.prop1km$sites == "Cathkin"] <- "J"
head(df.prop1km, 24)

### new columns in REM data frame
# 250m buffer
REMdata$wood250 <- 0
REMdata$wet250 <- 0
REMdata$urban250 <- 0
REMdata$water250 <- 0
REMdata$grass250 <- 0
REMdata$arable250 <- 0
# 1km buffer
REMdata$wood1km <- 0
REMdata$wet1km <- 0
REMdata$urban1km <- 0
REMdata$water1km <- 0
REMdata$grass1km <- 0
REMdata$arable1km <- 0

# identify useful columns 
prop_columns <- names(df.prop250[2:7])
# add proportions of 250m buffer zones
REMdata[REMdata$site_ID == "A0", prop_columns] <- df.prop250[df.prop250$site_ID == "A0", prop_columns]
REMdata[REMdata$site_ID == "A1", prop_columns] <- df.prop250[df.prop250$site_ID == "A1", prop_columns]
REMdata[REMdata$site_ID == "A2", prop_columns] <- df.prop250[df.prop250$site_ID == "A2", prop_columns]
REMdata[REMdata$site_ID == "B0", prop_columns] <- df.prop250[df.prop250$site_ID == "B0", prop_columns]
REMdata[REMdata$site_ID == "B2", prop_columns] <- df.prop250[df.prop250$site_ID == "B2", prop_columns]
REMdata[REMdata$site_ID == "C0", prop_columns] <- df.prop250[df.prop250$site_ID == "C0", prop_columns]
REMdata[REMdata$site_ID == "C1", prop_columns] <- df.prop250[df.prop250$site_ID == "C1", prop_columns]
REMdata[REMdata$site_ID == "C2", prop_columns] <- df.prop250[df.prop250$site_ID == "C2", prop_columns]
REMdata[REMdata$site_ID == "D1", prop_columns] <- df.prop250[df.prop250$site_ID == "D1", prop_columns]
REMdata[REMdata$site_ID == "D2", prop_columns] <- df.prop250[df.prop250$site_ID == "D2", prop_columns]
REMdata[REMdata$site_ID == "E0", prop_columns] <- df.prop250[df.prop250$site_ID == "E0", prop_columns]
REMdata[REMdata$site_ID == "E1", prop_columns] <- df.prop250[df.prop250$site_ID == "E1", prop_columns]
REMdata[REMdata$site_ID == "E2", prop_columns] <- df.prop250[df.prop250$site_ID == "E2", prop_columns]
REMdata[REMdata$site_ID == "F0", prop_columns] <- df.prop250[df.prop250$site_ID == "F0", prop_columns]
REMdata[REMdata$site_ID == "F1", prop_columns] <- df.prop250[df.prop250$site_ID == "F1", prop_columns]
REMdata[REMdata$site_ID == "F2", prop_columns] <- df.prop250[df.prop250$site_ID == "F2", prop_columns]
REMdata[REMdata$site_ID == "G0", prop_columns] <- df.prop250[df.prop250$site_ID == "G0", prop_columns]
REMdata[REMdata$site_ID == "G1", prop_columns] <- df.prop250[df.prop250$site_ID == "G1", prop_columns]
REMdata[REMdata$site_ID == "G2", prop_columns] <- df.prop250[df.prop250$site_ID == "G2", prop_columns]
REMdata[REMdata$site_ID == "H0", prop_columns] <- df.prop250[df.prop250$site_ID == "H0", prop_columns]
REMdata[REMdata$site_ID == "H1", prop_columns] <- df.prop250[df.prop250$site_ID == "H1", prop_columns]
REMdata[REMdata$site_ID == "H2", prop_columns] <- df.prop250[df.prop250$site_ID == "H2", prop_columns]
REMdata[REMdata$site_ID == "I", prop_columns] <- df.prop250[df.prop250$site_ID == "I", prop_columns]
REMdata[REMdata$site_ID == "J", prop_columns] <- df.prop250[df.prop250$site_ID == "J", prop_columns]
# redefine prop_columns
prop_columns <- names(df.prop1km[2:7])
# add proportions of 1km buffer zones
REMdata[REMdata$site_ID == "A0", prop_columns] <- df.prop1km[df.prop1km$site_ID == "A0", prop_columns]
REMdata[REMdata$site_ID == "A1", prop_columns] <- df.prop1km[df.prop1km$site_ID == "A1", prop_columns]
REMdata[REMdata$site_ID == "A2", prop_columns] <- df.prop1km[df.prop1km$site_ID == "A2", prop_columns]
REMdata[REMdata$site_ID == "B0", prop_columns] <- df.prop1km[df.prop1km$site_ID == "B0", prop_columns]
REMdata[REMdata$site_ID == "B2", prop_columns] <- df.prop1km[df.prop1km$site_ID == "B2", prop_columns]
REMdata[REMdata$site_ID == "C0", prop_columns] <- df.prop1km[df.prop1km$site_ID == "C0", prop_columns]
REMdata[REMdata$site_ID == "C1", prop_columns] <- df.prop1km[df.prop1km$site_ID == "C1", prop_columns]
REMdata[REMdata$site_ID == "C2", prop_columns] <- df.prop1km[df.prop1km$site_ID == "C2", prop_columns]
REMdata[REMdata$site_ID == "D1", prop_columns] <- df.prop1km[df.prop1km$site_ID == "D1", prop_columns]
REMdata[REMdata$site_ID == "D2", prop_columns] <- df.prop1km[df.prop1km$site_ID == "D2", prop_columns]
REMdata[REMdata$site_ID == "E0", prop_columns] <- df.prop1km[df.prop1km$site_ID == "E0", prop_columns]
REMdata[REMdata$site_ID == "E1", prop_columns] <- df.prop1km[df.prop1km$site_ID == "E1", prop_columns]
REMdata[REMdata$site_ID == "E2", prop_columns] <- df.prop1km[df.prop1km$site_ID == "E2", prop_columns]
REMdata[REMdata$site_ID == "F0", prop_columns] <- df.prop1km[df.prop1km$site_ID == "F0", prop_columns]
REMdata[REMdata$site_ID == "F1", prop_columns] <- df.prop1km[df.prop1km$site_ID == "F1", prop_columns]
REMdata[REMdata$site_ID == "F2", prop_columns] <- df.prop1km[df.prop1km$site_ID == "F2", prop_columns]
REMdata[REMdata$site_ID == "G0", prop_columns] <- df.prop1km[df.prop1km$site_ID == "G0", prop_columns]
REMdata[REMdata$site_ID == "G1", prop_columns] <- df.prop1km[df.prop1km$site_ID == "G1", prop_columns]
REMdata[REMdata$site_ID == "G2", prop_columns] <- df.prop1km[df.prop1km$site_ID == "G2", prop_columns]
REMdata[REMdata$site_ID == "H0", prop_columns] <- df.prop1km[df.prop1km$site_ID == "H0", prop_columns]
REMdata[REMdata$site_ID == "H1", prop_columns] <- df.prop1km[df.prop1km$site_ID == "H1", prop_columns]
REMdata[REMdata$site_ID == "H2", prop_columns] <- df.prop1km[df.prop1km$site_ID == "H2", prop_columns]
REMdata[REMdata$site_ID == "I", prop_columns] <- df.prop1km[df.prop1km$site_ID == "I", prop_columns]
REMdata[REMdata$site_ID == "J", prop_columns] <- df.prop1km[df.prop1km$site_ID == "J", prop_columns]

##############################################
### AVERAGE NOISE POLLUTION ACROSS BUFFERS ###
##############################################

# check order of sites (should be alphabetical)
head(spaces$SITE_ID, 24)

# extract noise pollution values, per site
np250 <- extract(noise, buff250)
np1km <- extract(noise, buff1km)

# mean noise pollution value, per site
# 250
mean_np250 <- np250 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_noise))
# 1km
mean_np1km <- np1km %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_noise))

# new column2 in REM data frame
REMdata$noise250 <- 0
REMdata$noise1km <- 0

# add mean noise pollution values for 250 buffer
REMdata[REMdata$site_ID == "A0", c("noise250")] <- mean_np250[mean_np250$ID == "1", c("mean")]
REMdata[REMdata$site_ID == "A1", c("noise250")] <- mean_np250[mean_np250$ID == "2", c("mean")]
REMdata[REMdata$site_ID == "A2", c("noise250")] <- mean_np250[mean_np250$ID == "3", c("mean")]
REMdata[REMdata$site_ID == "B0", c("noise250")] <- mean_np250[mean_np250$ID == "4", c("mean")]
REMdata[REMdata$site_ID == "B2", c("noise250")] <- mean_np250[mean_np250$ID == "5", c("mean")]
REMdata[REMdata$site_ID == "C0", c("noise250")] <- mean_np250[mean_np250$ID == "6", c("mean")]
REMdata[REMdata$site_ID == "C1", c("noise250")] <- mean_np250[mean_np250$ID == "7", c("mean")]
REMdata[REMdata$site_ID == "C2", c("noise250")] <- mean_np250[mean_np250$ID == "8", c("mean")]
REMdata[REMdata$site_ID == "D1", c("noise250")] <- mean_np250[mean_np250$ID == "9", c("mean")]
REMdata[REMdata$site_ID == "D2", c("noise250")] <- mean_np250[mean_np250$ID == "10", c("mean")]
REMdata[REMdata$site_ID == "E0", c("noise250")] <- mean_np250[mean_np250$ID == "11", c("mean")]
REMdata[REMdata$site_ID == "E1", c("noise250")] <- mean_np250[mean_np250$ID == "12", c("mean")]
REMdata[REMdata$site_ID == "E2", c("noise250")] <- mean_np250[mean_np250$ID == "13", c("mean")]
REMdata[REMdata$site_ID == "F0", c("noise250")] <- mean_np250[mean_np250$ID == "14", c("mean")]
REMdata[REMdata$site_ID == "F1", c("noise250")] <- mean_np250[mean_np250$ID == "15", c("mean")]
REMdata[REMdata$site_ID == "F2", c("noise250")] <- mean_np250[mean_np250$ID == "16", c("mean")]
REMdata[REMdata$site_ID == "G0", c("noise250")] <- mean_np250[mean_np250$ID == "17", c("mean")]
REMdata[REMdata$site_ID == "G1", c("noise250")] <- mean_np250[mean_np250$ID == "18", c("mean")]
REMdata[REMdata$site_ID == "G2", c("noise250")] <- mean_np250[mean_np250$ID == "19", c("mean")]
REMdata[REMdata$site_ID == "H0", c("noise250")] <- mean_np250[mean_np250$ID == "20", c("mean")]
REMdata[REMdata$site_ID == "H1", c("noise250")] <- mean_np250[mean_np250$ID == "21", c("mean")]
REMdata[REMdata$site_ID == "H2", c("noise250")] <- mean_np250[mean_np250$ID == "22", c("mean")]
REMdata[REMdata$site_ID == "I", c("noise250")] <- mean_np250[mean_np250$ID == "23", c("mean")]
REMdata[REMdata$site_ID == "J", c("noise250")] <- mean_np250[mean_np250$ID == "24", c("mean")]

summary(REMdata$noise250)
summary(mean_np250$mean)

# add mean noise pollution values for 1km buffer
REMdata[REMdata$site_ID == "A0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "1", c("mean")]
REMdata[REMdata$site_ID == "A1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "2", c("mean")]
REMdata[REMdata$site_ID == "A2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "3", c("mean")]
REMdata[REMdata$site_ID == "B0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "4", c("mean")]
REMdata[REMdata$site_ID == "B2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "5", c("mean")]
REMdata[REMdata$site_ID == "C0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "6", c("mean")]
REMdata[REMdata$site_ID == "C1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "7", c("mean")]
REMdata[REMdata$site_ID == "C2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "8", c("mean")]
REMdata[REMdata$site_ID == "D1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "9", c("mean")]
REMdata[REMdata$site_ID == "D2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "10", c("mean")]
REMdata[REMdata$site_ID == "E0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "11", c("mean")]
REMdata[REMdata$site_ID == "E1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "12", c("mean")]
REMdata[REMdata$site_ID == "E2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "13", c("mean")]
REMdata[REMdata$site_ID == "F0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "14", c("mean")]
REMdata[REMdata$site_ID == "F1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "15", c("mean")]
REMdata[REMdata$site_ID == "F2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "16", c("mean")]
REMdata[REMdata$site_ID == "G0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "17", c("mean")]
REMdata[REMdata$site_ID == "G1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "18", c("mean")]
REMdata[REMdata$site_ID == "G2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "19", c("mean")]
REMdata[REMdata$site_ID == "H0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "20", c("mean")]
REMdata[REMdata$site_ID == "H1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "21", c("mean")]
REMdata[REMdata$site_ID == "H2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "22", c("mean")]
REMdata[REMdata$site_ID == "I", c("noise1km")] <- mean_np1km[mean_np1km$ID == "23", c("mean")]
REMdata[REMdata$site_ID == "J", c("noise1km")] <- mean_np1km[mean_np1km$ID == "24", c("mean")]

summary(REMdata$noise1km)
summary(mean_np1km$mean)

##############################################
### AVERAGE LIGHT POLLUTION ACROSS BUFFERS ###
##############################################

# fix crs of buffer objects
crs(buff250) <- crs(all.locs)
crs(buff1km) <- crs(all.locs)

# extract light pollution values per site
lp250 <- extract(light$GCC_light_3, buff250)
lp1km <- extract(light$GCC_light_3, buff1km)

# mean light pollution per site
# 250
mean_lp250 <- lp250 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_light_3))
# 1km
mean_lp1km <- lp1km %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_light_3))

# new columns in REM data frame
REMdata$light250 <- 0
REMdata$light1km <- 0

# add mean light pollution values for 250 buffer
REMdata[REMdata$site_ID == "A0", c("light250")] <- mean_lp250[mean_lp250$ID == "1", c("mean")]
REMdata[REMdata$site_ID == "A1", c("light250")] <- mean_lp250[mean_lp250$ID == "2", c("mean")]
REMdata[REMdata$site_ID == "A2", c("light250")] <- mean_lp250[mean_lp250$ID == "3", c("mean")]
REMdata[REMdata$site_ID == "B0", c("light250")] <- mean_lp250[mean_lp250$ID == "4", c("mean")]
REMdata[REMdata$site_ID == "B2", c("light250")] <- mean_lp250[mean_lp250$ID == "5", c("mean")]
REMdata[REMdata$site_ID == "C0", c("light250")] <- mean_lp250[mean_lp250$ID == "6", c("mean")]
REMdata[REMdata$site_ID == "C1", c("light250")] <- mean_lp250[mean_lp250$ID == "7", c("mean")]
REMdata[REMdata$site_ID == "C2", c("light250")] <- mean_lp250[mean_lp250$ID == "8", c("mean")]
REMdata[REMdata$site_ID == "D1", c("light250")] <- mean_lp250[mean_lp250$ID == "9", c("mean")]
REMdata[REMdata$site_ID == "D2", c("light250")] <- mean_lp250[mean_lp250$ID == "10", c("mean")]
REMdata[REMdata$site_ID == "E0", c("light250")] <- mean_lp250[mean_lp250$ID == "11", c("mean")]
REMdata[REMdata$site_ID == "E1", c("light250")] <- mean_lp250[mean_lp250$ID == "12", c("mean")]
REMdata[REMdata$site_ID == "E2", c("light250")] <- mean_lp250[mean_lp250$ID == "13", c("mean")]
REMdata[REMdata$site_ID == "F0", c("light250")] <- mean_lp250[mean_lp250$ID == "14", c("mean")]
REMdata[REMdata$site_ID == "F1", c("light250")] <- mean_lp250[mean_lp250$ID == "15", c("mean")]
REMdata[REMdata$site_ID == "F2", c("light250")] <- mean_lp250[mean_lp250$ID == "16", c("mean")]
REMdata[REMdata$site_ID == "G0", c("light250")] <- mean_lp250[mean_lp250$ID == "17", c("mean")]
REMdata[REMdata$site_ID == "G1", c("light250")] <- mean_lp250[mean_lp250$ID == "18", c("mean")]
REMdata[REMdata$site_ID == "G2", c("light250")] <- mean_lp250[mean_lp250$ID == "19", c("mean")]
REMdata[REMdata$site_ID == "H0", c("light250")] <- mean_lp250[mean_lp250$ID == "20", c("mean")]
REMdata[REMdata$site_ID == "H1", c("light250")] <- mean_lp250[mean_lp250$ID == "21", c("mean")]
REMdata[REMdata$site_ID == "H2", c("light250")] <- mean_lp250[mean_lp250$ID == "22", c("mean")]
REMdata[REMdata$site_ID == "I", c("light250")] <- mean_lp250[mean_lp250$ID == "23", c("mean")]
REMdata[REMdata$site_ID == "J", c("light250")] <- mean_lp250[mean_lp250$ID == "24", c("mean")]

summary(mean_lp250$mean)
summary(REMdata$light250)

# add mean light pollution values for 1km buffer
REMdata[REMdata$site_ID == "A0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "1", c("mean")]
REMdata[REMdata$site_ID == "A1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "2", c("mean")]
REMdata[REMdata$site_ID == "A2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "3", c("mean")]
REMdata[REMdata$site_ID == "B0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "4", c("mean")]
REMdata[REMdata$site_ID == "B2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "5", c("mean")]
REMdata[REMdata$site_ID == "C0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "6", c("mean")]
REMdata[REMdata$site_ID == "C1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "7", c("mean")]
REMdata[REMdata$site_ID == "C2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "8", c("mean")]
REMdata[REMdata$site_ID == "D1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "9", c("mean")]
REMdata[REMdata$site_ID == "D2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "10", c("mean")]
REMdata[REMdata$site_ID == "E0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "11", c("mean")]
REMdata[REMdata$site_ID == "E1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "12", c("mean")]
REMdata[REMdata$site_ID == "E2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "13", c("mean")]
REMdata[REMdata$site_ID == "F0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "14", c("mean")]
REMdata[REMdata$site_ID == "F1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "15", c("mean")]
REMdata[REMdata$site_ID == "F2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "16", c("mean")]
REMdata[REMdata$site_ID == "G0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "17", c("mean")]
REMdata[REMdata$site_ID == "G1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "18", c("mean")]
REMdata[REMdata$site_ID == "G2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "19", c("mean")]
REMdata[REMdata$site_ID == "H0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "20", c("mean")]
REMdata[REMdata$site_ID == "H1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "21", c("mean")]
REMdata[REMdata$site_ID == "H2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "22", c("mean")]
REMdata[REMdata$site_ID == "I", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "23", c("mean")]
REMdata[REMdata$site_ID == "J", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "24", c("mean")]

summary(REMdata$light1km)
summary(mean_lp1km$mean)

########################################################
### SAME AGAIN, BUT WITH SQUIRRELS AND THEIR BUFFERS ###
########################################################

###############
### BUFFERS ###
###############

buff100 <- terra::buffer(site.centroids, width = 100)
buff400 <- terra::buffer(site.centroids, width = 400)

### 100m ###

# extract land cover in sites and buffers
int.list100 <- list()

for (i in 1:length(buff100)){
  int.list100[[i]] <- terra::extract(LC, buff100[i,])
  print(i)
}

head(int.list100[[1]]) 

# We're only interested in LC1
ss100 <- lapply(int.list100, "[", 2)

head(ss100[[1]]) 

# Create an empty dataframe to populate later
landcover100 <- data.frame(matrix(NA, nrow = length(buff100),
                                  ncol = 21))

# Add column names corresponding to landcover types
names(landcover100) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                          "11", "12", "13", "14", "15", "16", "17", "18",
                          "19", "20", "21")

head(landcover100)

# Run loop to add number of cells of each landcover type into dataframe
for (i in 1:length(buff100)){
  for ( j in 1:length(names(landcover100))) {
    landcover100[i,j] <- table(
      ss100[[i]])[as.character(names(landcover100)[j])]
  }}

# Name columns with landcover types
names(landcover100) <- c("Broadleaved woodland",
                         "Coniferous woodland",
                         "Arable",
                         "Improved grassland",
                         "Neutral grassland",
                         "Calcareous grassland",
                         "Acid grassland",
                         "Fen, marsh and swamp",
                         "Heather and shrub",
                         "Heather grassland",
                         "Bog",
                         "Inland rock",
                         "Saltwater",
                         "water",
                         "Supralittoral rock",
                         "Supralittoral sediment",
                         "Littoral rock",
                         "Littoral sediment",
                         "Saltmarsh",
                         "Urban",
                         "Suburban")

# Replace NAs with 0
landcover100[is.na(landcover100)] <- 0

# Calculate number of cells in each open space buffer
row_sum100 <- rowSums(landcover100)

# Calculate proportions by dividing number of cells of each landcover with number of cells in each open space buffer
prop100 <- landcover100/row_sum100

# Round the proportions
prop100 <- round(prop100, 3)

# Replace NA with 0
prop100[is.na(prop100)] <- 0

# new data frame to extract proportions into
df.prop100 <- as.data.frame(buff100[,c("sites")])

# group classes and add proportions
df.prop100$wood100 <- rowSums(prop100[,c(1,2)])# broadleaf and conifer
df.prop100$wet100 <- rowSums(prop100[,c(8, 11, 19)])# fen,marsh,swamp,bog, saltmarsh
df.prop100$urban100 <- rowSums(prop100[,c(20,21)])# urban suburban
df.prop100$water100 <- prop100[,c(14)] #water
df.prop100$grass100 <- rowSums(prop100[,c(4,5,6,7,9, 10)]) # improved, neutral, calcareous, acid, heather and shrub, heather grassland 
df.prop100$arable100 <- prop100[,c(3)] #arable


### 400 ###

# extract land cover in sites and buffers
int.list400 <- list()

for (i in 1:length(buff400)){
  int.list400[[i]] <- terra::extract(LC, buff400[i,])
  print(i)
}

head(int.list400[[1]]) 

# We're only interested in LC1
ss400 <- lapply(int.list400, "[", 2)

head(ss400[[1]]) 

# Create an empty dataframe to populate later
landcover400 <- data.frame(matrix(NA, nrow = length(buff400),
                                  ncol = 21))

# Add column names corresponding to landcover types
names(landcover400) <- c( "1","2","3","4","5","6", "7", "8", "9", "10",
                          "11", "12", "13", "14", "15", "16", "17", "18",
                          "19", "20", "21")

head(landcover400)

# Run loop to add number of cells of each landcover type into dataframe
for (i in 1:length(buff400)){
  for ( j in 1:length(names(landcover400))) {
    landcover400[i,j] <- table(
      ss400[[i]])[as.character(names(landcover400)[j])]
  }}

# Name columns with landcover types
names(landcover400) <- c("Broadleaved woodland",
                         "Coniferous woodland",
                         "Arable",
                         "Improved grassland",
                         "Neutral grassland",
                         "Calcareous grassland",
                         "Acid grassland",
                         "Fen, marsh and swamp",
                         "Heather and shrub",
                         "Heather grassland",
                         "Bog",
                         "Inland rock",
                         "Saltwater",
                         "water",
                         "Supralittoral rock",
                         "Supralittoral sediment",
                         "Littoral rock",
                         "Littoral sediment",
                         "Saltmarsh",
                         "Urban",
                         "Suburban")

# Replace NAs with 0
landcover400[is.na(landcover400)] <- 0

# Calculate number of cells in each open space buffer
row_sum400 <- rowSums(landcover400)

# Calculate proportions by dividing number of cells of each landcover with number of cells in each open space buffer
prop400 <- landcover400/row_sum400

# Round the proportions
prop400 <- round(prop400, 3)

# Replace NA with 0
prop400[is.na(prop400)] <- 0

# new data frame to extract proportions into
df.prop400 <- as.data.frame(buff400[,c("sites")])

# group classes and add proportions
df.prop400$wood400 <- rowSums(prop400[,c(1,2)])# broadleaf and conifer
df.prop400$wet400 <- rowSums(prop400[,c(8, 11, 19)])# fen,marsh,swamp,bog, saltmarsh
df.prop400$urban400 <- rowSums(prop400[,c(20,21)])# urban suburban
df.prop400$water400 <- prop400[,c(14)] #water
df.prop400$grass400 <- rowSums(prop400[,c(4,5,6,7,9, 10)]) # improved, neutral, calcareous, acid, heather and shrub, heather grassland 
df.prop400$arable400 <- prop400[,c(3)] #arable

#########################################################
### ENVIRONMENTAL VALUES OF BUFFERS TO REM DATA FRAME ###
#########################################################

head(squREM)
head(df.prop400)

# new site ID column in buffer dfs
df.prop100$site_ID <- NA
df.prop400$site_ID <- NA
# identify site IDs
df.prop100$site_ID[df.prop100$sites == "Blawarthill"] <- "A0"
df.prop100$site_ID[df.prop100$sites == "Trinley"] <- "A1"
df.prop100$site_ID[df.prop100$sites == "Dawsholm"] <- "A2"
df.prop100$site_ID[df.prop100$sites == "Fortingall"] <- "B0"
df.prop100$site_ID[df.prop100$sites == "Kelvingrove"] <- "B2"
df.prop100$site_ID[df.prop100$sites == "Maryhill"] <- "C0"
df.prop100$site_ID[df.prop100$sites == "Ruchill"] <- "C1"
df.prop100$site_ID[df.prop100$sites == "Woodside"] <- "C2"
df.prop100$site_ID[df.prop100$sites == "Cowlairs"] <- "D1"
df.prop100$site_ID[df.prop100$sites == "St_mungo"] <- "D2"
df.prop100$site_ID[df.prop100$sites == "G_Green"] <- "E0"
df.prop100$site_ID[df.prop100$sites == "Hogganfield"] <- "E1"
df.prop100$site_ID[df.prop100$sites == "Greenfield"] <- "E2"
df.prop100$site_ID[df.prop100$sites == "Lightburn"] <- "F0"
df.prop100$site_ID[df.prop100$sites == "Maxwell"] <- "F1"
df.prop100$site_ID[df.prop100$sites == "Balado"] <- "F2"
df.prop100$site_ID[df.prop100$sites == "Garrowhill"] <- "G0"
df.prop100$site_ID[df.prop100$sites == "Farmington"] <- "G1"
df.prop100$site_ID[df.prop100$sites == "Huntingtower"] <- "G2"
df.prop100$site_ID[df.prop100$sites == "Crookston"] <- "H0"
df.prop100$site_ID[df.prop100$sites == "Corkerhill"] <- "H1"
df.prop100$site_ID[df.prop100$sites == "King_G_V"] <- "H2"
df.prop100$site_ID[df.prop100$sites == "Pollock"] <- "I"
df.prop100$site_ID[df.prop100$sites == "Cathkin"] <- "J"
head(df.prop100, 24)
df.prop400$site_ID[df.prop400$sites == "Blawarthill"] <- "A0"
df.prop400$site_ID[df.prop400$sites == "Trinley"] <- "A1"
df.prop400$site_ID[df.prop400$sites == "Dawsholm"] <- "A2"
df.prop400$site_ID[df.prop400$sites == "Fortingall"] <- "B0"
df.prop400$site_ID[df.prop400$sites == "Kelvingrove"] <- "B2"
df.prop400$site_ID[df.prop400$sites == "Maryhill"] <- "C0"
df.prop400$site_ID[df.prop400$sites == "Ruchill"] <- "C1"
df.prop400$site_ID[df.prop400$sites == "Woodside"] <- "C2"
df.prop400$site_ID[df.prop400$sites == "Cowlairs"] <- "D1"
df.prop400$site_ID[df.prop400$sites == "St_mungo"] <- "D2"
df.prop400$site_ID[df.prop400$sites == "G_Green"] <- "E0"
df.prop400$site_ID[df.prop400$sites == "Hogganfield"] <- "E1"
df.prop400$site_ID[df.prop400$sites == "Greenfield"] <- "E2"
df.prop400$site_ID[df.prop400$sites == "Lightburn"] <- "F0"
df.prop400$site_ID[df.prop400$sites == "Maxwell"] <- "F1"
df.prop400$site_ID[df.prop400$sites == "Balado"] <- "F2"
df.prop400$site_ID[df.prop400$sites == "Garrowhill"] <- "G0"
df.prop400$site_ID[df.prop400$sites == "Farmington"] <- "G1"
df.prop400$site_ID[df.prop400$sites == "Huntingtower"] <- "G2"
df.prop400$site_ID[df.prop400$sites == "Crookston"] <- "H0"
df.prop400$site_ID[df.prop400$sites == "Corkerhill"] <- "H1"
df.prop400$site_ID[df.prop400$sites == "King_G_V"] <- "H2"
df.prop400$site_ID[df.prop400$sites == "Pollock"] <- "I"
df.prop400$site_ID[df.prop400$sites == "Cathkin"] <- "J"
head(df.prop400, 24)

### new columns in REM data frame
# 250m buffer
squREM$wood100 <- 0
squREM$wet100 <- 0
squREM$urban100 <- 0
squREM$water100 <- 0
squREM$grass100 <- 0
squREM$arable100 <- 0
# 400 buffer
squREM$wood400 <- 0
squREM$wet400 <- 0
squREM$urban400 <- 0
squREM$water400 <- 0
squREM$grass400 <- 0
squREM$arable400 <- 0

# identify useful columns 
prop_columns <- names(df.prop100[2:7])
# add proportions of 100m buffer zones
squREM[squREM$site_ID == "A0", prop_columns] <- df.prop100[df.prop100$site_ID == "A0", prop_columns]
squREM[squREM$site_ID == "A1", prop_columns] <- df.prop100[df.prop100$site_ID == "A1", prop_columns]
squREM[squREM$site_ID == "A2", prop_columns] <- df.prop100[df.prop100$site_ID == "A2", prop_columns]
squREM[squREM$site_ID == "B0", prop_columns] <- df.prop100[df.prop100$site_ID == "B0", prop_columns]
squREM[squREM$site_ID == "B2", prop_columns] <- df.prop100[df.prop100$site_ID == "B2", prop_columns]
squREM[squREM$site_ID == "C0", prop_columns] <- df.prop100[df.prop100$site_ID == "C0", prop_columns]
squREM[squREM$site_ID == "C1", prop_columns] <- df.prop100[df.prop100$site_ID == "C1", prop_columns]
squREM[squREM$site_ID == "C2", prop_columns] <- df.prop100[df.prop100$site_ID == "C2", prop_columns]
squREM[squREM$site_ID == "D1", prop_columns] <- df.prop100[df.prop100$site_ID == "D1", prop_columns]
squREM[squREM$site_ID == "D2", prop_columns] <- df.prop100[df.prop100$site_ID == "D2", prop_columns]
squREM[squREM$site_ID == "E0", prop_columns] <- df.prop100[df.prop100$site_ID == "E0", prop_columns]
squREM[squREM$site_ID == "E1", prop_columns] <- df.prop100[df.prop100$site_ID == "E1", prop_columns]
squREM[squREM$site_ID == "E2", prop_columns] <- df.prop100[df.prop100$site_ID == "E2", prop_columns]
squREM[squREM$site_ID == "F0", prop_columns] <- df.prop100[df.prop100$site_ID == "F0", prop_columns]
squREM[squREM$site_ID == "F1", prop_columns] <- df.prop100[df.prop100$site_ID == "F1", prop_columns]
squREM[squREM$site_ID == "F2", prop_columns] <- df.prop100[df.prop100$site_ID == "F2", prop_columns]
squREM[squREM$site_ID == "G0", prop_columns] <- df.prop100[df.prop100$site_ID == "G0", prop_columns]
squREM[squREM$site_ID == "G1", prop_columns] <- df.prop100[df.prop100$site_ID == "G1", prop_columns]
squREM[squREM$site_ID == "G2", prop_columns] <- df.prop100[df.prop100$site_ID == "G2", prop_columns]
squREM[squREM$site_ID == "H0", prop_columns] <- df.prop100[df.prop100$site_ID == "H0", prop_columns]
squREM[squREM$site_ID == "H1", prop_columns] <- df.prop100[df.prop100$site_ID == "H1", prop_columns]
squREM[squREM$site_ID == "H2", prop_columns] <- df.prop100[df.prop100$site_ID == "H2", prop_columns]
squREM[squREM$site_ID == "I", prop_columns] <- df.prop100[df.prop100$site_ID == "I", prop_columns]
squREM[squREM$site_ID == "J", prop_columns] <- df.prop100[df.prop100$site_ID == "J", prop_columns]
# redefine prop_columns
prop_columns <- names(df.prop400[2:7])
# add proportions of 400 buffer zones
squREM[squREM$site_ID == "A0", prop_columns] <- df.prop400[df.prop400$site_ID == "A0", prop_columns]
squREM[squREM$site_ID == "A1", prop_columns] <- df.prop400[df.prop400$site_ID == "A1", prop_columns]
squREM[squREM$site_ID == "A2", prop_columns] <- df.prop400[df.prop400$site_ID == "A2", prop_columns]
squREM[squREM$site_ID == "B0", prop_columns] <- df.prop400[df.prop400$site_ID == "B0", prop_columns]
squREM[squREM$site_ID == "B2", prop_columns] <- df.prop400[df.prop400$site_ID == "B2", prop_columns]
squREM[squREM$site_ID == "C0", prop_columns] <- df.prop400[df.prop400$site_ID == "C0", prop_columns]
squREM[squREM$site_ID == "C1", prop_columns] <- df.prop400[df.prop400$site_ID == "C1", prop_columns]
squREM[squREM$site_ID == "C2", prop_columns] <- df.prop400[df.prop400$site_ID == "C2", prop_columns]
squREM[squREM$site_ID == "D1", prop_columns] <- df.prop400[df.prop400$site_ID == "D1", prop_columns]
squREM[squREM$site_ID == "D2", prop_columns] <- df.prop400[df.prop400$site_ID == "D2", prop_columns]
squREM[squREM$site_ID == "E0", prop_columns] <- df.prop400[df.prop400$site_ID == "E0", prop_columns]
squREM[squREM$site_ID == "E1", prop_columns] <- df.prop400[df.prop400$site_ID == "E1", prop_columns]
squREM[squREM$site_ID == "E2", prop_columns] <- df.prop400[df.prop400$site_ID == "E2", prop_columns]
squREM[squREM$site_ID == "F0", prop_columns] <- df.prop400[df.prop400$site_ID == "F0", prop_columns]
squREM[squREM$site_ID == "F1", prop_columns] <- df.prop400[df.prop400$site_ID == "F1", prop_columns]
squREM[squREM$site_ID == "F2", prop_columns] <- df.prop400[df.prop400$site_ID == "F2", prop_columns]
squREM[squREM$site_ID == "G0", prop_columns] <- df.prop400[df.prop400$site_ID == "G0", prop_columns]
squREM[squREM$site_ID == "G1", prop_columns] <- df.prop400[df.prop400$site_ID == "G1", prop_columns]
squREM[squREM$site_ID == "G2", prop_columns] <- df.prop400[df.prop400$site_ID == "G2", prop_columns]
squREM[squREM$site_ID == "H0", prop_columns] <- df.prop400[df.prop400$site_ID == "H0", prop_columns]
squREM[squREM$site_ID == "H1", prop_columns] <- df.prop400[df.prop400$site_ID == "H1", prop_columns]
squREM[squREM$site_ID == "H2", prop_columns] <- df.prop400[df.prop400$site_ID == "H2", prop_columns]
squREM[squREM$site_ID == "I", prop_columns] <- df.prop400[df.prop400$site_ID == "I", prop_columns]
squREM[squREM$site_ID == "J", prop_columns] <- df.prop400[df.prop400$site_ID == "J", prop_columns]

##############################################
### AVERAGE NOISE POLLUTION ACROSS BUFFERS ###
##############################################

# check order of sites (should be alphabetical)
head(spaces$SITE_ID, 24)

# extract noise pollution values, per site
np100 <- extract(noise, buff100)
np400 <- extract(noise, buff400)

# mean noise pollution value, per site
# 100
mean_np100 <- np100 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_noise))
# 400
mean_np400 <- np400 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_noise))

# new columns in squREM data frame
squREM$noise100 <- 0
squREM$noise400 <- 0

# add mean noise pollution values for 100 buffer
squREM[squREM$site_ID == "A0", c("noise100")] <- mean_np100[mean_np100$ID == "1", c("mean")]
squREM[squREM$site_ID == "A1", c("noise100")] <- mean_np100[mean_np100$ID == "2", c("mean")]
squREM[squREM$site_ID == "A2", c("noise100")] <- mean_np100[mean_np100$ID == "3", c("mean")]
squREM[squREM$site_ID == "B0", c("noise100")] <- mean_np100[mean_np100$ID == "4", c("mean")]
squREM[squREM$site_ID == "B2", c("noise100")] <- mean_np100[mean_np100$ID == "5", c("mean")]
squREM[squREM$site_ID == "C0", c("noise100")] <- mean_np100[mean_np100$ID == "6", c("mean")]
squREM[squREM$site_ID == "C1", c("noise100")] <- mean_np100[mean_np100$ID == "7", c("mean")]
squREM[squREM$site_ID == "C2", c("noise100")] <- mean_np100[mean_np100$ID == "8", c("mean")]
squREM[squREM$site_ID == "D1", c("noise100")] <- mean_np100[mean_np100$ID == "9", c("mean")]
squREM[squREM$site_ID == "D2", c("noise100")] <- mean_np100[mean_np100$ID == "10", c("mean")]
squREM[squREM$site_ID == "E0", c("noise100")] <- mean_np100[mean_np100$ID == "11", c("mean")]
squREM[squREM$site_ID == "E1", c("noise100")] <- mean_np100[mean_np100$ID == "12", c("mean")]
squREM[squREM$site_ID == "E2", c("noise100")] <- mean_np100[mean_np100$ID == "13", c("mean")]
squREM[squREM$site_ID == "F0", c("noise100")] <- mean_np100[mean_np100$ID == "14", c("mean")]
squREM[squREM$site_ID == "F1", c("noise100")] <- mean_np100[mean_np100$ID == "15", c("mean")]
squREM[squREM$site_ID == "F2", c("noise100")] <- mean_np100[mean_np100$ID == "16", c("mean")]
squREM[squREM$site_ID == "G0", c("noise100")] <- mean_np100[mean_np100$ID == "17", c("mean")]
squREM[squREM$site_ID == "G1", c("noise100")] <- mean_np100[mean_np100$ID == "18", c("mean")]
squREM[squREM$site_ID == "G2", c("noise100")] <- mean_np100[mean_np100$ID == "19", c("mean")]
squREM[squREM$site_ID == "H0", c("noise100")] <- mean_np100[mean_np100$ID == "20", c("mean")]
squREM[squREM$site_ID == "H1", c("noise100")] <- mean_np100[mean_np100$ID == "21", c("mean")]
squREM[squREM$site_ID == "H2", c("noise100")] <- mean_np100[mean_np100$ID == "22", c("mean")]
squREM[squREM$site_ID == "I", c("noise100")] <- mean_np100[mean_np100$ID == "23", c("mean")]
squREM[squREM$site_ID == "J", c("noise100")] <- mean_np100[mean_np100$ID == "24", c("mean")]

summary(squREM$noise100)
summary(mean_np100$mean)

# add mean noise pollution values for 40 buffer
squREM[squREM$site_ID == "A0", c("noise400")] <- mean_np400[mean_np400$ID == "1", c("mean")]
squREM[squREM$site_ID == "A1", c("noise400")] <- mean_np400[mean_np400$ID == "2", c("mean")]
squREM[squREM$site_ID == "A2", c("noise400")] <- mean_np400[mean_np400$ID == "3", c("mean")]
squREM[squREM$site_ID == "B0", c("noise400")] <- mean_np400[mean_np400$ID == "4", c("mean")]
squREM[squREM$site_ID == "B2", c("noise400")] <- mean_np400[mean_np400$ID == "5", c("mean")]
squREM[squREM$site_ID == "C0", c("noise400")] <- mean_np400[mean_np400$ID == "6", c("mean")]
squREM[squREM$site_ID == "C1", c("noise400")] <- mean_np400[mean_np400$ID == "7", c("mean")]
squREM[squREM$site_ID == "C2", c("noise400")] <- mean_np400[mean_np400$ID == "8", c("mean")]
squREM[squREM$site_ID == "D1", c("noise400")] <- mean_np400[mean_np400$ID == "9", c("mean")]
squREM[squREM$site_ID == "D2", c("noise400")] <- mean_np400[mean_np400$ID == "10", c("mean")]
squREM[squREM$site_ID == "E0", c("noise400")] <- mean_np400[mean_np400$ID == "11", c("mean")]
squREM[squREM$site_ID == "E1", c("noise400")] <- mean_np400[mean_np400$ID == "12", c("mean")]
squREM[squREM$site_ID == "E2", c("noise400")] <- mean_np400[mean_np400$ID == "13", c("mean")]
squREM[squREM$site_ID == "F0", c("noise400")] <- mean_np400[mean_np400$ID == "14", c("mean")]
squREM[squREM$site_ID == "F1", c("noise400")] <- mean_np400[mean_np400$ID == "15", c("mean")]
squREM[squREM$site_ID == "F2", c("noise400")] <- mean_np400[mean_np400$ID == "16", c("mean")]
squREM[squREM$site_ID == "G0", c("noise400")] <- mean_np400[mean_np400$ID == "17", c("mean")]
squREM[squREM$site_ID == "G1", c("noise400")] <- mean_np400[mean_np400$ID == "18", c("mean")]
squREM[squREM$site_ID == "G2", c("noise400")] <- mean_np400[mean_np400$ID == "19", c("mean")]
squREM[squREM$site_ID == "H0", c("noise400")] <- mean_np400[mean_np400$ID == "20", c("mean")]
squREM[squREM$site_ID == "H1", c("noise400")] <- mean_np400[mean_np400$ID == "21", c("mean")]
squREM[squREM$site_ID == "H2", c("noise400")] <- mean_np400[mean_np400$ID == "22", c("mean")]
squREM[squREM$site_ID == "I", c("noise400")] <- mean_np400[mean_np400$ID == "23", c("mean")]
squREM[squREM$site_ID == "J", c("noise400")] <- mean_np400[mean_np400$ID == "24", c("mean")]

summary(squREM$noise400)
summary(mean_np400$mean)

##############################################
### AVERAGE LIGHT POLLUTION ACROSS BUFFERS ###
##############################################

# fix crs of buffer objects
crs(buff100) <- crs(all.locs)
crs(buff400) <- crs(all.locs)

# extract light pollution values per site
lp100 <- extract(light$GCC_light_3, buff100)
lp400 <- extract(light$GCC_light_3, buff400)

# mean light pollution per site
# 100
mean_lp100 <- lp100 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_light_3))
# 400
mean_lp400 <- lp400 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_light_3))

# new columns in squREM data frame
squREM$light100 <- 0
squREM$light400 <- 0

# add mean light pollution values for 100 buffer
squREM[squREM$site_ID == "A0", c("light100")] <- mean_lp100[mean_lp100$ID == "1", c("mean")]
squREM[squREM$site_ID == "A1", c("light100")] <- mean_lp100[mean_lp100$ID == "2", c("mean")]
squREM[squREM$site_ID == "A2", c("light100")] <- mean_lp100[mean_lp100$ID == "3", c("mean")]
squREM[squREM$site_ID == "B0", c("light100")] <- mean_lp100[mean_lp100$ID == "4", c("mean")]
squREM[squREM$site_ID == "B2", c("light100")] <- mean_lp100[mean_lp100$ID == "5", c("mean")]
squREM[squREM$site_ID == "C0", c("light100")] <- mean_lp100[mean_lp100$ID == "6", c("mean")]
squREM[squREM$site_ID == "C1", c("light100")] <- mean_lp100[mean_lp100$ID == "7", c("mean")]
squREM[squREM$site_ID == "C2", c("light100")] <- mean_lp100[mean_lp100$ID == "8", c("mean")]
squREM[squREM$site_ID == "D1", c("light100")] <- mean_lp100[mean_lp100$ID == "9", c("mean")]
squREM[squREM$site_ID == "D2", c("light100")] <- mean_lp100[mean_lp100$ID == "10", c("mean")]
squREM[squREM$site_ID == "E0", c("light100")] <- mean_lp100[mean_lp100$ID == "11", c("mean")]
squREM[squREM$site_ID == "E1", c("light100")] <- mean_lp100[mean_lp100$ID == "12", c("mean")]
squREM[squREM$site_ID == "E2", c("light100")] <- mean_lp100[mean_lp100$ID == "13", c("mean")]
squREM[squREM$site_ID == "F0", c("light100")] <- mean_lp100[mean_lp100$ID == "14", c("mean")]
squREM[squREM$site_ID == "F1", c("light100")] <- mean_lp100[mean_lp100$ID == "15", c("mean")]
squREM[squREM$site_ID == "F2", c("light100")] <- mean_lp100[mean_lp100$ID == "16", c("mean")]
squREM[squREM$site_ID == "G0", c("light100")] <- mean_lp100[mean_lp100$ID == "17", c("mean")]
squREM[squREM$site_ID == "G1", c("light100")] <- mean_lp100[mean_lp100$ID == "18", c("mean")]
squREM[squREM$site_ID == "G2", c("light100")] <- mean_lp100[mean_lp100$ID == "19", c("mean")]
squREM[squREM$site_ID == "H0", c("light100")] <- mean_lp100[mean_lp100$ID == "20", c("mean")]
squREM[squREM$site_ID == "H1", c("light100")] <- mean_lp100[mean_lp100$ID == "21", c("mean")]
squREM[squREM$site_ID == "H2", c("light100")] <- mean_lp100[mean_lp100$ID == "22", c("mean")]
squREM[squREM$site_ID == "I", c("light100")] <- mean_lp100[mean_lp100$ID == "23", c("mean")]
squREM[squREM$site_ID == "J", c("light100")] <- mean_lp100[mean_lp100$ID == "24", c("mean")]

summary(mean_lp100$mean)
summary(squREM$light100)

# add mean light pollution values for 400 buffer
squREM[squREM$site_ID == "A0", c("light400")] <- mean_lp400[mean_lp400$ID == "1", c("mean")]
squREM[squREM$site_ID == "A1", c("light400")] <- mean_lp400[mean_lp400$ID == "2", c("mean")]
squREM[squREM$site_ID == "A2", c("light400")] <- mean_lp400[mean_lp400$ID == "3", c("mean")]
squREM[squREM$site_ID == "B0", c("light400")] <- mean_lp400[mean_lp400$ID == "4", c("mean")]
squREM[squREM$site_ID == "B2", c("light400")] <- mean_lp400[mean_lp400$ID == "5", c("mean")]
squREM[squREM$site_ID == "C0", c("light400")] <- mean_lp400[mean_lp400$ID == "6", c("mean")]
squREM[squREM$site_ID == "C1", c("light400")] <- mean_lp400[mean_lp400$ID == "7", c("mean")]
squREM[squREM$site_ID == "C2", c("light400")] <- mean_lp400[mean_lp400$ID == "8", c("mean")]
squREM[squREM$site_ID == "D1", c("light400")] <- mean_lp400[mean_lp400$ID == "9", c("mean")]
squREM[squREM$site_ID == "D2", c("light400")] <- mean_lp400[mean_lp400$ID == "10", c("mean")]
squREM[squREM$site_ID == "E0", c("light400")] <- mean_lp400[mean_lp400$ID == "11", c("mean")]
squREM[squREM$site_ID == "E1", c("light400")] <- mean_lp400[mean_lp400$ID == "12", c("mean")]
squREM[squREM$site_ID == "E2", c("light400")] <- mean_lp400[mean_lp400$ID == "13", c("mean")]
squREM[squREM$site_ID == "F0", c("light400")] <- mean_lp400[mean_lp400$ID == "14", c("mean")]
squREM[squREM$site_ID == "F1", c("light400")] <- mean_lp400[mean_lp400$ID == "15", c("mean")]
squREM[squREM$site_ID == "F2", c("light400")] <- mean_lp400[mean_lp400$ID == "16", c("mean")]
squREM[squREM$site_ID == "G0", c("light400")] <- mean_lp400[mean_lp400$ID == "17", c("mean")]
squREM[squREM$site_ID == "G1", c("light400")] <- mean_lp400[mean_lp400$ID == "18", c("mean")]
squREM[squREM$site_ID == "G2", c("light400")] <- mean_lp400[mean_lp400$ID == "19", c("mean")]
squREM[squREM$site_ID == "H0", c("light400")] <- mean_lp400[mean_lp400$ID == "20", c("mean")]
squREM[squREM$site_ID == "H1", c("light400")] <- mean_lp400[mean_lp400$ID == "21", c("mean")]
squREM[squREM$site_ID == "H2", c("light400")] <- mean_lp400[mean_lp400$ID == "22", c("mean")]
squREM[squREM$site_ID == "I", c("light400")] <- mean_lp400[mean_lp400$ID == "23", c("mean")]
squREM[squREM$site_ID == "J", c("light400")] <- mean_lp400[mean_lp400$ID == "24", c("mean")]

summary(mean_lp400$mean)
summary(squREM$light400)

###############################################
### SPLIT INTO SPECIES DATA FRAMES AND SAVE ###
###############################################

squirrel <- squREM
fox <- subset(REMdata, REMdata$species == "fox")
deer <- subset(REMdata, REMdata$species == "deer")

write.csv(squirrel, file = "~/GALLANT Technician/Camera Trap Analysis/squirrel_REM_df.csv")
write.csv(fox, file = "~/GALLANT Technician/Camera Trap Analysis/fox_REM_df.csv")
write.csv(deer, file = "~/GALLANT Technician/Camera Trap Analysis/deer_REM_df.csv")
