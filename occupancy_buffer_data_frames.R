# OCCUPANCY DATA FRAMES

library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(gamlss)
library(terra)
library(sf)
library(car)
library(Hmisc)
library(lubridate)

setwd("~/GALLANT Technician/Camera Trap Analysis")

# open space polygons
os_sites <- vect("Openspacesites/buffer_sites.shp")
# remove data frame
os_df <- as.data.frame(os_sites)
names(os_df)
head(os_df[,46],24)
head(os_df[,35:46])

# camera placement data
cam.data <- read.csv("cam_placement.csv")
cam.data$Date_setup <- as.POSIXct(cam.data$Date_setup, tryFormats = c("%d/%m/%Y"))
cam.data$Date_retr <- as.POSIXct(cam.data$Date_retr, tryFormats = c("%d/%m/%Y"))

# mammal camera trap data
mammal.data <- read.csv("fulldata.csv")
mammal.data$placement <- ifelse(mammal.data$placement == "A", "1", "2")
# change site ID to site name
mammal.data$site <- as.factor(mammal.data$site)
levels(mammal.data$site)
mammal.data$site <- ifelse(mammal.data$site == "A0/1", "A0",
                           ifelse(mammal.data$site == "A1/1", "A1",
                                  ifelse(mammal.data$site == "A2/1", "A2",
                                         ifelse(mammal.data$site == "A2_2", "A2/2",
                                                ifelse(mammal.data$site == "B0/1", "B0",
                                                       ifelse(mammal.data$site == "B2/1", "B2",
                                                              ifelse(mammal.data$site == "B2_2", "B2/2",
                                                                     ifelse(mammal.data$site == "C0/1", "C0",
                                                                            ifelse(mammal.data$site == "C1/1", "C1",
                                                                                   ifelse(mammal.data$site == "C1_2", "C1/2",
                                                                                          ifelse(mammal.data$site == "C2/1", "C2",
                                                                                                 ifelse(mammal.data$site == "D1/1", "D1",
                                                                                                        ifelse(mammal.data$site == "D1_2", "D1/2",
                                                                                                               ifelse(mammal.data$site == "D2/1", "D2",
                                                                                                                      ifelse(mammal.data$site == "E0/1", "E0",
                                                                                                                             ifelse(mammal.data$site == "E0_2", "E0/2",
                                                                                                                                    ifelse(mammal.data$site == "E1/1", "E1",
                                                                                                                                           ifelse(mammal.data$site == "E1_2", "E1/2",
                                                                                                                                                  ifelse(mammal.data$site == "E2/1", "E2",
                                                                                                                                                         ifelse(mammal.data$site == "F0/1", "F0",
                                                                                                                                                                ifelse(mammal.data$site == "F1/1", "F1",
                                                                                                                                                                       ifelse(mammal.data$site == "F2/1", "F2",
                                                                                                                                                                              ifelse(mammal.data$site == "G0/1", "G0",
                                                                                                                                                                                     ifelse(mammal.data$site == "G1/1", "G1",
                                                                                                                                                                                            ifelse(mammal.data$site == "G2/1", "G2",
                                                                                                                                                                                                   ifelse(mammal.data$site == "H0/1", "H0",
                                                                                                                                                                                                          ifelse(mammal.data$site == "H1/1", "H1",
                                                                                                                                                                                                                 ifelse(mammal.data$site == "H2/1", "H2",
                                                                                                                                                                                                                        ifelse(mammal.data$site == "I0/1", "I0",
                                                                                                                                                                                                                               ifelse(mammal.data$site == "I1/1", "I1",
                                                                                                                                                                                                                                      ifelse(mammal.data$site == "I2/1", "I2",
                                                                                                                                                                                                                                             ifelse(mammal.data$site == "J0/1", "J0",
                                                                                                                                                                                                                                                    ifelse(mammal.data$site == "J1/1", "J1",
                                                                                                                                                                                                                                                           ifelse(mammal.data$site == "J2/1", "J2", NA)
                                                                                                                                                                                                                                                    )))))))))))))))))))))))))))))))))

levels(as.factor(mammal.data$site))

# separate by species
fox <- subset(mammal.data, mammal.data$ID_species == "fox")
deer <- subset(mammal.data, mammal.data$ID_species == "deer")
squirrel <- subset(mammal.data, mammal.data$ID_species == "squirrel")

# dog relative abundance
dog_RA <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/Anthropogenic Presence/dog_RA.csv")

# light pollution data
light <- terra::rast("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/Openspacesites/GCC_light.tif")

# noise pollution data
noise <- terra::rast("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/Openspacesites/GCC_noise.tif")

# land cover data
LC <- rast("~/GALLANT Technician/Camera Trap Analysis/Openspacesites/FME_35646466_1737655111641_7289/data/LCM.tif")

# REM site data
REM <- read.csv("~/GALLANT Technician/Camera Trap Analysis/fox_REM_df.csv")

#######################################################
### BINARY COLUMNS PER WEEK WITH PRESENCE OF ANIMAL ###
#######################################################

### SQUIRREL ###

# group data weekly
# make vector list for times squirrel was present at each site
occasions.squ <- vector("list", length = nrow(cam.data))
# loop through list, making rows for each hour of each day at each site
for (i in 1:nrow(cam.data)) {
  occasions.squ[[i]] <- data.frame(Session = cam.data$Session[i],
                                   Site = cam.data$Site[i],
                                   start = seq(from = ymd_hms(paste(cam.data$Date_setup[i], "00:00:00", sep = " ")),
                                               to = ymd_hms(paste(cam.data$Date_retr[i], "23:59:59", sep = " ")),
                                               by = 'week'
                                   )
  ) %>%
    mutate(end = lead(start, default = start[length(start)] + week(1)
    ))
  
}

# separate list into data frame
occasions.squ <- do.call(rbind.data.frame, occasions.squ)
# add column for presence/absence
occasions.squ$capt <- 0
head(occasions.squ)

# loop through squirrel data and store '1' for presence and '0' for absence at date/time
for (i in 1:nrow(squirrel)) {
  occasions.squ[occasions.squ$Session == as.character(squirrel$placement[i])
            & occasions.squ$Site == as.character(squirrel$site[i])
            & occasions.squ$start <= squirrel$datetime[i]
            & occasions.squ$end > squirrel$datetime[i], "capt"] <- 1
}

table(occasions.squ$Site, occasions.squ$capt)
table(occasions.squ$Site, occasions.squ$Session)

head(occasions.squ, 20)

# time difference between start and end
occasions.squ$time_diff <- difftime(occasions.squ$end, occasions.squ$start)
table(occasions.squ$capt, occasions.squ$time_diff) # 68 values 1 second long
occasions.squ <- subset(occasions.squ, occasions.squ$time_diff > 10)



### FOX ###

# group data weekly
# make vector list for times fox was present at each site
occasions.fox <- vector("list", length = nrow(cam.data))
# loop through list, making rows for each hour of each day at each site
for (i in 1:nrow(cam.data)) {
  occasions.fox[[i]] <- data.frame(Session = cam.data$Session[i],
                                   Site = cam.data$Site[i],
                                   start = seq(from = ymd_hms(paste(cam.data$Date_setup[i], "00:00:00", sep = " ")),
                                               to = ymd_hms(paste(cam.data$Date_retr[i], "23:59:59", sep = " ")),
                                               by = 'week'
                                   )
  ) %>%
    mutate(end = lead(start, default = start[length(start)] + week(1)
    ))
  
}

# separate list into data frame
occasions.fox <- do.call(rbind.data.frame, occasions.fox)
# add column for presence/absence
occasions.fox$capt <- 0
head(occasions.fox)

# loop through fox data and store '1' for presence and '0' for absence at date/time
for (i in 1:nrow(fox)) {
  occasions.fox[occasions.fox$Session == as.character(fox$placement[i])
                & occasions.fox$Site == as.character(fox$site[i])
                & occasions.fox$start <= fox$datetime[i]
                & occasions.fox$end > fox$datetime[i], "capt"] <- 1
}

table(occasions.fox$Site, occasions.fox$capt)
table(occasions.fox$Site, occasions.fox$Session)

head(occasions.fox, 20)

# time difference between start and end
occasions.fox$time_diff <- difftime(occasions.fox$end, occasions.fox$start)
table(occasions.fox$capt, occasions.fox$time_diff) # 68 values 1 second long
occasions.fox <- subset(occasions.fox, occasions.fox$time_diff > 10)



### DEER ###

# group data weekly
# make vector list for times squirrel was present at each site
occasions.dee <- vector("list", length = nrow(cam.data))
# loop through list, making rows for each hour of each day at each site
for (i in 1:nrow(cam.data)) {
  occasions.dee[[i]] <- data.frame(Session = cam.data$Session[i],
                                   Site = cam.data$Site[i],
                                   start = seq(from = ymd_hms(paste(cam.data$Date_setup[i], "00:00:00", sep = " ")),
                                               to = ymd_hms(paste(cam.data$Date_retr[i], "23:59:59", sep = " ")),
                                               by = 'week'
                                   )
  ) %>%
    mutate(end = lead(start, default = start[length(start)] + week(1)
    ))
  
}

# separate list into data frame
occasions.dee <- do.call(rbind.data.frame, occasions.dee)
# add column for presence/absence
occasions.dee$capt <- 0
head(occasions.dee)

# loop through squirrel data and store '1' for presence and '0' for absence at date/time
for (i in 1:nrow(deer)) {
  occasions.dee[occasions.dee$Session == as.character(deer$placement[i])
                & occasions.dee$Site == as.character(deer$site[i])
                & occasions.dee$start <= deer$datetime[i]
                & occasions.dee$end > deer$datetime[i], "capt"] <- 1
}

table(occasions.dee$Site, occasions.dee$capt)
table(occasions.dee$Site, occasions.dee$Session)

head(occasions.dee, 20)

# time difference between start and end
occasions.dee$time_diff <- difftime(occasions.dee$end, occasions.dee$start)
table(occasions.dee$capt, occasions.dee$time_diff) # 68 values 1 second long
occasions.dee <- subset(occasions.dee, occasions.dee$time_diff > 10)

####################
### MAKE BUFFERS ###
####################

# camera locations
cam1.locs <- vect("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/Openspacesites/Openspacesites/Camera_locations/RandomPoints_round1_sampling.shp")
cam2.locs <- vect("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/Openspacesites/Openspacesites/Camera_locations/points_projected_round2.shp")
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

# buffer around locations
buff100 <- terra::buffer(all.locs, width = 100) # SQ
buff400 <- terra::buffer(all.locs, width = 400) # SQ
buff250 <- terra::buffer(all.locs, width = 250) # DE&FO
buff1km <- terra::buffer(all.locs, width = 1000) # DE&FO

######################################
### 100m ENV COVARIATES - SQUIRREL ###
######################################

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
df.prop100 <- as.data.frame(buff100[,c("site", "placement")])

# group classes and add proportions
df.prop100$wood100 <- rowSums(prop100[,c(1,2)])# broadleaf and conifer
df.prop100$wet100 <- rowSums(prop100[,c(8, 11, 19)])# fen,marsh,swamp,bog, saltmarsh
df.prop100$urban100 <- rowSums(prop100[,c(20,21)])# urban suburban
df.prop100$water100 <- prop100[,c(14)] #water
df.prop100$grass100 <- rowSums(prop100[,c(4,5,6,7,9, 10)]) # improved, neutral, calcareous, acid, heather and shrub, heather grassland 
df.prop100$arable100 <- prop100[,c(3)] #arable

######################################
### 400m ENV COVARIATES - SQUIRREL ###
######################################

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
df.prop400 <- as.data.frame(buff400[,c("site", "placement")])

# group classes and add proportions
df.prop400$wood400 <- rowSums(prop400[,c(1,2)])# broadleaf and conifer
df.prop400$wet400 <- rowSums(prop400[,c(8, 11, 19)])# fen,marsh,swamp,bog, saltmarsh
df.prop400$urban400 <- rowSums(prop400[,c(20,21)])# urban suburban
df.prop400$water400 <- prop400[,c(14)] #water
df.prop400$grass400 <- rowSums(prop400[,c(4,5,6,7,9, 10)]) # improved, neutral, calcareous, acid, heather and shrub, heather grassland 
df.prop400$arable400 <- prop400[,c(3)] #arable

########################################
### 250m ENV COVARIATES - DEER & FOX ###
########################################

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
df.prop250 <- as.data.frame(buff250[,c("site", "placement")])

# group classes and add proportions
df.prop250$wood250 <- rowSums(prop250[,c(1,2)])# broadleaf and conifer
df.prop250$wet250 <- rowSums(prop250[,c(8, 11, 19)])# fen,marsh,swamp,bog, saltmarsh
df.prop250$urban250 <- rowSums(prop250[,c(20,21)])# urban suburban
df.prop250$water250 <- prop250[,c(14)] #water
df.prop250$grass250 <- rowSums(prop250[,c(4,5,6,7,9, 10)]) # improved, neutral, calcareous, acid, heather and shrub, heather grassland 
df.prop250$arable250 <- prop250[,c(3)] #arable

#######################################
### 1km ENV COVARIATES - DEER & FOX ###
#######################################

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
df.prop1km <- as.data.frame(buff1km[,c("site", "placement")])

# group classes and add proportions
df.prop1km$wood1km <- rowSums(prop1km[,c(1,2)])# broadleaf and conifer
df.prop1km$wet1km <- rowSums(prop1km[,c(8, 11, 19)])# fen,marsh,swamp,bog, saltmarsh
df.prop1km$urban1km <- rowSums(prop1km[,c(20,21)])# urban suburban
df.prop1km$water1km <- prop1km[,c(14)] #water
df.prop1km$grass1km <- rowSums(prop1km[,c(4,5,6,7,9, 10)]) # improved, neutral, calcareous, acid, heather and shrub, heather grassland 
df.prop1km$arable1km <- prop1km[,c(3)] #arable

##################################
### NOISE POLLUTION IN BUFFERS ###
##################################

# new objects
np100 <- extract(noise, buff100)
np250 <- extract(noise, buff250)
np400 <- extract(noise, buff400)
np1km <- extract(noise, buff1km)

# mean noise pollution value, per site
# 100
mean_np100 <- np100 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_noise))
# 250
mean_np250 <- np250 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_noise))
# 400
mean_np400 <- np400 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_noise))
# 1km
mean_np1km <- np1km %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_noise))

# new columns
occasions.squ$noise100 <- NA
occasions.squ$noise400 <- NA
occasions.fox$noise250 <- NA
occasions.fox$noise1km <- NA
occasions.dee$noise250 <- NA
occasions.dee$noise1km <- NA

##################################
### LIGHT POLLUTION IN BUFFERS ###
##################################

# change crs
crs(buff100) <- crs(all.locs)
crs(buff250) <- crs(all.locs)
crs(buff400) <- crs(all.locs)
crs(buff1km) <- crs(all.locs)

# new objects
lp100 <- extract(light$GCC_light_3, buff100)
lp250 <- extract(light$GCC_light_3, buff250)
lp400 <- extract(light$GCC_light_3, buff400)
lp1km <- extract(light$GCC_light_3, buff1km)

# mean light pollution per site
# 100
mean_lp100 <- lp100 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_light_3))
# 250
mean_lp250 <- lp250 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_light_3))
# 400
mean_lp400 <- lp400 %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_light_3))
# 1km
mean_lp1km <- lp1km %>%
  group_by(ID) %>%
  summarise(mean = mean(GCC_light_3))

occasions.squ$light100 <- NA
occasions.squ$light400 <- NA
occasions.fox$light250 <- NA
occasions.fox$light1km <- NA
occasions.dee$light250 <- NA
occasions.dee$light1km <- NA

######################################################
### EXTRACT ENVIRONMENTAL VARIABLES TO DATA FRAMES ###
######################################################

### SQUIRREL ###

# new columns in data frame
# 100m 
occasions.squ$wood100 <- NA
occasions.squ$wet100 <- NA
occasions.squ$urban100 <- NA
occasions.squ$water100 <- NA
occasions.squ$grass100 <- NA
occasions.squ$arable100 <- NA
# 400m
occasions.squ$wood400 <- NA
occasions.squ$wet400 <- NA
occasions.squ$urban400 <- NA
occasions.squ$water400 <- NA
occasions.squ$grass400 <- NA
occasions.squ$arable400 <- NA
# dog relative abundance
occasions.squ$dog_RA <- NA

# if in position 1 - split into positions?
occasions1.squ <- subset(occasions.squ, occasions.squ$Session == "1")
occasions2.squ <- subset(occasions.squ, occasions.squ$Session == "2")
names(occasions.squ)
# split prop dfs too
df1.prop100 <- subset(df.prop100, df.prop100$placement == "1")
df2.prop100 <- subset(df.prop100, df.prop100$placement == "2")
df1.prop400 <- subset(df.prop400, df.prop400$placement == "1")
df2.prop400 <- subset(df.prop400, df.prop400$placement == "2")


### 100m BUFFER

# if at site...cbind 6 columns
# define columns 
prop_columns <- names(occasions.squ)[11:16]
# add proportions for position 1
occasions1.squ[occasions1.squ$Site == "A0", prop_columns] <- df1.prop100[df1.prop100 == "A0", prop_columns]
occasions1.squ[occasions1.squ$Site == "A1", prop_columns] <- df1.prop100[df1.prop100 == "A1", prop_columns]
occasions1.squ[occasions1.squ$Site == "A2", prop_columns] <- df1.prop100[df1.prop100 == "A2", prop_columns]
occasions1.squ[occasions1.squ$Site == "A2/2", prop_columns] <- df1.prop100[df1.prop100 == "A2/2", prop_columns]
occasions1.squ[occasions1.squ$Site == "B0", prop_columns] <- df1.prop100[df1.prop100 == "B0", prop_columns]
occasions1.squ[occasions1.squ$Site == "B2", prop_columns] <- df1.prop100[df1.prop100 == "B2", prop_columns]
occasions1.squ[occasions1.squ$Site == "B2/2", prop_columns] <- df1.prop100[df1.prop100 == "B2/2", prop_columns]
occasions1.squ[occasions1.squ$Site == "C0", prop_columns] <- df1.prop100[df1.prop100 == "C0", prop_columns]
occasions1.squ[occasions1.squ$Site == "C1", prop_columns] <- df1.prop100[df1.prop100 == "C1", prop_columns]
occasions1.squ[occasions1.squ$Site == "C1/2", prop_columns] <- df1.prop100[df1.prop100 == "C1/2", prop_columns]
occasions1.squ[occasions1.squ$Site == "C2", prop_columns] <- df1.prop100[df1.prop100 == "C2", prop_columns]
occasions1.squ[occasions1.squ$Site == "D1", prop_columns] <- df1.prop100[df1.prop100 == "D1", prop_columns]
occasions1.squ[occasions1.squ$Site == "D1/2", prop_columns] <- df1.prop100[df1.prop100 == "D1/2", prop_columns]
occasions1.squ[occasions1.squ$Site == "D2", prop_columns] <- df1.prop100[df1.prop100 == "D2", prop_columns]
occasions1.squ[occasions1.squ$Site == "E0", prop_columns] <- df1.prop100[df1.prop100 == "E0", prop_columns]
occasions1.squ[occasions1.squ$Site == "E0/2", prop_columns] <- df1.prop100[df1.prop100 == "E0/2", prop_columns]
occasions1.squ[occasions1.squ$Site == "E1", prop_columns] <- df1.prop100[df1.prop100 == "E1", prop_columns]
occasions1.squ[occasions1.squ$Site == "E1/2", prop_columns] <- df1.prop100[df1.prop100 == "E1/2", prop_columns]
occasions1.squ[occasions1.squ$Site == "E2", prop_columns] <- df1.prop100[df1.prop100 == "E2", prop_columns]
occasions1.squ[occasions1.squ$Site == "F0", prop_columns] <- df1.prop100[df1.prop100 == "F0", prop_columns]
occasions1.squ[occasions1.squ$Site == "F1", prop_columns] <- df1.prop100[df1.prop100 == "F1", prop_columns]
occasions1.squ[occasions1.squ$Site == "F2", prop_columns] <- df1.prop100[df1.prop100 == "F2", prop_columns]
occasions1.squ[occasions1.squ$Site == "G0", prop_columns] <- df1.prop100[df1.prop100 == "G0", prop_columns]
occasions1.squ[occasions1.squ$Site == "G1", prop_columns] <- df1.prop100[df1.prop100 == "G1", prop_columns]
occasions1.squ[occasions1.squ$Site == "G2", prop_columns] <- df1.prop100[df1.prop100 == "G2", prop_columns]
occasions1.squ[occasions1.squ$Site == "H0", prop_columns] <- df1.prop100[df1.prop100 == "H0", prop_columns]
occasions1.squ[occasions1.squ$Site == "H1", prop_columns] <- df1.prop100[df1.prop100 == "H1", prop_columns]
occasions1.squ[occasions1.squ$Site == "H2", prop_columns] <- df1.prop100[df1.prop100 == "H2", prop_columns]
occasions1.squ[occasions1.squ$Site == "I0", prop_columns] <- df1.prop100[df1.prop100 == "I0", prop_columns]
occasions1.squ[occasions1.squ$Site == "I1", prop_columns] <- df1.prop100[df1.prop100 == "I1", prop_columns]
occasions1.squ[occasions1.squ$Site == "I2", prop_columns] <- df1.prop100[df1.prop100 == "I2", prop_columns]
occasions1.squ[occasions1.squ$Site == "J0", prop_columns] <- df1.prop100[df1.prop100 == "J0", prop_columns]
occasions1.squ[occasions1.squ$Site == "J1", prop_columns] <- df1.prop100[df1.prop100 == "J1", prop_columns]
occasions1.squ[occasions1.squ$Site == "J2", prop_columns] <- df1.prop100[df1.prop100 == "J2", prop_columns]
# add proportions for position 2
occasions2.squ[occasions2.squ$Site == "A0", prop_columns] <- df2.prop100[df2.prop100 == "A0", prop_columns]
occasions2.squ[occasions2.squ$Site == "A1", prop_columns] <- df2.prop100[df2.prop100 == "A1", prop_columns]
occasions2.squ[occasions2.squ$Site == "A2", prop_columns] <- df2.prop100[df2.prop100 == "A2", prop_columns]
occasions2.squ[occasions2.squ$Site == "A2/2", prop_columns] <- df2.prop100[df2.prop100 == "A2/2", prop_columns]
occasions2.squ[occasions2.squ$Site == "B0", prop_columns] <- df2.prop100[df2.prop100 == "B0", prop_columns]
occasions2.squ[occasions2.squ$Site == "B2", prop_columns] <- df2.prop100[df2.prop100 == "B2", prop_columns]
occasions2.squ[occasions2.squ$Site == "B2/2", prop_columns] <- df2.prop100[df2.prop100 == "B2/2", prop_columns]
occasions2.squ[occasions2.squ$Site == "C0", prop_columns] <- df2.prop100[df2.prop100 == "C0", prop_columns]
occasions2.squ[occasions2.squ$Site == "C1", prop_columns] <- df2.prop100[df2.prop100 == "C1", prop_columns]
occasions2.squ[occasions2.squ$Site == "C1/2", prop_columns] <- df2.prop100[df2.prop100 == "C1/2", prop_columns]
occasions2.squ[occasions2.squ$Site == "C2", prop_columns] <- df2.prop100[df2.prop100 == "C2", prop_columns]
occasions2.squ[occasions2.squ$Site == "D1", prop_columns] <- df2.prop100[df2.prop100 == "D1", prop_columns]
occasions2.squ[occasions2.squ$Site == "D1/2", prop_columns] <- df2.prop100[df2.prop100 == "D1/2", prop_columns]
occasions2.squ[occasions2.squ$Site == "D2", prop_columns] <- df2.prop100[df2.prop100 == "D2", prop_columns]
occasions2.squ[occasions2.squ$Site == "E0", prop_columns] <- df2.prop100[df2.prop100 == "E0", prop_columns]
occasions2.squ[occasions2.squ$Site == "E0/2", prop_columns] <- df2.prop100[df2.prop100 == "E0/2", prop_columns]
occasions2.squ[occasions2.squ$Site == "E1", prop_columns] <- df2.prop100[df2.prop100 == "E1", prop_columns]
occasions2.squ[occasions2.squ$Site == "E1/2", prop_columns] <- df2.prop100[df2.prop100 == "E1/2", prop_columns]
occasions2.squ[occasions2.squ$Site == "E2", prop_columns] <- df2.prop100[df2.prop100 == "E2", prop_columns]
occasions2.squ[occasions2.squ$Site == "F0", prop_columns] <- df2.prop100[df2.prop100 == "F0", prop_columns]
occasions2.squ[occasions2.squ$Site == "F1", prop_columns] <- df2.prop100[df2.prop100 == "F1", prop_columns]
occasions2.squ[occasions2.squ$Site == "F2", prop_columns] <- df2.prop100[df2.prop100 == "F2", prop_columns]
occasions2.squ[occasions2.squ$Site == "G0", prop_columns] <- df2.prop100[df2.prop100 == "G0", prop_columns]
occasions2.squ[occasions2.squ$Site == "G1", prop_columns] <- df2.prop100[df2.prop100 == "G1", prop_columns]
occasions2.squ[occasions2.squ$Site == "G2", prop_columns] <- df2.prop100[df2.prop100 == "G2", prop_columns]
occasions2.squ[occasions2.squ$Site == "H0", prop_columns] <- df2.prop100[df2.prop100 == "H0", prop_columns]
occasions2.squ[occasions2.squ$Site == "H1", prop_columns] <- df2.prop100[df2.prop100 == "H1", prop_columns]
occasions2.squ[occasions2.squ$Site == "H2", prop_columns] <- df2.prop100[df2.prop100 == "H2", prop_columns]
occasions2.squ[occasions2.squ$Site == "I0", prop_columns] <- df2.prop100[df2.prop100 == "I0", prop_columns]
occasions2.squ[occasions2.squ$Site == "I1", prop_columns] <- df2.prop100[df2.prop100 == "I1", prop_columns]
occasions2.squ[occasions2.squ$Site == "I2", prop_columns] <- df2.prop100[df2.prop100 == "I2", prop_columns]
occasions2.squ[occasions2.squ$Site == "J0", prop_columns] <- df2.prop100[df2.prop100 == "J0", prop_columns]
occasions2.squ[occasions2.squ$Site == "J1", prop_columns] <- df2.prop100[df2.prop100 == "J1", prop_columns]
occasions2.squ[occasions2.squ$Site == "J2", prop_columns] <- df2.prop100[df2.prop100 == "J2", prop_columns]

head(occasions1.squ, 10)
head(occasions2.squ, 10)
summary(occasions1.squ$water100)
summary(occasions1.squ$wood100)
summary(occasions1.squ$urban100)
summary(occasions1.squ$arable100)
summary(occasions1.squ$wet100)
summary(occasions1.squ$grass100)
summary(occasions2.squ$water100)
summary(occasions2.squ$wood100)
summary(occasions2.squ$urban100)
summary(occasions2.squ$arable100)
summary(occasions2.squ$wet100)
summary(occasions2.squ$grass100)

### 400m BUFFER

# if at site...cbind 6 columns
# define columns 
prop_columns400 <- names(occasions.squ)[17:22]
# add proportions for position 1
occasions1.squ[occasions1.squ$Site == "A0", prop_columns400] <- df1.prop400[df1.prop400 == "A0", prop_columns400]
occasions1.squ[occasions1.squ$Site == "A1", prop_columns400] <- df1.prop400[df1.prop400 == "A1", prop_columns400]
occasions1.squ[occasions1.squ$Site == "A2", prop_columns400] <- df1.prop400[df1.prop400 == "A2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "A2/2", prop_columns400] <- df1.prop400[df1.prop400 == "A2/2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "B0", prop_columns400] <- df1.prop400[df1.prop400 == "B0", prop_columns400]
occasions1.squ[occasions1.squ$Site == "B2", prop_columns400] <- df1.prop400[df1.prop400 == "B2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "B2/2", prop_columns400] <- df1.prop400[df1.prop400 == "B2/2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "C0", prop_columns400] <- df1.prop400[df1.prop400 == "C0", prop_columns400]
occasions1.squ[occasions1.squ$Site == "C1", prop_columns400] <- df1.prop400[df1.prop400 == "C1", prop_columns400]
occasions1.squ[occasions1.squ$Site == "C1/2", prop_columns400] <- df1.prop400[df1.prop400 == "C1/2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "C2", prop_columns400] <- df1.prop400[df1.prop400 == "C2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "D1", prop_columns400] <- df1.prop400[df1.prop400 == "D1", prop_columns400]
occasions1.squ[occasions1.squ$Site == "D1/2", prop_columns400] <- df1.prop400[df1.prop400 == "D1/2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "D2", prop_columns400] <- df1.prop400[df1.prop400 == "D2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "E0", prop_columns400] <- df1.prop400[df1.prop400 == "E0", prop_columns400]
occasions1.squ[occasions1.squ$Site == "E0/2", prop_columns400] <- df1.prop400[df1.prop400 == "E0/2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "E1", prop_columns400] <- df1.prop400[df1.prop400 == "E1", prop_columns400]
occasions1.squ[occasions1.squ$Site == "E1/2", prop_columns400] <- df1.prop400[df1.prop400 == "E1/2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "E2", prop_columns400] <- df1.prop400[df1.prop400 == "E2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "F0", prop_columns400] <- df1.prop400[df1.prop400 == "F0", prop_columns400]
occasions1.squ[occasions1.squ$Site == "F1", prop_columns400] <- df1.prop400[df1.prop400 == "F1", prop_columns400]
occasions1.squ[occasions1.squ$Site == "F2", prop_columns400] <- df1.prop400[df1.prop400 == "F2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "G0", prop_columns400] <- df1.prop400[df1.prop400 == "G0", prop_columns400]
occasions1.squ[occasions1.squ$Site == "G1", prop_columns400] <- df1.prop400[df1.prop400 == "G1", prop_columns400]
occasions1.squ[occasions1.squ$Site == "G2", prop_columns400] <- df1.prop400[df1.prop400 == "G2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "H0", prop_columns400] <- df1.prop400[df1.prop400 == "H0", prop_columns400]
occasions1.squ[occasions1.squ$Site == "H1", prop_columns400] <- df1.prop400[df1.prop400 == "H1", prop_columns400]
occasions1.squ[occasions1.squ$Site == "H2", prop_columns400] <- df1.prop400[df1.prop400 == "H2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "I0", prop_columns400] <- df1.prop400[df1.prop400 == "I0", prop_columns400]
occasions1.squ[occasions1.squ$Site == "I1", prop_columns400] <- df1.prop400[df1.prop400 == "I1", prop_columns400]
occasions1.squ[occasions1.squ$Site == "I2", prop_columns400] <- df1.prop400[df1.prop400 == "I2", prop_columns400]
occasions1.squ[occasions1.squ$Site == "J0", prop_columns400] <- df1.prop400[df1.prop400 == "J0", prop_columns400]
occasions1.squ[occasions1.squ$Site == "J1", prop_columns400] <- df1.prop400[df1.prop400 == "J1", prop_columns400]
occasions1.squ[occasions1.squ$Site == "J2", prop_columns400] <- df1.prop400[df1.prop400 == "J2", prop_columns400]
# add proportions for position 2
occasions2.squ[occasions2.squ$Site == "A0", prop_columns400] <- df2.prop400[df2.prop400 == "A0", prop_columns400]
occasions2.squ[occasions2.squ$Site == "A1", prop_columns400] <- df2.prop400[df2.prop400 == "A1", prop_columns400]
occasions2.squ[occasions2.squ$Site == "A2", prop_columns400] <- df2.prop400[df2.prop400 == "A2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "A2/2", prop_columns400] <- df2.prop400[df2.prop400 == "A2/2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "B0", prop_columns400] <- df2.prop400[df2.prop400 == "B0", prop_columns400]
occasions2.squ[occasions2.squ$Site == "B2", prop_columns400] <- df2.prop400[df2.prop400 == "B2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "B2/2", prop_columns400] <- df2.prop400[df2.prop400 == "B2/2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "C0", prop_columns400] <- df2.prop400[df2.prop400 == "C0", prop_columns400]
occasions2.squ[occasions2.squ$Site == "C1", prop_columns400] <- df2.prop400[df2.prop400 == "C1", prop_columns400]
occasions2.squ[occasions2.squ$Site == "C1/2", prop_columns400] <- df2.prop400[df2.prop400 == "C1/2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "C2", prop_columns400] <- df2.prop400[df2.prop400 == "C2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "D1", prop_columns400] <- df2.prop400[df2.prop400 == "D1", prop_columns400]
occasions2.squ[occasions2.squ$Site == "D1/2", prop_columns400] <- df2.prop400[df2.prop400 == "D1/2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "D2", prop_columns400] <- df2.prop400[df2.prop400 == "D2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "E0", prop_columns400] <- df2.prop400[df2.prop400 == "E0", prop_columns400]
occasions2.squ[occasions2.squ$Site == "E0/2", prop_columns400] <- df2.prop400[df2.prop400 == "E0/2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "E1", prop_columns400] <- df2.prop400[df2.prop400 == "E1", prop_columns400]
occasions2.squ[occasions2.squ$Site == "E1/2", prop_columns400] <- df2.prop400[df2.prop400 == "E1/2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "E2", prop_columns400] <- df2.prop400[df2.prop400 == "E2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "F0", prop_columns400] <- df2.prop400[df2.prop400 == "F0", prop_columns400]
occasions2.squ[occasions2.squ$Site == "F1", prop_columns400] <- df2.prop400[df2.prop400 == "F1", prop_columns400]
occasions2.squ[occasions2.squ$Site == "F2", prop_columns400] <- df2.prop400[df2.prop400 == "F2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "G0", prop_columns400] <- df2.prop400[df2.prop400 == "G0", prop_columns400]
occasions2.squ[occasions2.squ$Site == "G1", prop_columns400] <- df2.prop400[df2.prop400 == "G1", prop_columns400]
occasions2.squ[occasions2.squ$Site == "G2", prop_columns400] <- df2.prop400[df2.prop400 == "G2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "H0", prop_columns400] <- df2.prop400[df2.prop400 == "H0", prop_columns400]
occasions2.squ[occasions2.squ$Site == "H1", prop_columns400] <- df2.prop400[df2.prop400 == "H1", prop_columns400]
occasions2.squ[occasions2.squ$Site == "H2", prop_columns400] <- df2.prop400[df2.prop400 == "H2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "I0", prop_columns400] <- df2.prop400[df2.prop400 == "I0", prop_columns400]
occasions2.squ[occasions2.squ$Site == "I1", prop_columns400] <- df2.prop400[df2.prop400 == "I1", prop_columns400]
occasions2.squ[occasions2.squ$Site == "I2", prop_columns400] <- df2.prop400[df2.prop400 == "I2", prop_columns400]
occasions2.squ[occasions2.squ$Site == "J0", prop_columns400] <- df2.prop400[df2.prop400 == "J0", prop_columns400]
occasions2.squ[occasions2.squ$Site == "J1", prop_columns400] <- df2.prop400[df2.prop400 == "J1", prop_columns400]
occasions2.squ[occasions2.squ$Site == "J2", prop_columns400] <- df2.prop400[df2.prop400 == "J2", prop_columns400]

head(occasions1.squ, 10)
head(occasions2.squ, 10)
summary(occasions1.squ$water400)
summary(occasions1.squ$wood400)
summary(occasions1.squ$urban400)
summary(occasions1.squ$arable400)
summary(occasions1.squ$wet400)
summary(occasions1.squ$grass400)
summary(occasions2.squ$water400)
summary(occasions2.squ$wood400)
summary(occasions2.squ$urban400)
summary(occasions2.squ$arable400)
summary(occasions2.squ$wet400)
summary(occasions2.squ$grass400)

# add dog relative abundance (per camera)
head(dog_RA)
# add for first placement 1
occasions1.squ[occasions1.squ$Site == "A0", c("dog_RA")] <- dog_RA[dog_RA$site == "A0", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "A1", c("dog_RA")] <- dog_RA[dog_RA$site == "A1", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "A2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "A2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2/2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "B0", c("dog_RA")] <- dog_RA[dog_RA$site == "B0", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "B2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "B2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2/2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "C0", c("dog_RA")] <- dog_RA[dog_RA$site == "C0", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "C1", c("dog_RA")] <- dog_RA[dog_RA$site == "C1", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "C1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "C1/2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "C2", c("dog_RA")] <- dog_RA[dog_RA$site == "C2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "D1", c("dog_RA")] <- dog_RA[dog_RA$site == "D1", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "D1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "D1/2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "D2", c("dog_RA")] <- dog_RA[dog_RA$site == "D2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "E0", c("dog_RA")] <- dog_RA[dog_RA$site == "E0", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "E0/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E0/2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "E1", c("dog_RA")] <- dog_RA[dog_RA$site == "E1", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "E1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E1/2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "E2", c("dog_RA")] <- dog_RA[dog_RA$site == "E2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "F0", c("dog_RA")] <- dog_RA[dog_RA$site == "F0", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "F1", c("dog_RA")] <- dog_RA[dog_RA$site == "F1", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "F2", c("dog_RA")] <- dog_RA[dog_RA$site == "F2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "G0", c("dog_RA")] <- dog_RA[dog_RA$site == "G0", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "G1", c("dog_RA")] <- dog_RA[dog_RA$site == "G1", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "G2", c("dog_RA")] <- dog_RA[dog_RA$site == "G2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "H0", c("dog_RA")] <- dog_RA[dog_RA$site == "H0", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "H1", c("dog_RA")] <- dog_RA[dog_RA$site == "H1", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "H2", c("dog_RA")] <- dog_RA[dog_RA$site == "H2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "I0", c("dog_RA")] <- dog_RA[dog_RA$site == "I0", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "I1", c("dog_RA")] <- dog_RA[dog_RA$site == "I1", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "I2", c("dog_RA")] <- dog_RA[dog_RA$site == "I2", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "J0", c("dog_RA")] <- dog_RA[dog_RA$site == "J0", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "J1", c("dog_RA")] <- dog_RA[dog_RA$site == "J1", c("loc.mean1")]
occasions1.squ[occasions1.squ$Site == "J2", c("dog_RA")] <- dog_RA[dog_RA$site == "J2", c("loc.mean1")]
# add for first placement 2
occasions2.squ[occasions2.squ$Site == "A0", c("dog_RA")] <- dog_RA[dog_RA$site == "A0", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "A1", c("dog_RA")] <- dog_RA[dog_RA$site == "A1", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "A2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "A2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2/2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "B0", c("dog_RA")] <- dog_RA[dog_RA$site == "B0", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "B2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "B2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2/2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "C0", c("dog_RA")] <- dog_RA[dog_RA$site == "C0", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "C1", c("dog_RA")] <- dog_RA[dog_RA$site == "C1", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "C1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "C1/2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "C2", c("dog_RA")] <- dog_RA[dog_RA$site == "C2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "D1", c("dog_RA")] <- dog_RA[dog_RA$site == "D1", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "D1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "D1/2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "D2", c("dog_RA")] <- dog_RA[dog_RA$site == "D2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "E0", c("dog_RA")] <- dog_RA[dog_RA$site == "E0", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "E0/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E0/2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "E1", c("dog_RA")] <- dog_RA[dog_RA$site == "E1", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "E1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E1/2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "E2", c("dog_RA")] <- dog_RA[dog_RA$site == "E2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "F0", c("dog_RA")] <- dog_RA[dog_RA$site == "F0", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "F1", c("dog_RA")] <- dog_RA[dog_RA$site == "F1", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "F2", c("dog_RA")] <- dog_RA[dog_RA$site == "F2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "G0", c("dog_RA")] <- dog_RA[dog_RA$site == "G0", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "G1", c("dog_RA")] <- dog_RA[dog_RA$site == "G1", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "G2", c("dog_RA")] <- dog_RA[dog_RA$site == "G2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "H0", c("dog_RA")] <- dog_RA[dog_RA$site == "H0", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "H1", c("dog_RA")] <- dog_RA[dog_RA$site == "H1", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "H2", c("dog_RA")] <- dog_RA[dog_RA$site == "H2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "I0", c("dog_RA")] <- dog_RA[dog_RA$site == "I0", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "I1", c("dog_RA")] <- dog_RA[dog_RA$site == "I1", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "I2", c("dog_RA")] <- dog_RA[dog_RA$site == "I2", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "J0", c("dog_RA")] <- dog_RA[dog_RA$site == "J0", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "J1", c("dog_RA")] <- dog_RA[dog_RA$site == "J1", c("loc.mean2")]
occasions2.squ[occasions2.squ$Site == "J2", c("dog_RA")] <- dog_RA[dog_RA$site == "J2", c("loc.mean2")]

head(occasions1.squ, 10)
head(occasions2.squ, 10)
summary(as.numeric(occasions1.squ$dog_RA))
summary(occasions2.squ$dog_RA)

# add mean light pollution per buffer
### BUFFER 100 ###
# position 1
occasions1.squ[occasions1.squ$Site == "A0", c("light100")] <- mean_lp100[mean_lp100$ID == "1", c("mean")]
occasions1.squ[occasions1.squ$Site == "A1", c("light100")] <- mean_lp100[mean_lp100$ID == "2", c("mean")]
occasions1.squ[occasions1.squ$Site == "A2", c("light100")] <- mean_lp100[mean_lp100$ID == "3", c("mean")]
occasions1.squ[occasions1.squ$Site == "A2/2", c("light100")] <- mean_lp100[mean_lp100$ID == "4", c("mean")]
occasions1.squ[occasions1.squ$Site == "B0", c("light100")] <- mean_lp100[mean_lp100$ID == "5", c("mean")]
occasions1.squ[occasions1.squ$Site == "B2", c("light100")] <- mean_lp100[mean_lp100$ID == "6", c("mean")]
occasions1.squ[occasions1.squ$Site == "B2/2", c("light100")] <- mean_lp100[mean_lp100$ID == "7", c("mean")]
occasions1.squ[occasions1.squ$Site == "C0", c("light100")] <- mean_lp100[mean_lp100$ID == "8", c("mean")]
occasions1.squ[occasions1.squ$Site == "C1", c("light100")] <- mean_lp100[mean_lp100$ID == "9", c("mean")]
occasions1.squ[occasions1.squ$Site == "C1/2", c("light100")] <- mean_lp100[mean_lp100$ID == "10", c("mean")]
occasions1.squ[occasions1.squ$Site == "C2", c("light100")] <- mean_lp100[mean_lp100$ID == "11", c("mean")]
occasions1.squ[occasions1.squ$Site == "D1", c("light100")] <- mean_lp100[mean_lp100$ID == "12", c("mean")]
occasions1.squ[occasions1.squ$Site == "D1/2", c("light100")] <- mean_lp100[mean_lp100$ID == "13", c("mean")]
occasions1.squ[occasions1.squ$Site == "D2", c("light100")] <- mean_lp100[mean_lp100$ID == "14", c("mean")]
occasions1.squ[occasions1.squ$Site == "E0", c("light100")] <- mean_lp100[mean_lp100$ID == "15", c("mean")]
occasions1.squ[occasions1.squ$Site == "E0/2", c("light100")] <- mean_lp100[mean_lp100$ID == "16", c("mean")]
occasions1.squ[occasions1.squ$Site == "E1", c("light100")] <- mean_lp100[mean_lp100$ID == "17", c("mean")]
occasions1.squ[occasions1.squ$Site == "E1/2", c("light100")] <- mean_lp100[mean_lp100$ID == "18", c("mean")]
occasions1.squ[occasions1.squ$Site == "E2", c("light100")] <- mean_lp100[mean_lp100$ID == "19", c("mean")]
occasions1.squ[occasions1.squ$Site == "F0", c("light100")] <- mean_lp100[mean_lp100$ID == "20", c("mean")]
occasions1.squ[occasions1.squ$Site == "F1", c("light100")] <- mean_lp100[mean_lp100$ID == "21", c("mean")]
occasions1.squ[occasions1.squ$Site == "F2", c("light100")] <- mean_lp100[mean_lp100$ID == "22", c("mean")]
occasions1.squ[occasions1.squ$Site == "G0", c("light100")] <- mean_lp100[mean_lp100$ID == "23", c("mean")]
occasions1.squ[occasions1.squ$Site == "G1", c("light100")] <- mean_lp100[mean_lp100$ID == "24", c("mean")]
occasions1.squ[occasions1.squ$Site == "G2", c("light100")] <- mean_lp100[mean_lp100$ID == "25", c("mean")]
occasions1.squ[occasions1.squ$Site == "H0", c("light100")] <- mean_lp100[mean_lp100$ID == "26", c("mean")]
occasions1.squ[occasions1.squ$Site == "H1", c("light100")] <- mean_lp100[mean_lp100$ID == "27", c("mean")]
occasions1.squ[occasions1.squ$Site == "H2", c("light100")] <- mean_lp100[mean_lp100$ID == "28", c("mean")]
occasions1.squ[occasions1.squ$Site == "I0", c("light100")] <- mean_lp100[mean_lp100$ID == "29", c("mean")]
occasions1.squ[occasions1.squ$Site == "I1", c("light100")] <- mean_lp100[mean_lp100$ID == "30", c("mean")]
occasions1.squ[occasions1.squ$Site == "I2", c("light100")] <- mean_lp100[mean_lp100$ID == "31", c("mean")]
occasions1.squ[occasions1.squ$Site == "J0", c("light100")] <- mean_lp100[mean_lp100$ID == "32", c("mean")]
occasions1.squ[occasions1.squ$Site == "J1", c("light100")] <- mean_lp100[mean_lp100$ID == "33", c("mean")]
occasions1.squ[occasions1.squ$Site == "J2", c("light100")] <- mean_lp100[mean_lp100$ID == "34", c("mean")]
# position 2
occasions2.squ[occasions2.squ$Site == "A0", c("light100")] <- mean_lp100[mean_lp100$ID == "35", c("mean")]
occasions2.squ[occasions2.squ$Site == "A1", c("light100")] <- mean_lp100[mean_lp100$ID == "36", c("mean")]
occasions2.squ[occasions2.squ$Site == "A2", c("light100")] <- mean_lp100[mean_lp100$ID == "37", c("mean")]
occasions2.squ[occasions2.squ$Site == "A2/2", c("light100")] <- mean_lp100[mean_lp100$ID == "38", c("mean")]
occasions2.squ[occasions2.squ$Site == "B0", c("light100")] <- mean_lp100[mean_lp100$ID == "39", c("mean")]
occasions2.squ[occasions2.squ$Site == "C1", c("light100")] <- mean_lp100[mean_lp100$ID == "40", c("mean")]
occasions2.squ[occasions2.squ$Site == "C1/2", c("light100")] <- mean_lp100[mean_lp100$ID == "41", c("mean")]
occasions2.squ[occasions2.squ$Site == "C0", c("light100")] <- mean_lp100[mean_lp100$ID == "42", c("mean")]
occasions2.squ[occasions2.squ$Site == "C2", c("light100")] <- mean_lp100[mean_lp100$ID == "43", c("mean")]
occasions2.squ[occasions2.squ$Site == "D1", c("light100")] <- mean_lp100[mean_lp100$ID == "44", c("mean")]
occasions2.squ[occasions2.squ$Site == "D1/2", c("light100")] <- mean_lp100[mean_lp100$ID == "45", c("mean")]
occasions2.squ[occasions2.squ$Site == "E1/2", c("light100")] <- mean_lp100[mean_lp100$ID == "46", c("mean")]
occasions2.squ[occasions2.squ$Site == "E1", c("light100")] <- mean_lp100[mean_lp100$ID == "47", c("mean")]
occasions2.squ[occasions2.squ$Site == "F2", c("light100")] <- mean_lp100[mean_lp100$ID == "48", c("mean")]
occasions2.squ[occasions2.squ$Site == "G0", c("light100")] <- mean_lp100[mean_lp100$ID == "49", c("mean")]
occasions2.squ[occasions2.squ$Site == "G2", c("light100")] <- mean_lp100[mean_lp100$ID == "50", c("mean")]
occasions2.squ[occasions2.squ$Site == "E0/2", c("light100")] <- mean_lp100[mean_lp100$ID == "51", c("mean")]
occasions2.squ[occasions2.squ$Site == "E0", c("light100")] <- mean_lp100[mean_lp100$ID == "52", c("mean")]
occasions2.squ[occasions2.squ$Site == "D2", c("light100")] <- mean_lp100[mean_lp100$ID == "53", c("mean")]
occasions2.squ[occasions2.squ$Site == "F0", c("light100")] <- mean_lp100[mean_lp100$ID == "54", c("mean")]
occasions2.squ[occasions2.squ$Site == "E2", c("light100")] <- mean_lp100[mean_lp100$ID == "55", c("mean")]
occasions2.squ[occasions2.squ$Site == "G1", c("light100")] <- mean_lp100[mean_lp100$ID == "56", c("mean")]
occasions2.squ[occasions2.squ$Site == "J1", c("light100")] <- mean_lp100[mean_lp100$ID == "57", c("mean")]
occasions2.squ[occasions2.squ$Site == "J0", c("light100")] <- mean_lp100[mean_lp100$ID == "58", c("mean")]
occasions2.squ[occasions2.squ$Site == "B2", c("light100")] <- mean_lp100[mean_lp100$ID == "59", c("mean")]
occasions2.squ[occasions2.squ$Site == "B2/2", c("light100")] <- mean_lp100[mean_lp100$ID == "60", c("mean")]
occasions2.squ[occasions2.squ$Site == "F1", c("light100")] <- mean_lp100[mean_lp100$ID == "61", c("mean")]
occasions2.squ[occasions2.squ$Site == "I0", c("light100")] <- mean_lp100[mean_lp100$ID == "62", c("mean")]
occasions2.squ[occasions2.squ$Site == "I1", c("light100")] <- mean_lp100[mean_lp100$ID == "63", c("mean")]
occasions2.squ[occasions2.squ$Site == "I2", c("light100")] <- mean_lp100[mean_lp100$ID == "64", c("mean")]
occasions2.squ[occasions2.squ$Site == "H2", c("light100")] <- mean_lp100[mean_lp100$ID == "65", c("mean")]
occasions2.squ[occasions2.squ$Site == "H0", c("light100")] <- mean_lp100[mean_lp100$ID == "66", c("mean")]
occasions2.squ[occasions2.squ$Site == "H1", c("light100")] <- mean_lp100[mean_lp100$ID == "67", c("mean")]
occasions2.squ[occasions2.squ$Site == "J2", c("light100")] <- mean_lp100[mean_lp100$ID == "68", c("mean")]
### BUFFER 400 ###
# position 1
occasions1.squ[occasions1.squ$Site == "A0", c("light400")] <- mean_lp400[mean_lp400$ID == "1", c("mean")]
occasions1.squ[occasions1.squ$Site == "A1", c("light400")] <- mean_lp400[mean_lp400$ID == "2", c("mean")]
occasions1.squ[occasions1.squ$Site == "A2", c("light400")] <- mean_lp400[mean_lp400$ID == "3", c("mean")]
occasions1.squ[occasions1.squ$Site == "A2/2", c("light400")] <- mean_lp400[mean_lp400$ID == "4", c("mean")]
occasions1.squ[occasions1.squ$Site == "B0", c("light400")] <- mean_lp400[mean_lp400$ID == "5", c("mean")]
occasions1.squ[occasions1.squ$Site == "B2", c("light400")] <- mean_lp400[mean_lp400$ID == "6", c("mean")]
occasions1.squ[occasions1.squ$Site == "B2/2", c("light400")] <- mean_lp400[mean_lp400$ID == "7", c("mean")]
occasions1.squ[occasions1.squ$Site == "C0", c("light400")] <- mean_lp400[mean_lp400$ID == "8", c("mean")]
occasions1.squ[occasions1.squ$Site == "C1", c("light400")] <- mean_lp400[mean_lp400$ID == "9", c("mean")]
occasions1.squ[occasions1.squ$Site == "C1/2", c("light400")] <- mean_lp400[mean_lp400$ID == "10", c("mean")]
occasions1.squ[occasions1.squ$Site == "C2", c("light400")] <- mean_lp400[mean_lp400$ID == "11", c("mean")]
occasions1.squ[occasions1.squ$Site == "D1", c("light400")] <- mean_lp400[mean_lp400$ID == "12", c("mean")]
occasions1.squ[occasions1.squ$Site == "D1/2", c("light400")] <- mean_lp400[mean_lp400$ID == "13", c("mean")]
occasions1.squ[occasions1.squ$Site == "D2", c("light400")] <- mean_lp400[mean_lp400$ID == "14", c("mean")]
occasions1.squ[occasions1.squ$Site == "E0", c("light400")] <- mean_lp400[mean_lp400$ID == "15", c("mean")]
occasions1.squ[occasions1.squ$Site == "E0/2", c("light400")] <- mean_lp400[mean_lp400$ID == "16", c("mean")]
occasions1.squ[occasions1.squ$Site == "E1", c("light400")] <- mean_lp400[mean_lp400$ID == "17", c("mean")]
occasions1.squ[occasions1.squ$Site == "E1/2", c("light400")] <- mean_lp400[mean_lp400$ID == "18", c("mean")]
occasions1.squ[occasions1.squ$Site == "E2", c("light400")] <- mean_lp400[mean_lp400$ID == "19", c("mean")]
occasions1.squ[occasions1.squ$Site == "F0", c("light400")] <- mean_lp400[mean_lp400$ID == "20", c("mean")]
occasions1.squ[occasions1.squ$Site == "F1", c("light400")] <- mean_lp400[mean_lp400$ID == "21", c("mean")]
occasions1.squ[occasions1.squ$Site == "F2", c("light400")] <- mean_lp400[mean_lp400$ID == "22", c("mean")]
occasions1.squ[occasions1.squ$Site == "G0", c("light400")] <- mean_lp400[mean_lp400$ID == "23", c("mean")]
occasions1.squ[occasions1.squ$Site == "G1", c("light400")] <- mean_lp400[mean_lp400$ID == "24", c("mean")]
occasions1.squ[occasions1.squ$Site == "G2", c("light400")] <- mean_lp400[mean_lp400$ID == "25", c("mean")]
occasions1.squ[occasions1.squ$Site == "H0", c("light400")] <- mean_lp400[mean_lp400$ID == "26", c("mean")]
occasions1.squ[occasions1.squ$Site == "H1", c("light400")] <- mean_lp400[mean_lp400$ID == "27", c("mean")]
occasions1.squ[occasions1.squ$Site == "H2", c("light400")] <- mean_lp400[mean_lp400$ID == "28", c("mean")]
occasions1.squ[occasions1.squ$Site == "I0", c("light400")] <- mean_lp400[mean_lp400$ID == "29", c("mean")]
occasions1.squ[occasions1.squ$Site == "I1", c("light400")] <- mean_lp400[mean_lp400$ID == "30", c("mean")]
occasions1.squ[occasions1.squ$Site == "I2", c("light400")] <- mean_lp400[mean_lp400$ID == "31", c("mean")]
occasions1.squ[occasions1.squ$Site == "J0", c("light400")] <- mean_lp400[mean_lp400$ID == "32", c("mean")]
occasions1.squ[occasions1.squ$Site == "J1", c("light400")] <- mean_lp400[mean_lp400$ID == "33", c("mean")]
occasions1.squ[occasions1.squ$Site == "J2", c("light400")] <- mean_lp400[mean_lp400$ID == "34", c("mean")]
# position 2
occasions2.squ[occasions2.squ$Site == "A0", c("light400")] <- mean_lp400[mean_lp400$ID == "35", c("mean")]
occasions2.squ[occasions2.squ$Site == "A1", c("light400")] <- mean_lp400[mean_lp400$ID == "36", c("mean")]
occasions2.squ[occasions2.squ$Site == "A2", c("light400")] <- mean_lp400[mean_lp400$ID == "37", c("mean")]
occasions2.squ[occasions2.squ$Site == "A2/2", c("light400")] <- mean_lp400[mean_lp400$ID == "38", c("mean")]
occasions2.squ[occasions2.squ$Site == "B0", c("light400")] <- mean_lp400[mean_lp400$ID == "39", c("mean")]
occasions2.squ[occasions2.squ$Site == "C1", c("light400")] <- mean_lp400[mean_lp400$ID == "40", c("mean")]
occasions2.squ[occasions2.squ$Site == "C1/2", c("light400")] <- mean_lp400[mean_lp400$ID == "41", c("mean")]
occasions2.squ[occasions2.squ$Site == "C0", c("light400")] <- mean_lp400[mean_lp400$ID == "42", c("mean")]
occasions2.squ[occasions2.squ$Site == "C2", c("light400")] <- mean_lp400[mean_lp400$ID == "43", c("mean")]
occasions2.squ[occasions2.squ$Site == "D1", c("light400")] <- mean_lp400[mean_lp400$ID == "44", c("mean")]
occasions2.squ[occasions2.squ$Site == "D1/2", c("light400")] <- mean_lp400[mean_lp400$ID == "45", c("mean")]
occasions2.squ[occasions2.squ$Site == "E1/2", c("light400")] <- mean_lp400[mean_lp400$ID == "46", c("mean")]
occasions2.squ[occasions2.squ$Site == "E1", c("light400")] <- mean_lp400[mean_lp400$ID == "47", c("mean")]
occasions2.squ[occasions2.squ$Site == "F2", c("light400")] <- mean_lp400[mean_lp400$ID == "48", c("mean")]
occasions2.squ[occasions2.squ$Site == "G0", c("light400")] <- mean_lp400[mean_lp400$ID == "49", c("mean")]
occasions2.squ[occasions2.squ$Site == "G2", c("light400")] <- mean_lp400[mean_lp400$ID == "50", c("mean")]
occasions2.squ[occasions2.squ$Site == "E0/2", c("light400")] <- mean_lp400[mean_lp400$ID == "51", c("mean")]
occasions2.squ[occasions2.squ$Site == "E0", c("light400")] <- mean_lp400[mean_lp400$ID == "52", c("mean")]
occasions2.squ[occasions2.squ$Site == "D2", c("light400")] <- mean_lp400[mean_lp400$ID == "53", c("mean")]
occasions2.squ[occasions2.squ$Site == "F0", c("light400")] <- mean_lp400[mean_lp400$ID == "54", c("mean")]
occasions2.squ[occasions2.squ$Site == "E2", c("light400")] <- mean_lp400[mean_lp400$ID == "55", c("mean")]
occasions2.squ[occasions2.squ$Site == "G1", c("light400")] <- mean_lp400[mean_lp400$ID == "56", c("mean")]
occasions2.squ[occasions2.squ$Site == "J1", c("light400")] <- mean_lp400[mean_lp400$ID == "57", c("mean")]
occasions2.squ[occasions2.squ$Site == "J0", c("light400")] <- mean_lp400[mean_lp400$ID == "58", c("mean")]
occasions2.squ[occasions2.squ$Site == "B2", c("light400")] <- mean_lp400[mean_lp400$ID == "59", c("mean")]
occasions2.squ[occasions2.squ$Site == "B2/2", c("light400")] <- mean_lp400[mean_lp400$ID == "60", c("mean")]
occasions2.squ[occasions2.squ$Site == "F1", c("light400")] <- mean_lp400[mean_lp400$ID == "61", c("mean")]
occasions2.squ[occasions2.squ$Site == "I0", c("light400")] <- mean_lp400[mean_lp400$ID == "62", c("mean")]
occasions2.squ[occasions2.squ$Site == "I1", c("light400")] <- mean_lp400[mean_lp400$ID == "63", c("mean")]
occasions2.squ[occasions2.squ$Site == "I2", c("light400")] <- mean_lp400[mean_lp400$ID == "64", c("mean")]
occasions2.squ[occasions2.squ$Site == "H2", c("light400")] <- mean_lp400[mean_lp400$ID == "65", c("mean")]
occasions2.squ[occasions2.squ$Site == "H0", c("light400")] <- mean_lp400[mean_lp400$ID == "66", c("mean")]
occasions2.squ[occasions2.squ$Site == "H1", c("light400")] <- mean_lp400[mean_lp400$ID == "67", c("mean")]
occasions2.squ[occasions2.squ$Site == "J2", c("light400")] <- mean_lp400[mean_lp400$ID == "68", c("mean")]
head(occasions1.squ)
head(occasions2.squ)
summary(occasions1.squ$light100)
summary(occasions2.squ$light100)
summary(occasions1.squ$light400)
summary(occasions2.squ$light400)



# add noise pollution per site
### BUFFER 100 ###
# position 1
occasions1.squ[occasions1.squ$Site == "A0", c("noise100")] <- mean_np100[mean_np100$ID == "1", c("mean")]
occasions1.squ[occasions1.squ$Site == "A1", c("noise100")] <- mean_np100[mean_np100$ID == "2", c("mean")]
occasions1.squ[occasions1.squ$Site == "A2", c("noise100")] <- mean_np100[mean_np100$ID == "3", c("mean")]
occasions1.squ[occasions1.squ$Site == "A2/2", c("noise100")] <- mean_np100[mean_np100$ID == "4", c("mean")]
occasions1.squ[occasions1.squ$Site == "B0", c("noise100")] <- mean_np100[mean_np100$ID == "5", c("mean")]
occasions1.squ[occasions1.squ$Site == "B2", c("noise100")] <- mean_np100[mean_np100$ID == "6", c("mean")]
occasions1.squ[occasions1.squ$Site == "B2/2", c("noise100")] <- mean_np100[mean_np100$ID == "7", c("mean")]
occasions1.squ[occasions1.squ$Site == "C0", c("noise100")] <- mean_np100[mean_np100$ID == "8", c("mean")]
occasions1.squ[occasions1.squ$Site == "C1", c("noise100")] <- mean_np100[mean_np100$ID == "9", c("mean")]
occasions1.squ[occasions1.squ$Site == "C1/2", c("noise100")] <- mean_np100[mean_np100$ID == "10", c("mean")]
occasions1.squ[occasions1.squ$Site == "C2", c("noise100")] <- mean_np100[mean_np100$ID == "11", c("mean")]
occasions1.squ[occasions1.squ$Site == "D1", c("noise100")] <- mean_np100[mean_np100$ID == "12", c("mean")]
occasions1.squ[occasions1.squ$Site == "D1/2", c("noise100")] <- mean_np100[mean_np100$ID == "13", c("mean")]
occasions1.squ[occasions1.squ$Site == "D2", c("noise100")] <- mean_np100[mean_np100$ID == "14", c("mean")]
occasions1.squ[occasions1.squ$Site == "E0", c("noise100")] <- mean_np100[mean_np100$ID == "15", c("mean")]
occasions1.squ[occasions1.squ$Site == "E0/2", c("noise100")] <- mean_np100[mean_np100$ID == "16", c("mean")]
occasions1.squ[occasions1.squ$Site == "E1", c("noise100")] <- mean_np100[mean_np100$ID == "17", c("mean")]
occasions1.squ[occasions1.squ$Site == "E1/2", c("noise100")] <- mean_np100[mean_np100$ID == "18", c("mean")]
occasions1.squ[occasions1.squ$Site == "E2", c("noise100")] <- mean_np100[mean_np100$ID == "19", c("mean")]
occasions1.squ[occasions1.squ$Site == "F0", c("noise100")] <- mean_np100[mean_np100$ID == "20", c("mean")]
occasions1.squ[occasions1.squ$Site == "F1", c("noise100")] <- mean_np100[mean_np100$ID == "21", c("mean")]
occasions1.squ[occasions1.squ$Site == "F2", c("noise100")] <- mean_np100[mean_np100$ID == "22", c("mean")]
occasions1.squ[occasions1.squ$Site == "G0", c("noise100")] <- mean_np100[mean_np100$ID == "23", c("mean")]
occasions1.squ[occasions1.squ$Site == "G1", c("noise100")] <- mean_np100[mean_np100$ID == "24", c("mean")]
occasions1.squ[occasions1.squ$Site == "G2", c("noise100")] <- mean_np100[mean_np100$ID == "25", c("mean")]
occasions1.squ[occasions1.squ$Site == "H0", c("noise100")] <- mean_np100[mean_np100$ID == "26", c("mean")]
occasions1.squ[occasions1.squ$Site == "H1", c("noise100")] <- mean_np100[mean_np100$ID == "27", c("mean")]
occasions1.squ[occasions1.squ$Site == "H2", c("noise100")] <- mean_np100[mean_np100$ID == "28", c("mean")]
occasions1.squ[occasions1.squ$Site == "I0", c("noise100")] <- mean_np100[mean_np100$ID == "29", c("mean")]
occasions1.squ[occasions1.squ$Site == "I1", c("noise100")] <- mean_np100[mean_np100$ID == "30", c("mean")]
occasions1.squ[occasions1.squ$Site == "I2", c("noise100")] <- mean_np100[mean_np100$ID == "31", c("mean")]
occasions1.squ[occasions1.squ$Site == "J0", c("noise100")] <- mean_np100[mean_np100$ID == "32", c("mean")]
occasions1.squ[occasions1.squ$Site == "J1", c("noise100")] <- mean_np100[mean_np100$ID == "33", c("mean")]
occasions1.squ[occasions1.squ$Site == "J2", c("noise100")] <- mean_np100[mean_np100$ID == "34", c("mean")]
# position 2
occasions2.squ[occasions2.squ$Site == "A0", c("noise100")] <- mean_np100[mean_np100$ID == "35", c("mean")]
occasions2.squ[occasions2.squ$Site == "A1", c("noise100")] <- mean_np100[mean_np100$ID == "36", c("mean")]
occasions2.squ[occasions2.squ$Site == "A2", c("noise100")] <- mean_np100[mean_np100$ID == "37", c("mean")]
occasions2.squ[occasions2.squ$Site == "A2/2", c("noise100")] <- mean_np100[mean_np100$ID == "38", c("mean")]
occasions2.squ[occasions2.squ$Site == "B0", c("noise100")] <- mean_np100[mean_np100$ID == "39", c("mean")]
occasions2.squ[occasions2.squ$Site == "C1", c("noise100")] <- mean_np100[mean_np100$ID == "40", c("mean")]
occasions2.squ[occasions2.squ$Site == "C1/2", c("noise100")] <- mean_np100[mean_np100$ID == "41", c("mean")]
occasions2.squ[occasions2.squ$Site == "C0", c("noise100")] <- mean_np100[mean_np100$ID == "42", c("mean")]
occasions2.squ[occasions2.squ$Site == "C2", c("noise100")] <- mean_np100[mean_np100$ID == "43", c("mean")]
occasions2.squ[occasions2.squ$Site == "D1", c("noise100")] <- mean_np100[mean_np100$ID == "44", c("mean")]
occasions2.squ[occasions2.squ$Site == "D1/2", c("noise100")] <- mean_np100[mean_np100$ID == "45", c("mean")]
occasions2.squ[occasions2.squ$Site == "E1/2", c("noise100")] <- mean_np100[mean_np100$ID == "46", c("mean")]
occasions2.squ[occasions2.squ$Site == "E1", c("noise100")] <- mean_np100[mean_np100$ID == "47", c("mean")]
occasions2.squ[occasions2.squ$Site == "F2", c("noise100")] <- mean_np100[mean_np100$ID == "48", c("mean")]
occasions2.squ[occasions2.squ$Site == "G0", c("noise100")] <- mean_np100[mean_np100$ID == "49", c("mean")]
occasions2.squ[occasions2.squ$Site == "G2", c("noise100")] <- mean_np100[mean_np100$ID == "50", c("mean")]
occasions2.squ[occasions2.squ$Site == "E0/2", c("noise100")] <- mean_np100[mean_np100$ID == "51", c("mean")]
occasions2.squ[occasions2.squ$Site == "E0", c("noise100")] <- mean_np100[mean_np100$ID == "52", c("mean")]
occasions2.squ[occasions2.squ$Site == "D2", c("noise100")] <- mean_np100[mean_np100$ID == "53", c("mean")]
occasions2.squ[occasions2.squ$Site == "F0", c("noise100")] <- mean_np100[mean_np100$ID == "54", c("mean")]
occasions2.squ[occasions2.squ$Site == "E2", c("noise100")] <- mean_np100[mean_np100$ID == "55", c("mean")]
occasions2.squ[occasions2.squ$Site == "G1", c("noise100")] <- mean_np100[mean_np100$ID == "56", c("mean")]
occasions2.squ[occasions2.squ$Site == "J1", c("noise100")] <- mean_np100[mean_np100$ID == "57", c("mean")]
occasions2.squ[occasions2.squ$Site == "J0", c("noise100")] <- mean_np100[mean_np100$ID == "58", c("mean")]
occasions2.squ[occasions2.squ$Site == "B2", c("noise100")] <- mean_np100[mean_np100$ID == "59", c("mean")]
occasions2.squ[occasions2.squ$Site == "B2/2", c("noise100")] <- mean_np100[mean_np100$ID == "60", c("mean")]
occasions2.squ[occasions2.squ$Site == "F1", c("noise100")] <- mean_np100[mean_np100$ID == "61", c("mean")]
occasions2.squ[occasions2.squ$Site == "I0", c("noise100")] <- mean_np100[mean_np100$ID == "62", c("mean")]
occasions2.squ[occasions2.squ$Site == "I1", c("noise100")] <- mean_np100[mean_np100$ID == "63", c("mean")]
occasions2.squ[occasions2.squ$Site == "I2", c("noise100")] <- mean_np100[mean_np100$ID == "64", c("mean")]
occasions2.squ[occasions2.squ$Site == "H2", c("noise100")] <- mean_np100[mean_np100$ID == "65", c("mean")]
occasions2.squ[occasions2.squ$Site == "H0", c("noise100")] <- mean_np100[mean_np100$ID == "66", c("mean")]
occasions2.squ[occasions2.squ$Site == "H1", c("noise100")] <- mean_np100[mean_np100$ID == "67", c("mean")]
occasions2.squ[occasions2.squ$Site == "J2", c("noise100")] <- mean_np100[mean_np100$ID == "68", c("mean")]
### BUFFER 400 ###
# position 1
occasions1.squ[occasions1.squ$Site == "A0", c("noise400")] <- mean_np400[mean_np400$ID == "1", c("mean")]
occasions1.squ[occasions1.squ$Site == "A1", c("noise400")] <- mean_np400[mean_np400$ID == "2", c("mean")]
occasions1.squ[occasions1.squ$Site == "A2", c("noise400")] <- mean_np400[mean_np400$ID == "3", c("mean")]
occasions1.squ[occasions1.squ$Site == "A2/2", c("noise400")] <- mean_np400[mean_np400$ID == "4", c("mean")]
occasions1.squ[occasions1.squ$Site == "B0", c("noise400")] <- mean_np400[mean_np400$ID == "5", c("mean")]
occasions1.squ[occasions1.squ$Site == "B2", c("noise400")] <- mean_np400[mean_np400$ID == "6", c("mean")]
occasions1.squ[occasions1.squ$Site == "B2/2", c("noise400")] <- mean_np400[mean_np400$ID == "7", c("mean")]
occasions1.squ[occasions1.squ$Site == "C0", c("noise400")] <- mean_np400[mean_np400$ID == "8", c("mean")]
occasions1.squ[occasions1.squ$Site == "C1", c("noise400")] <- mean_np400[mean_np400$ID == "9", c("mean")]
occasions1.squ[occasions1.squ$Site == "C1/2", c("noise400")] <- mean_np400[mean_np400$ID == "10", c("mean")]
occasions1.squ[occasions1.squ$Site == "C2", c("noise400")] <- mean_np400[mean_np400$ID == "11", c("mean")]
occasions1.squ[occasions1.squ$Site == "D1", c("noise400")] <- mean_np400[mean_np400$ID == "12", c("mean")]
occasions1.squ[occasions1.squ$Site == "D1/2", c("noise400")] <- mean_np400[mean_np400$ID == "13", c("mean")]
occasions1.squ[occasions1.squ$Site == "D2", c("noise400")] <- mean_np400[mean_np400$ID == "14", c("mean")]
occasions1.squ[occasions1.squ$Site == "E0", c("noise400")] <- mean_np400[mean_np400$ID == "15", c("mean")]
occasions1.squ[occasions1.squ$Site == "E0/2", c("noise400")] <- mean_np400[mean_np400$ID == "16", c("mean")]
occasions1.squ[occasions1.squ$Site == "E1", c("noise400")] <- mean_np400[mean_np400$ID == "17", c("mean")]
occasions1.squ[occasions1.squ$Site == "E1/2", c("noise400")] <- mean_np400[mean_np400$ID == "18", c("mean")]
occasions1.squ[occasions1.squ$Site == "E2", c("noise400")] <- mean_np400[mean_np400$ID == "19", c("mean")]
occasions1.squ[occasions1.squ$Site == "F0", c("noise400")] <- mean_np400[mean_np400$ID == "20", c("mean")]
occasions1.squ[occasions1.squ$Site == "F1", c("noise400")] <- mean_np400[mean_np400$ID == "21", c("mean")]
occasions1.squ[occasions1.squ$Site == "F2", c("noise400")] <- mean_np400[mean_np400$ID == "22", c("mean")]
occasions1.squ[occasions1.squ$Site == "G0", c("noise400")] <- mean_np400[mean_np400$ID == "23", c("mean")]
occasions1.squ[occasions1.squ$Site == "G1", c("noise400")] <- mean_np400[mean_np400$ID == "24", c("mean")]
occasions1.squ[occasions1.squ$Site == "G2", c("noise400")] <- mean_np400[mean_np400$ID == "25", c("mean")]
occasions1.squ[occasions1.squ$Site == "H0", c("noise400")] <- mean_np400[mean_np400$ID == "26", c("mean")]
occasions1.squ[occasions1.squ$Site == "H1", c("noise400")] <- mean_np400[mean_np400$ID == "27", c("mean")]
occasions1.squ[occasions1.squ$Site == "H2", c("noise400")] <- mean_np400[mean_np400$ID == "28", c("mean")]
occasions1.squ[occasions1.squ$Site == "I0", c("noise400")] <- mean_np400[mean_np400$ID == "29", c("mean")]
occasions1.squ[occasions1.squ$Site == "I1", c("noise400")] <- mean_np400[mean_np400$ID == "30", c("mean")]
occasions1.squ[occasions1.squ$Site == "I2", c("noise400")] <- mean_np400[mean_np400$ID == "31", c("mean")]
occasions1.squ[occasions1.squ$Site == "J0", c("noise400")] <- mean_np400[mean_np400$ID == "32", c("mean")]
occasions1.squ[occasions1.squ$Site == "J1", c("noise400")] <- mean_np400[mean_np400$ID == "33", c("mean")]
occasions1.squ[occasions1.squ$Site == "J2", c("noise400")] <- mean_np400[mean_np400$ID == "34", c("mean")]
# position 2
occasions2.squ[occasions2.squ$Site == "A0", c("noise400")] <- mean_np400[mean_np400$ID == "35", c("mean")]
occasions2.squ[occasions2.squ$Site == "A1", c("noise400")] <- mean_np400[mean_np400$ID == "36", c("mean")]
occasions2.squ[occasions2.squ$Site == "A2", c("noise400")] <- mean_np400[mean_np400$ID == "37", c("mean")]
occasions2.squ[occasions2.squ$Site == "A2/2", c("noise400")] <- mean_np400[mean_np400$ID == "38", c("mean")]
occasions2.squ[occasions2.squ$Site == "B0", c("noise400")] <- mean_np400[mean_np400$ID == "39", c("mean")]
occasions2.squ[occasions2.squ$Site == "C1", c("noise400")] <- mean_np400[mean_np400$ID == "40", c("mean")]
occasions2.squ[occasions2.squ$Site == "C1/2", c("noise400")] <- mean_np400[mean_np400$ID == "41", c("mean")]
occasions2.squ[occasions2.squ$Site == "C0", c("noise400")] <- mean_np400[mean_np400$ID == "42", c("mean")]
occasions2.squ[occasions2.squ$Site == "C2", c("noise400")] <- mean_np400[mean_np400$ID == "43", c("mean")]
occasions2.squ[occasions2.squ$Site == "D1", c("noise400")] <- mean_np400[mean_np400$ID == "44", c("mean")]
occasions2.squ[occasions2.squ$Site == "D1/2", c("noise400")] <- mean_np400[mean_np400$ID == "45", c("mean")]
occasions2.squ[occasions2.squ$Site == "E1/2", c("noise400")] <- mean_np400[mean_np400$ID == "46", c("mean")]
occasions2.squ[occasions2.squ$Site == "E1", c("noise400")] <- mean_np400[mean_np400$ID == "47", c("mean")]
occasions2.squ[occasions2.squ$Site == "F2", c("noise400")] <- mean_np400[mean_np400$ID == "48", c("mean")]
occasions2.squ[occasions2.squ$Site == "G0", c("noise400")] <- mean_np400[mean_np400$ID == "49", c("mean")]
occasions2.squ[occasions2.squ$Site == "G2", c("noise400")] <- mean_np400[mean_np400$ID == "50", c("mean")]
occasions2.squ[occasions2.squ$Site == "E0/2", c("noise400")] <- mean_np400[mean_np400$ID == "51", c("mean")]
occasions2.squ[occasions2.squ$Site == "E0", c("noise400")] <- mean_np400[mean_np400$ID == "52", c("mean")]
occasions2.squ[occasions2.squ$Site == "D2", c("noise400")] <- mean_np400[mean_np400$ID == "53", c("mean")]
occasions2.squ[occasions2.squ$Site == "F0", c("noise400")] <- mean_np400[mean_np400$ID == "54", c("mean")]
occasions2.squ[occasions2.squ$Site == "E2", c("noise400")] <- mean_np400[mean_np400$ID == "55", c("mean")]
occasions2.squ[occasions2.squ$Site == "G1", c("noise400")] <- mean_np400[mean_np400$ID == "56", c("mean")]
occasions2.squ[occasions2.squ$Site == "J1", c("noise400")] <- mean_np400[mean_np400$ID == "57", c("mean")]
occasions2.squ[occasions2.squ$Site == "J0", c("noise400")] <- mean_np400[mean_np400$ID == "58", c("mean")]
occasions2.squ[occasions2.squ$Site == "B2", c("noise400")] <- mean_np400[mean_np400$ID == "59", c("mean")]
occasions2.squ[occasions2.squ$Site == "B2/2", c("noise400")] <- mean_np400[mean_np400$ID == "60", c("mean")]
occasions2.squ[occasions2.squ$Site == "F1", c("noise400")] <- mean_np400[mean_np400$ID == "61", c("mean")]
occasions2.squ[occasions2.squ$Site == "I0", c("noise400")] <- mean_np400[mean_np400$ID == "62", c("mean")]
occasions2.squ[occasions2.squ$Site == "I1", c("noise400")] <- mean_np400[mean_np400$ID == "63", c("mean")]
occasions2.squ[occasions2.squ$Site == "I2", c("noise400")] <- mean_np400[mean_np400$ID == "64", c("mean")]
occasions2.squ[occasions2.squ$Site == "H2", c("noise400")] <- mean_np400[mean_np400$ID == "65", c("mean")]
occasions2.squ[occasions2.squ$Site == "H0", c("noise400")] <- mean_np400[mean_np400$ID == "66", c("mean")]
occasions2.squ[occasions2.squ$Site == "H1", c("noise400")] <- mean_np400[mean_np400$ID == "67", c("mean")]
occasions2.squ[occasions2.squ$Site == "J2", c("noise400")] <- mean_np400[mean_np400$ID == "68", c("mean")]
head(occasions1.squ)
head(occasions2.squ)
summary(occasions1.squ$noise100)
summary(occasions2.squ$noise100)
summary(occasions1.squ$noise400)
summary(occasions2.squ$noise400)

# final data frame
final.sq.df <- rbind(occasions1.squ, occasions2.squ)

# add site area
final.sq.df$area <- NA
final.sq.df[final.sq.df$Site == "A0", c("area")] <- REM[REM$site_ID == "A0", "area"]
final.sq.df[final.sq.df$Site == "A1", c("area")] <- REM[REM$site_ID == "A1", "area"]
final.sq.df[final.sq.df$Site == "A2", c("area")] <- REM[REM$site_ID == "A2", "area"]
final.sq.df[final.sq.df$Site == "A2/2", c("area")] <- REM[REM$site_ID == "A2", "area"]
final.sq.df[final.sq.df$Site == "B0", c("area")] <- REM[REM$site_ID == "B0", "area"]
final.sq.df[final.sq.df$Site == "B2", c("area")] <- REM[REM$site_ID == "B2", "area"]
final.sq.df[final.sq.df$Site == "B2/2", c("area")] <- REM[REM$site_ID == "B2", "area"]
final.sq.df[final.sq.df$Site == "C0", c("area")] <- REM[REM$site_ID == "C0", "area"]
final.sq.df[final.sq.df$Site == "C1", c("area")] <- REM[REM$site_ID == "C1", "area"]
final.sq.df[final.sq.df$Site == "C1/2", c("area")] <- REM[REM$site_ID == "C1", "area"]
final.sq.df[final.sq.df$Site == "C2", c("area")] <- REM[REM$site_ID == "C2", "area"]
final.sq.df[final.sq.df$Site == "D1", c("area")] <- REM[REM$site_ID == "D1", "area"]
final.sq.df[final.sq.df$Site == "D1/2", c("area")] <- REM[REM$site_ID == "D1", "area"]
final.sq.df[final.sq.df$Site == "D2", c("area")] <- REM[REM$site_ID == "D2", "area"]
final.sq.df[final.sq.df$Site == "E0", c("area")] <- REM[REM$site_ID == "E0", "area"]
final.sq.df[final.sq.df$Site == "E0/2", c("area")] <- REM[REM$site_ID == "E0", "area"]
final.sq.df[final.sq.df$Site == "E1", c("area")] <- REM[REM$site_ID == "E1", "area"]
final.sq.df[final.sq.df$Site == "E1/2", c("area")] <- REM[REM$site_ID == "E1", "area"]
final.sq.df[final.sq.df$Site == "E2", c("area")] <- REM[REM$site_ID == "E2", "area"]
final.sq.df[final.sq.df$Site == "F0", c("area")] <- REM[REM$site_ID == "F0", "area"]
final.sq.df[final.sq.df$Site == "F1", c("area")] <- REM[REM$site_ID == "F1", "area"]
final.sq.df[final.sq.df$Site == "F2", c("area")] <- REM[REM$site_ID == "F2", "area"]
final.sq.df[final.sq.df$Site == "G0", c("area")] <- REM[REM$site_ID == "G0", "area"]
final.sq.df[final.sq.df$Site == "G1", c("area")] <- REM[REM$site_ID == "G1", "area"]
final.sq.df[final.sq.df$Site == "G2", c("area")] <- REM[REM$site_ID == "G2", "area"]
final.sq.df[final.sq.df$Site == "H0", c("area")] <- REM[REM$site_ID == "H0", "area"]
final.sq.df[final.sq.df$Site == "H1", c("area")] <- REM[REM$site_ID == "H1", "area"]
final.sq.df[final.sq.df$Site == "H2", c("area")] <- REM[REM$site_ID == "H2", "area"]
final.sq.df[final.sq.df$Site == "I0", c("area")] <- REM[REM$site_ID == "I", "area"]
final.sq.df[final.sq.df$Site == "I1", c("area")] <- REM[REM$site_ID == "I", "area"]
final.sq.df[final.sq.df$Site == "I2", c("area")] <- REM[REM$site_ID == "I", "area"]
final.sq.df[final.sq.df$Site == "J0", c("area")] <- REM[REM$site_ID == "J", "area"]
final.sq.df[final.sq.df$Site == "J1", c("area")] <- REM[REM$site_ID == "J", "area"]
final.sq.df[final.sq.df$Site == "J2", c("area")] <- REM[REM$site_ID == "J", "area"]

summary(final.sq.df$area)

# export data frame csv
write.csv(final.sq.df, file = "~/GALLANT Technician/Camera Trap Analysis/squirrel_occupancy_df.csv")



### FOR DEER AND FOX ###

# split prop dfs too
df1.prop250 <- subset(df.prop250, df.prop250$placement == "1")
df2.prop250 <- subset(df.prop250, df.prop250$placement == "2")
df1.prop1km <- subset(df.prop1km, df.prop1km$placement == "1")
df2.prop1km <- subset(df.prop1km, df.prop1km$placement == "2")



### DEER ###

# new columns in data frame
# 250m 
occasions.dee$wood250 <- NA
occasions.dee$wet250 <- NA
occasions.dee$urban250 <- NA
occasions.dee$water250 <- NA
occasions.dee$grass250 <- NA
occasions.dee$arable250 <- NA
# 1km
occasions.dee$wood1km <- NA
occasions.dee$wet1km <- NA
occasions.dee$urban1km <- NA
occasions.dee$water1km <- NA
occasions.dee$grass1km <- NA
occasions.dee$arable1km <- NA
# dog relative abundance
occasions.dee$dog_RA <- NA

# if in position 1 - split into positions?
occasions1.dee <- subset(occasions.dee, occasions.dee$Session == "1")
occasions2.dee <- subset(occasions.dee, occasions.dee$Session == "2")
names(occasions.dee)

### 250m BUFFER

# if at site...cbind 6 columns
# define columns 
prop_columns250 <- names(occasions.dee)[11:16]
# add proportions for position 1
occasions1.dee[occasions1.dee$Site == "A0", prop_columns250] <- df1.prop250[df1.prop250 == "A0", prop_columns250]
occasions1.dee[occasions1.dee$Site == "A1", prop_columns250] <- df1.prop250[df1.prop250 == "A1", prop_columns250]
occasions1.dee[occasions1.dee$Site == "A2", prop_columns250] <- df1.prop250[df1.prop250 == "A2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "A2/2", prop_columns250] <- df1.prop250[df1.prop250 == "A2/2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "B0", prop_columns250] <- df1.prop250[df1.prop250 == "B0", prop_columns250]
occasions1.dee[occasions1.dee$Site == "B2", prop_columns250] <- df1.prop250[df1.prop250 == "B2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "B2/2", prop_columns250] <- df1.prop250[df1.prop250 == "B2/2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "C0", prop_columns250] <- df1.prop250[df1.prop250 == "C0", prop_columns250]
occasions1.dee[occasions1.dee$Site == "C1", prop_columns250] <- df1.prop250[df1.prop250 == "C1", prop_columns250]
occasions1.dee[occasions1.dee$Site == "C1/2", prop_columns250] <- df1.prop250[df1.prop250 == "C1/2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "C2", prop_columns250] <- df1.prop250[df1.prop250 == "C2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "D1", prop_columns250] <- df1.prop250[df1.prop250 == "D1", prop_columns250]
occasions1.dee[occasions1.dee$Site == "D1/2", prop_columns250] <- df1.prop250[df1.prop250 == "D1/2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "D2", prop_columns250] <- df1.prop250[df1.prop250 == "D2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "E0", prop_columns250] <- df1.prop250[df1.prop250 == "E0", prop_columns250]
occasions1.dee[occasions1.dee$Site == "E0/2", prop_columns250] <- df1.prop250[df1.prop250 == "E0/2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "E1", prop_columns250] <- df1.prop250[df1.prop250 == "E1", prop_columns250]
occasions1.dee[occasions1.dee$Site == "E1/2", prop_columns250] <- df1.prop250[df1.prop250 == "E1/2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "E2", prop_columns250] <- df1.prop250[df1.prop250 == "E2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "F0", prop_columns250] <- df1.prop250[df1.prop250 == "F0", prop_columns250]
occasions1.dee[occasions1.dee$Site == "F1", prop_columns250] <- df1.prop250[df1.prop250 == "F1", prop_columns250]
occasions1.dee[occasions1.dee$Site == "F2", prop_columns250] <- df1.prop250[df1.prop250 == "F2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "G0", prop_columns250] <- df1.prop250[df1.prop250 == "G0", prop_columns250]
occasions1.dee[occasions1.dee$Site == "G1", prop_columns250] <- df1.prop250[df1.prop250 == "G1", prop_columns250]
occasions1.dee[occasions1.dee$Site == "G2", prop_columns250] <- df1.prop250[df1.prop250 == "G2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "H0", prop_columns250] <- df1.prop250[df1.prop250 == "H0", prop_columns250]
occasions1.dee[occasions1.dee$Site == "H1", prop_columns250] <- df1.prop250[df1.prop250 == "H1", prop_columns250]
occasions1.dee[occasions1.dee$Site == "H2", prop_columns250] <- df1.prop250[df1.prop250 == "H2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "I0", prop_columns250] <- df1.prop250[df1.prop250 == "I0", prop_columns250]
occasions1.dee[occasions1.dee$Site == "I1", prop_columns250] <- df1.prop250[df1.prop250 == "I1", prop_columns250]
occasions1.dee[occasions1.dee$Site == "I2", prop_columns250] <- df1.prop250[df1.prop250 == "I2", prop_columns250]
occasions1.dee[occasions1.dee$Site == "J0", prop_columns250] <- df1.prop250[df1.prop250 == "J0", prop_columns250]
occasions1.dee[occasions1.dee$Site == "J1", prop_columns250] <- df1.prop250[df1.prop250 == "J1", prop_columns250]
occasions1.dee[occasions1.dee$Site == "J2", prop_columns250] <- df1.prop250[df1.prop250 == "J2", prop_columns250]
# add proportions for position 2
occasions2.dee[occasions2.dee$Site == "A0", prop_columns250] <- df2.prop250[df2.prop250 == "A0", prop_columns250]
occasions2.dee[occasions2.dee$Site == "A1", prop_columns250] <- df2.prop250[df2.prop250 == "A1", prop_columns250]
occasions2.dee[occasions2.dee$Site == "A2", prop_columns250] <- df2.prop250[df2.prop250 == "A2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "A2/2", prop_columns250] <- df2.prop250[df2.prop250 == "A2/2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "B0", prop_columns250] <- df2.prop250[df2.prop250 == "B0", prop_columns250]
occasions2.dee[occasions2.dee$Site == "B2", prop_columns250] <- df2.prop250[df2.prop250 == "B2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "B2/2", prop_columns250] <- df2.prop250[df2.prop250 == "B2/2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "C0", prop_columns250] <- df2.prop250[df2.prop250 == "C0", prop_columns250]
occasions2.dee[occasions2.dee$Site == "C1", prop_columns250] <- df2.prop250[df2.prop250 == "C1", prop_columns250]
occasions2.dee[occasions2.dee$Site == "C1/2", prop_columns250] <- df2.prop250[df2.prop250 == "C1/2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "C2", prop_columns250] <- df2.prop250[df2.prop250 == "C2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "D1", prop_columns250] <- df2.prop250[df2.prop250 == "D1", prop_columns250]
occasions2.dee[occasions2.dee$Site == "D1/2", prop_columns250] <- df2.prop250[df2.prop250 == "D1/2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "D2", prop_columns250] <- df2.prop250[df2.prop250 == "D2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "E0", prop_columns250] <- df2.prop250[df2.prop250 == "E0", prop_columns250]
occasions2.dee[occasions2.dee$Site == "E0/2", prop_columns250] <- df2.prop250[df2.prop250 == "E0/2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "E1", prop_columns250] <- df2.prop250[df2.prop250 == "E1", prop_columns250]
occasions2.dee[occasions2.dee$Site == "E1/2", prop_columns250] <- df2.prop250[df2.prop250 == "E1/2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "E2", prop_columns250] <- df2.prop250[df2.prop250 == "E2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "F0", prop_columns250] <- df2.prop250[df2.prop250 == "F0", prop_columns250]
occasions2.dee[occasions2.dee$Site == "F1", prop_columns250] <- df2.prop250[df2.prop250 == "F1", prop_columns250]
occasions2.dee[occasions2.dee$Site == "F2", prop_columns250] <- df2.prop250[df2.prop250 == "F2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "G0", prop_columns250] <- df2.prop250[df2.prop250 == "G0", prop_columns250]
occasions2.dee[occasions2.dee$Site == "G1", prop_columns250] <- df2.prop250[df2.prop250 == "G1", prop_columns250]
occasions2.dee[occasions2.dee$Site == "G2", prop_columns250] <- df2.prop250[df2.prop250 == "G2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "H0", prop_columns250] <- df2.prop250[df2.prop250 == "H0", prop_columns250]
occasions2.dee[occasions2.dee$Site == "H1", prop_columns250] <- df2.prop250[df2.prop250 == "H1", prop_columns250]
occasions2.dee[occasions2.dee$Site == "H2", prop_columns250] <- df2.prop250[df2.prop250 == "H2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "I0", prop_columns250] <- df2.prop250[df2.prop250 == "I0", prop_columns250]
occasions2.dee[occasions2.dee$Site == "I1", prop_columns250] <- df2.prop250[df2.prop250 == "I1", prop_columns250]
occasions2.dee[occasions2.dee$Site == "I2", prop_columns250] <- df2.prop250[df2.prop250 == "I2", prop_columns250]
occasions2.dee[occasions2.dee$Site == "J0", prop_columns250] <- df2.prop250[df2.prop250 == "J0", prop_columns250]
occasions2.dee[occasions2.dee$Site == "J1", prop_columns250] <- df2.prop250[df2.prop250 == "J1", prop_columns250]
occasions2.dee[occasions2.dee$Site == "J2", prop_columns250] <- df2.prop250[df2.prop250 == "J2", prop_columns250]

head(occasions1.dee, 10)
head(occasions2.dee, 10)
summary(occasions1.dee$wood250)
summary(occasions1.dee$wet250)
summary(occasions1.dee$water250)
summary(occasions1.dee$arable250)
summary(occasions1.dee$grass250)
summary(occasions1.dee$urban250)
summary(occasions2.dee$wood250)
summary(occasions2.dee$wet250)
summary(occasions2.dee$water250)
summary(occasions2.dee$arable250)
summary(occasions2.dee$grass250)
summary(occasions2.dee$urban250)

### 1km BUFFER

# if at site...cbind 6 columns
# define columns 
prop_columns1km <- names(occasions.dee)[17:22]
# add proportions for position 1
occasions1.dee[occasions1.dee$Site == "A0", prop_columns1km] <- df1.prop1km[df1.prop1km == "A0", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "A1", prop_columns1km] <- df1.prop1km[df1.prop1km == "A1", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "A2", prop_columns1km] <- df1.prop1km[df1.prop1km == "A2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "A2/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "A2/2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "B0", prop_columns1km] <- df1.prop1km[df1.prop1km == "B0", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "B2", prop_columns1km] <- df1.prop1km[df1.prop1km == "B2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "B2/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "B2/2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "C0", prop_columns1km] <- df1.prop1km[df1.prop1km == "C0", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "C1", prop_columns1km] <- df1.prop1km[df1.prop1km == "C1", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "C1/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "C1/2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "C2", prop_columns1km] <- df1.prop1km[df1.prop1km == "C2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "D1", prop_columns1km] <- df1.prop1km[df1.prop1km == "D1", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "D1/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "D1/2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "D2", prop_columns1km] <- df1.prop1km[df1.prop1km == "D2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "E0", prop_columns1km] <- df1.prop1km[df1.prop1km == "E0", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "E0/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "E0/2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "E1", prop_columns1km] <- df1.prop1km[df1.prop1km == "E1", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "E1/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "E1/2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "E2", prop_columns1km] <- df1.prop1km[df1.prop1km == "E2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "F0", prop_columns1km] <- df1.prop1km[df1.prop1km == "F0", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "F1", prop_columns1km] <- df1.prop1km[df1.prop1km == "F1", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "F2", prop_columns1km] <- df1.prop1km[df1.prop1km == "F2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "G0", prop_columns1km] <- df1.prop1km[df1.prop1km == "G0", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "G1", prop_columns1km] <- df1.prop1km[df1.prop1km == "G1", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "G2", prop_columns1km] <- df1.prop1km[df1.prop1km == "G2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "H0", prop_columns1km] <- df1.prop1km[df1.prop1km == "H0", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "H1", prop_columns1km] <- df1.prop1km[df1.prop1km == "H1", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "H2", prop_columns1km] <- df1.prop1km[df1.prop1km == "H2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "I0", prop_columns1km] <- df1.prop1km[df1.prop1km == "I0", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "I1", prop_columns1km] <- df1.prop1km[df1.prop1km == "I1", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "I2", prop_columns1km] <- df1.prop1km[df1.prop1km == "I2", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "J0", prop_columns1km] <- df1.prop1km[df1.prop1km == "J0", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "J1", prop_columns1km] <- df1.prop1km[df1.prop1km == "J1", prop_columns1km]
occasions1.dee[occasions1.dee$Site == "J2", prop_columns1km] <- df1.prop1km[df1.prop1km == "J2", prop_columns1km]
# add proportions for position 2
occasions2.dee[occasions2.dee$Site == "A0", prop_columns1km] <- df2.prop1km[df2.prop1km == "A0", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "A1", prop_columns1km] <- df2.prop1km[df2.prop1km == "A1", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "A2", prop_columns1km] <- df2.prop1km[df2.prop1km == "A2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "A2/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "A2/2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "B0", prop_columns1km] <- df2.prop1km[df2.prop1km == "B0", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "B2", prop_columns1km] <- df2.prop1km[df2.prop1km == "B2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "B2/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "B2/2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "C0", prop_columns1km] <- df2.prop1km[df2.prop1km == "C0", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "C1", prop_columns1km] <- df2.prop1km[df2.prop1km == "C1", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "C1/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "C1/2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "C2", prop_columns1km] <- df2.prop1km[df2.prop1km == "C2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "D1", prop_columns1km] <- df2.prop1km[df2.prop1km == "D1", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "D1/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "D1/2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "D2", prop_columns1km] <- df2.prop1km[df2.prop1km == "D2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "E0", prop_columns1km] <- df2.prop1km[df2.prop1km == "E0", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "E0/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "E0/2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "E1", prop_columns1km] <- df2.prop1km[df2.prop1km == "E1", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "E1/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "E1/2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "E2", prop_columns1km] <- df2.prop1km[df2.prop1km == "E2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "F0", prop_columns1km] <- df2.prop1km[df2.prop1km == "F0", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "F1", prop_columns1km] <- df2.prop1km[df2.prop1km == "F1", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "F2", prop_columns1km] <- df2.prop1km[df2.prop1km == "F2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "G0", prop_columns1km] <- df2.prop1km[df2.prop1km == "G0", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "G1", prop_columns1km] <- df2.prop1km[df2.prop1km == "G1", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "G2", prop_columns1km] <- df2.prop1km[df2.prop1km == "G2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "H0", prop_columns1km] <- df2.prop1km[df2.prop1km == "H0", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "H1", prop_columns1km] <- df2.prop1km[df2.prop1km == "H1", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "H2", prop_columns1km] <- df2.prop1km[df2.prop1km == "H2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "I0", prop_columns1km] <- df2.prop1km[df2.prop1km == "I0", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "I1", prop_columns1km] <- df2.prop1km[df2.prop1km == "I1", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "I2", prop_columns1km] <- df2.prop1km[df2.prop1km == "I2", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "J0", prop_columns1km] <- df2.prop1km[df2.prop1km == "J0", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "J1", prop_columns1km] <- df2.prop1km[df2.prop1km == "J1", prop_columns1km]
occasions2.dee[occasions2.dee$Site == "J2", prop_columns1km] <- df2.prop1km[df2.prop1km == "J2", prop_columns1km]

head(occasions1.dee, 10)
head(occasions2.dee, 10)
summary(occasions1.dee$wood1km)
summary(occasions1.dee$wet1km)
summary(occasions1.dee$water1km)
summary(occasions1.dee$arable1km)
summary(occasions1.dee$grass1km)
summary(occasions1.dee$urban1km)
summary(occasions2.dee$wood1km)
summary(occasions2.dee$wet1km)
summary(occasions2.dee$water1km)
summary(occasions2.dee$arable1km)
summary(occasions2.dee$grass1km)
summary(occasions2.dee$urban1km)

# add dog relative abundance (per camera)
# add for first placement 1
occasions1.dee[occasions1.dee$Site == "A0", c("dog_RA")] <- dog_RA[dog_RA$site == "A0", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "A1", c("dog_RA")] <- dog_RA[dog_RA$site == "A1", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "A2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "A2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2/2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "B0", c("dog_RA")] <- dog_RA[dog_RA$site == "B0", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "B2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "B2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2/2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "C0", c("dog_RA")] <- dog_RA[dog_RA$site == "C0", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "C1", c("dog_RA")] <- dog_RA[dog_RA$site == "C1", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "C1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "C1/2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "C2", c("dog_RA")] <- dog_RA[dog_RA$site == "C2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "D1", c("dog_RA")] <- dog_RA[dog_RA$site == "D1", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "D1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "D1/2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "D2", c("dog_RA")] <- dog_RA[dog_RA$site == "D2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "E0", c("dog_RA")] <- dog_RA[dog_RA$site == "E0", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "E0/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E0/2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "E1", c("dog_RA")] <- dog_RA[dog_RA$site == "E1", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "E1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E1/2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "E2", c("dog_RA")] <- dog_RA[dog_RA$site == "E2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "F0", c("dog_RA")] <- dog_RA[dog_RA$site == "F0", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "F1", c("dog_RA")] <- dog_RA[dog_RA$site == "F1", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "F2", c("dog_RA")] <- dog_RA[dog_RA$site == "F2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "G0", c("dog_RA")] <- dog_RA[dog_RA$site == "G0", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "G1", c("dog_RA")] <- dog_RA[dog_RA$site == "G1", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "G2", c("dog_RA")] <- dog_RA[dog_RA$site == "G2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "H0", c("dog_RA")] <- dog_RA[dog_RA$site == "H0", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "H1", c("dog_RA")] <- dog_RA[dog_RA$site == "H1", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "H2", c("dog_RA")] <- dog_RA[dog_RA$site == "H2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "I0", c("dog_RA")] <- dog_RA[dog_RA$site == "I0", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "I1", c("dog_RA")] <- dog_RA[dog_RA$site == "I1", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "I2", c("dog_RA")] <- dog_RA[dog_RA$site == "I2", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "J0", c("dog_RA")] <- dog_RA[dog_RA$site == "J0", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "J1", c("dog_RA")] <- dog_RA[dog_RA$site == "J1", c("loc.mean1")]
occasions1.dee[occasions1.dee$Site == "J2", c("dog_RA")] <- dog_RA[dog_RA$site == "J2", c("loc.mean1")]
# add for first placement 2
occasions2.dee[occasions2.dee$Site == "A0", c("dog_RA")] <- dog_RA[dog_RA$site == "A0", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "A1", c("dog_RA")] <- dog_RA[dog_RA$site == "A1", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "A2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "A2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2/2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "B0", c("dog_RA")] <- dog_RA[dog_RA$site == "B0", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "B2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "B2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2/2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "C0", c("dog_RA")] <- dog_RA[dog_RA$site == "C0", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "C1", c("dog_RA")] <- dog_RA[dog_RA$site == "C1", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "C1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "C1/2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "C2", c("dog_RA")] <- dog_RA[dog_RA$site == "C2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "D1", c("dog_RA")] <- dog_RA[dog_RA$site == "D1", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "D1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "D1/2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "D2", c("dog_RA")] <- dog_RA[dog_RA$site == "D2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "E0", c("dog_RA")] <- dog_RA[dog_RA$site == "E0", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "E0/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E0/2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "E1", c("dog_RA")] <- dog_RA[dog_RA$site == "E1", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "E1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E1/2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "E2", c("dog_RA")] <- dog_RA[dog_RA$site == "E2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "F0", c("dog_RA")] <- dog_RA[dog_RA$site == "F0", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "F1", c("dog_RA")] <- dog_RA[dog_RA$site == "F1", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "F2", c("dog_RA")] <- dog_RA[dog_RA$site == "F2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "G0", c("dog_RA")] <- dog_RA[dog_RA$site == "G0", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "G1", c("dog_RA")] <- dog_RA[dog_RA$site == "G1", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "G2", c("dog_RA")] <- dog_RA[dog_RA$site == "G2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "H0", c("dog_RA")] <- dog_RA[dog_RA$site == "H0", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "H1", c("dog_RA")] <- dog_RA[dog_RA$site == "H1", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "H2", c("dog_RA")] <- dog_RA[dog_RA$site == "H2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "I0", c("dog_RA")] <- dog_RA[dog_RA$site == "I0", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "I1", c("dog_RA")] <- dog_RA[dog_RA$site == "I1", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "I2", c("dog_RA")] <- dog_RA[dog_RA$site == "I2", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "J0", c("dog_RA")] <- dog_RA[dog_RA$site == "J0", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "J1", c("dog_RA")] <- dog_RA[dog_RA$site == "J1", c("loc.mean2")]
occasions2.dee[occasions2.dee$Site == "J2", c("dog_RA")] <- dog_RA[dog_RA$site == "J2", c("loc.mean2")]

head(occasions1.dee, 10)
head(occasions2.dee, 10)
summary(as.numeric(occasions1.dee$dog_RA))
summary(occasions2.dee$dog_RA)

# add light pollution per site
### BUFFER 250 ###
# position 1
occasions1.dee[occasions1.dee$Site == "A0", c("light250")] <- mean_lp250[mean_lp250$ID == "1", c("mean")]
occasions1.dee[occasions1.dee$Site == "A1", c("light250")] <- mean_lp250[mean_lp250$ID == "2", c("mean")]
occasions1.dee[occasions1.dee$Site == "A2", c("light250")] <- mean_lp250[mean_lp250$ID == "3", c("mean")]
occasions1.dee[occasions1.dee$Site == "A2/2", c("light250")] <- mean_lp250[mean_lp250$ID == "4", c("mean")]
occasions1.dee[occasions1.dee$Site == "B0", c("light250")] <- mean_lp250[mean_lp250$ID == "5", c("mean")]
occasions1.dee[occasions1.dee$Site == "B2", c("light250")] <- mean_lp250[mean_lp250$ID == "6", c("mean")]
occasions1.dee[occasions1.dee$Site == "B2/2", c("light250")] <- mean_lp250[mean_lp250$ID == "7", c("mean")]
occasions1.dee[occasions1.dee$Site == "C0", c("light250")] <- mean_lp250[mean_lp250$ID == "8", c("mean")]
occasions1.dee[occasions1.dee$Site == "C1", c("light250")] <- mean_lp250[mean_lp250$ID == "9", c("mean")]
occasions1.dee[occasions1.dee$Site == "C1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "10", c("mean")]
occasions1.dee[occasions1.dee$Site == "C2", c("light250")] <- mean_lp250[mean_lp250$ID == "11", c("mean")]
occasions1.dee[occasions1.dee$Site == "D1", c("light250")] <- mean_lp250[mean_lp250$ID == "12", c("mean")]
occasions1.dee[occasions1.dee$Site == "D1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "13", c("mean")]
occasions1.dee[occasions1.dee$Site == "D2", c("light250")] <- mean_lp250[mean_lp250$ID == "14", c("mean")]
occasions1.dee[occasions1.dee$Site == "E0", c("light250")] <- mean_lp250[mean_lp250$ID == "15", c("mean")]
occasions1.dee[occasions1.dee$Site == "E0/2", c("light250")] <- mean_lp250[mean_lp250$ID == "16", c("mean")]
occasions1.dee[occasions1.dee$Site == "E1", c("light250")] <- mean_lp250[mean_lp250$ID == "17", c("mean")]
occasions1.dee[occasions1.dee$Site == "E1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "18", c("mean")]
occasions1.dee[occasions1.dee$Site == "E2", c("light250")] <- mean_lp250[mean_lp250$ID == "19", c("mean")]
occasions1.dee[occasions1.dee$Site == "F0", c("light250")] <- mean_lp250[mean_lp250$ID == "20", c("mean")]
occasions1.dee[occasions1.dee$Site == "F1", c("light250")] <- mean_lp250[mean_lp250$ID == "21", c("mean")]
occasions1.dee[occasions1.dee$Site == "F2", c("light250")] <- mean_lp250[mean_lp250$ID == "22", c("mean")]
occasions1.dee[occasions1.dee$Site == "G0", c("light250")] <- mean_lp250[mean_lp250$ID == "23", c("mean")]
occasions1.dee[occasions1.dee$Site == "G1", c("light250")] <- mean_lp250[mean_lp250$ID == "24", c("mean")]
occasions1.dee[occasions1.dee$Site == "G2", c("light250")] <- mean_lp250[mean_lp250$ID == "25", c("mean")]
occasions1.dee[occasions1.dee$Site == "H0", c("light250")] <- mean_lp250[mean_lp250$ID == "26", c("mean")]
occasions1.dee[occasions1.dee$Site == "H1", c("light250")] <- mean_lp250[mean_lp250$ID == "27", c("mean")]
occasions1.dee[occasions1.dee$Site == "H2", c("light250")] <- mean_lp250[mean_lp250$ID == "28", c("mean")]
occasions1.dee[occasions1.dee$Site == "I0", c("light250")] <- mean_lp250[mean_lp250$ID == "29", c("mean")]
occasions1.dee[occasions1.dee$Site == "I1", c("light250")] <- mean_lp250[mean_lp250$ID == "30", c("mean")]
occasions1.dee[occasions1.dee$Site == "I2", c("light250")] <- mean_lp250[mean_lp250$ID == "31", c("mean")]
occasions1.dee[occasions1.dee$Site == "J0", c("light250")] <- mean_lp250[mean_lp250$ID == "32", c("mean")]
occasions1.dee[occasions1.dee$Site == "J1", c("light250")] <- mean_lp250[mean_lp250$ID == "33", c("mean")]
occasions1.dee[occasions1.dee$Site == "J2", c("light250")] <- mean_lp250[mean_lp250$ID == "34", c("mean")]
# position 2
occasions2.dee[occasions2.dee$Site == "A0", c("light250")] <- mean_lp250[mean_lp250$ID == "35", c("mean")]
occasions2.dee[occasions2.dee$Site == "A1", c("light250")] <- mean_lp250[mean_lp250$ID == "36", c("mean")]
occasions2.dee[occasions2.dee$Site == "A2", c("light250")] <- mean_lp250[mean_lp250$ID == "37", c("mean")]
occasions2.dee[occasions2.dee$Site == "A2/2", c("light250")] <- mean_lp250[mean_lp250$ID == "38", c("mean")]
occasions2.dee[occasions2.dee$Site == "B0", c("light250")] <- mean_lp250[mean_lp250$ID == "39", c("mean")]
occasions2.dee[occasions2.dee$Site == "C1", c("light250")] <- mean_lp250[mean_lp250$ID == "40", c("mean")]
occasions2.dee[occasions2.dee$Site == "C1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "41", c("mean")]
occasions2.dee[occasions2.dee$Site == "C0", c("light250")] <- mean_lp250[mean_lp250$ID == "42", c("mean")]
occasions2.dee[occasions2.dee$Site == "C2", c("light250")] <- mean_lp250[mean_lp250$ID == "43", c("mean")]
occasions2.dee[occasions2.dee$Site == "D1", c("light250")] <- mean_lp250[mean_lp250$ID == "44", c("mean")]
occasions2.dee[occasions2.dee$Site == "D1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "45", c("mean")]
occasions2.dee[occasions2.dee$Site == "E1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "46", c("mean")]
occasions2.dee[occasions2.dee$Site == "E1", c("light250")] <- mean_lp250[mean_lp250$ID == "47", c("mean")]
occasions2.dee[occasions2.dee$Site == "F2", c("light250")] <- mean_lp250[mean_lp250$ID == "48", c("mean")]
occasions2.dee[occasions2.dee$Site == "G0", c("light250")] <- mean_lp250[mean_lp250$ID == "49", c("mean")]
occasions2.dee[occasions2.dee$Site == "G2", c("light250")] <- mean_lp250[mean_lp250$ID == "50", c("mean")]
occasions2.dee[occasions2.dee$Site == "E0/2", c("light250")] <- mean_lp250[mean_lp250$ID == "51", c("mean")]
occasions2.dee[occasions2.dee$Site == "E0", c("light250")] <- mean_lp250[mean_lp250$ID == "52", c("mean")]
occasions2.dee[occasions2.dee$Site == "D2", c("light250")] <- mean_lp250[mean_lp250$ID == "53", c("mean")]
occasions2.dee[occasions2.dee$Site == "F0", c("light250")] <- mean_lp250[mean_lp250$ID == "54", c("mean")]
occasions2.dee[occasions2.dee$Site == "E2", c("light250")] <- mean_lp250[mean_lp250$ID == "55", c("mean")]
occasions2.dee[occasions2.dee$Site == "G1", c("light250")] <- mean_lp250[mean_lp250$ID == "56", c("mean")]
occasions2.dee[occasions2.dee$Site == "J1", c("light250")] <- mean_lp250[mean_lp250$ID == "57", c("mean")]
occasions2.dee[occasions2.dee$Site == "J0", c("light250")] <- mean_lp250[mean_lp250$ID == "58", c("mean")]
occasions2.dee[occasions2.dee$Site == "B2", c("light250")] <- mean_lp250[mean_lp250$ID == "59", c("mean")]
occasions2.dee[occasions2.dee$Site == "B2/2", c("light250")] <- mean_lp250[mean_lp250$ID == "60", c("mean")]
occasions2.dee[occasions2.dee$Site == "F1", c("light250")] <- mean_lp250[mean_lp250$ID == "61", c("mean")]
occasions2.dee[occasions2.dee$Site == "I0", c("light250")] <- mean_lp250[mean_lp250$ID == "62", c("mean")]
occasions2.dee[occasions2.dee$Site == "I1", c("light250")] <- mean_lp250[mean_lp250$ID == "63", c("mean")]
occasions2.dee[occasions2.dee$Site == "I2", c("light250")] <- mean_lp250[mean_lp250$ID == "64", c("mean")]
occasions2.dee[occasions2.dee$Site == "H2", c("light250")] <- mean_lp250[mean_lp250$ID == "65", c("mean")]
occasions2.dee[occasions2.dee$Site == "H0", c("light250")] <- mean_lp250[mean_lp250$ID == "66", c("mean")]
occasions2.dee[occasions2.dee$Site == "H1", c("light250")] <- mean_lp250[mean_lp250$ID == "67", c("mean")]
occasions2.dee[occasions2.dee$Site == "J2", c("light250")] <- mean_lp250[mean_lp250$ID == "68", c("mean")]
### BUFFER 1km ###
# position 1
occasions1.dee[occasions1.dee$Site == "A0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "1", c("mean")]
occasions1.dee[occasions1.dee$Site == "A1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "2", c("mean")]
occasions1.dee[occasions1.dee$Site == "A2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "3", c("mean")]
occasions1.dee[occasions1.dee$Site == "A2/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "4", c("mean")]
occasions1.dee[occasions1.dee$Site == "B0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "5", c("mean")]
occasions1.dee[occasions1.dee$Site == "B2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "6", c("mean")]
occasions1.dee[occasions1.dee$Site == "B2/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "7", c("mean")]
occasions1.dee[occasions1.dee$Site == "C0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "8", c("mean")]
occasions1.dee[occasions1.dee$Site == "C1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "9", c("mean")]
occasions1.dee[occasions1.dee$Site == "C1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "10", c("mean")]
occasions1.dee[occasions1.dee$Site == "C2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "11", c("mean")]
occasions1.dee[occasions1.dee$Site == "D1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "12", c("mean")]
occasions1.dee[occasions1.dee$Site == "D1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "13", c("mean")]
occasions1.dee[occasions1.dee$Site == "D2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "14", c("mean")]
occasions1.dee[occasions1.dee$Site == "E0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "15", c("mean")]
occasions1.dee[occasions1.dee$Site == "E0/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "16", c("mean")]
occasions1.dee[occasions1.dee$Site == "E1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "17", c("mean")]
occasions1.dee[occasions1.dee$Site == "E1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "18", c("mean")]
occasions1.dee[occasions1.dee$Site == "E2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "19", c("mean")]
occasions1.dee[occasions1.dee$Site == "F0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "20", c("mean")]
occasions1.dee[occasions1.dee$Site == "F1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "21", c("mean")]
occasions1.dee[occasions1.dee$Site == "F2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "22", c("mean")]
occasions1.dee[occasions1.dee$Site == "G0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "23", c("mean")]
occasions1.dee[occasions1.dee$Site == "G1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "24", c("mean")]
occasions1.dee[occasions1.dee$Site == "G2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "25", c("mean")]
occasions1.dee[occasions1.dee$Site == "H0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "26", c("mean")]
occasions1.dee[occasions1.dee$Site == "H1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "27", c("mean")]
occasions1.dee[occasions1.dee$Site == "H2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "28", c("mean")]
occasions1.dee[occasions1.dee$Site == "I0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "29", c("mean")]
occasions1.dee[occasions1.dee$Site == "I1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "30", c("mean")]
occasions1.dee[occasions1.dee$Site == "I2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "31", c("mean")]
occasions1.dee[occasions1.dee$Site == "J0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "32", c("mean")]
occasions1.dee[occasions1.dee$Site == "J1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "33", c("mean")]
occasions1.dee[occasions1.dee$Site == "J2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "34", c("mean")]
# position 2
occasions2.dee[occasions2.dee$Site == "A0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "35", c("mean")]
occasions2.dee[occasions2.dee$Site == "A1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "36", c("mean")]
occasions2.dee[occasions2.dee$Site == "A2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "37", c("mean")]
occasions2.dee[occasions2.dee$Site == "A2/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "38", c("mean")]
occasions2.dee[occasions2.dee$Site == "B0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "39", c("mean")]
occasions2.dee[occasions2.dee$Site == "C1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "40", c("mean")]
occasions2.dee[occasions2.dee$Site == "C1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "41", c("mean")]
occasions2.dee[occasions2.dee$Site == "C0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "42", c("mean")]
occasions2.dee[occasions2.dee$Site == "C2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "43", c("mean")]
occasions2.dee[occasions2.dee$Site == "D1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "44", c("mean")]
occasions2.dee[occasions2.dee$Site == "D1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "45", c("mean")]
occasions2.dee[occasions2.dee$Site == "E1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "46", c("mean")]
occasions2.dee[occasions2.dee$Site == "E1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "47", c("mean")]
occasions2.dee[occasions2.dee$Site == "F2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "48", c("mean")]
occasions2.dee[occasions2.dee$Site == "G0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "49", c("mean")]
occasions2.dee[occasions2.dee$Site == "G2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "50", c("mean")]
occasions2.dee[occasions2.dee$Site == "E0/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "51", c("mean")]
occasions2.dee[occasions2.dee$Site == "E0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "52", c("mean")]
occasions2.dee[occasions2.dee$Site == "D2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "53", c("mean")]
occasions2.dee[occasions2.dee$Site == "F0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "54", c("mean")]
occasions2.dee[occasions2.dee$Site == "E2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "55", c("mean")]
occasions2.dee[occasions2.dee$Site == "G1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "56", c("mean")]
occasions2.dee[occasions2.dee$Site == "J1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "57", c("mean")]
occasions2.dee[occasions2.dee$Site == "J0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "58", c("mean")]
occasions2.dee[occasions2.dee$Site == "B2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "59", c("mean")]
occasions2.dee[occasions2.dee$Site == "B2/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "60", c("mean")]
occasions2.dee[occasions2.dee$Site == "F1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "61", c("mean")]
occasions2.dee[occasions2.dee$Site == "I0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "62", c("mean")]
occasions2.dee[occasions2.dee$Site == "I1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "63", c("mean")]
occasions2.dee[occasions2.dee$Site == "I2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "64", c("mean")]
occasions2.dee[occasions2.dee$Site == "H2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "65", c("mean")]
occasions2.dee[occasions2.dee$Site == "H0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "66", c("mean")]
occasions2.dee[occasions2.dee$Site == "H1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "67", c("mean")]
occasions2.dee[occasions2.dee$Site == "J2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "68", c("mean")]
head(occasions1.dee)
head(occasions2.dee)
summary(occasions1.dee$light250)
summary(occasions2.dee$light250)
summary(occasions1.dee$light1km)
summary(occasions2.dee$light1km)



# add noise pollution per site
### BUFFER 250 ###
# position 1
occasions1.dee[occasions1.dee$Site == "A0", c("noise250")] <- mean_np250[mean_np250$ID == "1", c("mean")]
occasions1.dee[occasions1.dee$Site == "A1", c("noise250")] <- mean_np250[mean_np250$ID == "2", c("mean")]
occasions1.dee[occasions1.dee$Site == "A2", c("noise250")] <- mean_np250[mean_np250$ID == "3", c("mean")]
occasions1.dee[occasions1.dee$Site == "A2/2", c("noise250")] <- mean_np250[mean_np250$ID == "4", c("mean")]
occasions1.dee[occasions1.dee$Site == "B0", c("noise250")] <- mean_np250[mean_np250$ID == "5", c("mean")]
occasions1.dee[occasions1.dee$Site == "B2", c("noise250")] <- mean_np250[mean_np250$ID == "6", c("mean")]
occasions1.dee[occasions1.dee$Site == "B2/2", c("noise250")] <- mean_np250[mean_np250$ID == "7", c("mean")]
occasions1.dee[occasions1.dee$Site == "C0", c("noise250")] <- mean_np250[mean_np250$ID == "8", c("mean")]
occasions1.dee[occasions1.dee$Site == "C1", c("noise250")] <- mean_np250[mean_np250$ID == "9", c("mean")]
occasions1.dee[occasions1.dee$Site == "C1/2", c("noise250")] <- mean_np250[mean_np250$ID == "10", c("mean")]
occasions1.dee[occasions1.dee$Site == "C2", c("noise250")] <- mean_np250[mean_np250$ID == "11", c("mean")]
occasions1.dee[occasions1.dee$Site == "D1", c("noise250")] <- mean_np250[mean_np250$ID == "12", c("mean")]
occasions1.dee[occasions1.dee$Site == "D1/2", c("noise250")] <- mean_np250[mean_np250$ID == "13", c("mean")]
occasions1.dee[occasions1.dee$Site == "D2", c("noise250")] <- mean_np250[mean_np250$ID == "14", c("mean")]
occasions1.dee[occasions1.dee$Site == "E0", c("noise250")] <- mean_np250[mean_np250$ID == "15", c("mean")]
occasions1.dee[occasions1.dee$Site == "E0/2", c("noise250")] <- mean_np250[mean_np250$ID == "16", c("mean")]
occasions1.dee[occasions1.dee$Site == "E1", c("noise250")] <- mean_np250[mean_np250$ID == "17", c("mean")]
occasions1.dee[occasions1.dee$Site == "E1/2", c("noise250")] <- mean_np250[mean_np250$ID == "18", c("mean")]
occasions1.dee[occasions1.dee$Site == "E2", c("noise250")] <- mean_np250[mean_np250$ID == "19", c("mean")]
occasions1.dee[occasions1.dee$Site == "F0", c("noise250")] <- mean_np250[mean_np250$ID == "20", c("mean")]
occasions1.dee[occasions1.dee$Site == "F1", c("noise250")] <- mean_np250[mean_np250$ID == "21", c("mean")]
occasions1.dee[occasions1.dee$Site == "F2", c("noise250")] <- mean_np250[mean_np250$ID == "22", c("mean")]
occasions1.dee[occasions1.dee$Site == "G0", c("noise250")] <- mean_np250[mean_np250$ID == "23", c("mean")]
occasions1.dee[occasions1.dee$Site == "G1", c("noise250")] <- mean_np250[mean_np250$ID == "24", c("mean")]
occasions1.dee[occasions1.dee$Site == "G2", c("noise250")] <- mean_np250[mean_np250$ID == "25", c("mean")]
occasions1.dee[occasions1.dee$Site == "H0", c("noise250")] <- mean_np250[mean_np250$ID == "26", c("mean")]
occasions1.dee[occasions1.dee$Site == "H1", c("noise250")] <- mean_np250[mean_np250$ID == "27", c("mean")]
occasions1.dee[occasions1.dee$Site == "H2", c("noise250")] <- mean_np250[mean_np250$ID == "28", c("mean")]
occasions1.dee[occasions1.dee$Site == "I0", c("noise250")] <- mean_np250[mean_np250$ID == "29", c("mean")]
occasions1.dee[occasions1.dee$Site == "I1", c("noise250")] <- mean_np250[mean_np250$ID == "30", c("mean")]
occasions1.dee[occasions1.dee$Site == "I2", c("noise250")] <- mean_np250[mean_np250$ID == "31", c("mean")]
occasions1.dee[occasions1.dee$Site == "J0", c("noise250")] <- mean_np250[mean_np250$ID == "32", c("mean")]
occasions1.dee[occasions1.dee$Site == "J1", c("noise250")] <- mean_np250[mean_np250$ID == "33", c("mean")]
occasions1.dee[occasions1.dee$Site == "J2", c("noise250")] <- mean_np250[mean_np250$ID == "34", c("mean")]
# position 2
occasions2.dee[occasions2.dee$Site == "A0", c("noise250")] <- mean_np250[mean_np250$ID == "35", c("mean")]
occasions2.dee[occasions2.dee$Site == "A1", c("noise250")] <- mean_np250[mean_np250$ID == "36", c("mean")]
occasions2.dee[occasions2.dee$Site == "A2", c("noise250")] <- mean_np250[mean_np250$ID == "37", c("mean")]
occasions2.dee[occasions2.dee$Site == "A2/2", c("noise250")] <- mean_np250[mean_np250$ID == "38", c("mean")]
occasions2.dee[occasions2.dee$Site == "B0", c("noise250")] <- mean_np250[mean_np250$ID == "39", c("mean")]
occasions2.dee[occasions2.dee$Site == "C1", c("noise250")] <- mean_np250[mean_np250$ID == "40", c("mean")]
occasions2.dee[occasions2.dee$Site == "C1/2", c("noise250")] <- mean_np250[mean_np250$ID == "41", c("mean")]
occasions2.dee[occasions2.dee$Site == "C0", c("noise250")] <- mean_np250[mean_np250$ID == "42", c("mean")]
occasions2.dee[occasions2.dee$Site == "C2", c("noise250")] <- mean_np250[mean_np250$ID == "43", c("mean")]
occasions2.dee[occasions2.dee$Site == "D1", c("noise250")] <- mean_np250[mean_np250$ID == "44", c("mean")]
occasions2.dee[occasions2.dee$Site == "D1/2", c("noise250")] <- mean_np250[mean_np250$ID == "45", c("mean")]
occasions2.dee[occasions2.dee$Site == "E1/2", c("noise250")] <- mean_np250[mean_np250$ID == "46", c("mean")]
occasions2.dee[occasions2.dee$Site == "E1", c("noise250")] <- mean_np250[mean_np250$ID == "47", c("mean")]
occasions2.dee[occasions2.dee$Site == "F2", c("noise250")] <- mean_np250[mean_np250$ID == "48", c("mean")]
occasions2.dee[occasions2.dee$Site == "G0", c("noise250")] <- mean_np250[mean_np250$ID == "49", c("mean")] 
occasions2.dee[occasions2.dee$Site == "G2", c("noise250")] <- mean_np250[mean_np250$ID == "50", c("mean")]
occasions2.dee[occasions2.dee$Site == "E0/2", c("noise250")] <- mean_np250[mean_np250$ID == "51", c("mean")]
occasions2.dee[occasions2.dee$Site == "E0", c("noise250")] <- mean_np250[mean_np250$ID == "52", c("mean")]
occasions2.dee[occasions2.dee$Site == "D2", c("noise250")] <- mean_np250[mean_np250$ID == "53", c("mean")]
occasions2.dee[occasions2.dee$Site == "F0", c("noise250")] <- mean_np250[mean_np250$ID == "54", c("mean")]
occasions2.dee[occasions2.dee$Site == "E2", c("noise250")] <- mean_np250[mean_np250$ID == "55", c("mean")]
occasions2.dee[occasions2.dee$Site == "G1", c("noise250")] <- mean_np250[mean_np250$ID == "56", c("mean")]
occasions2.dee[occasions2.dee$Site == "J1", c("noise250")] <- mean_np250[mean_np250$ID == "57", c("mean")]
occasions2.dee[occasions2.dee$Site == "J0", c("noise250")] <- mean_np250[mean_np250$ID == "58", c("mean")]
occasions2.dee[occasions2.dee$Site == "B2", c("noise250")] <- mean_np250[mean_np250$ID == "59", c("mean")]
occasions2.dee[occasions2.dee$Site == "B2/2", c("noise250")] <- mean_np250[mean_np250$ID == "60", c("mean")]
occasions2.dee[occasions2.dee$Site == "F1", c("noise250")] <- mean_np250[mean_np250$ID == "61", c("mean")]
occasions2.dee[occasions2.dee$Site == "I0", c("noise250")] <- mean_np250[mean_np250$ID == "62", c("mean")]
occasions2.dee[occasions2.dee$Site == "I1", c("noise250")] <- mean_np250[mean_np250$ID == "63", c("mean")]
occasions2.dee[occasions2.dee$Site == "I2", c("noise250")] <- mean_np250[mean_np250$ID == "64", c("mean")]
occasions2.dee[occasions2.dee$Site == "H2", c("noise250")] <- mean_np250[mean_np250$ID == "65", c("mean")]
occasions2.dee[occasions2.dee$Site == "H0", c("noise250")] <- mean_np250[mean_np250$ID == "66", c("mean")]
occasions2.dee[occasions2.dee$Site == "H1", c("noise250")] <- mean_np250[mean_np250$ID == "67", c("mean")]
occasions2.dee[occasions2.dee$Site == "J2", c("noise250")] <- mean_np250[mean_np250$ID == "68", c("mean")]
### BUFFER 1km ###
# position 1
occasions1.dee[occasions1.dee$Site == "A0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "1", c("mean")]
occasions1.dee[occasions1.dee$Site == "A1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "2", c("mean")]
occasions1.dee[occasions1.dee$Site == "A2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "3", c("mean")]
occasions1.dee[occasions1.dee$Site == "A2/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "4", c("mean")]
occasions1.dee[occasions1.dee$Site == "B0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "5", c("mean")]
occasions1.dee[occasions1.dee$Site == "B2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "6", c("mean")]
occasions1.dee[occasions1.dee$Site == "B2/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "7", c("mean")]
occasions1.dee[occasions1.dee$Site == "C0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "8", c("mean")]
occasions1.dee[occasions1.dee$Site == "C1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "9", c("mean")]
occasions1.dee[occasions1.dee$Site == "C1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "10", c("mean")]
occasions1.dee[occasions1.dee$Site == "C2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "11", c("mean")]
occasions1.dee[occasions1.dee$Site == "D1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "12", c("mean")]
occasions1.dee[occasions1.dee$Site == "D1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "13", c("mean")]
occasions1.dee[occasions1.dee$Site == "D2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "14", c("mean")]
occasions1.dee[occasions1.dee$Site == "E0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "15", c("mean")]
occasions1.dee[occasions1.dee$Site == "E0/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "16", c("mean")]
occasions1.dee[occasions1.dee$Site == "E1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "17", c("mean")]
occasions1.dee[occasions1.dee$Site == "E1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "18", c("mean")]
occasions1.dee[occasions1.dee$Site == "E2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "19", c("mean")]
occasions1.dee[occasions1.dee$Site == "F0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "20", c("mean")]
occasions1.dee[occasions1.dee$Site == "F1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "21", c("mean")]
occasions1.dee[occasions1.dee$Site == "F2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "22", c("mean")]
occasions1.dee[occasions1.dee$Site == "G0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "23", c("mean")]
occasions1.dee[occasions1.dee$Site == "G1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "24", c("mean")]
occasions1.dee[occasions1.dee$Site == "G2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "25", c("mean")]
occasions1.dee[occasions1.dee$Site == "H0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "26", c("mean")]
occasions1.dee[occasions1.dee$Site == "H1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "27", c("mean")]
occasions1.dee[occasions1.dee$Site == "H2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "28", c("mean")]
occasions1.dee[occasions1.dee$Site == "I0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "29", c("mean")]
occasions1.dee[occasions1.dee$Site == "I1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "30", c("mean")]
occasions1.dee[occasions1.dee$Site == "I2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "31", c("mean")]
occasions1.dee[occasions1.dee$Site == "J0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "32", c("mean")]
occasions1.dee[occasions1.dee$Site == "J1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "33", c("mean")]
occasions1.dee[occasions1.dee$Site == "J2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "34", c("mean")]
# position 2
occasions2.dee[occasions2.dee$Site == "A0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "35", c("mean")]
occasions2.dee[occasions2.dee$Site == "A1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "36", c("mean")]
occasions2.dee[occasions2.dee$Site == "A2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "37", c("mean")]
occasions2.dee[occasions2.dee$Site == "A2/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "38", c("mean")]
occasions2.dee[occasions2.dee$Site == "B0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "39", c("mean")]
occasions2.dee[occasions2.dee$Site == "C1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "40", c("mean")]
occasions2.dee[occasions2.dee$Site == "C1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "41", c("mean")]
occasions2.dee[occasions2.dee$Site == "C0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "42", c("mean")]
occasions2.dee[occasions2.dee$Site == "C2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "43", c("mean")]
occasions2.dee[occasions2.dee$Site == "D1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "44", c("mean")]
occasions2.dee[occasions2.dee$Site == "D1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "45", c("mean")]
occasions2.dee[occasions2.dee$Site == "E1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "46", c("mean")]
occasions2.dee[occasions2.dee$Site == "E1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "47", c("mean")]
occasions2.dee[occasions2.dee$Site == "F2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "48", c("mean")]
occasions2.dee[occasions2.dee$Site == "G0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "49", c("mean")]
occasions2.dee[occasions2.dee$Site == "G2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "50", c("mean")]
occasions2.dee[occasions2.dee$Site == "E0/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "51", c("mean")]
occasions2.dee[occasions2.dee$Site == "E0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "52", c("mean")]
occasions2.dee[occasions2.dee$Site == "D2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "53", c("mean")]
occasions2.dee[occasions2.dee$Site == "F0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "54", c("mean")]
occasions2.dee[occasions2.dee$Site == "E2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "55", c("mean")]
occasions2.dee[occasions2.dee$Site == "G1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "56", c("mean")]
occasions2.dee[occasions2.dee$Site == "J1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "57", c("mean")]
occasions2.dee[occasions2.dee$Site == "J0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "58", c("mean")]
occasions2.dee[occasions2.dee$Site == "B2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "59", c("mean")]
occasions2.dee[occasions2.dee$Site == "B2/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "60", c("mean")]
occasions2.dee[occasions2.dee$Site == "F1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "61", c("mean")]
occasions2.dee[occasions2.dee$Site == "I0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "62", c("mean")]
occasions2.dee[occasions2.dee$Site == "I1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "63", c("mean")]
occasions2.dee[occasions2.dee$Site == "I2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "64", c("mean")]
occasions2.dee[occasions2.dee$Site == "H2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "65", c("mean")]
occasions2.dee[occasions2.dee$Site == "H0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "66", c("mean")]
occasions2.dee[occasions2.dee$Site == "H1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "67", c("mean")]
occasions2.dee[occasions2.dee$Site == "J2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "68", c("mean")]
head(occasions1.dee)
head(occasions2.dee)
summary(occasions1.dee$noise1km)
summary(occasions2.dee$noise1km)
summary(occasions1.dee$noise250)
summary(occasions2.dee$noise250)


# final data frame
final.de.df <- rbind(occasions1.dee, occasions2.dee)

# add site area
final.de.df$area <- NA
final.de.df[final.de.df$Site == "A0", c("area")] <- REM[REM$site_ID == "A0", "area"]
final.de.df[final.de.df$Site == "A1", c("area")] <- REM[REM$site_ID == "A1", "area"]
final.de.df[final.de.df$Site == "A2", c("area")] <- REM[REM$site_ID == "A2", "area"]
final.de.df[final.de.df$Site == "A2/2", c("area")] <- REM[REM$site_ID == "A2", "area"]
final.de.df[final.de.df$Site == "B0", c("area")] <- REM[REM$site_ID == "B0", "area"]
final.de.df[final.de.df$Site == "B2", c("area")] <- REM[REM$site_ID == "B2", "area"]
final.de.df[final.de.df$Site == "B2/2", c("area")] <- REM[REM$site_ID == "B2", "area"]
final.de.df[final.de.df$Site == "C0", c("area")] <- REM[REM$site_ID == "C0", "area"]
final.de.df[final.de.df$Site == "C1", c("area")] <- REM[REM$site_ID == "C1", "area"]
final.de.df[final.de.df$Site == "C1/2", c("area")] <- REM[REM$site_ID == "C1", "area"]
final.de.df[final.de.df$Site == "C2", c("area")] <- REM[REM$site_ID == "C2", "area"]
final.de.df[final.de.df$Site == "D1", c("area")] <- REM[REM$site_ID == "D1", "area"]
final.de.df[final.de.df$Site == "D1/2", c("area")] <- REM[REM$site_ID == "D1", "area"]
final.de.df[final.de.df$Site == "D2", c("area")] <- REM[REM$site_ID == "D2", "area"]
final.de.df[final.de.df$Site == "E0", c("area")] <- REM[REM$site_ID == "E0", "area"]
final.de.df[final.de.df$Site == "E0/2", c("area")] <- REM[REM$site_ID == "E0", "area"]
final.de.df[final.de.df$Site == "E1", c("area")] <- REM[REM$site_ID == "E1", "area"]
final.de.df[final.de.df$Site == "E1/2", c("area")] <- REM[REM$site_ID == "E1", "area"]
final.de.df[final.de.df$Site == "E2", c("area")] <- REM[REM$site_ID == "E2", "area"]
final.de.df[final.de.df$Site == "F0", c("area")] <- REM[REM$site_ID == "F0", "area"]
final.de.df[final.de.df$Site == "F1", c("area")] <- REM[REM$site_ID == "F1", "area"]
final.de.df[final.de.df$Site == "F2", c("area")] <- REM[REM$site_ID == "F2", "area"]
final.de.df[final.de.df$Site == "G0", c("area")] <- REM[REM$site_ID == "G0", "area"]
final.de.df[final.de.df$Site == "G1", c("area")] <- REM[REM$site_ID == "G1", "area"]
final.de.df[final.de.df$Site == "G2", c("area")] <- REM[REM$site_ID == "G2", "area"]
final.de.df[final.de.df$Site == "H0", c("area")] <- REM[REM$site_ID == "H0", "area"]
final.de.df[final.de.df$Site == "H1", c("area")] <- REM[REM$site_ID == "H1", "area"]
final.de.df[final.de.df$Site == "H2", c("area")] <- REM[REM$site_ID == "H2", "area"]
final.de.df[final.de.df$Site == "I0", c("area")] <- REM[REM$site_ID == "I", "area"]
final.de.df[final.de.df$Site == "I1", c("area")] <- REM[REM$site_ID == "I", "area"]
final.de.df[final.de.df$Site == "I2", c("area")] <- REM[REM$site_ID == "I", "area"]
final.de.df[final.de.df$Site == "J0", c("area")] <- REM[REM$site_ID == "J", "area"]
final.de.df[final.de.df$Site == "J1", c("area")] <- REM[REM$site_ID == "J", "area"]
final.de.df[final.de.df$Site == "J2", c("area")] <- REM[REM$site_ID == "J", "area"]

summary(final.de.df$area)

# export data frame csv
write.csv(final.de.df, file = "~/GALLANT Technician/Camera Trap Analysis/deer_occupancy_df.csv")



### FOX ###

# new columns in data frame
# 250m 
occasions.fox$wood250 <- NA
occasions.fox$wet250 <- NA
occasions.fox$urban250 <- NA
occasions.fox$water250 <- NA
occasions.fox$grass250 <- NA
occasions.fox$arable250 <- NA
# 1km
occasions.fox$wood1km <- NA
occasions.fox$wet1km <- NA
occasions.fox$urban1km <- NA
occasions.fox$water1km <- NA
occasions.fox$grass1km <- NA
occasions.fox$arable1km <- NA
# dog relative abundance
occasions.fox$dog_RA <- NA

# if in position 1 - split into positions?
occasions1.fox <- subset(occasions.fox, occasions.fox$Session == "1")
occasions2.fox <- subset(occasions.fox, occasions.fox$Session == "2")
names(occasions.dee)

### 250m BUFFER

# if at site...cbind 6 columns
# add proportions for position 1
occasions1.fox[occasions1.fox$Site == "A0", prop_columns250] <- df1.prop250[df1.prop250 == "A0", prop_columns250]
occasions1.fox[occasions1.fox$Site == "A1", prop_columns250] <- df1.prop250[df1.prop250 == "A1", prop_columns250]
occasions1.fox[occasions1.fox$Site == "A2", prop_columns250] <- df1.prop250[df1.prop250 == "A2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "A2/2", prop_columns250] <- df1.prop250[df1.prop250 == "A2/2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "B0", prop_columns250] <- df1.prop250[df1.prop250 == "B0", prop_columns250]
occasions1.fox[occasions1.fox$Site == "B2", prop_columns250] <- df1.prop250[df1.prop250 == "B2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "B2/2", prop_columns250] <- df1.prop250[df1.prop250 == "B2/2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "C0", prop_columns250] <- df1.prop250[df1.prop250 == "C0", prop_columns250]
occasions1.fox[occasions1.fox$Site == "C1", prop_columns250] <- df1.prop250[df1.prop250 == "C1", prop_columns250]
occasions1.fox[occasions1.fox$Site == "C1/2", prop_columns250] <- df1.prop250[df1.prop250 == "C1/2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "C2", prop_columns250] <- df1.prop250[df1.prop250 == "C2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "D1", prop_columns250] <- df1.prop250[df1.prop250 == "D1", prop_columns250]
occasions1.fox[occasions1.fox$Site == "D1/2", prop_columns250] <- df1.prop250[df1.prop250 == "D1/2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "D2", prop_columns250] <- df1.prop250[df1.prop250 == "D2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "E0", prop_columns250] <- df1.prop250[df1.prop250 == "E0", prop_columns250]
occasions1.fox[occasions1.fox$Site == "E0/2", prop_columns250] <- df1.prop250[df1.prop250 == "E0/2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "E1", prop_columns250] <- df1.prop250[df1.prop250 == "E1", prop_columns250]
occasions1.fox[occasions1.fox$Site == "E1/2", prop_columns250] <- df1.prop250[df1.prop250 == "E1/2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "E2", prop_columns250] <- df1.prop250[df1.prop250 == "E2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "F0", prop_columns250] <- df1.prop250[df1.prop250 == "F0", prop_columns250]
occasions1.fox[occasions1.fox$Site == "F1", prop_columns250] <- df1.prop250[df1.prop250 == "F1", prop_columns250]
occasions1.fox[occasions1.fox$Site == "F2", prop_columns250] <- df1.prop250[df1.prop250 == "F2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "G0", prop_columns250] <- df1.prop250[df1.prop250 == "G0", prop_columns250]
occasions1.fox[occasions1.fox$Site == "G1", prop_columns250] <- df1.prop250[df1.prop250 == "G1", prop_columns250]
occasions1.fox[occasions1.fox$Site == "G2", prop_columns250] <- df1.prop250[df1.prop250 == "G2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "H0", prop_columns250] <- df1.prop250[df1.prop250 == "H0", prop_columns250]
occasions1.fox[occasions1.fox$Site == "H1", prop_columns250] <- df1.prop250[df1.prop250 == "H1", prop_columns250]
occasions1.fox[occasions1.fox$Site == "H2", prop_columns250] <- df1.prop250[df1.prop250 == "H2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "I0", prop_columns250] <- df1.prop250[df1.prop250 == "I0", prop_columns250]
occasions1.fox[occasions1.fox$Site == "I1", prop_columns250] <- df1.prop250[df1.prop250 == "I1", prop_columns250]
occasions1.fox[occasions1.fox$Site == "I2", prop_columns250] <- df1.prop250[df1.prop250 == "I2", prop_columns250]
occasions1.fox[occasions1.fox$Site == "J0", prop_columns250] <- df1.prop250[df1.prop250 == "J0", prop_columns250]
occasions1.fox[occasions1.fox$Site == "J1", prop_columns250] <- df1.prop250[df1.prop250 == "J1", prop_columns250]
occasions1.fox[occasions1.fox$Site == "J2", prop_columns250] <- df1.prop250[df1.prop250 == "J2", prop_columns250]
# add proportions for position 2
occasions2.fox[occasions2.fox$Site == "A0", prop_columns250] <- df2.prop250[df2.prop250 == "A0", prop_columns250]
occasions2.fox[occasions2.fox$Site == "A1", prop_columns250] <- df2.prop250[df2.prop250 == "A1", prop_columns250]
occasions2.fox[occasions2.fox$Site == "A2", prop_columns250] <- df2.prop250[df2.prop250 == "A2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "A2/2", prop_columns250] <- df2.prop250[df2.prop250 == "A2/2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "B0", prop_columns250] <- df2.prop250[df2.prop250 == "B0", prop_columns250]
occasions2.fox[occasions2.fox$Site == "B2", prop_columns250] <- df2.prop250[df2.prop250 == "B2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "B2/2", prop_columns250] <- df2.prop250[df2.prop250 == "B2/2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "C0", prop_columns250] <- df2.prop250[df2.prop250 == "C0", prop_columns250]
occasions2.fox[occasions2.fox$Site == "C1", prop_columns250] <- df2.prop250[df2.prop250 == "C1", prop_columns250]
occasions2.fox[occasions2.fox$Site == "C1/2", prop_columns250] <- df2.prop250[df2.prop250 == "C1/2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "C2", prop_columns250] <- df2.prop250[df2.prop250 == "C2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "D1", prop_columns250] <- df2.prop250[df2.prop250 == "D1", prop_columns250]
occasions2.fox[occasions2.fox$Site == "D1/2", prop_columns250] <- df2.prop250[df2.prop250 == "D1/2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "D2", prop_columns250] <- df2.prop250[df2.prop250 == "D2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "E0", prop_columns250] <- df2.prop250[df2.prop250 == "E0", prop_columns250]
occasions2.fox[occasions2.fox$Site == "E0/2", prop_columns250] <- df2.prop250[df2.prop250 == "E0/2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "E1", prop_columns250] <- df2.prop250[df2.prop250 == "E1", prop_columns250]
occasions2.fox[occasions2.fox$Site == "E1/2", prop_columns250] <- df2.prop250[df2.prop250 == "E1/2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "E2", prop_columns250] <- df2.prop250[df2.prop250 == "E2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "F0", prop_columns250] <- df2.prop250[df2.prop250 == "F0", prop_columns250]
occasions2.fox[occasions2.fox$Site == "F1", prop_columns250] <- df2.prop250[df2.prop250 == "F1", prop_columns250]
occasions2.fox[occasions2.fox$Site == "F2", prop_columns250] <- df2.prop250[df2.prop250 == "F2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "G0", prop_columns250] <- df2.prop250[df2.prop250 == "G0", prop_columns250]
occasions2.fox[occasions2.fox$Site == "G1", prop_columns250] <- df2.prop250[df2.prop250 == "G1", prop_columns250]
occasions2.fox[occasions2.fox$Site == "G2", prop_columns250] <- df2.prop250[df2.prop250 == "G2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "H0", prop_columns250] <- df2.prop250[df2.prop250 == "H0", prop_columns250]
occasions2.fox[occasions2.fox$Site == "H1", prop_columns250] <- df2.prop250[df2.prop250 == "H1", prop_columns250]
occasions2.fox[occasions2.fox$Site == "H2", prop_columns250] <- df2.prop250[df2.prop250 == "H2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "I0", prop_columns250] <- df2.prop250[df2.prop250 == "I0", prop_columns250]
occasions2.fox[occasions2.fox$Site == "I1", prop_columns250] <- df2.prop250[df2.prop250 == "I1", prop_columns250]
occasions2.fox[occasions2.fox$Site == "I2", prop_columns250] <- df2.prop250[df2.prop250 == "I2", prop_columns250]
occasions2.fox[occasions2.fox$Site == "J0", prop_columns250] <- df2.prop250[df2.prop250 == "J0", prop_columns250]
occasions2.fox[occasions2.fox$Site == "J1", prop_columns250] <- df2.prop250[df2.prop250 == "J1", prop_columns250]
occasions2.fox[occasions2.fox$Site == "J2", prop_columns250] <- df2.prop250[df2.prop250 == "J2", prop_columns250]

### 1km BUFFER

# if at site...cbind 6 columns
# add proportions for position 1
occasions1.fox[occasions1.fox$Site == "A0", prop_columns1km] <- df1.prop1km[df1.prop1km == "A0", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "A1", prop_columns1km] <- df1.prop1km[df1.prop1km == "A1", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "A2", prop_columns1km] <- df1.prop1km[df1.prop1km == "A2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "A2/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "A2/2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "B0", prop_columns1km] <- df1.prop1km[df1.prop1km == "B0", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "B2", prop_columns1km] <- df1.prop1km[df1.prop1km == "B2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "B2/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "B2/2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "C0", prop_columns1km] <- df1.prop1km[df1.prop1km == "C0", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "C1", prop_columns1km] <- df1.prop1km[df1.prop1km == "C1", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "C1/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "C1/2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "C2", prop_columns1km] <- df1.prop1km[df1.prop1km == "C2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "D1", prop_columns1km] <- df1.prop1km[df1.prop1km == "D1", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "D1/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "D1/2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "D2", prop_columns1km] <- df1.prop1km[df1.prop1km == "D2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "E0", prop_columns1km] <- df1.prop1km[df1.prop1km == "E0", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "E0/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "E0/2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "E1", prop_columns1km] <- df1.prop1km[df1.prop1km == "E1", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "E1/2", prop_columns1km] <- df1.prop1km[df1.prop1km == "E1/2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "E2", prop_columns1km] <- df1.prop1km[df1.prop1km == "E2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "F0", prop_columns1km] <- df1.prop1km[df1.prop1km == "F0", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "F1", prop_columns1km] <- df1.prop1km[df1.prop1km == "F1", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "F2", prop_columns1km] <- df1.prop1km[df1.prop1km == "F2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "G0", prop_columns1km] <- df1.prop1km[df1.prop1km == "G0", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "G1", prop_columns1km] <- df1.prop1km[df1.prop1km == "G1", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "G2", prop_columns1km] <- df1.prop1km[df1.prop1km == "G2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "H0", prop_columns1km] <- df1.prop1km[df1.prop1km == "H0", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "H1", prop_columns1km] <- df1.prop1km[df1.prop1km == "H1", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "H2", prop_columns1km] <- df1.prop1km[df1.prop1km == "H2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "I0", prop_columns1km] <- df1.prop1km[df1.prop1km == "I0", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "I1", prop_columns1km] <- df1.prop1km[df1.prop1km == "I1", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "I2", prop_columns1km] <- df1.prop1km[df1.prop1km == "I2", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "J0", prop_columns1km] <- df1.prop1km[df1.prop1km == "J0", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "J1", prop_columns1km] <- df1.prop1km[df1.prop1km == "J1", prop_columns1km]
occasions1.fox[occasions1.fox$Site == "J2", prop_columns1km] <- df1.prop1km[df1.prop1km == "J2", prop_columns1km]
# add proportions for position 2
occasions2.fox[occasions2.fox$Site == "A0", prop_columns1km] <- df2.prop1km[df2.prop1km == "A0", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "A1", prop_columns1km] <- df2.prop1km[df2.prop1km == "A1", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "A2", prop_columns1km] <- df2.prop1km[df2.prop1km == "A2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "A2/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "A2/2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "B0", prop_columns1km] <- df2.prop1km[df2.prop1km == "B0", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "B2", prop_columns1km] <- df2.prop1km[df2.prop1km == "B2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "B2/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "B2/2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "C0", prop_columns1km] <- df2.prop1km[df2.prop1km == "C0", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "C1", prop_columns1km] <- df2.prop1km[df2.prop1km == "C1", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "C1/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "C1/2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "C2", prop_columns1km] <- df2.prop1km[df2.prop1km == "C2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "D1", prop_columns1km] <- df2.prop1km[df2.prop1km == "D1", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "D1/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "D1/2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "D2", prop_columns1km] <- df2.prop1km[df2.prop1km == "D2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "E0", prop_columns1km] <- df2.prop1km[df2.prop1km == "E0", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "E0/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "E0/2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "E1", prop_columns1km] <- df2.prop1km[df2.prop1km == "E1", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "E1/2", prop_columns1km] <- df2.prop1km[df2.prop1km == "E1/2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "E2", prop_columns1km] <- df2.prop1km[df2.prop1km == "E2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "F0", prop_columns1km] <- df2.prop1km[df2.prop1km == "F0", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "F1", prop_columns1km] <- df2.prop1km[df2.prop1km == "F1", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "F2", prop_columns1km] <- df2.prop1km[df2.prop1km == "F2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "G0", prop_columns1km] <- df2.prop1km[df2.prop1km == "G0", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "G1", prop_columns1km] <- df2.prop1km[df2.prop1km == "G1", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "G2", prop_columns1km] <- df2.prop1km[df2.prop1km == "G2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "H0", prop_columns1km] <- df2.prop1km[df2.prop1km == "H0", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "H1", prop_columns1km] <- df2.prop1km[df2.prop1km == "H1", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "H2", prop_columns1km] <- df2.prop1km[df2.prop1km == "H2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "I0", prop_columns1km] <- df2.prop1km[df2.prop1km == "I0", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "I1", prop_columns1km] <- df2.prop1km[df2.prop1km == "I1", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "I2", prop_columns1km] <- df2.prop1km[df2.prop1km == "I2", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "J0", prop_columns1km] <- df2.prop1km[df2.prop1km == "J0", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "J1", prop_columns1km] <- df2.prop1km[df2.prop1km == "J1", prop_columns1km]
occasions2.fox[occasions2.fox$Site == "J2", prop_columns1km] <- df2.prop1km[df2.prop1km == "J2", prop_columns1km]

head(occasions1.fox, 10)
head(occasions2.fox, 10)
summary(occasions1.fox$wood1km)
summary(occasions1.fox$wet1km)
summary(occasions1.fox$water1km)
summary(occasions1.fox$arable1km)
summary(occasions1.fox$urban1km)
summary(occasions1.fox$grass1km)
summary(occasions2.fox$wood1km)
summary(occasions2.fox$wet1km)
summary(occasions2.fox$water1km)
summary(occasions2.fox$arable1km)
summary(occasions2.fox$urban1km)
summary(occasions2.fox$grass1km)

# add dog relative abundance (per camera)
# add for first placement 1
occasions1.fox[occasions1.fox$Site == "A0", c("dog_RA")] <- dog_RA[dog_RA$site == "A0", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "A1", c("dog_RA")] <- dog_RA[dog_RA$site == "A1", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "A2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "A2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2/2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "B0", c("dog_RA")] <- dog_RA[dog_RA$site == "B0", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "B2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "B2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2/2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "C0", c("dog_RA")] <- dog_RA[dog_RA$site == "C0", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "C1", c("dog_RA")] <- dog_RA[dog_RA$site == "C1", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "C1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "C1/2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "C2", c("dog_RA")] <- dog_RA[dog_RA$site == "C2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "D1", c("dog_RA")] <- dog_RA[dog_RA$site == "D1", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "D1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "D1/2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "D2", c("dog_RA")] <- dog_RA[dog_RA$site == "D2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "E0", c("dog_RA")] <- dog_RA[dog_RA$site == "E0", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "E0/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E0/2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "E1", c("dog_RA")] <- dog_RA[dog_RA$site == "E1", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "E1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E1/2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "E2", c("dog_RA")] <- dog_RA[dog_RA$site == "E2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "F0", c("dog_RA")] <- dog_RA[dog_RA$site == "F0", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "F1", c("dog_RA")] <- dog_RA[dog_RA$site == "F1", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "F2", c("dog_RA")] <- dog_RA[dog_RA$site == "F2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "G0", c("dog_RA")] <- dog_RA[dog_RA$site == "G0", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "G1", c("dog_RA")] <- dog_RA[dog_RA$site == "G1", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "G2", c("dog_RA")] <- dog_RA[dog_RA$site == "G2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "H0", c("dog_RA")] <- dog_RA[dog_RA$site == "H0", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "H1", c("dog_RA")] <- dog_RA[dog_RA$site == "H1", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "H2", c("dog_RA")] <- dog_RA[dog_RA$site == "H2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "I0", c("dog_RA")] <- dog_RA[dog_RA$site == "I0", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "I1", c("dog_RA")] <- dog_RA[dog_RA$site == "I1", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "I2", c("dog_RA")] <- dog_RA[dog_RA$site == "I2", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "J0", c("dog_RA")] <- dog_RA[dog_RA$site == "J0", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "J1", c("dog_RA")] <- dog_RA[dog_RA$site == "J1", c("loc.mean1")]
occasions1.fox[occasions1.fox$Site == "J2", c("dog_RA")] <- dog_RA[dog_RA$site == "J2", c("loc.mean1")]
# add for first placement 2
occasions2.fox[occasions2.fox$Site == "A0", c("dog_RA")] <- dog_RA[dog_RA$site == "A0", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "A1", c("dog_RA")] <- dog_RA[dog_RA$site == "A1", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "A2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "A2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "A2/2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "B0", c("dog_RA")] <- dog_RA[dog_RA$site == "B0", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "B2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "B2/2", c("dog_RA")] <- dog_RA[dog_RA$site == "B2/2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "C0", c("dog_RA")] <- dog_RA[dog_RA$site == "C0", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "C1", c("dog_RA")] <- dog_RA[dog_RA$site == "C1", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "C1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "C1/2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "C2", c("dog_RA")] <- dog_RA[dog_RA$site == "C2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "D1", c("dog_RA")] <- dog_RA[dog_RA$site == "D1", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "D1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "D1/2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "D2", c("dog_RA")] <- dog_RA[dog_RA$site == "D2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "E0", c("dog_RA")] <- dog_RA[dog_RA$site == "E0", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "E0/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E0/2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "E1", c("dog_RA")] <- dog_RA[dog_RA$site == "E1", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "E1/2", c("dog_RA")] <- dog_RA[dog_RA$site == "E1/2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "E2", c("dog_RA")] <- dog_RA[dog_RA$site == "E2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "F0", c("dog_RA")] <- dog_RA[dog_RA$site == "F0", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "F1", c("dog_RA")] <- dog_RA[dog_RA$site == "F1", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "F2", c("dog_RA")] <- dog_RA[dog_RA$site == "F2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "G0", c("dog_RA")] <- dog_RA[dog_RA$site == "G0", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "G1", c("dog_RA")] <- dog_RA[dog_RA$site == "G1", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "G2", c("dog_RA")] <- dog_RA[dog_RA$site == "G2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "H0", c("dog_RA")] <- dog_RA[dog_RA$site == "H0", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "H1", c("dog_RA")] <- dog_RA[dog_RA$site == "H1", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "H2", c("dog_RA")] <- dog_RA[dog_RA$site == "H2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "I0", c("dog_RA")] <- dog_RA[dog_RA$site == "I0", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "I1", c("dog_RA")] <- dog_RA[dog_RA$site == "I1", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "I2", c("dog_RA")] <- dog_RA[dog_RA$site == "I2", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "J0", c("dog_RA")] <- dog_RA[dog_RA$site == "J0", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "J1", c("dog_RA")] <- dog_RA[dog_RA$site == "J1", c("loc.mean2")]
occasions2.fox[occasions2.fox$Site == "J2", c("dog_RA")] <- dog_RA[dog_RA$site == "J2", c("loc.mean2")]

head(occasions1.fox, 10)
head(occasions2.fox, 10)
summary(as.numeric(occasions1.fox$dog_RA))
summary(occasions2.fox$dog_RA)

# add light pollution per site
### BUFFER 250 ###
# position 1
occasions1.fox[occasions1.fox$Site == "A0", c("light250")] <- mean_lp250[mean_lp250$ID == "1", c("mean")]
occasions1.fox[occasions1.fox$Site == "A1", c("light250")] <- mean_lp250[mean_lp250$ID == "2", c("mean")]
occasions1.fox[occasions1.fox$Site == "A2", c("light250")] <- mean_lp250[mean_lp250$ID == "3", c("mean")]
occasions1.fox[occasions1.fox$Site == "A2/2", c("light250")] <- mean_lp250[mean_lp250$ID == "4", c("mean")]
occasions1.fox[occasions1.fox$Site == "B0", c("light250")] <- mean_lp250[mean_lp250$ID == "5", c("mean")]
occasions1.fox[occasions1.fox$Site == "B2", c("light250")] <- mean_lp250[mean_lp250$ID == "6", c("mean")]
occasions1.fox[occasions1.fox$Site == "B2/2", c("light250")] <- mean_lp250[mean_lp250$ID == "7", c("mean")]
occasions1.fox[occasions1.fox$Site == "C0", c("light250")] <- mean_lp250[mean_lp250$ID == "8", c("mean")]
occasions1.fox[occasions1.fox$Site == "C1", c("light250")] <- mean_lp250[mean_lp250$ID == "9", c("mean")]
occasions1.fox[occasions1.fox$Site == "C1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "10", c("mean")]
occasions1.fox[occasions1.fox$Site == "C2", c("light250")] <- mean_lp250[mean_lp250$ID == "11", c("mean")]
occasions1.fox[occasions1.fox$Site == "D1", c("light250")] <- mean_lp250[mean_lp250$ID == "12", c("mean")]
occasions1.fox[occasions1.fox$Site == "D1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "13", c("mean")]
occasions1.fox[occasions1.fox$Site == "D2", c("light250")] <- mean_lp250[mean_lp250$ID == "14", c("mean")]
occasions1.fox[occasions1.fox$Site == "E0", c("light250")] <- mean_lp250[mean_lp250$ID == "15", c("mean")]
occasions1.fox[occasions1.fox$Site == "E0/2", c("light250")] <- mean_lp250[mean_lp250$ID == "16", c("mean")]
occasions1.fox[occasions1.fox$Site == "E1", c("light250")] <- mean_lp250[mean_lp250$ID == "17", c("mean")]
occasions1.fox[occasions1.fox$Site == "E1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "18", c("mean")]
occasions1.fox[occasions1.fox$Site == "E2", c("light250")] <- mean_lp250[mean_lp250$ID == "19", c("mean")]
occasions1.fox[occasions1.fox$Site == "F0", c("light250")] <- mean_lp250[mean_lp250$ID == "20", c("mean")]
occasions1.fox[occasions1.fox$Site == "F1", c("light250")] <- mean_lp250[mean_lp250$ID == "21", c("mean")]
occasions1.fox[occasions1.fox$Site == "F2", c("light250")] <- mean_lp250[mean_lp250$ID == "22", c("mean")]
occasions1.fox[occasions1.fox$Site == "G0", c("light250")] <- mean_lp250[mean_lp250$ID == "23", c("mean")]
occasions1.fox[occasions1.fox$Site == "G1", c("light250")] <- mean_lp250[mean_lp250$ID == "24", c("mean")]
occasions1.fox[occasions1.fox$Site == "G2", c("light250")] <- mean_lp250[mean_lp250$ID == "25", c("mean")]
occasions1.fox[occasions1.fox$Site == "H0", c("light250")] <- mean_lp250[mean_lp250$ID == "26", c("mean")]
occasions1.fox[occasions1.fox$Site == "H1", c("light250")] <- mean_lp250[mean_lp250$ID == "27", c("mean")]
occasions1.fox[occasions1.fox$Site == "H2", c("light250")] <- mean_lp250[mean_lp250$ID == "28", c("mean")]
occasions1.fox[occasions1.fox$Site == "I0", c("light250")] <- mean_lp250[mean_lp250$ID == "29", c("mean")]
occasions1.fox[occasions1.fox$Site == "I1", c("light250")] <- mean_lp250[mean_lp250$ID == "30", c("mean")]
occasions1.fox[occasions1.fox$Site == "I2", c("light250")] <- mean_lp250[mean_lp250$ID == "31", c("mean")]
occasions1.fox[occasions1.fox$Site == "J0", c("light250")] <- mean_lp250[mean_lp250$ID == "32", c("mean")]
occasions1.fox[occasions1.fox$Site == "J1", c("light250")] <- mean_lp250[mean_lp250$ID == "33", c("mean")]
occasions1.fox[occasions1.fox$Site == "J2", c("light250")] <- mean_lp250[mean_lp250$ID == "34", c("mean")]
# position 2
occasions2.fox[occasions2.fox$Site == "A0", c("light250")] <- mean_lp250[mean_lp250$ID == "35", c("mean")]
occasions2.fox[occasions2.fox$Site == "A1", c("light250")] <- mean_lp250[mean_lp250$ID == "36", c("mean")]
occasions2.fox[occasions2.fox$Site == "A2", c("light250")] <- mean_lp250[mean_lp250$ID == "37", c("mean")]
occasions2.fox[occasions2.fox$Site == "A2/2", c("light250")] <- mean_lp250[mean_lp250$ID == "38", c("mean")]
occasions2.fox[occasions2.fox$Site == "B0", c("light250")] <- mean_lp250[mean_lp250$ID == "39", c("mean")]
occasions2.fox[occasions2.fox$Site == "C1", c("light250")] <- mean_lp250[mean_lp250$ID == "40", c("mean")]
occasions2.fox[occasions2.fox$Site == "C1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "41", c("mean")]
occasions2.fox[occasions2.fox$Site == "C0", c("light250")] <- mean_lp250[mean_lp250$ID == "42", c("mean")]
occasions2.fox[occasions2.fox$Site == "C2", c("light250")] <- mean_lp250[mean_lp250$ID == "43", c("mean")]
occasions2.fox[occasions2.fox$Site == "D1", c("light250")] <- mean_lp250[mean_lp250$ID == "44", c("mean")]
occasions2.fox[occasions2.fox$Site == "D1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "45", c("mean")]
occasions2.fox[occasions2.fox$Site == "E1/2", c("light250")] <- mean_lp250[mean_lp250$ID == "46", c("mean")]
occasions2.fox[occasions2.fox$Site == "E1", c("light250")] <- mean_lp250[mean_lp250$ID == "47", c("mean")]
occasions2.fox[occasions2.fox$Site == "F2", c("light250")] <- mean_lp250[mean_lp250$ID == "48", c("mean")]
occasions2.fox[occasions2.fox$Site == "G0", c("light250")] <- mean_lp250[mean_lp250$ID == "49", c("mean")]
occasions2.fox[occasions2.fox$Site == "G2", c("light250")] <- mean_lp250[mean_lp250$ID == "50", c("mean")]
occasions2.fox[occasions2.fox$Site == "E0/2", c("light250")] <- mean_lp250[mean_lp250$ID == "51", c("mean")]
occasions2.fox[occasions2.fox$Site == "E0", c("light250")] <- mean_lp250[mean_lp250$ID == "52", c("mean")]
occasions2.fox[occasions2.fox$Site == "D2", c("light250")] <- mean_lp250[mean_lp250$ID == "53", c("mean")]
occasions2.fox[occasions2.fox$Site == "F0", c("light250")] <- mean_lp250[mean_lp250$ID == "54", c("mean")]
occasions2.fox[occasions2.fox$Site == "E2", c("light250")] <- mean_lp250[mean_lp250$ID == "55", c("mean")]
occasions2.fox[occasions2.fox$Site == "G1", c("light250")] <- mean_lp250[mean_lp250$ID == "56", c("mean")]
occasions2.fox[occasions2.fox$Site == "J1", c("light250")] <- mean_lp250[mean_lp250$ID == "57", c("mean")]
occasions2.fox[occasions2.fox$Site == "J0", c("light250")] <- mean_lp250[mean_lp250$ID == "58", c("mean")]
occasions2.fox[occasions2.fox$Site == "B2", c("light250")] <- mean_lp250[mean_lp250$ID == "59", c("mean")]
occasions2.fox[occasions2.fox$Site == "B2/2", c("light250")] <- mean_lp250[mean_lp250$ID == "60", c("mean")]
occasions2.fox[occasions2.fox$Site == "F1", c("light250")] <- mean_lp250[mean_lp250$ID == "61", c("mean")]
occasions2.fox[occasions2.fox$Site == "I0", c("light250")] <- mean_lp250[mean_lp250$ID == "62", c("mean")]
occasions2.fox[occasions2.fox$Site == "I1", c("light250")] <- mean_lp250[mean_lp250$ID == "63", c("mean")]
occasions2.fox[occasions2.fox$Site == "I2", c("light250")] <- mean_lp250[mean_lp250$ID == "64", c("mean")]
occasions2.fox[occasions2.fox$Site == "H2", c("light250")] <- mean_lp250[mean_lp250$ID == "65", c("mean")]
occasions2.fox[occasions2.fox$Site == "H0", c("light250")] <- mean_lp250[mean_lp250$ID == "66", c("mean")]
occasions2.fox[occasions2.fox$Site == "H1", c("light250")] <- mean_lp250[mean_lp250$ID == "67", c("mean")]
occasions2.fox[occasions2.fox$Site == "J2", c("light250")] <- mean_lp250[mean_lp250$ID == "68", c("mean")]
### BUFFER 1km ###
# position 1
occasions1.fox[occasions1.fox$Site == "A0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "1", c("mean")]
occasions1.fox[occasions1.fox$Site == "A1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "2", c("mean")]
occasions1.fox[occasions1.fox$Site == "A2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "3", c("mean")]
occasions1.fox[occasions1.fox$Site == "A2/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "4", c("mean")]
occasions1.fox[occasions1.fox$Site == "B0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "5", c("mean")]
occasions1.fox[occasions1.fox$Site == "B2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "6", c("mean")]
occasions1.fox[occasions1.fox$Site == "B2/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "7", c("mean")]
occasions1.fox[occasions1.fox$Site == "C0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "8", c("mean")]
occasions1.fox[occasions1.fox$Site == "C1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "9", c("mean")]
occasions1.fox[occasions1.fox$Site == "C1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "10", c("mean")]
occasions1.fox[occasions1.fox$Site == "C2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "11", c("mean")]
occasions1.fox[occasions1.fox$Site == "D1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "12", c("mean")]
occasions1.fox[occasions1.fox$Site == "D1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "13", c("mean")]
occasions1.fox[occasions1.fox$Site == "D2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "14", c("mean")]
occasions1.fox[occasions1.fox$Site == "E0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "15", c("mean")]
occasions1.fox[occasions1.fox$Site == "E0/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "16", c("mean")]
occasions1.fox[occasions1.fox$Site == "E1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "17", c("mean")]
occasions1.fox[occasions1.fox$Site == "E1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "18", c("mean")]
occasions1.fox[occasions1.fox$Site == "E2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "19", c("mean")]
occasions1.fox[occasions1.fox$Site == "F0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "20", c("mean")]
occasions1.fox[occasions1.fox$Site == "F1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "21", c("mean")]
occasions1.fox[occasions1.fox$Site == "F2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "22", c("mean")]
occasions1.fox[occasions1.fox$Site == "G0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "23", c("mean")]
occasions1.fox[occasions1.fox$Site == "G1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "24", c("mean")]
occasions1.fox[occasions1.fox$Site == "G2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "25", c("mean")]
occasions1.fox[occasions1.fox$Site == "H0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "26", c("mean")]
occasions1.fox[occasions1.fox$Site == "H1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "27", c("mean")]
occasions1.fox[occasions1.fox$Site == "H2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "28", c("mean")]
occasions1.fox[occasions1.fox$Site == "I0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "29", c("mean")]
occasions1.fox[occasions1.fox$Site == "I1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "30", c("mean")]
occasions1.fox[occasions1.fox$Site == "I2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "31", c("mean")]
occasions1.fox[occasions1.fox$Site == "J0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "32", c("mean")]
occasions1.fox[occasions1.fox$Site == "J1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "33", c("mean")]
occasions1.fox[occasions1.fox$Site == "J2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "34", c("mean")]
# position 2
occasions2.fox[occasions2.fox$Site == "A0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "35", c("mean")]
occasions2.fox[occasions2.fox$Site == "A1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "36", c("mean")]
occasions2.fox[occasions2.fox$Site == "A2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "37", c("mean")]
occasions2.fox[occasions2.fox$Site == "A2/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "38", c("mean")]
occasions2.fox[occasions2.fox$Site == "B0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "39", c("mean")]
occasions2.fox[occasions2.fox$Site == "C1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "40", c("mean")]
occasions2.fox[occasions2.fox$Site == "C1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "41", c("mean")]
occasions2.fox[occasions2.fox$Site == "C0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "42", c("mean")]
occasions2.fox[occasions2.fox$Site == "C2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "43", c("mean")]
occasions2.fox[occasions2.fox$Site == "D1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "44", c("mean")]
occasions2.fox[occasions2.fox$Site == "D1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "45", c("mean")]
occasions2.fox[occasions2.fox$Site == "E1/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "46", c("mean")]
occasions2.fox[occasions2.fox$Site == "E1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "47", c("mean")]
occasions2.fox[occasions2.fox$Site == "F2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "48", c("mean")]
occasions2.fox[occasions2.fox$Site == "G0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "49", c("mean")]
occasions2.fox[occasions2.fox$Site == "G2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "50", c("mean")]
occasions2.fox[occasions2.fox$Site == "E0/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "51", c("mean")]
occasions2.fox[occasions2.fox$Site == "E0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "52", c("mean")]
occasions2.fox[occasions2.fox$Site == "D2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "53", c("mean")]
occasions2.fox[occasions2.fox$Site == "F0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "54", c("mean")]
occasions2.fox[occasions2.fox$Site == "E2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "55", c("mean")]
occasions2.fox[occasions2.fox$Site == "G1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "56", c("mean")]
occasions2.fox[occasions2.fox$Site == "J1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "57", c("mean")]
occasions2.fox[occasions2.fox$Site == "J0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "58", c("mean")]
occasions2.fox[occasions2.fox$Site == "B2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "59", c("mean")]
occasions2.fox[occasions2.fox$Site == "B2/2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "60", c("mean")]
occasions2.fox[occasions2.fox$Site == "F1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "61", c("mean")]
occasions2.fox[occasions2.fox$Site == "I0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "62", c("mean")]
occasions2.fox[occasions2.fox$Site == "I1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "63", c("mean")]
occasions2.fox[occasions2.fox$Site == "I2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "64", c("mean")]
occasions2.fox[occasions2.fox$Site == "H2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "65", c("mean")]
occasions2.fox[occasions2.fox$Site == "H0", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "66", c("mean")]
occasions2.fox[occasions2.fox$Site == "H1", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "67", c("mean")]
occasions2.fox[occasions2.fox$Site == "J2", c("light1km")] <- mean_lp1km[mean_lp1km$ID == "68", c("mean")]
head(occasions1.fox)
head(occasions2.fox)
summary(occasions1.fox$light250)
summary(occasions2.fox$light250)
summary(occasions1.fox$light1km)
summary(occasions2.fox$light1km)



# add noise pollution per site
### BUFFER 250 ###
# position 1
occasions1.fox[occasions1.fox$Site == "A0", c("noise250")] <- mean_np250[mean_np250$ID == "1", c("mean")]
occasions1.fox[occasions1.fox$Site == "A1", c("noise250")] <- mean_np250[mean_np250$ID == "2", c("mean")]
occasions1.fox[occasions1.fox$Site == "A2", c("noise250")] <- mean_np250[mean_np250$ID == "3", c("mean")]
occasions1.fox[occasions1.fox$Site == "A2/2", c("noise250")] <- mean_np250[mean_np250$ID == "4", c("mean")]
occasions1.fox[occasions1.fox$Site == "B0", c("noise250")] <- mean_np250[mean_np250$ID == "5", c("mean")]
occasions1.fox[occasions1.fox$Site == "B2", c("noise250")] <- mean_np250[mean_np250$ID == "6", c("mean")]
occasions1.fox[occasions1.fox$Site == "B2/2", c("noise250")] <- mean_np250[mean_np250$ID == "7", c("mean")]
occasions1.fox[occasions1.fox$Site == "C0", c("noise250")] <- mean_np250[mean_np250$ID == "8", c("mean")]
occasions1.fox[occasions1.fox$Site == "C1", c("noise250")] <- mean_np250[mean_np250$ID == "9", c("mean")]
occasions1.fox[occasions1.fox$Site == "C1/2", c("noise250")] <- mean_np250[mean_np250$ID == "10", c("mean")]
occasions1.fox[occasions1.fox$Site == "C2", c("noise250")] <- mean_np250[mean_np250$ID == "11", c("mean")]
occasions1.fox[occasions1.fox$Site == "D1", c("noise250")] <- mean_np250[mean_np250$ID == "12", c("mean")]
occasions1.fox[occasions1.fox$Site == "D1/2", c("noise250")] <- mean_np250[mean_np250$ID == "13", c("mean")]
occasions1.fox[occasions1.fox$Site == "D2", c("noise250")] <- mean_np250[mean_np250$ID == "14", c("mean")]
occasions1.fox[occasions1.fox$Site == "E0", c("noise250")] <- mean_np250[mean_np250$ID == "15", c("mean")]
occasions1.fox[occasions1.fox$Site == "E0/2", c("noise250")] <- mean_np250[mean_np250$ID == "16", c("mean")]
occasions1.fox[occasions1.fox$Site == "E1", c("noise250")] <- mean_np250[mean_np250$ID == "17", c("mean")]
occasions1.fox[occasions1.fox$Site == "E1/2", c("noise250")] <- mean_np250[mean_np250$ID == "18", c("mean")]
occasions1.fox[occasions1.fox$Site == "E2", c("noise250")] <- mean_np250[mean_np250$ID == "19", c("mean")]
occasions1.fox[occasions1.fox$Site == "F0", c("noise250")] <- mean_np250[mean_np250$ID == "20", c("mean")]
occasions1.fox[occasions1.fox$Site == "F1", c("noise250")] <- mean_np250[mean_np250$ID == "21", c("mean")]
occasions1.fox[occasions1.fox$Site == "F2", c("noise250")] <- mean_np250[mean_np250$ID == "22", c("mean")]
occasions1.fox[occasions1.fox$Site == "G0", c("noise250")] <- mean_np250[mean_np250$ID == "23", c("mean")]
occasions1.fox[occasions1.fox$Site == "G1", c("noise250")] <- mean_np250[mean_np250$ID == "24", c("mean")]
occasions1.fox[occasions1.fox$Site == "G2", c("noise250")] <- mean_np250[mean_np250$ID == "25", c("mean")]
occasions1.fox[occasions1.fox$Site == "H0", c("noise250")] <- mean_np250[mean_np250$ID == "26", c("mean")]
occasions1.fox[occasions1.fox$Site == "H1", c("noise250")] <- mean_np250[mean_np250$ID == "27", c("mean")]
occasions1.fox[occasions1.fox$Site == "H2", c("noise250")] <- mean_np250[mean_np250$ID == "28", c("mean")]
occasions1.fox[occasions1.fox$Site == "I0", c("noise250")] <- mean_np250[mean_np250$ID == "29", c("mean")]
occasions1.fox[occasions1.fox$Site == "I1", c("noise250")] <- mean_np250[mean_np250$ID == "30", c("mean")]
occasions1.fox[occasions1.fox$Site == "I2", c("noise250")] <- mean_np250[mean_np250$ID == "31", c("mean")]
occasions1.fox[occasions1.fox$Site == "J0", c("noise250")] <- mean_np250[mean_np250$ID == "32", c("mean")]
occasions1.fox[occasions1.fox$Site == "J1", c("noise250")] <- mean_np250[mean_np250$ID == "33", c("mean")]
occasions1.fox[occasions1.fox$Site == "J2", c("noise250")] <- mean_np250[mean_np250$ID == "34", c("mean")]
# position 2
occasions2.fox[occasions2.fox$Site == "A0", c("noise250")] <- mean_np250[mean_np250$ID == "35", c("mean")]
occasions2.fox[occasions2.fox$Site == "A1", c("noise250")] <- mean_np250[mean_np250$ID == "36", c("mean")]
occasions2.fox[occasions2.fox$Site == "A2", c("noise250")] <- mean_np250[mean_np250$ID == "37", c("mean")]
occasions2.fox[occasions2.fox$Site == "A2/2", c("noise250")] <- mean_np250[mean_np250$ID == "38", c("mean")]
occasions2.fox[occasions2.fox$Site == "B0", c("noise250")] <- mean_np250[mean_np250$ID == "39", c("mean")]
occasions2.fox[occasions2.fox$Site == "C1", c("noise250")] <- mean_np250[mean_np250$ID == "40", c("mean")]
occasions2.fox[occasions2.fox$Site == "C1/2", c("noise250")] <- mean_np250[mean_np250$ID == "41", c("mean")]
occasions2.fox[occasions2.fox$Site == "C0", c("noise250")] <- mean_np250[mean_np250$ID == "42", c("mean")]
occasions2.fox[occasions2.fox$Site == "C2", c("noise250")] <- mean_np250[mean_np250$ID == "43", c("mean")]
occasions2.fox[occasions2.fox$Site == "D1", c("noise250")] <- mean_np250[mean_np250$ID == "44", c("mean")]
occasions2.fox[occasions2.fox$Site == "D1/2", c("noise250")] <- mean_np250[mean_np250$ID == "45", c("mean")]
occasions2.fox[occasions2.fox$Site == "E1/2", c("noise250")] <- mean_np250[mean_np250$ID == "46", c("mean")]
occasions2.fox[occasions2.fox$Site == "E1", c("noise250")] <- mean_np250[mean_np250$ID == "47", c("mean")]
occasions2.fox[occasions2.fox$Site == "F2", c("noise250")] <- mean_np250[mean_np250$ID == "48", c("mean")]
occasions2.fox[occasions2.fox$Site == "G0", c("noise250")] <- mean_np250[mean_np250$ID == "49", c("mean")]
occasions2.fox[occasions2.fox$Site == "G2", c("noise250")] <- mean_np250[mean_np250$ID == "50", c("mean")]
occasions2.fox[occasions2.fox$Site == "E0/2", c("noise250")] <- mean_np250[mean_np250$ID == "51", c("mean")]
occasions2.fox[occasions2.fox$Site == "E0", c("noise250")] <- mean_np250[mean_np250$ID == "52", c("mean")]
occasions2.fox[occasions2.fox$Site == "D2", c("noise250")] <- mean_np250[mean_np250$ID == "53", c("mean")]
occasions2.fox[occasions2.fox$Site == "F0", c("noise250")] <- mean_np250[mean_np250$ID == "54", c("mean")]
occasions2.fox[occasions2.fox$Site == "E2", c("noise250")] <- mean_np250[mean_np250$ID == "55", c("mean")]
occasions2.fox[occasions2.fox$Site == "G1", c("noise250")] <- mean_np250[mean_np250$ID == "56", c("mean")]
occasions2.fox[occasions2.fox$Site == "J1", c("noise250")] <- mean_np250[mean_np250$ID == "57", c("mean")]
occasions2.fox[occasions2.fox$Site == "J0", c("noise250")] <- mean_np250[mean_np250$ID == "58", c("mean")]
occasions2.fox[occasions2.fox$Site == "B2", c("noise250")] <- mean_np250[mean_np250$ID == "59", c("mean")]
occasions2.fox[occasions2.fox$Site == "B2/2", c("noise250")] <- mean_np250[mean_np250$ID == "60", c("mean")]
occasions2.fox[occasions2.fox$Site == "F1", c("noise250")] <- mean_np250[mean_np250$ID == "61", c("mean")]
occasions2.fox[occasions2.fox$Site == "I0", c("noise250")] <- mean_np250[mean_np250$ID == "62", c("mean")]
occasions2.fox[occasions2.fox$Site == "I1", c("noise250")] <- mean_np250[mean_np250$ID == "63", c("mean")]
occasions2.fox[occasions2.fox$Site == "I2", c("noise250")] <- mean_np250[mean_np250$ID == "64", c("mean")]
occasions2.fox[occasions2.fox$Site == "H2", c("noise250")] <- mean_np250[mean_np250$ID == "65", c("mean")]
occasions2.fox[occasions2.fox$Site == "H0", c("noise250")] <- mean_np250[mean_np250$ID == "66", c("mean")]
occasions2.fox[occasions2.fox$Site == "H1", c("noise250")] <- mean_np250[mean_np250$ID == "67", c("mean")]
occasions2.fox[occasions2.fox$Site == "J2", c("noise250")] <- mean_np250[mean_np250$ID == "68", c("mean")]
### BUFFER 1km ###
# position 1
occasions1.fox[occasions1.fox$Site == "A0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "1", c("mean")]
occasions1.fox[occasions1.fox$Site == "A1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "2", c("mean")]
occasions1.fox[occasions1.fox$Site == "A2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "3", c("mean")]
occasions1.fox[occasions1.fox$Site == "A2/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "4", c("mean")]
occasions1.fox[occasions1.fox$Site == "B0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "5", c("mean")]
occasions1.fox[occasions1.fox$Site == "B2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "6", c("mean")]
occasions1.fox[occasions1.fox$Site == "B2/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "7", c("mean")]
occasions1.fox[occasions1.fox$Site == "C0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "8", c("mean")]
occasions1.fox[occasions1.fox$Site == "C1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "9", c("mean")]
occasions1.fox[occasions1.fox$Site == "C1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "10", c("mean")]
occasions1.fox[occasions1.fox$Site == "C2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "11", c("mean")]
occasions1.fox[occasions1.fox$Site == "D1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "12", c("mean")]
occasions1.fox[occasions1.fox$Site == "D1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "13", c("mean")]
occasions1.fox[occasions1.fox$Site == "D2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "14", c("mean")]
occasions1.fox[occasions1.fox$Site == "E0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "15", c("mean")]
occasions1.fox[occasions1.fox$Site == "E0/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "16", c("mean")]
occasions1.fox[occasions1.fox$Site == "E1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "17", c("mean")]
occasions1.fox[occasions1.fox$Site == "E1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "18", c("mean")]
occasions1.fox[occasions1.fox$Site == "E2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "19", c("mean")]
occasions1.fox[occasions1.fox$Site == "F0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "20", c("mean")]
occasions1.fox[occasions1.fox$Site == "F1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "21", c("mean")]
occasions1.fox[occasions1.fox$Site == "F2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "22", c("mean")]
occasions1.fox[occasions1.fox$Site == "G0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "23", c("mean")]
occasions1.fox[occasions1.fox$Site == "G1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "24", c("mean")]
occasions1.fox[occasions1.fox$Site == "G2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "25", c("mean")]
occasions1.fox[occasions1.fox$Site == "H0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "26", c("mean")]
occasions1.fox[occasions1.fox$Site == "H1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "27", c("mean")]
occasions1.fox[occasions1.fox$Site == "H2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "28", c("mean")]
occasions1.fox[occasions1.fox$Site == "I0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "29", c("mean")]
occasions1.fox[occasions1.fox$Site == "I1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "30", c("mean")]
occasions1.fox[occasions1.fox$Site == "I2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "31", c("mean")]
occasions1.fox[occasions1.fox$Site == "J0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "32", c("mean")]
occasions1.fox[occasions1.fox$Site == "J1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "33", c("mean")]
occasions1.fox[occasions1.fox$Site == "J2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "34", c("mean")]
# position 2
occasions2.fox[occasions2.fox$Site == "A0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "35", c("mean")]
occasions2.fox[occasions2.fox$Site == "A1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "36", c("mean")]
occasions2.fox[occasions2.fox$Site == "A2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "37", c("mean")]
occasions2.fox[occasions2.fox$Site == "A2/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "38", c("mean")]
occasions2.fox[occasions2.fox$Site == "B0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "39", c("mean")]
occasions2.fox[occasions2.fox$Site == "C1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "40", c("mean")]
occasions2.fox[occasions2.fox$Site == "C1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "41", c("mean")]
occasions2.fox[occasions2.fox$Site == "C0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "42", c("mean")]
occasions2.fox[occasions2.fox$Site == "C2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "43", c("mean")]
occasions2.fox[occasions2.fox$Site == "D1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "44", c("mean")]
occasions2.fox[occasions2.fox$Site == "D1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "45", c("mean")]
occasions2.fox[occasions2.fox$Site == "E1/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "46", c("mean")]
occasions2.fox[occasions2.fox$Site == "E1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "47", c("mean")]
occasions2.fox[occasions2.fox$Site == "F2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "48", c("mean")]
occasions2.fox[occasions2.fox$Site == "G0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "49", c("mean")]
occasions2.fox[occasions2.fox$Site == "G2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "50", c("mean")]
occasions2.fox[occasions2.fox$Site == "E0/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "51", c("mean")]
occasions2.fox[occasions2.fox$Site == "E0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "52", c("mean")]
occasions2.fox[occasions2.fox$Site == "D2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "53", c("mean")]
occasions2.fox[occasions2.fox$Site == "F0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "54", c("mean")]
occasions2.fox[occasions2.fox$Site == "E2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "55", c("mean")]
occasions2.fox[occasions2.fox$Site == "G1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "56", c("mean")]
occasions2.fox[occasions2.fox$Site == "J1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "57", c("mean")]
occasions2.fox[occasions2.fox$Site == "J0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "58", c("mean")]
occasions2.fox[occasions2.fox$Site == "B2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "59", c("mean")]
occasions2.fox[occasions2.fox$Site == "B2/2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "60", c("mean")]
occasions2.fox[occasions2.fox$Site == "F1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "61", c("mean")]
occasions2.fox[occasions2.fox$Site == "I0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "62", c("mean")]
occasions2.fox[occasions2.fox$Site == "I1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "63", c("mean")]
occasions2.fox[occasions2.fox$Site == "I2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "64", c("mean")]
occasions2.fox[occasions2.fox$Site == "H2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "65", c("mean")]
occasions2.fox[occasions2.fox$Site == "H0", c("noise1km")] <- mean_np1km[mean_np1km$ID == "66", c("mean")]
occasions2.fox[occasions2.fox$Site == "H1", c("noise1km")] <- mean_np1km[mean_np1km$ID == "67", c("mean")]
occasions2.fox[occasions2.fox$Site == "J2", c("noise1km")] <- mean_np1km[mean_np1km$ID == "68", c("mean")]
head(occasions1.fox)
head(occasions2.fox)
summary(occasions1.fox$noise1km)
summary(occasions2.fox$noise1km)
summary(occasions1.fox$noise250)
summary(occasions2.fox$noise250)


# final data frame
final.fo.df <- rbind(occasions1.fox, occasions2.fox)

# add site area
final.fo.df$area <- NA
final.fo.df[final.fo.df$Site == "A0", c("area")] <- REM[REM$site_ID == "A0", "area"]
final.fo.df[final.fo.df$Site == "A1", c("area")] <- REM[REM$site_ID == "A1", "area"]
final.fo.df[final.fo.df$Site == "A2", c("area")] <- REM[REM$site_ID == "A2", "area"]
final.fo.df[final.fo.df$Site == "A2/2", c("area")] <- REM[REM$site_ID == "A2", "area"]
final.fo.df[final.fo.df$Site == "B0", c("area")] <- REM[REM$site_ID == "B0", "area"]
final.fo.df[final.fo.df$Site == "B2", c("area")] <- REM[REM$site_ID == "B2", "area"]
final.fo.df[final.fo.df$Site == "B2/2", c("area")] <- REM[REM$site_ID == "B2", "area"]
final.fo.df[final.fo.df$Site == "C0", c("area")] <- REM[REM$site_ID == "C0", "area"]
final.fo.df[final.fo.df$Site == "C1", c("area")] <- REM[REM$site_ID == "C1", "area"]
final.fo.df[final.fo.df$Site == "C1/2", c("area")] <- REM[REM$site_ID == "C1", "area"]
final.fo.df[final.fo.df$Site == "C2", c("area")] <- REM[REM$site_ID == "C2", "area"]
final.fo.df[final.fo.df$Site == "D1", c("area")] <- REM[REM$site_ID == "D1", "area"]
final.fo.df[final.fo.df$Site == "D1/2", c("area")] <- REM[REM$site_ID == "D1", "area"]
final.fo.df[final.fo.df$Site == "D2", c("area")] <- REM[REM$site_ID == "D2", "area"]
final.fo.df[final.fo.df$Site == "E0", c("area")] <- REM[REM$site_ID == "E0", "area"]
final.fo.df[final.fo.df$Site == "E0/2", c("area")] <- REM[REM$site_ID == "E0", "area"]
final.fo.df[final.fo.df$Site == "E1", c("area")] <- REM[REM$site_ID == "E1", "area"]
final.fo.df[final.fo.df$Site == "E1/2", c("area")] <- REM[REM$site_ID == "E1", "area"]
final.fo.df[final.fo.df$Site == "E2", c("area")] <- REM[REM$site_ID == "E2", "area"]
final.fo.df[final.fo.df$Site == "F0", c("area")] <- REM[REM$site_ID == "F0", "area"]
final.fo.df[final.fo.df$Site == "F1", c("area")] <- REM[REM$site_ID == "F1", "area"]
final.fo.df[final.fo.df$Site == "F2", c("area")] <- REM[REM$site_ID == "F2", "area"]
final.fo.df[final.fo.df$Site == "G0", c("area")] <- REM[REM$site_ID == "G0", "area"]
final.fo.df[final.fo.df$Site == "G1", c("area")] <- REM[REM$site_ID == "G1", "area"]
final.fo.df[final.fo.df$Site == "G2", c("area")] <- REM[REM$site_ID == "G2", "area"]
final.fo.df[final.fo.df$Site == "H0", c("area")] <- REM[REM$site_ID == "H0", "area"]
final.fo.df[final.fo.df$Site == "H1", c("area")] <- REM[REM$site_ID == "H1", "area"]
final.fo.df[final.fo.df$Site == "H2", c("area")] <- REM[REM$site_ID == "H2", "area"]
final.fo.df[final.fo.df$Site == "I0", c("area")] <- REM[REM$site_ID == "I", "area"]
final.fo.df[final.fo.df$Site == "I1", c("area")] <- REM[REM$site_ID == "I", "area"]
final.fo.df[final.fo.df$Site == "I2", c("area")] <- REM[REM$site_ID == "I", "area"]
final.fo.df[final.fo.df$Site == "J0", c("area")] <- REM[REM$site_ID == "J", "area"]
final.fo.df[final.fo.df$Site == "J1", c("area")] <- REM[REM$site_ID == "J", "area"]
final.fo.df[final.fo.df$Site == "J2", c("area")] <- REM[REM$site_ID == "J", "area"]

summary(final.fo.df$area)

# export data frame csv
write.csv(final.fo.df, file = "~/GALLANT Technician/Camera Trap Analysis/fox_occupancy_df.csv")
