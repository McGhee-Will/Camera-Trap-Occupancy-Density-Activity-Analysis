# ACTIVITY MODELLING DATA FRAME

# Binary, per hour, presence/absence
# Environmental characteristics in buffer around camera
# Dog_RA per camera
# Light and noise stat at camera

library(dplyr)
library(grid)
library(gridExtra)
library(GLMMadaptive)
library(ggpubr)
library(mgcv)
library(tidyr)
library(lubridate)
library(lmtest)
library(activity)
library(overlap)
library(circular)
library(nimble)
library(brms)
library(forcats)
library(MESS)
library(suncalc)
library(grateful)

setwd("~/GALLANT Technician/Camera Trap Analysis")

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

# split into species
squirrel <- subset(mammal.data, mammal.data$ID_species == "squirrel")
fox <- subset(mammal.data, mammal.data$ID_species == "fox")
deer <- subset(mammal.data, mammal.data$ID_species == "deer")

# camera placement data
cam.data <- read.csv("cam_placement.csv")
cam.data$Date_setup <- as.POSIXct(cam.data$Date_setup, tryFormats = c("%d/%m/%Y"))
cam.data$Date_retr <- as.POSIXct(cam.data$Date_retr, tryFormats = c("%d/%m/%Y"))

# steal buffer info from occupancy data frames
occ.squ <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/squirrel_occupancy_df.csv")
occ.dee <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/deer_occupancy_df.csv")
occ.fox <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/fox_occupancy_df.csv")

################
### SQUIRREL ###
################

# make vector list for times squirrel was present at each site
occasions.sq <- vector("list", length = nrow(cam.data))
# loop through list, making rows for each hour of each day at each site
for (i in 1:nrow(cam.data)) {
  occasions.sq[[i]] <- data.frame(Session = cam.data$Session[i],
                                  Site = cam.data$Site[i],
                                  start = seq(from = ymd_hms(paste(cam.data$Date_setup[i], "00:00:00", sep = " ")),
                                              to = ymd_hms(paste(cam.data$Date_retr[i], "23:59:59", sep = " ")),
                                              by = '60 min'
                               )
  ) %>%
    mutate(end = c(start[2:length(start)],
                   start[length(start)] + minutes(60)
    ))
  
}
# separate list into data frame
occasions.sq <- do.call(rbind.data.frame, occasions.sq)
# add column for presence/absence
occasions.sq$capt <- 0
head(occasions.sq)

# loop through squirrel data and store '1' for presence and '0' for absence at date/time
for (i in 1:nrow(squirrel)) {
  occasions.sq[occasions.sq$Session == as.character(squirrel$placement[i])
               & occasions.sq$Site == as.character(squirrel$site[i])
               & occasions.sq$start <= squirrel$datetime[i]
               & occasions.sq$end > squirrel$datetime[i], "capt"] <- 1
}

table(occasions.sq$capt)

# add columns
occasions.sq$Time <- hour(occasions.sq$start)
occasions.sq$Site <- as.factor(occasions.sq$Site)
occasions.sq$Session <- as.factor(occasions.sq$Session)
occasions.sq$month <- as.factor(months(occasions.sq$start))
# land cover
occasions.sq$wood100 <- NA
occasions.sq$wet100 <- NA
occasions.sq$urban100 <- NA
occasions.sq$water100 <- NA
occasions.sq$grass100 <- NA
occasions.sq$arable100 <- NA
occasions.sq$wood400 <- NA
occasions.sq$wet400 <- NA
occasions.sq$urban400 <- NA
occasions.sq$water400 <- NA
occasions.sq$grass400 <- NA
occasions.sq$arable400 <- NA
# dog_RA
occasions.sq$dog_RA <- NA
# light and noise
occasions.sq$light100 <- NA
occasions.sq$light400 <- NA
occasions.sq$noise100 <- NA
occasions.sq$noise400 <- NA

# split both occasions.sq and sq occupancy into positions 1 and 2
occasions1.sq <- subset(occasions.sq, occasions.sq$Session == "1")
occasions2.sq <- subset(occasions.sq, occasions.sq$Session == "2")
occ1.squ <- subset(occ.squ, occ.squ$Session == "1")
occ2.squ <- subset(occ.squ, occ.squ$Session == "2")

# group occ.squ elements so only one row exists for each site
occ1.squ <- occ1.squ %>%
  group_by(Site) %>%
  summarise(# 100
    wood100 = mean(wood100),
    wet100 = mean(wet100),
    urban100 = mean(urban100),
    water100 = mean(water100),
    grass100 = mean(grass100),
    arable100 = mean(arable100),
    # 400
    wood400 = mean(wood400),
    wet400 = mean(wet400),
    urban400 = mean(urban400),
    water400 = mean(water400),
    grass400 = mean(grass400),
    arable400 = mean(arable400),
    # dog_RA
    dog_RA = mean(dog_RA),
    # light and noise
    light100 = mean(light100),
    light400 = mean(light400),
    noise100 = mean(noise100),
    noise400 = mean(noise400))

occ2.squ <- occ2.squ %>%
  group_by(Site) %>%
  summarise(# 100
    wood100 = mean(wood100),
    wet100 = mean(wet100),
    urban100 = mean(urban100),
    water100 = mean(water100),
    grass100 = mean(grass100),
    arable100 = mean(arable100),
    # 400
    wood400 = mean(wood400),
    wet400 = mean(wet400),
    urban400 = mean(urban400),
    water400 = mean(water400),
    grass400 = mean(grass400),
    arable400 = mean(arable400),
    # dog_RA
    dog_RA = mean(dog_RA),
    # light and noise
    light100 = mean(light100),
    light400 = mean(light400),
    noise100 = mean(noise100),
    noise400 = mean(noise400))

# select columns to modify
sq.cols <- names(occasions.sq[8:24])
# add env covariates for first placement
occasions1.sq[occasions1.sq$Site == "A0", sq.cols] <- occ1.squ[occ1.squ$Site == "A0", sq.cols]
occasions1.sq[occasions1.sq$Site == "A1", sq.cols] <- occ1.squ[occ1.squ$Site == "A1", sq.cols]
occasions1.sq[occasions1.sq$Site == "A2", sq.cols] <- occ1.squ[occ1.squ$Site == "A2", sq.cols]
occasions1.sq[occasions1.sq$Site == "A2/2", sq.cols] <- occ1.squ[occ1.squ$Site == "A2/2", sq.cols]
occasions1.sq[occasions1.sq$Site == "B0", sq.cols] <- occ1.squ[occ1.squ$Site == "B0", sq.cols]
occasions1.sq[occasions1.sq$Site == "B2", sq.cols] <- occ1.squ[occ1.squ$Site == "B2", sq.cols]
occasions1.sq[occasions1.sq$Site == "B2/2", sq.cols] <- occ1.squ[occ1.squ$Site == "B2/2", sq.cols]
occasions1.sq[occasions1.sq$Site == "C0", sq.cols] <- occ1.squ[occ1.squ$Site == "C0", sq.cols]
occasions1.sq[occasions1.sq$Site == "C1", sq.cols] <- occ1.squ[occ1.squ$Site == "C1", sq.cols]
occasions1.sq[occasions1.sq$Site == "C1/2", sq.cols] <- occ1.squ[occ1.squ$Site == "C1/2", sq.cols]
occasions1.sq[occasions1.sq$Site == "C2", sq.cols] <- occ1.squ[occ1.squ$Site == "C2", sq.cols]
occasions1.sq[occasions1.sq$Site == "D1", sq.cols] <- occ1.squ[occ1.squ$Site == "D1", sq.cols]
occasions1.sq[occasions1.sq$Site == "D1/2", sq.cols] <- occ1.squ[occ1.squ$Site == "D1/2", sq.cols]
occasions1.sq[occasions1.sq$Site == "D2", sq.cols] <- occ1.squ[occ1.squ$Site == "D2", sq.cols]
# occasions1.sq[occasions1.sq$Site == "E0", sq.cols] <- occ1.squ[occ1.squ$Site == "E0", sq.cols] camera wasn't there
occasions1.sq[occasions1.sq$Site == "E0/2", sq.cols] <- occ1.squ[occ1.squ$Site == "E0/2", sq.cols]
occasions1.sq[occasions1.sq$Site == "E1", sq.cols] <- occ1.squ[occ1.squ$Site == "E1", sq.cols]
occasions1.sq[occasions1.sq$Site == "E1/2", sq.cols] <- occ1.squ[occ1.squ$Site == "E1/2", sq.cols]
occasions1.sq[occasions1.sq$Site == "E2", sq.cols] <- occ1.squ[occ1.squ$Site == "E2", sq.cols]
occasions1.sq[occasions1.sq$Site == "F0", sq.cols] <- occ1.squ[occ1.squ$Site == "F0", sq.cols]
occasions1.sq[occasions1.sq$Site == "F1", sq.cols] <- occ1.squ[occ1.squ$Site == "F1", sq.cols]
occasions1.sq[occasions1.sq$Site == "F2", sq.cols] <- occ1.squ[occ1.squ$Site == "F2", sq.cols]
occasions1.sq[occasions1.sq$Site == "G0", sq.cols] <- occ1.squ[occ1.squ$Site == "G0", sq.cols]
occasions1.sq[occasions1.sq$Site == "G1", sq.cols] <- occ1.squ[occ1.squ$Site == "G1", sq.cols]
occasions1.sq[occasions1.sq$Site == "G2", sq.cols] <- occ1.squ[occ1.squ$Site == "G2", sq.cols]
occasions1.sq[occasions1.sq$Site == "H0", sq.cols] <- occ1.squ[occ1.squ$Site == "H0", sq.cols]
occasions1.sq[occasions1.sq$Site == "H1", sq.cols] <- occ1.squ[occ1.squ$Site == "H1", sq.cols]
occasions1.sq[occasions1.sq$Site == "H2", sq.cols] <- occ1.squ[occ1.squ$Site == "H2", sq.cols]
occasions1.sq[occasions1.sq$Site == "I0", sq.cols] <- occ1.squ[occ1.squ$Site == "I0", sq.cols]
occasions1.sq[occasions1.sq$Site == "I1", sq.cols] <- occ1.squ[occ1.squ$Site == "I1", sq.cols]
occasions1.sq[occasions1.sq$Site == "I2", sq.cols] <- occ1.squ[occ1.squ$Site == "I2", sq.cols]
occasions1.sq[occasions1.sq$Site == "J0", sq.cols] <- occ1.squ[occ1.squ$Site == "J0", sq.cols]
occasions1.sq[occasions1.sq$Site == "J1", sq.cols] <- occ1.squ[occ1.squ$Site == "J1", sq.cols]
occasions1.sq[occasions1.sq$Site == "J2", sq.cols] <- occ1.squ[occ1.squ$Site == "J2", sq.cols]
# add env covariates for second placement
occasions2.sq[occasions2.sq$Site == "A0", sq.cols] <- occ2.squ[occ2.squ$Site == "A0", sq.cols]
occasions2.sq[occasions2.sq$Site == "A1", sq.cols] <- occ2.squ[occ2.squ$Site == "A1", sq.cols]
occasions2.sq[occasions2.sq$Site == "A2", sq.cols] <- occ2.squ[occ2.squ$Site == "A2", sq.cols]
occasions2.sq[occasions2.sq$Site == "A2/2", sq.cols] <- occ2.squ[occ2.squ$Site == "A2/2", sq.cols]
occasions2.sq[occasions2.sq$Site == "B0", sq.cols] <- occ2.squ[occ2.squ$Site == "B0", sq.cols]
occasions2.sq[occasions2.sq$Site == "B2", sq.cols] <- occ2.squ[occ2.squ$Site == "B2", sq.cols]
occasions2.sq[occasions2.sq$Site == "B2/2", sq.cols] <- occ2.squ[occ2.squ$Site == "B2/2", sq.cols]
occasions2.sq[occasions2.sq$Site == "C0", sq.cols] <- occ2.squ[occ2.squ$Site == "C0", sq.cols]
occasions2.sq[occasions2.sq$Site == "C1", sq.cols] <- occ2.squ[occ2.squ$Site == "C1", sq.cols]
occasions2.sq[occasions2.sq$Site == "C1/2", sq.cols] <- occ2.squ[occ2.squ$Site == "C1/2", sq.cols]
occasions2.sq[occasions2.sq$Site == "C2", sq.cols] <- occ2.squ[occ2.squ$Site == "C2", sq.cols]
occasions2.sq[occasions2.sq$Site == "D1", sq.cols] <- occ2.squ[occ2.squ$Site == "D1", sq.cols]
occasions2.sq[occasions2.sq$Site == "D1/2", sq.cols] <- occ2.squ[occ2.squ$Site == "D1/2", sq.cols]
occasions2.sq[occasions2.sq$Site == "D2", sq.cols] <- occ2.squ[occ2.squ$Site == "D2", sq.cols]
occasions2.sq[occasions2.sq$Site == "E0", sq.cols] <- occ2.squ[occ2.squ$Site == "E0", sq.cols]
occasions2.sq[occasions2.sq$Site == "E0/2", sq.cols] <- occ2.squ[occ2.squ$Site == "E0/2", sq.cols]
occasions2.sq[occasions2.sq$Site == "E1", sq.cols] <- occ2.squ[occ2.squ$Site == "E1", sq.cols]
occasions2.sq[occasions2.sq$Site == "E1/2", sq.cols] <- occ2.squ[occ2.squ$Site == "E1/2", sq.cols]
occasions2.sq[occasions2.sq$Site == "E2", sq.cols] <- occ2.squ[occ2.squ$Site == "E2", sq.cols]
occasions2.sq[occasions2.sq$Site == "F0", sq.cols] <- occ2.squ[occ2.squ$Site == "F0", sq.cols]
occasions2.sq[occasions2.sq$Site == "F1", sq.cols] <- occ2.squ[occ2.squ$Site == "F1", sq.cols]
occasions2.sq[occasions2.sq$Site == "F2", sq.cols] <- occ2.squ[occ2.squ$Site == "F2", sq.cols]
occasions2.sq[occasions2.sq$Site == "G0", sq.cols] <- occ2.squ[occ2.squ$Site == "G0", sq.cols]
occasions2.sq[occasions2.sq$Site == "G1", sq.cols] <- occ2.squ[occ2.squ$Site == "G1", sq.cols]
occasions2.sq[occasions2.sq$Site == "G2", sq.cols] <- occ2.squ[occ2.squ$Site == "G2", sq.cols]
occasions2.sq[occasions2.sq$Site == "H0", sq.cols] <- occ2.squ[occ2.squ$Site == "H0", sq.cols]
occasions2.sq[occasions2.sq$Site == "H1", sq.cols] <- occ2.squ[occ2.squ$Site == "H1", sq.cols]
occasions2.sq[occasions2.sq$Site == "H2", sq.cols] <- occ2.squ[occ2.squ$Site == "H2", sq.cols]
occasions2.sq[occasions2.sq$Site == "I0", sq.cols] <- occ2.squ[occ2.squ$Site == "I0", sq.cols]
occasions2.sq[occasions2.sq$Site == "I1", sq.cols] <- occ2.squ[occ2.squ$Site == "I1", sq.cols]
occasions2.sq[occasions2.sq$Site == "I2", sq.cols] <- occ2.squ[occ2.squ$Site == "I2", sq.cols]
occasions2.sq[occasions2.sq$Site == "J0", sq.cols] <- occ2.squ[occ2.squ$Site == "J0", sq.cols]
occasions2.sq[occasions2.sq$Site == "J1", sq.cols] <- occ2.squ[occ2.squ$Site == "J1", sq.cols]
occasions2.sq[occasions2.sq$Site == "J2", sq.cols] <- occ2.squ[occ2.squ$Site == "J2", sq.cols]

head(occasions1.sq)
head(occasions2.sq)
summary(occasions1.sq$grass100)
# remove NAs for E0
occasions1.sq <- na.omit(occasions1.sq)

# bind to one final data frame
occasions.sq <- rbind(occasions1.sq, occasions2.sq)

# area column
occasions.sq$area <- NA
occasions.sq$area <- occ.squ$area[match(occasions.sq$Site, occ.squ$Site)] # WISH I KNEW ABOUT THIS FUNCTION EARLIER
summary(occasions.sq$area)

write.csv(occasions.sq, "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/squ_activity_df.csv")

############
### DEER ###
############

# make vector list for times squirrel was present at each site
occasions.de <- vector("list", length = nrow(cam.data))
# loop through list, making rows for each hour of each day at each site
for (i in 1:nrow(cam.data)) {
  occasions.de[[i]] <- data.frame(Session = cam.data$Session[i],
                                  Site = cam.data$Site[i],
                                  start = seq(from = ymd_hms(paste(cam.data$Date_setup[i], "00:00:00", sep = " ")),
                                              to = ymd_hms(paste(cam.data$Date_retr[i], "23:59:59", sep = " ")),
                                              by = '60 min'
                                  )
  ) %>%
    mutate(end = c(start[2:length(start)],
                   start[length(start)] + minutes(60)
    ))
  
}

# separate list into data frame
occasions.de <- do.call(rbind.data.frame, occasions.de)
# add column for presence/absence
occasions.de$capt <- 0
head(occasions.de)

# loop through fox data and store '1' for presence and '0' for absence at date/time
for (i in 1:nrow(deer)) {
  occasions.de[occasions.de$Session == as.character(deer$placement[i])
               & occasions.de$Site == as.character(deer$site[i])
               & occasions.de$start <= deer$datetime[i]
               & occasions.de$end > deer$datetime[i], "capt"] <- 1
}

table(occasions.de$capt)

# add columns
occasions.de$Time <- hour(occasions.de$start)
occasions.de$Site <- as.factor(occasions.de$Site)
occasions.de$Session <- as.factor(occasions.de$Session)
occasions.de$month <- as.factor(months(occasions.de$start))
# land cover
occasions.de$wood250 <- NA
occasions.de$wet250 <- NA
occasions.de$urban250 <- NA
occasions.de$water250 <- NA
occasions.de$grass250 <- NA
occasions.de$arable250 <- NA
occasions.de$wood1km <- NA
occasions.de$wet1km <- NA
occasions.de$urban1km <- NA
occasions.de$water1km <- NA
occasions.de$grass1km <- NA
occasions.de$arable1km <- NA
# dog_RA
occasions.de$dog_RA <- NA
# light and noise
occasions.de$light250 <- NA
occasions.de$light1km <- NA
occasions.de$noise250 <- NA
occasions.de$noise1km <- NA

# split both occasions.de and deer occupancy into positions 1 and 2
occasions1.de <- subset(occasions.de, occasions.de$Session == "1")
occasions2.de <- subset(occasions.de, occasions.de$Session == "2")
occ1.dee <- subset(occ.dee, occ.dee$Session == "1")
occ2.dee <- subset(occ.dee, occ.dee$Session == "2")

# group occ.squ elements so only one row exists for each site
occ1.dee <- occ1.dee %>%
  group_by(Site) %>%
  summarise(# 250
    wood250 = mean(wood250),
    wet250 = mean(wet250),
    urban250 = mean(urban250),
    water250 = mean(water250),
    grass250 = mean(grass250),
    arable250 = mean(arable250),
    # 1km
    wood1km = mean(wood1km),
    wet1km = mean(wet1km),
    urban1km = mean(urban1km),
    water1km = mean(water1km),
    grass1km = mean(grass1km),
    arable1km = mean(arable1km),
    # dog_RA
    dog_RA = mean(dog_RA),
    # light and noise
    light250 = mean(light250),
    light1km = mean(light1km),
    noise250 = mean(noise250),
    noise1km = mean(noise1km))

occ2.dee <- occ2.dee %>%
  group_by(Site) %>%
  summarise(# 250
    wood250 = mean(wood250),
    wet250 = mean(wet250),
    urban250 = mean(urban250),
    water250 = mean(water250),
    grass250 = mean(grass250),
    arable250 = mean(arable250),
    # 1km
    wood1km = mean(wood1km),
    wet1km = mean(wet1km),
    urban1km = mean(urban1km),
    water1km = mean(water1km),
    grass1km = mean(grass1km),
    arable1km = mean(arable1km),
    # dog_RA
    dog_RA = mean(dog_RA),
    # light and noise
    light250 = mean(light250),
    light1km = mean(light1km),
    noise250 = mean(noise250),
    noise1km = mean(noise1km))

# select columns to modify
de.cols <- names(occasions.de[8:24])
# add env covariates for first placement
occasions1.de[occasions1.de$Site == "A0", de.cols] <- occ1.dee[occ1.dee$Site == "A0", de.cols]
occasions1.de[occasions1.de$Site == "A1", de.cols] <- occ1.dee[occ1.dee$Site == "A1", de.cols]
occasions1.de[occasions1.de$Site == "A2", de.cols] <- occ1.dee[occ1.dee$Site == "A2", de.cols]
occasions1.de[occasions1.de$Site == "A2/2", de.cols] <- occ1.dee[occ1.dee$Site == "A2/2", de.cols]
occasions1.de[occasions1.de$Site == "B0", de.cols] <- occ1.dee[occ1.dee$Site == "B0", de.cols]
occasions1.de[occasions1.de$Site == "B2", de.cols] <- occ1.dee[occ1.dee$Site == "B2", de.cols]
occasions1.de[occasions1.de$Site == "B2/2", de.cols] <- occ1.dee[occ1.dee$Site == "B2/2", de.cols]
occasions1.de[occasions1.de$Site == "C0", de.cols] <- occ1.dee[occ1.dee$Site == "C0", de.cols]
occasions1.de[occasions1.de$Site == "C1", de.cols] <- occ1.dee[occ1.dee$Site == "C1", de.cols]
occasions1.de[occasions1.de$Site == "C1/2", de.cols] <- occ1.dee[occ1.dee$Site == "C1/2", de.cols]
occasions1.de[occasions1.de$Site == "C2", de.cols] <- occ1.dee[occ1.dee$Site == "C2", de.cols]
occasions1.de[occasions1.de$Site == "D1", de.cols] <- occ1.dee[occ1.dee$Site == "D1", de.cols]
occasions1.de[occasions1.de$Site == "D1/2", de.cols] <- occ1.dee[occ1.dee$Site == "D1/2", de.cols]
occasions1.de[occasions1.de$Site == "D2", de.cols] <- occ1.dee[occ1.dee$Site == "D2", de.cols]
#occasions1.de[occasions1.de$Site == "E0", de.cols] <- occ1.dee[occ1.dee$Site == "E0", de.cols] #camera wasn't there
occasions1.de[occasions1.de$Site == "E0/2", de.cols] <- occ1.dee[occ1.dee$Site == "E0/2", de.cols]
occasions1.de[occasions1.de$Site == "E1", de.cols] <- occ1.dee[occ1.dee$Site == "E1", de.cols]
occasions1.de[occasions1.de$Site == "E1/2", de.cols] <- occ1.dee[occ1.dee$Site == "E1/2", de.cols]
occasions1.de[occasions1.de$Site == "E2", de.cols] <- occ1.dee[occ1.dee$Site == "E2", de.cols]
occasions1.de[occasions1.de$Site == "F0", de.cols] <- occ1.dee[occ1.dee$Site == "F0", de.cols]
occasions1.de[occasions1.de$Site == "F1", de.cols] <- occ1.dee[occ1.dee$Site == "F1", de.cols]
occasions1.de[occasions1.de$Site == "F2", de.cols] <- occ1.dee[occ1.dee$Site == "F2", de.cols]
occasions1.de[occasions1.de$Site == "G0", de.cols] <- occ1.dee[occ1.dee$Site == "G0", de.cols]
occasions1.de[occasions1.de$Site == "G1", de.cols] <- occ1.dee[occ1.dee$Site == "G1", de.cols]
occasions1.de[occasions1.de$Site == "G2", de.cols] <- occ1.dee[occ1.dee$Site == "G2", de.cols]
occasions1.de[occasions1.de$Site == "H0", de.cols] <- occ1.dee[occ1.dee$Site == "H0", de.cols]
occasions1.de[occasions1.de$Site == "H1", de.cols] <- occ1.dee[occ1.dee$Site == "H1", de.cols]
occasions1.de[occasions1.de$Site == "H2", de.cols] <- occ1.dee[occ1.dee$Site == "H2", de.cols]
occasions1.de[occasions1.de$Site == "I0", de.cols] <- occ1.dee[occ1.dee$Site == "I0", de.cols]
occasions1.de[occasions1.de$Site == "I1", de.cols] <- occ1.dee[occ1.dee$Site == "I1", de.cols]
occasions1.de[occasions1.de$Site == "I2", de.cols] <- occ1.dee[occ1.dee$Site == "I2", de.cols]
occasions1.de[occasions1.de$Site == "J0", de.cols] <- occ1.dee[occ1.dee$Site == "J0", de.cols]
occasions1.de[occasions1.de$Site == "J1", de.cols] <- occ1.dee[occ1.dee$Site == "J1", de.cols]
occasions1.de[occasions1.de$Site == "J2", de.cols] <- occ1.dee[occ1.dee$Site == "J2", de.cols]
# add env covariates for second placement
occasions2.de[occasions2.de$Site == "A0", de.cols] <- occ2.dee[occ2.dee$Site == "A0", de.cols]
occasions2.de[occasions2.de$Site == "A1", de.cols] <- occ2.dee[occ2.dee$Site == "A1", de.cols]
occasions2.de[occasions2.de$Site == "A2", de.cols] <- occ2.dee[occ2.dee$Site == "A2", de.cols]
occasions2.de[occasions2.de$Site == "A2/2", de.cols] <- occ2.dee[occ2.dee$Site == "A2/2", de.cols]
occasions2.de[occasions2.de$Site == "B0", de.cols] <- occ2.dee[occ2.dee$Site == "B0", de.cols]
occasions2.de[occasions2.de$Site == "B2", de.cols] <- occ2.dee[occ2.dee$Site == "B2", de.cols]
occasions2.de[occasions2.de$Site == "B2/2", de.cols] <- occ2.dee[occ2.dee$Site == "B2/2", de.cols]
occasions2.de[occasions2.de$Site == "C0", de.cols] <- occ2.dee[occ2.dee$Site == "C0", de.cols]
occasions2.de[occasions2.de$Site == "C1", de.cols] <- occ2.dee[occ2.dee$Site == "C1", de.cols]
occasions2.de[occasions2.de$Site == "C1/2", de.cols] <- occ2.dee[occ2.dee$Site == "C1/2", de.cols]
occasions2.de[occasions2.de$Site == "C2", de.cols] <- occ2.dee[occ2.dee$Site == "C2", de.cols]
occasions2.de[occasions2.de$Site == "D1", de.cols] <- occ2.dee[occ2.dee$Site == "D1", de.cols]
occasions2.de[occasions2.de$Site == "D1/2", de.cols] <- occ2.dee[occ2.dee$Site == "D1/2", de.cols]
occasions2.de[occasions2.de$Site == "D2", de.cols] <- occ2.dee[occ2.dee$Site == "D2", de.cols]
occasions2.de[occasions2.de$Site == "E0", de.cols] <- occ2.dee[occ2.dee$Site == "E0", de.cols]
occasions2.de[occasions2.de$Site == "E0/2", de.cols] <- occ2.dee[occ2.dee$Site == "E0/2", de.cols]
occasions2.de[occasions2.de$Site == "E1", de.cols] <- occ2.dee[occ2.dee$Site == "E1", de.cols]
occasions2.de[occasions2.de$Site == "E1/2", de.cols] <- occ2.dee[occ2.dee$Site == "E1/2", de.cols]
occasions2.de[occasions2.de$Site == "E2", de.cols] <- occ2.dee[occ2.dee$Site == "E2", de.cols]
occasions2.de[occasions2.de$Site == "F0", de.cols] <- occ2.dee[occ2.dee$Site == "F0", de.cols]
occasions2.de[occasions2.de$Site == "F1", de.cols] <- occ2.dee[occ2.dee$Site == "F1", de.cols]
occasions2.de[occasions2.de$Site == "F2", de.cols] <- occ2.dee[occ2.dee$Site == "F2", de.cols]
occasions2.de[occasions2.de$Site == "G0", de.cols] <- occ2.dee[occ2.dee$Site == "G0", de.cols]
occasions2.de[occasions2.de$Site == "G1", de.cols] <- occ2.dee[occ2.dee$Site == "G1", de.cols]
occasions2.de[occasions2.de$Site == "G2", de.cols] <- occ2.dee[occ2.dee$Site == "G2", de.cols]
occasions2.de[occasions2.de$Site == "H0", de.cols] <- occ2.dee[occ2.dee$Site == "H0", de.cols]
occasions2.de[occasions2.de$Site == "H1", de.cols] <- occ2.dee[occ2.dee$Site == "H1", de.cols]
occasions2.de[occasions2.de$Site == "H2", de.cols] <- occ2.dee[occ2.dee$Site == "H2", de.cols]
occasions2.de[occasions2.de$Site == "I0", de.cols] <- occ2.dee[occ2.dee$Site == "I0", de.cols]
occasions2.de[occasions2.de$Site == "I1", de.cols] <- occ2.dee[occ2.dee$Site == "I1", de.cols]
occasions2.de[occasions2.de$Site == "I2", de.cols] <- occ2.dee[occ2.dee$Site == "I2", de.cols]
occasions2.de[occasions2.de$Site == "J0", de.cols] <- occ2.dee[occ2.dee$Site == "J0", de.cols]
occasions2.de[occasions2.de$Site == "J1", de.cols] <- occ2.dee[occ2.dee$Site == "J1", de.cols]
occasions2.de[occasions2.de$Site == "J2", de.cols] <- occ2.dee[occ2.dee$Site == "J2", de.cols]

head(occasions1.de)
head(occasions2.de)
summary(occasions1.de$grass250)
subset(occasions1.de, is.na(grass250))
# remove NAs at E0 (camera didn't last)
occasions1.de <- na.omit(occasions1.de)
summary(occasions2.de$grass250)
unique(occ2.dee$Site)


# bind to one final data frame
occasions.de <- rbind(occasions1.de, occasions2.de)

# area column
occasions.de$area <- NA
occasions.de$area <- occ.dee$area[match(occasions.de$Site, occ.dee$Site)]
summary(occasions.de$area)

write.csv(occasions.de, "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/deer_activity_df.csv")

###########
### FOX ###
###########

# make vector list for times squirrel was present at each site
occasions.fo <- vector("list", length = nrow(cam.data))
# loop through list, making rows for each hour of each day at each site
for (i in 1:nrow(cam.data)) {
  occasions.fo[[i]] <- data.frame(Session = cam.data$Session[i],
                                  Site = cam.data$Site[i],
                                  start = seq(from = ymd_hms(paste(cam.data$Date_setup[i], "00:00:00", sep = " ")),
                                              to = ymd_hms(paste(cam.data$Date_retr[i], "23:59:59", sep = " ")),
                                              by = '60 min'
                                  )
  ) %>%
    mutate(end = c(start[2:length(start)],
                   start[length(start)] + minutes(60)
    ))
  
}
# separate list into data frame
occasions.fo <- do.call(rbind.data.frame, occasions.fo)
# add column for presence/absence
occasions.fo$capt <- 0
head(occasions.fo)

# loop through fox data and store '1' for presence and '0' for absence at date/time
for (i in 1:nrow(fox)) {
  occasions.fo[occasions.fo$Session == as.character(fox$placement[i])
               & occasions.fo$Site == as.character(fox$site[i])
               & occasions.fo$start <= fox$datetime[i]
               & occasions.fo$end > fox$datetime[i], "capt"] <- 1
}

table(occasions.fo$capt)

# add columns
occasions.fo$Time <- hour(occasions.fo$start)
occasions.fo$Site <- as.factor(occasions.fo$Site)
occasions.fo$Session <- as.factor(occasions.fo$Session)
occasions.fo$month <- as.factor(months(occasions.fo$start))
# land cover
occasions.fo$wood250 <- NA
occasions.fo$wet250 <- NA
occasions.fo$urban250 <- NA
occasions.fo$water250 <- NA
occasions.fo$grass250 <- NA
occasions.fo$arable250 <- NA
occasions.fo$wood1km <- NA
occasions.fo$wet1km <- NA
occasions.fo$urban1km <- NA
occasions.fo$water1km <- NA
occasions.fo$grass1km <- NA
occasions.fo$arable1km <- NA
# dog_RA
occasions.fo$dog_RA <- NA
# light and noise
occasions.fo$light250 <- NA
occasions.fo$light1km <- NA
occasions.fo$noise250 <- NA
occasions.fo$noise1km <- NA

# split both occasions.sq and sq occupancy into positions 1 and 2
occasions1.fo <- subset(occasions.fo, occasions.fo$Session == "1")
occasions2.fo <- subset(occasions.fo, occasions.fo$Session == "2")
occ1.fox <- subset(occ.fox, occ.fox$Session == "1")
occ2.fox <- subset(occ.fox, occ.fox$Session == "2")

# group occ.squ elements so only one row exists for each site
occ1.fox <- occ1.fox %>%
  group_by(Site) %>%
  summarise(# 250
    wood250 = mean(wood250),
    wet250 = mean(wet250),
    urban250 = mean(urban250),
    water250 = mean(water250),
    grass250 = mean(grass250),
    arable250 = mean(arable250),
    # 1km
    wood1km = mean(wood1km),
    wet1km = mean(wet1km),
    urban1km = mean(urban1km),
    water1km = mean(water1km),
    grass1km = mean(grass1km),
    arable1km = mean(arable1km),
    # dog_RA
    dog_RA = mean(dog_RA),
    # light and noise
    light250 = mean(light1km),
    light1km = mean(light1km),
    noise250 = mean(noise250),
    noise1km = mean(noise1km))

occ2.fox <- occ2.fox %>%
  group_by(Site) %>%
  summarise(# 250
    wood250 = mean(wood250),
    wet250 = mean(wet250),
    urban250 = mean(urban250),
    water250 = mean(water250),
    grass250 = mean(grass250),
    arable250 = mean(arable250),
    # 1km
    wood1km = mean(wood1km),
    wet1km = mean(wet1km),
    urban1km = mean(urban1km),
    water1km = mean(water1km),
    grass1km = mean(grass1km),
    arable1km = mean(arable1km),
    # dog_RA
    dog_RA = mean(dog_RA),
    # light and noise
    light250 = mean(light250),
    light1km = mean(light1km),
    noise250 = mean(noise250),
    noise1km = mean(noise1km))

# select columns to modify
fo.cols <- names(occasions.fo[8:24])
# add env covariates for first placement
occasions1.fo[occasions1.fo$Site == "A0", fo.cols] <- occ1.fox[occ1.fox$Site == "A0", fo.cols]
occasions1.fo[occasions1.fo$Site == "A1", fo.cols] <- occ1.fox[occ1.fox$Site == "A1", fo.cols]
occasions1.fo[occasions1.fo$Site == "A2", fo.cols] <- occ1.fox[occ1.fox$Site == "A2", fo.cols]
occasions1.fo[occasions1.fo$Site == "A2/2", fo.cols] <- occ1.fox[occ1.fox$Site == "A2/2", fo.cols]
occasions1.fo[occasions1.fo$Site == "B0", fo.cols] <- occ1.fox[occ1.fox$Site == "B0", fo.cols]
occasions1.fo[occasions1.fo$Site == "B2", fo.cols] <- occ1.fox[occ1.fox$Site == "B2", fo.cols]
occasions1.fo[occasions1.fo$Site == "B2/2", fo.cols] <- occ1.fox[occ1.fox$Site == "B2/2", fo.cols]
occasions1.fo[occasions1.fo$Site == "C0", fo.cols] <- occ1.fox[occ1.fox$Site == "C0", fo.cols]
occasions1.fo[occasions1.fo$Site == "C1", fo.cols] <- occ1.fox[occ1.fox$Site == "C1", fo.cols]
occasions1.fo[occasions1.fo$Site == "C1/2", fo.cols] <- occ1.fox[occ1.fox$Site == "C1/2", fo.cols]
occasions1.fo[occasions1.fo$Site == "C2", fo.cols] <- occ1.fox[occ1.fox$Site == "C2", fo.cols]
occasions1.fo[occasions1.fo$Site == "D1", fo.cols] <- occ1.fox[occ1.fox$Site == "D1", fo.cols]
occasions1.fo[occasions1.fo$Site == "D1/2", fo.cols] <- occ1.fox[occ1.fox$Site == "D1/2", fo.cols]
occasions1.fo[occasions1.fo$Site == "D2", fo.cols] <- occ1.fox[occ1.fox$Site == "D2", fo.cols]
# occasions1.fo[occasions1.fo$Site == "E0", fo.cols] <- occ1.fox[occ1.fox$Site == "E0", fo.cols] camera wasn't there
occasions1.fo[occasions1.fo$Site == "E0/2", fo.cols] <- occ1.fox[occ1.fox$Site == "E0/2", fo.cols]
occasions1.fo[occasions1.fo$Site == "E1", fo.cols] <- occ1.fox[occ1.fox$Site == "E1", fo.cols]
occasions1.fo[occasions1.fo$Site == "E1/2", fo.cols] <- occ1.fox[occ1.fox$Site == "E1/2", fo.cols]
occasions1.fo[occasions1.fo$Site == "E2", fo.cols] <- occ1.fox[occ1.fox$Site == "E2", fo.cols]
occasions1.fo[occasions1.fo$Site == "F0", fo.cols] <- occ1.fox[occ1.fox$Site == "F0", fo.cols]
occasions1.fo[occasions1.fo$Site == "F1", fo.cols] <- occ1.fox[occ1.fox$Site == "F1", fo.cols]
occasions1.fo[occasions1.fo$Site == "F2", fo.cols] <- occ1.fox[occ1.fox$Site == "F2", fo.cols]
occasions1.fo[occasions1.fo$Site == "G0", fo.cols] <- occ1.fox[occ1.fox$Site == "G0", fo.cols]
occasions1.fo[occasions1.fo$Site == "G1", fo.cols] <- occ1.fox[occ1.fox$Site == "G1", fo.cols]
occasions1.fo[occasions1.fo$Site == "G2", fo.cols] <- occ1.fox[occ1.fox$Site == "G2", fo.cols]
occasions1.fo[occasions1.fo$Site == "H0", fo.cols] <- occ1.fox[occ1.fox$Site == "H0", fo.cols]
occasions1.fo[occasions1.fo$Site == "H1", fo.cols] <- occ1.fox[occ1.fox$Site == "H1", fo.cols]
occasions1.fo[occasions1.fo$Site == "H2", fo.cols] <- occ1.fox[occ1.fox$Site == "H2", fo.cols]
occasions1.fo[occasions1.fo$Site == "I0", fo.cols] <- occ1.fox[occ1.fox$Site == "I0", fo.cols]
occasions1.fo[occasions1.fo$Site == "I1", fo.cols] <- occ1.fox[occ1.fox$Site == "I1", fo.cols]
occasions1.fo[occasions1.fo$Site == "I2", fo.cols] <- occ1.fox[occ1.fox$Site == "I2", fo.cols]
occasions1.fo[occasions1.fo$Site == "J0", fo.cols] <- occ1.fox[occ1.fox$Site == "J0", fo.cols]
occasions1.fo[occasions1.fo$Site == "J1", fo.cols] <- occ1.fox[occ1.fox$Site == "J1", fo.cols]
occasions1.fo[occasions1.fo$Site == "J2", fo.cols] <- occ1.fox[occ1.fox$Site == "J2", fo.cols]
# add env covariates for second placement
occasions2.fo[occasions2.fo$Site == "A0", fo.cols] <- occ2.fox[occ2.fox$Site == "A0", fo.cols]
occasions2.fo[occasions2.fo$Site == "A1", fo.cols] <- occ2.fox[occ2.fox$Site == "A1", fo.cols]
occasions2.fo[occasions2.fo$Site == "A2", fo.cols] <- occ2.fox[occ2.fox$Site == "A2", fo.cols]
occasions2.fo[occasions2.fo$Site == "A2/2", fo.cols] <- occ2.fox[occ2.fox$Site == "A2/2", fo.cols]
occasions2.fo[occasions2.fo$Site == "B0", fo.cols] <- occ2.fox[occ2.fox$Site == "B0", fo.cols]
occasions2.fo[occasions2.fo$Site == "B2", fo.cols] <- occ2.fox[occ2.fox$Site == "B2", fo.cols]
occasions2.fo[occasions2.fo$Site == "B2/2", fo.cols] <- occ2.fox[occ2.fox$Site == "B2/2", fo.cols]
occasions2.fo[occasions2.fo$Site == "C0", fo.cols] <- occ2.fox[occ2.fox$Site == "C0", fo.cols]
occasions2.fo[occasions2.fo$Site == "C1", fo.cols] <- occ2.fox[occ2.fox$Site == "C1", fo.cols]
occasions2.fo[occasions2.fo$Site == "C1/2", fo.cols] <- occ2.fox[occ2.fox$Site == "C1/2", fo.cols]
occasions2.fo[occasions2.fo$Site == "C2", fo.cols] <- occ2.fox[occ2.fox$Site == "C2", fo.cols]
occasions2.fo[occasions2.fo$Site == "D1", fo.cols] <- occ2.fox[occ2.fox$Site == "D1", fo.cols]
occasions2.fo[occasions2.fo$Site == "D1/2", fo.cols] <- occ2.fox[occ2.fox$Site == "D1/2", fo.cols]
occasions2.fo[occasions2.fo$Site == "D2", fo.cols] <- occ2.fox[occ2.fox$Site == "D2", fo.cols]
occasions2.fo[occasions2.fo$Site == "E0", fo.cols] <- occ2.fox[occ2.fox$Site == "E0", fo.cols]
occasions2.fo[occasions2.fo$Site == "E0/2", fo.cols] <- occ2.fox[occ2.fox$Site == "E0/2", fo.cols]
occasions2.fo[occasions2.fo$Site == "E1", fo.cols] <- occ2.fox[occ2.fox$Site == "E1", fo.cols]
occasions2.fo[occasions2.fo$Site == "E1/2", fo.cols] <- occ2.fox[occ2.fox$Site == "E1/2", fo.cols]
occasions2.fo[occasions2.fo$Site == "E2", fo.cols] <- occ2.fox[occ2.fox$Site == "E2", fo.cols]
occasions2.fo[occasions2.fo$Site == "F0", fo.cols] <- occ2.fox[occ2.fox$Site == "F0", fo.cols]
occasions2.fo[occasions2.fo$Site == "F1", fo.cols] <- occ2.fox[occ2.fox$Site == "F1", fo.cols]
occasions2.fo[occasions2.fo$Site == "F2", fo.cols] <- occ2.fox[occ2.fox$Site == "F2", fo.cols]
occasions2.fo[occasions2.fo$Site == "G0", fo.cols] <- occ2.fox[occ2.fox$Site == "G0", fo.cols]
occasions2.fo[occasions2.fo$Site == "G1", fo.cols] <- occ2.fox[occ2.fox$Site == "G1", fo.cols]
occasions2.fo[occasions2.fo$Site == "G2", fo.cols] <- occ2.fox[occ2.fox$Site == "G2", fo.cols]
occasions2.fo[occasions2.fo$Site == "H0", fo.cols] <- occ2.fox[occ2.fox$Site == "H0", fo.cols]
occasions2.fo[occasions2.fo$Site == "H1", fo.cols] <- occ2.fox[occ2.fox$Site == "H1", fo.cols]
occasions2.fo[occasions2.fo$Site == "H2", fo.cols] <- occ2.fox[occ2.fox$Site == "H2", fo.cols]
occasions2.fo[occasions2.fo$Site == "I0", fo.cols] <- occ2.fox[occ2.fox$Site == "I0", fo.cols]
occasions2.fo[occasions2.fo$Site == "I1", fo.cols] <- occ2.fox[occ2.fox$Site == "I1", fo.cols]
occasions2.fo[occasions2.fo$Site == "I2", fo.cols] <- occ2.fox[occ2.fox$Site == "I2", fo.cols]
occasions2.fo[occasions2.fo$Site == "J0", fo.cols] <- occ2.fox[occ2.fox$Site == "J0", fo.cols]
occasions2.fo[occasions2.fo$Site == "J1", fo.cols] <- occ2.fox[occ2.fox$Site == "J1", fo.cols]
occasions2.fo[occasions2.fo$Site == "J2", fo.cols] <- occ2.fox[occ2.fox$Site == "J2", fo.cols]

head(occasions1.fo)
head(occasions2.fo)
summary(occasions1.fo$wood1km)
occasions1.fo <- na.omit(occasions1.fo)

# bind to one final data frame
occasions.fo <- rbind(occasions1.fo, occasions2.fo)

# area column
occasions.fo$area <- NA
occasions.fo$area <- occ.fox$area[match(occasions.fo$Site, occ.fox$Site)]
summary(occasions.fo$area)

write.csv(occasions.fo, "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/fox_activity_df.csv")
