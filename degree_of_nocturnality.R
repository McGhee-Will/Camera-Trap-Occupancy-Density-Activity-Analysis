# DEGREE OF NOCTURNALITY

library(overlap)
library(terra)
library(activity)
library(dplyr)
library(sp)
library(car)
library(glmmTMB)
library(broom)
library(broom.mixed)
library(performance)

setwd("~/GALLANT Technician/Camera Trap Analysis")
mammal.data <- read.csv("fulldata.csv")
# camera locations
cam1.locs <- vect("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/Openspacesites/Openspacesites/Camera_locations/RandomPoints_round1_sampling.shp")
cam2.locs <- vect("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/Openspacesites/Openspacesites/Camera_locations/points_projected_round2.shp")

###################
### FORMAT DATA ###
###################

# change placement values
mammal.data$placement <- ifelse(mammal.data$placement == "A", '1', '2')

# fix site column
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


# split by species
fox <- subset(mammal.data, mammal.data$N_fox_AD > 0)
deer <- subset(mammal.data, mammal.data$N_roe_deer_AD > 0)
squ <- subset(mammal.data, mammal.data$N_grey_squirrel > 0)

# remove '0' events
fox <- subset(fox, fox$EVENT == '1')
deer <- subset(deer, deer$EVENT == '1')
squ <- subset(squ, squ$EVENT == '1')

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

# set as factors
fox$site <- as.factor(fox$site)
fox$placement <- as.factor(fox$placement)
deer$site <- as.factor(deer$site)
deer$placement <- as.factor(deer$placement)
squ$site <- as.factor(squ$site)
squ$placement <- as.factor(squ$placement)

# session_site columns
fox$session_site <- interaction(fox$placement, fox$site)
deer$session_site <- interaction(deer$placement, deer$site)
squ$session_site <- interaction(squ$placement, squ$site)
all.locs$session_site <- interaction(all.locs$placement, all.locs$site)

#############################
### DAY OR NIGHT PRESENCE ###
#############################

# transform crs
all.locs.wgs <- terra::project(all.locs, "EPSG:4326") 

# create coords dataframe
coords_df <- data.frame(
  session_site = all.locs.wgs$session_site,
  lon = crds(all.locs.wgs)[, 1],  # longitude
  lat = crds(all.locs.wgs)[, 2]   # latitude
)


#### FOX ####
# fix datetime
fox$datetime <- as.POSIXct(fox$datetime, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/London")

# add month column
fox$month <- months(fox$datetime)
fox$month <- ifelse(fox$month == "April", '4', fox$month)
fox$month <- ifelse(fox$month == "May", '5', fox$month)
fox$month <- ifelse(fox$month == "June", '6', fox$month)
fox$month <- ifelse(fox$month == "July", '7', fox$month)
fox$month <- ifelse(fox$month == "August", '8', fox$month)
fox$month <- ifelse(fox$month == "September", '9', fox$month)
fox$month <- as.numeric(fox$month)

# radian time
fox$time2 <- gettime(fox$datetime, scale = "radian")

# spatialpoints object
fox <- left_join(fox, coords_df, by = "session_site")
fox_coords_matrix <- cbind(fox$lon, fox$lat)
fox_coords_sp <- SpatialPoints(fox_coords_matrix, proj4string = CRS("+proj=longlat +datum=WGS84"))

# solar time
fox$solar_time <- sunTime(fox$time2, fox$datetime, fox_coords_sp)

# classify day/night
fox$day_night <- ifelse(
  fox$solar_time >= pi/2 & fox$solar_time <= 3*pi/2,
  "day", "night"
)
table(fox$day_night, fox$site)


#### DEER ####
# fix datetime
deer$datetime <- as.POSIXct(deer$datetime, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/London")

# add month column
deer$month <- months(deer$datetime)
deer$month <- ifelse(deer$month == "April", '4', deer$month)
deer$month <- ifelse(deer$month == "May", '5', deer$month)
deer$month <- ifelse(deer$month == "June", '6', deer$month)
deer$month <- ifelse(deer$month == "July", '7', deer$month)
deer$month <- ifelse(deer$month == "August", '8', deer$month)
deer$month <- ifelse(deer$month == "September", '9', deer$month)
deer$month <- as.numeric(deer$month)

# radian time
deer$time2 <- gettime(deer$datetime, scale = "radian")

# spatialpoints object
deer <- left_join(deer, coords_df, by = "session_site")
deer_coords_matrix <- cbind(deer$lon, deer$lat)
deer_coords_sp <- SpatialPoints(deer_coords_matrix, proj4string = CRS("+proj=longlat +datum=WGS84"))

# solar time
deer$solar_time <- sunTime(deer$time2, deer$datetime, deer_coords_sp)

# classify day/night
deer$day_night <- ifelse(
  deer$solar_time >= pi/2 & deer$solar_time <= 3*pi/2,
  "day", "night"
)
table(deer$day_night, deer$site)

#### SQUIRREL ####
# fix datetime
squ$datetime <- as.POSIXct(squ$datetime, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/London")

# add month column
squ$month <- months(squ$datetime)
squ$month <- ifelse(squ$month == "April", '4', squ$month)
squ$month <- ifelse(squ$month == "May", '5', squ$month)
squ$month <- ifelse(squ$month == "June", '6', squ$month)
squ$month <- ifelse(squ$month == "July", '7', squ$month)
squ$month <- ifelse(squ$month == "August", '8', squ$month)
squ$month <- ifelse(squ$month == "September", '9', squ$month)
squ$month <- as.numeric(squ$month)

# radian time
squ$time2 <- gettime(squ$datetime, scale = "radian")

# spatialpoints object
squ <- left_join(squ, coords_df, by = "session_site")
squ_coords_matrix <- cbind(squ$lon, squ$lat)
squ_coords_sp <- SpatialPoints(squ_coords_matrix, proj4string = CRS("+proj=longlat +datum=WGS84"))

# solar time
squ$solar_time <- sunTime(squ$time2, squ$datetime, squ_coords_sp)

# classify day/night
squ$day_night <- ifelse(
  squ$solar_time >= pi/2 & squ$solar_time <= 3*pi/2,
  "day", "night"
)
table(squ$day_night, squ$site)

###############################
### TOTALS DURING DAY/NIGHT ###
###############################

### SQUIRREL ###
# new object with total counts of squirrels at day/night
squ_final <- squ %>%
  group_by(session_site, month, day_night) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = day_night,
    values_from = n,
    values_fill = list(n = 0)
  )

# calculate proportion of points at night/day, per camera location
squ_final$prop_night <- squ_final$night / (squ_final$day + squ_final$night)
summary(squ_final$prop_night)
squ_final$prop_day <- squ_final$day / (squ_final$day + squ_final$night)
summary(squ_final$prop_day)
# rename
squ_final <- squ_final %>%
  dplyr::rename(Session_Site = session_site)

head(squ_final)

### DEER ###
# new object with total counts of deer at day/night
dee_final <- deer %>%
  group_by(session_site, month, day_night) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = day_night,
    values_from = n,
    values_fill = list(n = 0)
  )
# calculate proportion of points at night/day, per camera location
dee_final$prop_night <- dee_final$night / (dee_final$day + dee_final$night)
summary(dee_final$prop_night)
dee_final$prop_day <- dee_final$day / (dee_final$day + dee_final$night)
summary(dee_final$prop_day)
# rename
dee_final <- dee_final %>%
  dplyr::rename(Session_Site = session_site)

head(dee_final)

### FOX ###
# new object with total counts of foxes at day/night
fox_final <- fox %>%
  group_by(session_site, month, day_night) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = day_night,
    values_from = n,
    values_fill = list(n = 0)
  )
# calculate proportion of points at night/day, per camera location
fox_final$prop_night <- fox_final$night / (fox_final$day + fox_final$night)
summary(fox_final$prop_night)
fox_final$prop_day <- fox_final$day / (fox_final$day + fox_final$night)
summary(fox_final$prop_day)
# rename
fox_final <- fox_final %>%
  dplyr::rename(Session_Site = session_site)

head(fox_final)

################################################################
### TRANSFORM NOCTURNALITY - Cribari-Neto and Zeileis (2010) ###
################################################################

# (Y * (n − 1) + 0.5) ∕ n
# Y = value of given index (nocturnality)
# n = sample size (total number of observations for a given index)

# total sample size column
squ_final$n <- squ_final$day + squ_final$night
dee_final$n <- dee_final$day + dee_final$night
fox_final$n <- fox_final$day + fox_final$night

# transformed nocturnality
squ_final$noc_t <- (squ_final$prop_night * (squ_final$n - 1) + 0.5) / squ_final$n
dee_final$noc_t <- (dee_final$prop_night * (dee_final$n - 1) + 0.5) / dee_final$n
fox_final$noc_t <- (fox_final$prop_night * (fox_final$n - 1) + 0.5) / fox_final$n

###################################
### ADD ENIVORNMENTAL VARIABLES ###
###################################

# steal buffer stats from occupancy data frames
occ.squ <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/squirrel_occupancy_df.csv")
occ.dee <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/deer_occupancy_df.csv")
occ.fox <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/fox_occupancy_df.csv")
summary(occ.fox)
# session/site column
occ.squ$Session_Site <- interaction(occ.squ$Session, occ.squ$Site)
occ.dee$Session_Site <- interaction(occ.dee$Session, occ.dee$Site)
occ.fox$Session_Site <- interaction(occ.fox$Session, occ.fox$Site)

### SQUIRREL ###
# land cover columns
squ_final$wood100 <- NA
squ_final$urban100 <- NA
squ_final$wood400 <- NA
squ_final$urban400 <- NA
# dog
squ_final$dog_RA <- NA
# light noise
squ_final$light100 <- NA
squ_final$light400 <- NA
squ_final$noise100 <- NA
squ_final$noise400 <- NA
# area
squ_final$area <- NA

# fill columns
# env vars - 100
squ_final$wood100 <- occ.squ$wood100[match(squ_final$Session_Site, occ.squ$Session_Site)]
squ_final$urban100 <- occ.squ$urban100[match(squ_final$Session_Site, occ.squ$Session_Site)]
# 400
squ_final$wood400 <- occ.squ$wood400[match(squ_final$Session_Site, occ.squ$Session_Site)]
squ_final$urban400 <- occ.squ$urban400[match(squ_final$Session_Site, occ.squ$Session_Site)]
# dog RA
squ_final$dog_RA <- occ.squ$dog_RA[match(squ_final$Session_Site, occ.squ$Session_Site)]
# light noise
squ_final$light100 <- occ.squ$light100[match(squ_final$Session_Site, occ.squ$Session_Site)]
squ_final$light400 <- occ.squ$light400[match(squ_final$Session_Site, occ.squ$Session_Site)]
squ_final$noise100 <- occ.squ$noise100[match(squ_final$Session_Site, occ.squ$Session_Site)]
squ_final$noise400 <- occ.squ$noise400[match(squ_final$Session_Site, occ.squ$Session_Site)]
# area
squ_final$area <- occ.squ$area[match(squ_final$Session_Site, occ.squ$Session_Site)]
summary(squ_final)


### DEER ###
# land cover columns
dee_final$wood250 <- NA
dee_final$urban250 <- NA
dee_final$wood1km <- NA
dee_final$urban1km <- NA
# dog
dee_final$dog_RA <- NA
# light noise
dee_final$light250 <- NA
dee_final$light1km <- NA
dee_final$noise250 <- NA
dee_final$noise1km <- NA
# area
dee_final$area <- NA

# fill columns
# env vars - 250
dee_final$wood250 <- occ.dee$wood250[match(dee_final$Session_Site, occ.dee$Session_Site)]
dee_final$urban250 <- occ.dee$urban250[match(dee_final$Session_Site, occ.dee$Session_Site)]
# 1km
dee_final$wood1km <- occ.dee$wood1km[match(dee_final$Session_Site, occ.dee$Session_Site)]
dee_final$urban1km <- occ.dee$urban1km[match(dee_final$Session_Site, occ.dee$Session_Site)]
# dog RA
dee_final$dog_RA <- occ.dee$dog_RA[match(dee_final$Session_Site, occ.dee$Session_Site)]
# light noise
dee_final$light250 <- occ.dee$light250[match(dee_final$Session_Site, occ.dee$Session_Site)]
dee_final$light1km <- occ.dee$light1km[match(dee_final$Session_Site, occ.dee$Session_Site)]
dee_final$noise250 <- occ.dee$noise250[match(dee_final$Session_Site, occ.dee$Session_Site)]
dee_final$noise1km <- occ.dee$noise1km[match(dee_final$Session_Site, occ.dee$Session_Site)]
# area
dee_final$area <- occ.dee$area[match(dee_final$Session_Site, occ.dee$Session_Site)]
summary(dee_final)


### FOX ###
# land cover columns
fox_final$wood250 <- NA
fox_final$urban250 <- NA
fox_final$wood1km <- NA
fox_final$urban1km <- NA
# dog
fox_final$dog_RA <- NA
# light noise
fox_final$light250 <- NA
fox_final$light1km <- NA
fox_final$noise250 <- NA
fox_final$noise1km <- NA
# area
fox_final$area <- NA

# fill columns
# env vars - 250
fox_final$wood250 <- occ.fox$wood250[match(fox_final$Session_Site, occ.fox$Session_Site)]
fox_final$urban250 <- occ.fox$urban250[match(fox_final$Session_Site, occ.fox$Session_Site)]
# 1km
fox_final$wood1km <- occ.fox$wood1km[match(fox_final$Session_Site, occ.fox$Session_Site)]
fox_final$urban1km <- occ.fox$urban1km[match(fox_final$Session_Site, occ.fox$Session_Site)]
# dog RA
fox_final$dog_RA <- occ.fox$dog_RA[match(fox_final$Session_Site, occ.fox$Session_Site)]
# light noise
fox_final$light250 <- occ.fox$light250[match(fox_final$Session_Site, occ.fox$Session_Site)]
fox_final$light1km <- occ.fox$light1km[match(fox_final$Session_Site, occ.fox$Session_Site)]
fox_final$noise250 <- occ.fox$noise250[match(fox_final$Session_Site, occ.fox$Session_Site)]
fox_final$noise1km <- occ.fox$noise1km[match(fox_final$Session_Site, occ.fox$Session_Site)]
# area
fox_final$area <- occ.fox$area[match(fox_final$Session_Site, occ.fox$Session_Site)]
summary(fox_final)

########################
### SAVE DATA FRAMES ###
########################

write.csv(squ_final, file = "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/squ_noct.csv")
write.csv(dee_final, file = "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/deer_noct.csv")
write.csv(fox_final, file = "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/fox_noct.csv")

squ_final <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/squ_noct.csv")
dee_final <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/deer_noct.csv")
fox_final <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/fox_noct.csv")


## NO SQUIRREL MODELS - 99.4% OF POINTS IN DAYTIME

#############
### SCALE ###
#############
# FOX
fox_final$area <- scale(fox_final$area)
fox_final$light1km <- scale(fox_final$light1km)
fox_final$light250 <- scale(fox_final$light250)
fox_final$noise1km <- scale(fox_final$noise1km)
fox_final$noise250 <- scale(fox_final$noise250)
# DEER
dee_final$area <- scale(dee_final$area)
dee_final$light1km <- scale(dee_final$light1km)
dee_final$light250 <- scale(dee_final$light250)
dee_final$noise1km <- scale(dee_final$noise1km)
dee_final$noise250 <- scale(dee_final$noise250)

####################
### COLLINEARITY ###
####################

DEE250_CO <- lm(noc_t ~ wood250 + urban250 + dog_RA + area + light250 + noise250 + month,
                data = dee_final)
vif(DEE250_CO) # FINE

DEE1KM_CO <- lm(noc_t ~ wood1km + urban1km + dog_RA + area + light1km + noise1km + month,
                data = dee_final)
vif(DEE1KM_CO) # FINE

FOX250_CO <- lm(noc_t ~ wood250 + urban250 + dog_RA + area + light250 + noise250 + month,
                data = fox_final)
vif(FOX250_CO) # FINE

FOX1KM_CO <- lm(noc_t ~ wood1km + urban1km + dog_RA + area + light1km + noise1km + month,
                data = fox_final)
vif(FOX1KM_CO) # FINE


###################
### DEER MODELS ###
###################

### LOCAL ###
full.dee250 <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + area + light250 + noise250 + month + (1|Session_Site),
                       data = dee_final,
                       family = beta_family())
summary(full.dee250)

# remove month
dee250.2 <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + area + light250 + noise250 + (1|Session_Site),
                    data = dee_final,
                    family = beta_family(),
                    control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
AIC(full.dee250, dee250.2) # FULL LOWER - KEEP MONTH

# remove noise
dee250.3 <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + area + light250 + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee250, dee250.3) # DIFF < 1 - KEEP NOISE

# remove light
dee250.4 <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + area + noise250 + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee250, dee250.4) # DIFF < 2 - KEEP LIGHT

# remove area
dee250.5 <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + light250 + noise250 + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee250, dee250.5) # DIFF < 2 - KEEP AREA

# remove dog_RA
dee250.6 <- glmmTMB(noc_t ~ wood250 + urban250 + area + light250 + noise250 + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee250, dee250.6) # FULL LOWER - KEEP DOG_RA

# remove urban
dee250.7 <- glmmTMB(noc_t ~ wood250 + dog_RA + area + light250 + noise250 + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee250, dee250.7) # DIFF = 2 - KEEP URBAN

# remove woodland
dee250.8 <- glmmTMB(noc_t ~ urban250 + dog_RA + area + light250 + noise250 + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee250, dee250.8) # DIFF < 2 - KEEP WOODLAND


# FINAL LOCAL DEER
dee250.final <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + area + light250 + noise250 + month + (1|Session_Site),
                        data = dee_final,
                        family = beta_family())
summary(dee250.final)
r2(dee250.final) # 0.109



### LANDSCAPE ###
full.dee1km <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + area + light1km + noise1km + month + (1|Session_Site),
                       data = dee_final,
                       family = beta_family())
summary(full.dee1km)

# remove month
dee1km.2 <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + area + light1km + noise1km + (1|Session_Site),
                    data = dee_final,
                    family = beta_family(),
                    control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
AIC(full.dee1km, dee1km.2) # FULL LOWER - KEEP MONTH

# remove noise
dee1km.3 <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + area + light1km + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee1km, dee1km.3) # DIFF < 2 - KEEP NOISE

# remove light
dee1km.4 <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + area + noise1km + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee1km, dee1km.4) # DIFF < 2 - KEEP LIGHT

# remove area
dee1km.5 <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + light1km + noise1km + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee1km, dee1km.5) # DIFF < 1 - KEEP AREA

# remove dog_RA
dee1km.6 <- glmmTMB(noc_t ~ wood1km + urban1km + area + light1km + noise1km + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee1km, dee1km.6) # FULL LOWER - KEEP DOG_RA

# remove urban
dee1km.7 <- glmmTMB(noc_t ~ wood1km + dog_RA + area + light1km + noise1km + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee1km, dee1km.7) # DIFF < 2 - KEEP URBAN

# remove woodland
dee1km.8 <- glmmTMB(noc_t ~ urban1km + dog_RA + area + light1km + noise1km + month + (1|Session_Site),
                    data = dee_final,
                    family = beta_family())
AIC(full.dee1km, dee1km.8) # DIFF < 1 - KEEP WOODLAND


# FINAL LANDSCAPE DEER
dee1km.final <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + area + light1km + noise1km + month + (1|Session_Site),
                        data = dee_final,
                        family = beta_family())
summary(dee1km.final)
r2(dee1km.final) # 0.150

##################
### FOX MODELS ###
##################

### LOCAL ###
full.fox250 <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + area + light250 + noise250 + month + (1|Session_Site),
                       data = fox_final,
                       family = beta_family())
summary(full.fox250)

# remove month
fox250.2 <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + area + light250 + noise250 + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox250, fox250.2) # DIFF < 1 - KEEP MONTH

# remove noise
fox250.3 <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + area + light250 + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox250, fox250.3) # DIFF < 2 - KEEP NOISE

# remove light
fox250.4 <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + area + noise250 + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox250, fox250.4) # DIFF < 2 - KEEP LIGHT

# remove area
fox250.5 <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + light250 + noise250 + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox250, fox250.5) # DIFF < 2 - KEEP AREA

# remove dog_RA
fox250.6 <- glmmTMB(noc_t ~ wood250 + urban250 + area + light250 + noise250 + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox250, fox250.6) # DIFF < 2 - KEEP DOG_RA

# remove urban
fox250.7 <- glmmTMB(noc_t ~ wood250 + dog_RA + area + light250 + noise250 + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox250, fox250.7) # DIFF < 2 - KEEP URBAN

# remove woodland
fox250.8 <- glmmTMB(noc_t ~ urban250 + dog_RA + area + light250 + noise250 + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox250, fox250.8) # DIFF < 2 - KEEP WOODLAND


# FINAL FOX LOCAL 
fox250.final <- glmmTMB(noc_t ~ wood250 + urban250 + dog_RA + area + light250 + noise250 + month + (1|Session_Site),
                        data = fox_final,
                        family = beta_family())
summary(fox250.final)
r2(fox250.final) # 0.160


### LANDSCAPE ###
full.fox1km <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + area + light1km + noise1km + month + (1|Session_Site),
                       data = fox_final,
                       family = beta_family())
summary(full.fox1km)

# remove month
fox1km.2 <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + area + light1km + noise1km + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox1km, fox1km.2) # DIFF < 1 - KEEP MONTH

# remove noise
fox1km.3 <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + area + light1km + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox1km, fox1km.3) # DIFF < 2 - KEEP NOISE

# remove light
fox1km.4 <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + area + noise1km + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox1km, fox1km.4) # DIFF < 2 - KEEP LIGHT

# remove area
fox1km.5 <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + light1km + noise1km + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox1km, fox1km.5) # DIFF < 2 - KEEP AREA

# remove dog_RA
fox1km.6 <- glmmTMB(noc_t ~ wood1km + urban1km + area + light1km + noise1km + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox1km, fox1km.6) # DIFF < 1 - KEEP DOG_RA

# remove urban
fox1km.7 <- glmmTMB(noc_t ~ wood1km + dog_RA + area + light1km + noise1km + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox1km, fox1km.7) # DIFF < 2 - KEEP URBAN

# remove woodland
fox1km.8 <- glmmTMB(noc_t ~ urban1km + dog_RA + area + light1km + noise1km + month + (1|Session_Site),
                    data = fox_final,
                    family = beta_family())
AIC(full.fox1km, fox1km.8) # DIFF < 2 - KEEP WOODLAND


# FINAL LANDSCAPE FOX
fox1km.final <- glmmTMB(noc_t ~ wood1km + urban1km + dog_RA + area + light1km + noise1km + month + (1|Session_Site),
                        data = fox_final,
                        family = beta_family())
summary(fox1km.final)
r2(fox1km.final) # 0.108


###############################
### FOREST PLOT DATA FRAMES ###
###############################

# extract coefficiants and add columns
noc.flan <- tidy(fox1km.final, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) 
noc.flan$species <- "Red Fox"
noc.flan$scale <- "Landscape"
noc.flan$model <- "Nocturnality"
noc.floc <- tidy(fox250.final, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
noc.floc$species <- "Red Fox"
noc.floc$scale <- "Local"
noc.floc$model <- "Nocturnality"
noc.dlan <- tidy(dee1km.final, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
noc.dlan$species <- "Roe Deer"
noc.dlan$scale <- "Landscape"
noc.dlan$model <- "Nocturnality"
noc.dloc <- tidy(dee250.final, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
noc.dloc$species <- "Roe Deer"
noc.dloc$scale <- "Local"
noc.dloc$model <- "Nocturnality"
# combine to one df
results <- rbind(noc.flan, noc.floc, noc.dlan, noc.dloc)

# rename vars
results$term <- ifelse(results$term == "wood1km", "woodland", results$term)
results$term <- ifelse(results$term == "wood250", "woodland", results$term)
results$term <- ifelse(results$term == "urban1km", "urban", results$term)
results$term <- ifelse(results$term == "urban250", "urban", results$term)
results$term <- ifelse(results$term == "noise1km", "noise", results$term)
results$term <- ifelse(results$term == "noise250", "noise", results$term)
results$term <- ifelse(results$term == "light1km", "light", results$term)
results$term <- ifelse(results$term == "light250", "light", results$term)

write.csv(results, file = "~/GALLANT Technician/Camera Trap Analysis/forest.noct.csv")
