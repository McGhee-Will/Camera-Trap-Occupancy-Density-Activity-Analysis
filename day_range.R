# Calculating day range of animals

library(ggplot2)
library(activity)

#####################
### AVERAGE SPEED ###
#####################

data <- read.csv("mammal_average_speed.csv")
head(data)

# correct mistake for intervals column
data$Intervals <- data$Intervals + 1

# add speed column
data$speed <- data$Distance_m/(data$Intervals*data$Time_s)

#################################
### CALCULTE FOR EACH SPECIES ###
#################################

deer <- subset(data, data$Species == "Roe_deer")
fox <- subset(data, data$Species == "Red_fox")
squirrel <- subset(data, data$Species == "Grey_squirrel")

drmsp <- mean(deer$speed) # 1.028 m/s
fomsp <- mean(fox$speed) # 0.604 m/s
sqmsp <- mean(squirrel$speed) # 0.442 m/s

# Need to combine with activity level and convert to km/day

#### HYPOTHETICAL
### IF ACTIVE CONSTANTLY

deer.speed <- (drmsp * (60*60*24))/1000 # 88.85 km/day
fox.speed <- (fomsp * (60*60*24))/1000 # 52.15 km/day
squirrel.speed <- (sqmsp * (60*60*24))/1000 # 38.17 km/day

######################
### ACTIVITY LEVEL ###
######################

mammal.data <- read.csv("mammal_cameratrap_data2024.csv")

foxEV <- subset(mammal.data, mammal.data$N_fox_AD > 0)
foxEV <- subset(foxEV, foxEV$EVENT == 1)
deerEV <- subset(mammal.data, mammal.data$N_roe_deer_AD > 0)
deerEV <- subset(deerEV, deerEV$EVENT == 1)
squirrelEV <- subset(mammal.data, mammal.data$N_grey_squirrel > 0)
squirrelEV <- subset(squirrelEV, squirrelEV$EVENT == 1)

# make POSIXct
foxEV$datetime <- as.POSIXct(foxEV$datetime, "%d/%m/%Y %H:%M", tz = "GMT")
deerEV$datetime <- as.POSIXct(deerEV$datetime, "%d/%m/%Y %H:%M", tz = "GMT")
squirrelEV$datetime <- as.POSIXct(squirrelEV$datetime, "%d/%m/%Y %H:%M", tz = "GMT")

### FOX
fox.time2 <- gettime(foxEV$datetime, scale = c("radian"))

# activity
xx <- seq(0, 2*pi, pi/256)
pdf <- dvmkern(xx, fox.time2)
plot(xx, pdf, type = "l")

fox.act <- fitact(fox.time2)
plot(fox.act)

fox.activity <- fox.act@act


### SQUIRREL
squirrel.time2 <- gettime(squirrelEV$datetime, scale = c("radian"))

# activity
pdf2 <- dvmkern(xx, squirrel.time2)
plot(xx, pdf2, type = "l")

squirrel.act <- fitact(squirrel.time2)
plot(squirrel.act)

squirrel.activity <- squirrel.act@act


# DEER
deer.time2 <- gettime(deerEV$datetime, scale = c("radian"))

# activity
pdf3 <- dvmkern(xx, deer.time2)
plot(xx, pdf3, type = "l")

deer.act <- fitact(deer.time2)
plot(deer.act)

deer.activity <- deer.act@act

#################
### DAY RANGE ###
#################

fox.dr <- fox.speed*fox.activity # 17.11
deer.dr <- deer.speed*deer.activity # 42.49
squirrel.dr <- squirrel.speed*squirrel.activity # 20.16
