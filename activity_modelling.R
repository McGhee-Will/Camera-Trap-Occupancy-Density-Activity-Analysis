# ACTIVITY MODELLING

require(lmerTest)
require(ggplot2)
require(broom)
require(emmeans)
require(plyr)
require(dplyr)
require(tidyverse)
require(reshape2)
require(purrr)
require(ggpubr)
require(readxl)
require(mgcv)
require(gamm4)
require(readr)
require(arm)
require(merTools)
require(suncalc)
require(lubridate)
require(sjPlot)
require(sjmisc)
require(performance)
require(brms)
require(tidygam)
library(mgcv)
library(pROC)
library(caret)
library(gratia)
library(DHARMa)

fox <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/fox_activity_df.csv")
deer <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/deer_activity_df.csv")
squirrel <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/squ_activity_df.csv")

###################
### FORMAT DATA ###
###################

# set as factors
fox$Site <- as.factor(fox$Site)
fox$Session <- as.factor(fox$Session)
deer$Site <- as.factor(deer$Site)
deer$Session <- as.factor(deer$Session)
squirrel$Site <- as.factor(squirrel$Site)
squirrel$Session <- as.factor(squirrel$Session)

# new column with julian day
fox$date <- as.POSIXct(fox$start,format = "%d/%m/%Y")
fox$datej <- as.integer(format(fox$date, "%j"))
deer$date <- as.POSIXct(deer$start,format = "%d/%m/%Y")
deer$datej <- as.integer(format(deer$date, "%j"))
squirrel$date <- as.POSIXct(squirrel$start,format = "%d/%m/%Y")
squirrel$datej <- as.integer(format(squirrel$date, "%j"))

# new column with numbered months
fox$month2 <- NA
fox$month2 <- ifelse(fox$month == "April", 4, fox$month2)
fox$month2 <- ifelse(fox$month == "May", 5, fox$month2)
fox$month2 <- ifelse(fox$month == "June", 6, fox$month2)
fox$month2 <- ifelse(fox$month == "July", 7, fox$month2)
fox$month2 <- ifelse(fox$month == "August", 8, fox$month2)
fox$month2 <- ifelse(fox$month == "September", 9, fox$month2)
levels(as.factor(fox$month2))
summary(fox$month2)
deer$month2 <- NA
deer$month2 <- ifelse(deer$month == "April", 4, deer$month2)
deer$month2 <- ifelse(deer$month == "May", 5, deer$month2)
deer$month2 <- ifelse(deer$month == "June", 6, deer$month2)
deer$month2 <- ifelse(deer$month == "July", 7, deer$month2)
deer$month2 <- ifelse(deer$month == "August", 8, deer$month2)
deer$month2 <- ifelse(deer$month == "September", 9, deer$month2)
levels(as.factor(deer$month2))
summary(deer$month2)
squirrel$month2 <- NA
squirrel$month2 <- ifelse(squirrel$month == "April", 4, squirrel$month2)
squirrel$month2 <- ifelse(squirrel$month == "May", 5, squirrel$month2)
squirrel$month2 <- ifelse(squirrel$month == "June", 6, squirrel$month2)
squirrel$month2 <- ifelse(squirrel$month == "July", 7, squirrel$month2)
squirrel$month2 <- ifelse(squirrel$month == "August", 8, squirrel$month2)
squirrel$month2 <- ifelse(squirrel$month == "September", 9, squirrel$month2)
levels(as.factor(squirrel$month2))
summary(squirrel$month2)

######################
### GROUP BY MONTH ###
######################

### SQUIRREL ###

# new data frame grouping captures by month, time of day and site
squirrel2 <- squirrel %>%
  group_by(month2, Time, Site, Session) %>%
  summarise(success = sum(capt),
            n_events = n(),
            failure = n_events - success)

# new columns in data frame
squirrel2$wood100 <- NA
squirrel2$urban100 <- NA
squirrel2$wood400 <- NA
squirrel2$urban400 <- NA
squirrel2$dog_RA <- NA
squirrel2$light100 <- NA
squirrel2$light400 <- NA
squirrel2$noise100 <- NA
squirrel2$noise400 <- NA
squirrel2$area <- NA

# combined site and session columns
squirrel$SiteSession <- interaction(squirrel$Site, squirrel$Session, drop = TRUE)
squirrel2$SiteSession <- interaction(squirrel2$Site, squirrel2$Session, drop = TRUE)

# add predictor variables
squirrel2$wood100 <- squirrel$wood100[match(squirrel2$SiteSession, squirrel$SiteSession)]
squirrel2$urban100 <- squirrel$urban100[match(squirrel2$SiteSession, squirrel$SiteSession)]
squirrel2$wood400 <- squirrel$wood400[match(squirrel2$SiteSession, squirrel$SiteSession)]
squirrel2$urban400 <- squirrel$urban400[match(squirrel2$SiteSession, squirrel$SiteSession)]
squirrel2$dog_RA <- squirrel$dog_RA[match(squirrel2$SiteSession, squirrel$SiteSession)]
squirrel2$light100 <- squirrel$light100[match(squirrel2$SiteSession, squirrel$SiteSession)]
squirrel2$light400 <- squirrel$light400[match(squirrel2$SiteSession, squirrel$SiteSession)]
squirrel2$noise100 <- squirrel$noise100[match(squirrel2$SiteSession, squirrel$SiteSession)]
squirrel2$noise400 <- squirrel$noise400[match(squirrel2$SiteSession, squirrel$SiteSession)]
squirrel2$area <- squirrel$area[match(squirrel2$SiteSession, squirrel$SiteSession)]

### DEER ###

# new data frame grouping captures by month, time of day and site
deer2 <- deer %>%
  group_by(month2, Time, Site, Session) %>%
  summarise(success = sum(capt),
            n_events = n(),
            failure = n_events - success)

# new columns in data frame
deer2$wood250 <- NA
deer2$urban250 <- NA
deer2$wood1km <- NA
deer2$urban1km <- NA
deer2$dog_RA <- NA
deer2$light250 <- NA
deer2$light1km <- NA
deer2$noise250 <- NA
deer2$noise1km <- NA
deer2$area <- NA

# combined site and session columns
deer$SiteSession <- interaction(deer$Site, deer$Session, drop = TRUE)
deer2$SiteSession <- interaction(deer2$Site, deer2$Session, drop = TRUE)

# add predictor variables
deer2$wood250 <- deer$wood250[match(deer2$SiteSession, deer$SiteSession)]
deer2$urban250 <- deer$urban250[match(deer2$SiteSession, deer$SiteSession)]
deer2$wood1km <- deer$wood1km[match(deer2$SiteSession, deer$SiteSession)]
deer2$urban1km <- deer$urban1km[match(deer2$SiteSession, deer$SiteSession)]
deer2$dog_RA <- deer$dog_RA[match(deer2$SiteSession, deer$SiteSession)]
deer2$light250 <- deer$light250[match(deer2$SiteSession, deer$SiteSession)]
deer2$light1km <- deer$light1km[match(deer2$SiteSession, deer$SiteSession)]
deer2$noise250 <- deer$noise250[match(deer2$SiteSession, deer$SiteSession)]
deer2$noise1km <- deer$noise1km[match(deer2$SiteSession, deer$SiteSession)]
deer2$area <- deer$area[match(deer2$SiteSession, deer$SiteSession)]

### FOX ###

# new data frame grouping captures by month, time of day and site
fox2 <- fox %>%
  group_by(month2, Time, Site, Session) %>%
  summarise(success = sum(capt),
            n_events = n(),
            failure = n_events - success)

# new columns in data frame
fox2$wood250 <- NA
fox2$urban250 <- NA
fox2$wood1km <- NA
fox2$urban1km <- NA
fox2$dog_RA <- NA
fox2$light250 <- NA
fox2$light1km <- NA
fox2$noise250 <- NA
fox2$noise1km <- NA
fox2$area <- NA

# combined site and session columns
fox$SiteSession <- interaction(fox$Site, fox$Session, drop = TRUE)
fox2$SiteSession <- interaction(fox2$Site, fox2$Session, drop = TRUE)

# add predictor variables
fox2$wood250 <- fox$wood250[match(fox2$SiteSession, fox$SiteSession)]
fox2$urban250 <- fox$urban250[match(fox2$SiteSession, fox$SiteSession)]
fox2$wood1km <- fox$wood1km[match(fox2$SiteSession, fox$SiteSession)]
fox2$urban1km <- fox$urban1km[match(fox2$SiteSession, fox$SiteSession)]
fox2$dog_RA <- fox$dog_RA[match(fox2$SiteSession, fox$SiteSession)]
fox2$light250 <- fox$light250[match(fox2$SiteSession, fox$SiteSession)]
fox2$light1km <- fox$light1km[match(fox2$SiteSession, fox$SiteSession)]
fox2$noise250 <- fox$noise250[match(fox2$SiteSession, fox$SiteSession)]
fox2$noise1km <- fox$noise1km[match(fox2$SiteSession, fox$SiteSession)]
fox2$area <- fox$area[match(fox2$SiteSession, fox$SiteSession)]

head(squirrel2)
head(deer2)
head(fox2)

################
### SQUIRREL ###
################

# LOCAL #

# global model
squ100.mod <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 20) + 
                    s(scale(Time), bs = "cc", k = 20, by = scale(urban100)) + # interaction: time*urban
                    s(scale(Time), bs = "cc", k = 20, by = scale(wood100)) + # interaction: time*wood
                    s(scale(Time), bs = "cc", k = 20, by = scale(area)) + # interaction: time*area
                    s(scale(Time), bs = "cc", k = 20, by = scale(light100)) + # interaction: time*light
                    s(scale(Time), bs = "cc", k = 20, by = scale(noise100)) + # interaction: time*noise
                    s(scale(Time), bs = "cc", k = 20, by = scale(dog_RA)) + # interaction: time*dog_RA
                    s(scale(Time), bs = "cc", k = 20, by = scale(month2)) + # interaction: time*month
                    s(SiteSession, bs = 're') + # random effect
                    scale(urban100) + # urban effect on activity level
                    scale(wood100) + # woodland effect on activity level
                    scale(area) + # area effect
                    scale(month2) + # month effect on activity
                    scale(dog_RA) + # dog effect on activity
                    scale(noise100) + # noise effect on activity
                    scale(light100), # light effect on activity
                  knots = list(Time = c(0,23)), 
                  data = squirrel2, 
                  method = "REML",
                  family = "binomial")
gam.check(squ100.mod)
summary(squ100.mod)
concurvity(squ100.mod)
plot(squ100.mod)

# gratia: visualise smooths
draw(squ100.mod)

# qq plot
appraise(squ100.mod, method = "simulate")

# drop one
squ100.2 <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 20) + 
                  s(scale(Time), bs = "cc", k = 20, by = scale(urban100)) + # interaction: time*urban
                  s(scale(Time), bs = "cc", k = 20, by = scale(wood100)) + # interaction: time*wood
                  s(scale(Time), bs = "cc", k = 20, by = scale(area)) + # interaction: time*area
                  s(scale(Time), bs = "cc", k = 20, by = scale(light100)) + # interaction: time*light
                  s(scale(Time), bs = "cc", k = 20, by = scale(noise100)) + # interaction: time*noise
                  s(scale(Time), bs = "cc", k = 20, by = scale(dog_RA)) + # interaction: time*dog_RA
                  s(scale(Time), bs = "cc", k = 20, by = scale(month2)) + # interaction: time*month
                  s(SiteSession, bs = 're') + # random effect
                  scale(urban100) + # urban effect on activity level
                  scale(wood100) + # woodland effect on activity level
                  scale(area) + # area effect
                  scale(month2) + # month effect on activity
                  scale(dog_RA) + # dog effect on activity
                  scale(noise100),# + # noise effect on activity
                  #scale(light100), # light effect on activity
                knots = list(Time = c(0,23)), 
                data = squirrel2, 
                method = "REML",
                family = "binomial")

AIC(squ100.mod, squ100.2)
# urban interaction: KEEP - FULL LOWER
# woodland interaction: KEEP - FULL LOWER
# area interaction: KEEP - NO DIFFERENCE
# light interaction: KEEP - FULL LOWER
# noise interaction: KEEP - FULL LOWER
# dog interaction: KEEP - NO DIFFERENCE
# month interaction: KEEP - FULL LOWER
# urban: KEEP - NO DIFFERENCE
# wood: KEEP - NO DIFFERENCE
# area: KEEP - NO DIFFERENCE
# month: KEEP - NO DIFFERENCE
# dog_RA: KEEP - NO DIFFERENCE
# noise: KEEP - NO DIFFERENCE
# light: KEEP - NO DIFFERENCE
gam.check(squ100.2)

# FINAL
squ100.final <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 20) + 
                      s(scale(Time), bs = "cc", k = 20, by = scale(urban100)) + # interaction: time*urban
                      s(scale(Time), bs = "cc", k = 20, by = scale(wood100)) + # interaction: time*wood
                      s(scale(Time), bs = "cc", k = 20, by = scale(area)) + # interaction: time*area
                      s(scale(Time), bs = "cc", k = 20, by = scale(light100)) + # interaction: time*light
                      s(scale(Time), bs = "cc", k = 20, by = scale(noise100)) + # interaction: time*noise
                      s(scale(Time), bs = "cc", k = 20, by = scale(dog_RA)) + # interaction: time*dog_RA
                      s(scale(Time), bs = "cc", k = 20, by = scale(month2)) + # interaction: time*month
                      s(SiteSession, bs = 're') + # random effect
                      scale(urban100) + # urban effect on activity level
                      scale(wood100) + # woodland effect on activity level
                      scale(area) + # area effect
                      scale(month2) + # month effect on activity
                      scale(dog_RA) + # dog effect on activity
                      scale(noise100) + # noise effect on activity
                      scale(light100), # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = squirrel2, 
                    method = "REML",
                    family = "binomial",
                    select = T)
gam.check(squ100.final)
summary(squ100.final)
concurvity(squ100.final)
plot(squ100.final)
draw(squ100.final, select = 1:4)
appraise(squ100.final, method = "simulate")


# LANDSCAPE #

# global model
squ400.mod <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 25) + 
                    s(scale(Time), bs = "cc", k = 25, by = scale(urban400)) + # interaction: time*urban
                    s(scale(Time), bs = "cc", k = 25, by = scale(wood400)) + # interaction: time*wood
                    #s(scale(Time), bs = "cc", k = 20, by = scale(area)) + # interaction: time*area
                    s(scale(Time), bs = "cc", k = 25, by = scale(light400)) + # interaction: time*light
                    s(scale(Time), bs = "cc", k = 25, by = scale(noise400)) + # interaction: time*noise
                    s(scale(Time), bs = "cc", k = 25, by = scale(dog_RA)) + # interaction: time*dog_RA
                    s(scale(Time), bs = "cc", k = 25, by = scale(month2)) + # interaction: time*month
                    s(SiteSession, bs = 're') + # random effect
                    scale(urban400) + # urban effect on activity level
                    scale(wood400) + # woodland effect on activity level
                    scale(area) + # area effect
                    scale(month2) + # month effect on activity
                    scale(dog_RA) + # dog effect on activity
                    scale(noise400) + # noise effect on activity
                    scale(light400), # light effect on activity
                  knots = list(Time = c(0,23)), 
                  data = squirrel2, 
                  method = "REML",
                  family = "binomial")
gam.check(squ400.mod)
summary(squ400.mod)
concurvity(squ400.mod) # remove area
plot(squ400.mod)

# first drop
squ400.2 <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 25) + 
                  s(scale(Time), bs = "cc", k = 25, by = scale(urban400)) + # interaction: time*urban
                  s(scale(Time), bs = "cc", k = 25, by = scale(wood400)) + # interaction: time*wood
                  s(scale(Time), bs = "cc", k = 25, by = scale(light400)) + # interaction: time*light
                  s(scale(Time), bs = "cc", k = 25, by = scale(noise400)) + # interaction: time*noise
                  s(scale(Time), bs = "cc", k = 25, by = scale(dog_RA)) + # interaction: time*dog_RA
                  s(scale(Time), bs = "cc", k = 25, by = scale(month2)) + # interaction: time*month
                  s(SiteSession, bs = 're') + # random effect
                  scale(urban400) + # urban effect on activity level
                  scale(wood400) + # woodland effect on activity level
                  scale(area) + # area effect
                  scale(month2) + # month effect on activity
                  scale(dog_RA) + # dog effect on activity
                  scale(noise400),# + # noise effect on activity
                  #scale(light400), # light effect on activity
                knots = list(Time = c(0,23)), 
                data = squirrel2, 
                method = "REML",
                family = "binomial")

AIC(squ400.mod, squ400.2)
# urban interaction: KEEP - DIFF < 1
# woodland interaction: KEEP - FULL LOWER
# light interaction: KEEP - DIFF < 1
# noise interaction: KEEP - DIFF < 4
# dog interaction: KEEP - NO DIFFERENCE
# month interaction: KEEP - FULL LOWER
# urban: KEEP - NO DIFFERENCE
# wood: KEEP - NO DIFFERENCE
# area: KEEP - FULL LOWER
# month: KEEP - NO DIFFERENCE
# dog_RA: KEEP - NO DIFFERENCE
# noise: KEEP - NO DIFFERENCE
# light: KEEP - NO DIFFERENCE

# FINAL
squ400.final <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 25) + 
                      s(scale(Time), bs = "cc", k = 25, by = scale(urban400)) + # interaction: time*urban
                      s(scale(Time), bs = "cc", k = 25, by = scale(wood400)) + # interaction: time*wood
                      s(scale(Time), bs = "cc", k = 25, by = scale(light400)) + # interaction: time*light
                      s(scale(Time), bs = "cc", k = 25, by = scale(noise400)) + # interaction: time*noise
                      s(scale(Time), bs = "cc", k = 25, by = scale(dog_RA)) + # interaction: time*dog_RA
                      s(scale(Time), bs = "cc", k = 25, by = scale(month2)) + # interaction: time*month
                      s(SiteSession, bs = 're') + # random effect
                      scale(urban400) + # urban effect on activity level
                      scale(wood400) + # woodland effect on activity level
                      scale(area) + # area effect
                      scale(month2) + # month effect on activity
                      scale(dog_RA) + # dog effect on activity
                      scale(noise400) + # noise effect on activity
                      scale(light400), # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = squirrel2, 
                    method = "REML",
                    family = "binomial")
gam.check(squ400.final)
summary(squ400.final)
concurvity(squ400.final)
plot(squ400.final)
appraise(squ400.final, method = "simulate")

############
### DEER ###
############

# LOCAL #

# global model
dee250.mod <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 20) + 
                    s(scale(Time), bs = "cc", k = 20, by = scale(urban250)) + # interaction: time*urban
                    s(scale(Time), bs = "cc", k = 20, by = scale(wood250)) + # interaction: time*wood
                    #s(scale(Time), bs = "cc", k = 20, by = scale(area)) + # interaction: time*area
                    s(scale(Time), bs = "cc", k = 20, by = scale(light250)) + # interaction: time*light
                    s(scale(Time), bs = "cc", k = 20, by = scale(noise250)) + # interaction: time*noise
                    s(scale(Time), bs = "cc", k = 20, by = scale(dog_RA)) + # interaction: time*dog_RA
                    s(scale(Time), bs = "cc", k = 20, by = scale(month2)) + # interaction: time*month
                    s(Site, bs = 're') + # random effect
                    scale(urban250) + # urban effect on activity level
                    scale(wood250) + # woodland effect on activity level
                    scale(area) + # area effect
                    scale(month2) + # month effect on activity
                    scale(dog_RA) + # dog effect on activity
                    scale(noise250) + # noise effect on activity
                    scale(light250), # light effect on activity
                  knots = list(Time = c(0,23)), 
                  data = deer2, 
                  method = "REML",
                  family = "binomial")
gam.check(dee250.mod)
summary(dee250.mod)
concurvity(dee250.mod) # remove area
plot(dee250.mod)

# first drop
dee250.2 <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 20) + 
                  s(scale(Time), bs = "cc", k = 20, by = scale(urban250)) + # interaction: time*urban
                  s(scale(Time), bs = "cc", k = 20, by = scale(wood250)) + # interaction: time*wood
                  s(scale(Time), bs = "cc", k = 20, by = scale(light250)) + # interaction: time*light
                  s(scale(Time), bs = "cc", k = 20, by = scale(noise250)) + # interaction: time*noise
                  s(scale(Time), bs = "cc", k = 20, by = scale(dog_RA)) + # interaction: time*dog_RA
                  s(scale(Time), bs = "cc", k = 20, by = scale(month2)) + # interaction: time*month
                  s(Site, bs = 're') + # random effect
                  scale(urban250) + # urban effect on activity level
                  scale(wood250) + # woodland effect on activity level
                  scale(area) + # area effect
                  scale(month2) + # month effect on activity
                  scale(dog_RA) + # dog effect on activity
                  scale(noise250),# + # noise effect on activity
                  #scale(light250), # light effect on activity
                knots = list(Time = c(0,23)), 
                data = deer2, 
                method = "REML",
                family = "binomial")

AIC(dee250.mod, dee250.2)
# urban interaction: KEEP - DIFF < 1
# woodland interaction: KEEP - FULL LOWER
# light interaction: KEEP - FULL LOWER
# noise interaction: KEEP - FULL LOWER
# dog interaction: KEEP - FULL LOWER
# month interaction: KEEP - FULL LOWER
# urban: KEEP - NO DIFFERENCE
# woodland: KEEP - NO DIFFERENCE
# area: KEEP - FULL LOWER
# month: KEEP - NO DIFFERENCE
# dog_RA: KEEP - NO DIFFERENCE
# noise: KEEP - NO DIFFERENCE
# light: KEEP - NO DIFFERENCE

# FINAL
dee250.final <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 20) + 
                      s(scale(Time), bs = "cc", k = 20, by = scale(urban250)) + # interaction: time*urban
                      s(scale(Time), bs = "cc", k = 20, by = scale(wood250)) + # interaction: time*wood
                      s(scale(Time), bs = "cc", k = 20, by = scale(light250)) + # interaction: time*light
                      s(scale(Time), bs = "cc", k = 20, by = scale(noise250)) + # interaction: time*noise
                      s(scale(Time), bs = "cc", k = 20, by = scale(dog_RA)) + # interaction: time*dog_RA
                      s(scale(Time), bs = "cc", k = 20, by = scale(month2)) + # interaction: time*month
                      s(Site, bs = 're') + # random effect
                      scale(urban250) + # urban effect on activity level
                      scale(wood250) + # woodland effect on activity level
                      scale(area) + # area effect
                      scale(month2) + # month effect on activity
                      scale(dog_RA) + # dog effect on activity
                      scale(noise250) + # noise effect on activity
                      scale(light250), # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = deer2, 
                    method = "REML",
                    family = "binomial")
gam.check(dee250.final)
summary(dee250.final)
concurvity(dee250.final)
plot(dee250.final)
appraise(dee250.final, method = "simulate")


# LANDSCAPE #

# global model
dee1km.mod <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 20) + 
                    s(scale(Time), bs = "cc", k = 20, by = scale(urban1km)) + # interaction: time*urban
                    #s(scale(Time), bs = "cc", k = 20, by = scale(wood1km)) + # interaction: time*wood
                    #s(scale(Time), bs = "cc", k = 20, by = scale(area)) + # interaction: time*area
                    s(scale(Time), bs = "cc", k = 20, by = scale(light1km)) + # interaction: time*light
                    s(scale(Time), bs = "cc", k = 20, by = scale(noise1km)) + # interaction: time*noise
                    s(scale(Time), bs = "cc", k = 20, by = scale(dog_RA)) + # interaction: time*dog_RA
                    s(scale(Time), bs = "cc", k = 20, by = scale(month2)) + # interaction: time*month
                    s(Site, bs = 're') + # random effect
                    scale(urban1km) + # urban effect on activity level
                    scale(wood1km) + # woodland effect on activity level
                    scale(area) + # area effect
                    scale(month2) + # month effect on activity
                    scale(dog_RA) + # dog effect on activity
                    scale(noise1km) + # noise effect on activity
                    scale(light1km), # light effect on activity
                  knots = list(Time = c(0,23)), 
                  data = deer2, 
                  method = "REML",
                  family = "binomial")
gam.check(dee1km.mod)
summary(dee1km.mod)
concurvity(dee1km.mod) # remove area
plot(dee1km.mod)

# first drop
dee1km.2 <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 20) + 
                  s(scale(Time), bs = "cc", k = 20, by = scale(urban1km)) + # interaction: time*urban
                  s(scale(Time), bs = "cc", k = 20, by = scale(light1km)) + # interaction: time*light
                  s(scale(Time), bs = "cc", k = 20, by = scale(noise1km)) + # interaction: time*noise
                  s(scale(Time), bs = "cc", k = 20, by = scale(dog_RA)) + # interaction: time*dog_RA
                  s(scale(Time), bs = "cc", k = 20, by = scale(month2)) + # interaction: time*month
                  s(Site, bs = 're') + # random effect
                  scale(urban1km) + # urban effect on activity level
                  scale(wood1km) + # woodland effect on activity level
                  scale(area) + # area effect
                  scale(month2) + # month effect on activity
                  scale(dog_RA) + # dog effect on activity
                  scale(noise1km),# + # noise effect on activity
                  #scale(light1km), # light effect on activity
                knots = list(Time = c(0,23)), 
                data = deer2, 
                method = "REML",
                family = "binomial")

AIC(dee1km.mod, dee1km.2)
# urban interaction: KEEP - DIFF < 3
# light interaction: KEEP - LOWER IN FULL
# noise interaction: KEEP - LOWER IN FULL
# dog interaction: KEEP - LOWER IN FULL
# month interaction: KEEP - LOWER IN FULL
# urban: KEEP - NO DIFFERENCE
# woodland: KEEP - LOWER IN FULL
# area: KEEP - LOWER IN FULL
# month: KEEP - LOWER IN FULL
# dog_RA: KEEP - LOWER IN FULL
# noise: KEEP - NO DIFFERENCE
# light: KEEP - NO DIFFERENCE

# FINAL
dee1km.final <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 20) + 
                      s(scale(Time), bs = "cc", k = 20, by = scale(urban1km)) + # interaction: time*urban
                      s(scale(Time), bs = "cc", k = 20, by = scale(light1km)) + # interaction: time*light
                      s(scale(Time), bs = "cc", k = 20, by = scale(noise1km)) + # interaction: time*noise
                      s(scale(Time), bs = "cc", k = 20, by = scale(dog_RA)) + # interaction: time*dog_RA
                      s(scale(Time), bs = "cc", k = 20, by = scale(month2)) + # interaction: time*month
                      s(Site, bs = 're') + # random effect
                      scale(urban1km) + # urban effect on activity level
                      scale(wood1km) + # woodland effect on activity level
                      scale(area) + # area effect
                      scale(month2) + # month effect on activity
                      scale(dog_RA) + # dog effect on activity
                      scale(noise1km) + # noise effect on activity
                      scale(light1km), # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = deer2, 
                    method = "REML",
                    family = "binomial")
gam.check(dee1km.final)
summary(dee1km.final)
concurvity(dee1km.final)
plot(dee1km.final)
appraise(dee1km.final, method = "simulate")

###########
### FOX ###
###########

# LOCAL #

# global model
fox250.mod <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 22) + 
                    s(scale(Time), bs = "cc", k = 22, by = scale(urban250)) + # interaction: time*urban
                    s(scale(Time), bs = "cc", k = 22, by = scale(wood250)) + # interaction: time*wood
                    #s(scale(Time), bs = "cc", k = 22, by = scale(area)) + # interaction: time*area
                    s(scale(Time), bs = "cc", k = 22, by = scale(light250)) + # interaction: time*light
                    s(scale(Time), bs = "cc", k = 22, by = scale(noise250)) + # interaction: time*noise
                    s(scale(Time), bs = "cc", k = 22, by = scale(dog_RA)) + # interaction: time*dog_RA
                    s(scale(Time), bs = "cc", k = 22, by = scale(month2)) + # interaction: time*month
                    s(Site, bs = 're') + # random effect
                    scale(urban250) + # urban effect on activity level
                    scale(wood250) + # woodland effect on activity level
                    scale(area) + # area effect
                    scale(month2) + # month effect on activity
                    scale(dog_RA) + # dog effect on activity
                    scale(noise250) + # noise effect on activity
                    scale(light250), # light effect on activity
                  knots = list(Time = c(0,23)), 
                  data = fox2, 
                  method = "REML",
                  family = "binomial")
gam.check(fox250.mod)
summary(fox250.mod)
concurvity(fox250.mod) # remove area
plot(fox250.mod)

# first drop
fox250.2 <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 22) + 
                  s(scale(Time), bs = "cc", k = 22, by = scale(urban250)) + # interaction: time*urban
                  s(scale(Time), bs = "cc", k = 22, by = scale(wood250)) + # interaction: time*wood
                  s(scale(Time), bs = "cc", k = 22, by = scale(light250)) + # interaction: time*light
                  s(scale(Time), bs = "cc", k = 22, by = scale(noise250)) + # interaction: time*noise
                  s(scale(Time), bs = "cc", k = 22, by = scale(dog_RA)) + # interaction: time*dog_RA
                  s(scale(Time), bs = "cc", k = 22, by = scale(month2)) + # interaction: time*month
                  s(Site, bs = 're') + # random effect
                  scale(urban250) + # urban effect on activity level
                  scale(wood250) + # woodland effect on activity level
                  scale(area) + # area effect
                  scale(month2) + # month effect on activity
                  scale(dog_RA) + # dog effect on activity
                  scale(noise250),# + # noise effect on activity
                  #scale(light250), # light effect on activity
                knots = list(Time = c(0,23)), 
                data = fox2, 
                method = "REML",
                family = "binomial")

AIC(fox250.mod, fox250.2)
# urban interaction: KEEP - FULL LOWER
# woodland interaction: KEEP - FULL LOWER
# light interaction: KEEP - FULL LOWER
# noise interaction: KEEP - FULL LOWER
# dog interaction: KEEP - FULL LOWER
# month interaction: KEEP - FULL LOWER
# urban: KEEP - NO DIFFERENCE
# woodland: KEEP - NO DIFFERENCE
# area: KEEP - DIFF < 1
# month: KEEP - NO DIFFERENCE
# dog_RA: KEEP - NO DIFFERENCE
# noise: KEEP - NO DIFFERENCE
# light: KEEP - NO DIFFERENCE

# FINAL
fox250.final <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 22) + 
                      s(scale(Time), bs = "cc", k = 22, by = scale(urban250)) + # interaction: time*urban
                      s(scale(Time), bs = "cc", k = 22, by = scale(wood250)) + # interaction: time*wood
                      s(scale(Time), bs = "cc", k = 22, by = scale(light250)) + # interaction: time*light
                      s(scale(Time), bs = "cc", k = 22, by = scale(noise250)) + # interaction: time*noise
                      s(scale(Time), bs = "cc", k = 22, by = scale(dog_RA)) + # interaction: time*dog_RA
                      s(scale(Time), bs = "cc", k = 22, by = scale(month2)) + # interaction: time*month
                      s(Site, bs = 're') + # random effect
                      scale(urban250) + # urban effect on activity level
                      scale(wood250) + # woodland effect on activity level
                      scale(area) + # area effect
                      scale(month2) + # month effect on activity
                      scale(dog_RA) + # dog effect on activity
                      scale(noise250) + # noise effect on activity
                      scale(light250), # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = fox2, 
                    method = "REML",
                    family = "binomial")
gam.check(fox250.final)
summary(fox250.final)
concurvity(fox250.final)
plot(fox250.final)
appraise(fox250.final, method = "simulate")


# LANDSCAPE #

# global model
fox1km.mod <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 24) + 
                    s(scale(Time), bs = "cc", k = 24, by = scale(urban1km)) + # interaction: time*urban
                    #s(scale(Time), bs = "cc", k = 24, by = scale(wood1km)) + # interaction: time*wood
                    #s(scale(Time), bs = "cc", k = 24, by = scale(area)) + # interaction: time*area
                    s(scale(Time), bs = "cc", k = 24, by = scale(light1km)) + # interaction: time*light
                    s(scale(Time), bs = "cc", k = 24, by = scale(noise1km)) + # interaction: time*noise
                    s(scale(Time), bs = "cc", k = 24, by = scale(dog_RA)) + # interaction: time*dog_RA
                    s(scale(Time), bs = "cc", k = 24, by = scale(month2)) + # interaction: time*month
                    s(Site, bs = 're') + # random effect
                    scale(urban1km) + # urban effect on activity level
                    scale(wood1km) + # woodland effect on activity level
                    scale(area) + # area effect
                    scale(month2) + # month effect on activity
                    scale(dog_RA) + # dog effect on activity
                    scale(noise1km) + # noise effect on activity
                    scale(light1km), # light effect on activity
                  knots = list(Time = c(0,23)), 
                  data = fox2, 
                  method = "REML",
                  family = "binomial")
gam.check(fox1km.mod)
summary(fox1km.mod)
concurvity(fox1km.mod) # remove area and woodland
plot(fox1km.mod)

# first drop
fox1km.2 <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 24) + 
                  s(scale(Time), bs = "cc", k = 24, by = scale(urban1km)) + # interaction: time*urban
                  s(scale(Time), bs = "cc", k = 24, by = scale(light1km)) + # interaction: time*light
                  s(scale(Time), bs = "cc", k = 24, by = scale(noise1km)) + # interaction: time*noise
                  s(scale(Time), bs = "cc", k = 24, by = scale(dog_RA)) + # interaction: time*dog_RA
                  s(scale(Time), bs = "cc", k = 24, by = scale(month2)) + # interaction: time*month
                  s(Site, bs = 're') + # random effect
                  scale(urban1km) + # urban effect on activity level
                  scale(wood1km) + # woodland effect on activity level
                  scale(area) + # area effect
                  scale(month2) + # month effect on activity
                  scale(dog_RA) + # dog effect on activity
                  scale(noise1km),# + # noise effect on activity
                  #scale(light1km), # light effect on activity
                knots = list(Time = c(0,23)), 
                data = fox2, 
                method = "REML",
                family = "binomial")

AIC(fox1km.mod, fox1km.2)
# urban interaction: KEEP - FULL LOWER
# light interaction: KEEP - FULL LOWER
# noise interaction: KEEP - FULL LOWER
# dog interaction: KEEP - FULL LOWER
# month interaction: KEEP - FULL LOWER
# urban: KEEP - FULL LOWER
# woodland: KEEP - FULL LOWER
# area: KEEP - FULL LOWER
# month: KEEP - FULL LOWER
# dog_RA: KEEP - FULL LOWER
# noise: KEEP - FULL LOWER
# light: KEEP - FULL LOWER

# FINAL
fox1km.final <- bam(cbind(success, failure) ~  s(scale(Time), bs = "cc", k = 24) + 
                      s(scale(Time), bs = "cc", k = 24, by = scale(urban1km)) + # interaction: time*urban
                      s(scale(Time), bs = "cc", k = 24, by = scale(light1km)) + # interaction: time*light
                      s(scale(Time), bs = "cc", k = 24, by = scale(noise1km)) + # interaction: time*noise
                      s(scale(Time), bs = "cc", k = 24, by = scale(dog_RA)) + # interaction: time*dog_RA
                      s(scale(Time), bs = "cc", k = 24, by = scale(month2)) + # interaction: time*month
                      s(Site, bs = 're') + # random effect
                      scale(urban1km) + # urban effect on activity level
                      scale(wood1km) + # woodland effect on activity level
                      scale(area) + # area effect
                      scale(month2) + # month effect on activity
                      scale(dog_RA) + # dog effect on activity
                      scale(noise1km) + # noise effect on activity
                      scale(light1km), # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = fox2, 
                    method = "REML",
                    family = "binomial")
gam.check(fox1km.final)
summary(fox1km.final)
concurvity(fox1km.final)
plot(fox1km.final)
appraise(fox1km.final, method = "simulate")
