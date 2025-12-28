# ACTIVITY FINAL MODELS AND PLOTS

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
library(itsadug)
library(broom.mixed)
library(ggh4x)

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
fox$date <- as.POSIXct(fox$start, format = "%Y-%m-%d")
fox$datej <- as.integer(format(fox$date, "%j"))
deer$date <- as.POSIXct(deer$start, format = "%Y-%m-%d")
deer$datej <- as.integer(format(deer$date, "%j"))
squirrel$date <- as.POSIXct(squirrel$start, format = "%Y-%m-%d")
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

#######################
### SCALE VARIABLES ###
#######################
# useful later, for graphs

# deer
deer2$Time_s <- scale(deer2$Time)
deer2$wood250_s <- scale(deer2$wood250)
deer2$wood1km_s <- scale(deer2$wood1km)
deer2$urban250_s <- scale(deer2$urban250)
deer2$urban1km_s <- scale(deer2$urban1km)
deer2$noise250_s <- scale(deer2$noise250)
deer2$noise1km_s <- scale(deer2$noise1km)
deer2$light250_s <- scale(deer2$light250)
deer2$light1km_s <- scale(deer2$light1km)
deer2$dog_RA_s <- scale(deer2$dog_RA)
deer2$area_s <- scale(deer2$area)
deer2$month2_s <- scale(deer2$month2)
# fox
fox2$Time_s <- scale(fox2$Time)
fox2$wood250_s <- scale(fox2$wood250)
fox2$wood1km_s <- scale(fox2$wood1km)
fox2$urban250_s <- scale(fox2$urban250)
fox2$urban1km_s <- scale(fox2$urban1km)
fox2$noise250_s <- scale(fox2$noise250)
fox2$noise1km_s <- scale(fox2$noise1km)
fox2$light250_s <- scale(fox2$light250)
fox2$light1km_s <- scale(fox2$light1km)
fox2$dog_RA_s <- scale(fox2$dog_RA)
fox2$area_s <- scale(fox2$area)
fox2$month2_s <- scale(fox2$month2)
# squirrel
squirrel2$Time_s <- scale(squirrel2$Time)
squirrel2$wood100_s <- scale(squirrel2$wood100)
squirrel2$wood400_s <- scale(squirrel2$wood400)
squirrel2$urban100_s <- scale(squirrel2$urban100)
squirrel2$urban400_s <- scale(squirrel2$urban400)
squirrel2$noise100_s <- scale(squirrel2$noise100)
squirrel2$noise400_s <- scale(squirrel2$noise400)
squirrel2$light100_s <- scale(squirrel2$light100)
squirrel2$light400_s <- scale(squirrel2$light400)
squirrel2$dog_RA_s <- scale(squirrel2$dog_RA)
squirrel2$area_s <- scale(squirrel2$area)
squirrel2$month2_s <- scale(squirrel2$month2)

####################
### FINAL MODELS ###
####################

# FINAL - local squirrel
squ100.final <- bam(cbind(success, failure) ~  s(Time_s, bs = "cc", k = 20) + 
                      s(Time_s, bs = "cc", k = 20, by = urban100_s) + # interaction: time*urban
                      s(Time_s, bs = "cc", k = 20, by = wood100_s) + # interaction: time*wood
                      s(Time_s, bs = "cc", k = 20, by = area_s) + # interaction: time*area
                      s(Time_s, bs = "cc", k = 20, by = light100_s) + # interaction: time*light
                      s(Time_s, bs = "cc", k = 20, by = noise100_s) + # interaction: time*noise
                      s(Time_s, bs = "cc", k = 20, by = dog_RA_s) + # interaction: time*dog_RA
                      s(Time_s, bs = "cc", k = 20, by = month2_s) + # interaction: time*month
                      s(SiteSession, bs = 're') + # random effect
                      urban100_s + # urban effect on activity level
                      wood100_s + # woodland effect on activity level
                      area_s + # area effect
                      month2_s + # month effect on activity
                      dog_RA_s + # dog effect on activity
                      noise100_s + # noise effect on activity
                      light100_s, # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = squirrel2, 
                    method = "REML",
                    family = "binomial",
                    select = T)
summary(squ100.final)
gam.check(squ100.final)
# qq plots
appraise(squ100.final, method = "simulate")

# FINAL - landscape squirrel
squ400.final <- bam(cbind(success, failure) ~  s(Time_s, bs = "cc", k = 20) + 
                      s(Time_s, bs = "cc", k = 20, by = urban400_s) + # interaction: time*urban
                      s(Time_s, bs = "cc", k = 20, by = wood400_s) + # interaction: time*wood
                      s(Time_s, bs = "cc", k = 20, by = light400_s) + # interaction: time*light
                      s(Time_s, bs = "cc", k = 20, by = noise400_s) + # interaction: time*noise
                      s(Time_s, bs = "cc", k = 20, by = dog_RA_s) + # interaction: time*dog_RA
                      s(Time_s, bs = "cc", k = 20, by = month2_s) + # interaction: time*month
                      s(SiteSession, bs = 're') + # random effect
                      urban400_s + # urban effect on activity level
                      wood400_s + # woodland effect on activity level
                      area_s + # area effect
                      month2_s + # month effect on activity
                      dog_RA_s + # dog effect on activity
                      noise400_s + # noise effect on activity
                      light400_s, # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = squirrel2, 
                    method = "REML",
                    family = "binomial")
summary(squ400.final)
gam.check(squ400.final)
# qq plots
appraise(squ400.final, method = "simulate")

# FINAL - local deer
dee250.final <- bam(cbind(success, failure) ~  s(Time_s, bs = "cc", k = 20) + 
                      s(Time_s, bs = "cc", k = 20, by = urban250_s) + # interaction: time*urban
                      s(Time_s, bs = "cc", k = 20, by = wood250_s) + # interaction: time*wood
                      s(Time_s, bs = "cc", k = 20, by = light250_s) + # interaction: time*light
                      s(Time_s, bs = "cc", k = 20, by = noise250_s) + # interaction: time*noise
                      s(Time_s, bs = "cc", k = 20, by = dog_RA_s) + # interaction: time*dog_RA
                      s(Time_s, bs = "cc", k = 20, by = month2_s) + # interaction: time*month
                      s(SiteSession, bs = 're') + # random effect
                      urban250_s + # urban effect on activity level
                      wood250_s + # woodland effect on activity level
                      area_s + # area effect
                      month2_s + # month effect on activity
                      dog_RA_s + # dog effect on activity
                      noise250_s + # noise effect on activity
                      light250_s, # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = deer2, 
                    method = "REML",
                    family = "binomial")
summary(dee250.final)
gam.check(dee250.final)
# qq plots
appraise(dee250.final, method = "simulate")

# FINAL - landscape deer
dee1km.final <- bam(cbind(success, failure) ~  s(Time_s, bs = "cc", k = 20) + 
                      s(Time_s, bs = "cc", k = 20, by = urban1km_s) + # interaction: time*urban
                      s(Time_s, bs = "cc", k = 20, by = light1km_s) + # interaction: time*light
                      s(Time_s, bs = "cc", k = 20, by = noise1km_s) + # interaction: time*noise
                      s(Time_s, bs = "cc", k = 20, by = dog_RA_s) + # interaction: time*dog_RA
                      s(Time_s, bs = "cc", k = 20, by = month2_s) + # interaction: time*month
                      s(SiteSession, bs = 're') + # random effect
                      urban1km_s + # urban effect on activity level
                      wood1km_s + # woodland effect on activity level
                      area_s + # area effect
                      month2_s + # month effect on activity
                      dog_RA_s + # dog effect on activity
                      noise1km_s + # noise effect on activity
                      light1km_s, # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = deer2, 
                    method = "REML",
                    family = "binomial")
summary(dee1km.final)
gam.check(dee1km.final)
# qq plots
appraise(dee1km.final, method = "simulate")

# FINAL - local fox
fox250.final <- bam(cbind(success, failure) ~  s(Time_s, bs = "cc", k = 22) + 
                      s(Time_s, bs = "cc", k = 22, by = urban250_s) + # interaction: time*urban
                      s(Time_s, bs = "cc", k = 22, by = wood250_s) + # interaction: time*wood
                      s(Time_s, bs = "cc", k = 22, by = light250_s) + # interaction: time*light
                      s(Time_s, bs = "cc", k = 22, by = noise250_s) + # interaction: time*noise
                      s(Time_s, bs = "cc", k = 22, by = dog_RA_s) + # interaction: time*dog_RA
                      s(Time_s, bs = "cc", k = 22, by = month2_s) + # interaction: time*month
                      s(SiteSession, bs = 're') + # random effect
                      urban250_s + # urban effect on activity level
                      wood250_s + # woodland effect on activity level
                      area_s + # area effect
                      month2_s + # month effect on activity
                      dog_RA_s + # dog effect on activity
                      noise250_s + # noise effect on activity
                      light250_s, # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = fox2, 
                    method = "REML",
                    family = "binomial")
summary(fox250.final)
gam.check(fox250.final)
# qq plots
appraise(fox250.final, method = "simulate")

# FINAL - landscape fox
fox1km.final <- bam(cbind(success, failure) ~  s(Time_s, bs = "cc", k = 24) + 
                      s(Time_s, bs = "cc", k = 24, by = urban1km_s) + # interaction: time*urban
                      s(Time_s, bs = "cc", k = 24, by = light1km_s) + # interaction: time*light
                      s(Time_s, bs = "cc", k = 24, by = noise1km_s) + # interaction: time*noise
                      s(Time_s, bs = "cc", k = 24, by = dog_RA_s) + # interaction: time*dog_RA
                      s(Time_s, bs = "cc", k = 24, by = month2_s) + # interaction: time*month
                      s(SiteSession, bs = 're') + # random effect
                      urban1km_s + # urban effect on activity level
                      wood1km_s + # woodland effect on activity level
                      area_s + # area effect
                      month2_s + # month effect on activity
                      dog_RA_s + # dog effect on activity
                      noise1km_s + # noise effect on activity
                      light1km_s, # light effect on activity
                    knots = list(Time = c(0,23)), 
                    data = fox2, 
                    method = "REML",
                    family = "binomial")
summary(fox1km.final)
gam.check(fox1km.final)
# qq plots
appraise(fox1km.final, method = "simulate")



##############################
##############################
###### PLOTCRASTINATION ######
##############################
##############################


##########################
### PARAMETRIC EFFECTS ###
##########################

plot_model(dee250.final, 
           type = "est", 
           transform = "plogis", 
           title = "Parametric Effects on Activity Probability",
           show.values = TRUE)

####################################################
### ACTIVITY AT DIFFERENT LEVELS OF URBANISATION ###
####################################################

# SQU100 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(squirrel2$dog_RA)
area <- quantile(squirrel2$area)
light <- quantile(squirrel2$light100)
noise <- quantile(squirrel2$noise100)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = month,
                      #urban100 = rep(c(min(squirrel2$urban100), mean(squirrel2$urban100), max(squirrel2$urban100)), times = 1),
                      urban100 = unique(squirrel2$urban100),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light100 = light[c("25%", "75%")],
                      noise100 = noise[c("25%", "75%")])
newdat$wood100 <- 0.9-newdat$urban100

# scaled values of predictors
newdat$urban100_s <- scale(newdat$urban100)
newdat$wood100_s <- scale(newdat$wood100)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise100_s <- scale(newdat$noise100)
newdat$light100_s <- scale(newdat$light100)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ100pred <- predict(squ100.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ100pred <- as.data.frame(squ100pred)

# add predictions to original data frame
newdat$preds <- squ100pred$fit
newdat$se <- squ100pred$se.fit

# urbanisation categories
newdat$urbancat <- cut(
  newdat$urban100,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
squloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, urbancat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squloc_binned)
squloc_binned$scale <- "Local"
squloc_binned$species <- "Grey Squirrel"



# SQU400 - TIME, urban, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(squirrel2$dog_RA)
area <- quantile(squirrel2$area)
light <- quantile(squirrel2$light400)
noise <- quantile(squirrel2$noise400)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = month,
                      #urban100 = rep(c(min(squirrel2$urban400), mean(squirrel2$urban400), max(squirrel2$urban400)), times = 1),
                      urban400 = unique(squirrel2$urban400),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light400 = light[c("25%", "75%")],
                      noise400 = noise[c("25%", "75%")])
newdat$wood400 <- 0.9 - newdat$urban400

# scaled values of predictors
newdat$urban400_s <- scale(newdat$urban400)
newdat$wood400_s <- scale(newdat$wood400)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise400_s <- scale(newdat$noise400)
newdat$light400_s <- scale(newdat$light400)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ400pred <- predict(squ400.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ400pred <- as.data.frame(squ400pred)

# add predictions to original data frame
newdat$preds <- squ400pred$fit
newdat$se <- squ400pred$se.fit

# urbanisation categories
newdat$urbancat <- cut(
  newdat$urban400,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
squlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, urbancat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squlan_binned)
squlan_binned$scale <- "Landscape"
squlan_binned$species <- "Grey Squirrel"



# FOX250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(fox2$dog_RA)
area <- quantile(fox2$area)
light <- quantile(fox2$light250)
noise <- quantile(fox2$noise250)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(fox2$urban250), mean(fox2$urban250), max(fox2$urban250)), times = 1),
                      urban250 = unique(fox2$urban250),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light250 = light[c("25%", "75%")],
                      noise250 = noise[c("25%", "75%")])
newdat$wood250 <- 0.9-newdat$urban250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox250pred <- predict(fox250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox250pred <- as.data.frame(fox250pred)

# add predictions to original data frame
newdat$preds <- fox250pred$fit
newdat$se <- fox250pred$se.fit

# urbanisation categories
newdat$urbancat <- cut(
  newdat$urban250,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
foxloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, urbancat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxloc_binned)
foxloc_binned$scale <- "Local"
foxloc_binned$species <- "Red Fox"



# FOX1KM - TIME, urban, wood, light noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(fox2$dog_RA)
area <- quantile(fox2$area)
light <- quantile(fox2$light1km)
noise <- quantile(fox2$noise1km)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(fox2$urban1km), mean(fox2$urban1km), max(fox2$urban1km)), times = 1),
                      urban1km = unique(fox2$urban1km),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light1km = light[c("25%", "75%")],
                      noise1km = noise[c("25%", "75%")])
newdat$wood1km <- 0.9-newdat$urban1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox1kmpred <- predict(fox1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox1kmpred <- as.data.frame(fox1kmpred)

# add predictions to original data frame
newdat$preds <- fox1kmpred$fit
newdat$se <- fox1kmpred$se.fit

# urbanisation categories
newdat$urbancat <- cut(
  newdat$urban1km,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
foxlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, urbancat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxlan_binned)
foxlan_binned$scale <- "Landscape"
foxlan_binned$species <- "Red Fox"



# DEER250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(deer2$dog_RA)
area <- quantile(deer2$area)
light <- quantile(deer2$light250)
noise <- quantile(deer2$noise250)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      urban250 = unique(deer2$urban250),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light250 = light[c("25%", "75%")],
                      noise250 = noise[c("25%", "75%")])
newdat$wood250 <- 0.9-newdat$urban250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer250pred <- predict(dee250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer250pred <- as.data.frame(deer250pred)

# add predictions to original data frame
newdat$preds <- deer250pred$fit
newdat$se <- deer250pred$se.fit

# urbanisation categories
newdat$urbancat <- cut(
  newdat$urban250,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
deerloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, urbancat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerloc_binned)
deerloc_binned$scale <- "Local"
deerloc_binned$species <- "Roe Deer"



# DEER1KM - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession 

# find quantile values for variables
dog <- quantile(deer2$dog_RA)
area <- quantile(deer2$area)
light <- quantile(deer2$light1km)
noise <- quantile(deer2$noise1km)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      urban1km = unique(deer2$urban1km),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light1km = light[c("25%", "75%")],
                      noise1km = noise[c("25%", "75%")])
newdat$wood1km <- 0.9-newdat$urban1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer1kmpred <- predict(dee1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer1kmpred <- as.data.frame(deer1kmpred)

# add predictions to original data frame
newdat$preds <- deer1kmpred$fit
newdat$se <- deer1kmpred$se.fit

# urbanisation categories
newdat$urbancat <- cut(
  newdat$urban1km,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
deerlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, urbancat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerlan_binned)
deerlan_binned$scale <- "Landscape"
deerlan_binned$species <- "Roe Deer"



# full data frame
urb_all_binned <- rbind(deerloc_binned, deerlan_binned, 
                    squloc_binned, squlan_binned, 
                    foxloc_binned, foxlan_binned)

write.csv(urb_all_binned, file = "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/urban_activity_df.csv")

urb_all_binned <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/urban_activity_df.csv")

urb_all_binned$urbancat <- factor(urb_all_binned$urbancat, levels = c("Low", "Medium", "High"))

# plot
ggplot(urb_all_binned, aes(x = mean_time, y = mean_pred, color = urbancat)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_pred - se_pred, ymax = mean_pred + se_pred, fill = urbancat), alpha = 0.07) +
  labs(x = "Time", y = "Activity", color = "Urbanisation", fill = "Urbanisation") +
  theme_bw() +
  scale_color_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  scale_fill_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  #facet_grid(rows = vars(scale), cols = vars(species))
  facet_grid(factor(scale, levels = c("Local", "Landscape")) ~ species) +
  theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )




### LIGHT

# SQU100 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(squirrel2$dog_RA)
area <- quantile(squirrel2$area)
urban <- quantile(squirrel2$urban100)
noise <- quantile(squirrel2$noise100)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = month,
                      #urban100 = rep(c(min(squirrel2$urban100), mean(squirrel2$urban100), max(squirrel2$urban100)), times = 1),
                      light100 = unique(squirrel2$light100),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban100 = urban[c("25%", "75%")],
                      noise100 = noise[c("25%", "75%")])
newdat$wood100 <- 0.9-newdat$urban100

# scaled values of predictors
newdat$urban100_s <- scale(newdat$urban100)
newdat$wood100_s <- scale(newdat$wood100)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise100_s <- scale(newdat$noise100)
newdat$light100_s <- scale(newdat$light100)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ100pred <- predict(squ100.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ100pred <- as.data.frame(squ100pred)

# add predictions to original data frame
newdat$preds <- squ100pred$fit
newdat$se <- squ100pred$se.fit

# urbanisation categories
newdat$lightcat <- cut(
  newdat$light100,
  breaks = c(0, 10, 20, 60),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
squloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, lightcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squloc_binned)
squloc_binned$scale <- "Local"
squloc_binned$species <- "Grey Squirrel"



# SQU400 - TIME, urban, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(squirrel2$dog_RA)
area <- quantile(squirrel2$area)
urban <- quantile(squirrel2$urban400)
noise <- quantile(squirrel2$noise400)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = month,
                      #urban100 = rep(c(min(squirrel2$urban400), mean(squirrel2$urban400), max(squirrel2$urban400)), times = 1),
                      light400 = unique(squirrel2$light400),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban400 = urban[c("25%", "75%")],
                      noise400 = noise[c("25%", "75%")])
newdat$wood400 <- 0.9 - newdat$urban400

# scaled values of predictors
newdat$urban400_s <- scale(newdat$urban400)
newdat$wood400_s <- scale(newdat$wood400)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise400_s <- scale(newdat$noise400)
newdat$light400_s <- scale(newdat$light400)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ400pred <- predict(squ400.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ400pred <- as.data.frame(squ400pred)

# add predictions to original data frame
newdat$preds <- squ400pred$fit
newdat$se <- squ400pred$se.fit

# urbanisation categories
newdat$lightcat <- cut(
  newdat$light400,
  breaks = c(0, 10, 20, 60),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
squlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, lightcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squlan_binned)
squlan_binned$scale <- "Landscape"
squlan_binned$species <- "Grey Squirrel"



# FOX250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(fox2$dog_RA)
area <- quantile(fox2$area)
urban <- quantile(fox2$urban250)
noise <- quantile(fox2$noise250)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(fox2$urban250), mean(fox2$urban250), max(fox2$urban250)), times = 1),
                      light250 = unique(fox2$light250),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban250 = urban[c("25%", "75%")],
                      noise250 = noise[c("25%", "75%")])
newdat$wood250 <- 0.9-newdat$urban250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox250pred <- predict(fox250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox250pred <- as.data.frame(fox250pred)

# add predictions to original data frame
newdat$preds <- fox250pred$fit
newdat$se <- fox250pred$se.fit

# urbanisation categories
newdat$lightcat <- cut(
  newdat$light250,
  breaks = c(0, 10, 20, 60),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
foxloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, lightcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxloc_binned)
foxloc_binned$scale <- "Local"
foxloc_binned$species <- "Red Fox"



# FOX1KM - TIME, urban, wood, light noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(fox2$dog_RA)
area <- quantile(fox2$area)
urban <- quantile(fox2$urban1km)
noise <- quantile(fox2$noise1km)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(fox2$urban1km), mean(fox2$urban1km), max(fox2$urban1km)), times = 1),
                      light1km = unique(fox2$light1km),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban1km = urban[c("25%", "75%")],
                      noise1km = noise[c("25%", "75%")])
newdat$wood1km <- 0.9-newdat$urban1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox1kmpred <- predict(fox1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox1kmpred <- as.data.frame(fox1kmpred)

# add predictions to original data frame
newdat$preds <- fox1kmpred$fit
newdat$se <- fox1kmpred$se.fit

# urbanisation categories
newdat$lightcat <- cut(
  newdat$light1km,
  breaks = c(0, 10, 20, 60),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
foxlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, lightcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxlan_binned)
foxlan_binned$scale <- "Landscape"
foxlan_binned$species <- "Red Fox"



# DEER250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(deer2$dog_RA)
area <- quantile(deer2$area)
urban <- quantile(deer2$urban250)
noise <- quantile(deer2$noise250)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      light250 = unique(deer2$light250),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban250 = urban[c("25%", "75%")],
                      noise250 = noise[c("25%", "75%")])
newdat$wood250 <- 0.9-newdat$urban250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer250pred <- predict(dee250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer250pred <- as.data.frame(deer250pred)

# add predictions to original data frame
newdat$preds <- deer250pred$fit
newdat$se <- deer250pred$se.fit

# urbanisation categories
newdat$lightcat <- cut(
  newdat$light250,
  breaks = c(0, 10, 20, 60),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
deerloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, lightcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerloc_binned)
deerloc_binned$scale <- "Local"
deerloc_binned$species <- "Roe Deer"



# DEER1KM - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession 

# find quantile values for variables
dog <- quantile(deer2$dog_RA)
area <- quantile(deer2$area)
urban <- quantile(deer2$urban1km)
noise <- quantile(deer2$noise1km)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      light1km = unique(deer2$light1km),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban1km = urban[c("25%", "75%")],
                      noise1km = noise[c("25%", "75%")])
newdat$wood1km <- 0.9-newdat$urban1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer1kmpred <- predict(dee1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer1kmpred <- as.data.frame(deer1kmpred)

# add predictions to original data frame
newdat$preds <- deer1kmpred$fit
newdat$se <- deer1kmpred$se.fit

# urbanisation categories
newdat$lightcat <- cut(
  newdat$light1km,
  breaks = c(0, 10, 20, 60),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
deerlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, lightcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerlan_binned)
deerlan_binned$scale <- "Landscape"
deerlan_binned$species <- "Roe Deer"



# full data frame
light_all_binned <- rbind(deerloc_binned, deerlan_binned, 
                        squloc_binned, squlan_binned, 
                        foxloc_binned, foxlan_binned)

write.csv(light_all_binned, file = "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/light_activity_df.csv")

light_all_binned <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/light_activity_df.csv")

light_all_binned$lightcat <- factor(light_all_binned$lightcat, levels = c("Low", "Medium", "High"))
light_all_binned$species <- factor(light_all_binned$species, levels = c("Grey Squirrel", "Red Fox", "Roe Deer"))

# plot
ggplot(light_all_binned, aes(x = mean_time, y = mean_pred, color = lightcat)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = mean_pred - se_pred, ymax = mean_pred + se_pred, fill = lightcat), alpha = 0.07) +
  labs(x = "Time", y = "Activity", color = "Light Pollution", fill = "Light Pollution") +
  theme_bw() +
  scale_color_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  scale_fill_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  #facet_grid(rows = vars(scale), cols = vars(species))
  facet_grid(factor(scale, levels = c("Local", "Landscape")) ~ species) +
  theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )



### NOISE

# SQU100 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(squirrel2$dog_RA)
area <- quantile(squirrel2$area)
urban <- quantile(squirrel2$urban100)
light <- quantile(squirrel2$light100)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = month,
                      #urban100 = rep(c(min(squirrel2$urban100), mean(squirrel2$urban100), max(squirrel2$urban100)), times = 1),
                      noise100 = unique(squirrel2$noise100),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban100 = urban[c("25%", "75%")],
                      light100 = light[c("25%", "75%")])
newdat$wood100 <- 0.9-newdat$urban100

# scaled values of predictors
newdat$urban100_s <- scale(newdat$urban100)
newdat$wood100_s <- scale(newdat$wood100)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise100_s <- scale(newdat$noise100)
newdat$light100_s <- scale(newdat$light100)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ100pred <- predict(squ100.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ100pred <- as.data.frame(squ100pred)

# add predictions to original data frame
newdat$preds <- squ100pred$fit
newdat$se <- squ100pred$se.fit

# urbanisation categories
newdat$noisecat <- cut(
  newdat$noise100,
  breaks = c(0, 40, 53, 66),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
squloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, noisecat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squloc_binned)
squloc_binned$scale <- "Local"
squloc_binned$species <- "Grey Squirrel"



# SQU400 - TIME, urban, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(squirrel2$dog_RA)
area <- quantile(squirrel2$area)
urban <- quantile(squirrel2$urban400)
light <- quantile(squirrel2$light400)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = month,
                      #urban100 = rep(c(min(squirrel2$urban400), mean(squirrel2$urban400), max(squirrel2$urban400)), times = 1),
                      noise400 = unique(squirrel2$noise400),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban400 = urban[c("25%", "75%")],
                      light400 = light[c("25%", "75%")])
newdat$wood400 <- 0.9 - newdat$urban400

# scaled values of predictors
newdat$urban400_s <- scale(newdat$urban400)
newdat$wood400_s <- scale(newdat$wood400)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise400_s <- scale(newdat$noise400)
newdat$light400_s <- scale(newdat$light400)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ400pred <- predict(squ400.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ400pred <- as.data.frame(squ400pred)

# add predictions to original data frame
newdat$preds <- squ400pred$fit
newdat$se <- squ400pred$se.fit

# urbanisation categories
newdat$noisecat <- cut(
  newdat$noise400,
  breaks = c(0, 40, 53, 66),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
squlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, noisecat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squlan_binned)
squlan_binned$scale <- "Landscape"
squlan_binned$species <- "Grey Squirrel"



# FOX250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(fox2$dog_RA)
area <- quantile(fox2$area)
urban <- quantile(fox2$urban250)
light <- quantile(fox2$light250)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(fox2$urban250), mean(fox2$urban250), max(fox2$urban250)), times = 1),
                      noise250 = unique(fox2$noise250),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban250 = urban[c("25%", "75%")],
                      light250 = light[c("25%", "75%")])
newdat$wood250 <- 0.9-newdat$urban250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox250pred <- predict(fox250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox250pred <- as.data.frame(fox250pred)

# add predictions to original data frame
newdat$preds <- fox250pred$fit
newdat$se <- fox250pred$se.fit

# urbanisation categories
newdat$noisecat <- cut(
  newdat$noise250,
  breaks = c(0, 40, 53, 66),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
foxloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, noisecat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxloc_binned)
foxloc_binned$scale <- "Local"
foxloc_binned$species <- "Red Fox"



# FOX1KM - TIME, urban, wood, light noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(fox2$dog_RA)
area <- quantile(fox2$area)
urban <- quantile(fox2$urban1km)
light <- quantile(fox2$light1km)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(fox2$urban1km), mean(fox2$urban1km), max(fox2$urban1km)), times = 1),
                      noise1km = unique(fox2$noise1km),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban1km = urban[c("25%", "75%")],
                      light1km = light[c("25%", "75%")])
newdat$wood1km <- 0.9-newdat$urban1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox1kmpred <- predict(fox1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox1kmpred <- as.data.frame(fox1kmpred)

# add predictions to original data frame
newdat$preds <- fox1kmpred$fit
newdat$se <- fox1kmpred$se.fit

# urbanisation categories
newdat$noisecat <- cut(
  newdat$noise1km,
  breaks = c(0, 40, 53, 66),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
foxlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, noisecat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxlan_binned)
foxlan_binned$scale <- "Landscape"
foxlan_binned$species <- "Red Fox"



# DEER250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(deer2$dog_RA)
area <- quantile(deer2$area)
urban <- quantile(deer2$urban250)
light <- quantile(deer2$light250)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      noise250 = unique(deer2$noise250),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban250 = urban[c("25%", "75%")],
                      light250 = light[c("25%", "75%")])
newdat$wood250 <- 0.9-newdat$urban250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer250pred <- predict(dee250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer250pred <- as.data.frame(deer250pred)

# add predictions to original data frame
newdat$preds <- deer250pred$fit
newdat$se <- deer250pred$se.fit

# urbanisation categories
newdat$noisecat <- cut(
  newdat$noise250,
  breaks = c(0, 40, 53, 66),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
deerloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, noisecat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerloc_binned)
deerloc_binned$scale <- "Local"
deerloc_binned$species <- "Roe Deer"



# DEER1KM - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession 

# find quantile values for variables
dog <- quantile(deer2$dog_RA)
area <- quantile(deer2$area)
urban <- quantile(deer2$urban1km)
light <- quantile(deer2$light1km)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      noise1km = unique(deer2$noise1km),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban1km = urban[c("25%", "75%")],
                      light1km = light[c("25%", "75%")])
newdat$wood1km <- 0.9-newdat$urban1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer1kmpred <- predict(dee1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer1kmpred <- as.data.frame(deer1kmpred)

# add predictions to original data frame
newdat$preds <- deer1kmpred$fit
newdat$se <- deer1kmpred$se.fit

# urbanisation categories
newdat$noisecat <- cut(
  newdat$noise1km,
  breaks = c(0, 40, 53, 66),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
deerlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, noisecat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerlan_binned)
deerlan_binned$scale <- "Landscape"
deerlan_binned$species <- "Roe Deer"



# full data frame
noise_all_binned <- rbind(deerloc_binned, deerlan_binned, 
                          squloc_binned, squlan_binned, 
                          foxloc_binned, foxlan_binned)

write.csv(noise_all_binned, file = "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/noise_activity_df.csv")

noise_all_binned <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/noise_activity_df.csv")

noise_all_binned$noisecat <- factor(noise_all_binned$noisecat, levels = c("Low", "Medium", "High"))
noise_all_binned$species <- factor(noise_all_binned$species, levels = c("Grey Squirrel", "Red Fox", "Roe Deer"))

# plot
ggplot(noise_all_binned, aes(x = mean_time, y = mean_pred, color = noisecat)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = mean_pred - se_pred, ymax = mean_pred + se_pred, fill = noisecat), alpha = 0.07) +
  labs(x = "Time", y = "Activity", color = "Noise Pollution", fill = "Noise Pollution") +
  theme_bw() +
  scale_color_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  scale_fill_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  #facet_grid(rows = vars(scale), cols = vars(species))
  facet_grid(factor(scale, levels = c("Local", "Landscape")) ~ species) +
  theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )



##### WOODLAND #####


# SQU100 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(squirrel2$dog_RA)
area <- quantile(squirrel2$area)
light <- quantile(squirrel2$light100)
noise <- quantile(squirrel2$noise100)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = month,
                      #urban100 = rep(c(min(squirrel2$urban100), mean(squirrel2$urban100), max(squirrel2$urban100)), times = 1),
                      wood100 = unique(squirrel2$wood100),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light100 = light[c("25%", "75%")],
                      noise100 = noise[c("25%", "75%")])
newdat$urban100 <- 0.9-newdat$wood100

# scaled values of predictors
newdat$urban100_s <- scale(newdat$urban100)
newdat$wood100_s <- scale(newdat$wood100)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise100_s <- scale(newdat$noise100)
newdat$light100_s <- scale(newdat$light100)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ100pred <- predict(squ100.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ100pred <- as.data.frame(squ100pred)

# add predictions to original data frame
newdat$preds <- squ100pred$fit
newdat$se <- squ100pred$se.fit

# urbanisation categories
newdat$woodcat <- cut(
  newdat$wood100,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
squloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, woodcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squloc_binned)
squloc_binned$scale <- "Local"
squloc_binned$species <- "Grey Squirrel"



# SQU400 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(squirrel2$dog_RA)
area <- quantile(squirrel2$area)
light <- quantile(squirrel2$light400)
noise <- quantile(squirrel2$noise400)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = month,
                      wood400 = unique(squirrel2$wood400),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light400 = light[c("25%", "75%")],
                      noise400 = noise[c("25%", "75%")])
newdat$urban400 <- 0.9-newdat$wood400

# scaled values of predictors
newdat$urban400_s <- scale(newdat$urban400)
newdat$wood400_s <- scale(newdat$wood400)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise400_s <- scale(newdat$noise400)
newdat$light400_s <- scale(newdat$light400)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ400pred <- predict(squ400.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ400pred <- as.data.frame(squ400pred)

# add predictions to original data frame
newdat$preds <- squ400pred$fit
newdat$se <- squ400pred$se.fit

# urbanisation categories
newdat$woodcat <- cut(
  newdat$wood400,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
squlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, woodcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squlan_binned)
squlan_binned$scale <- "Landscape"
squlan_binned$species <- "Grey Squirrel"




# FOX250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(fox2$dog_RA)
area <- quantile(fox2$area)
light <- quantile(fox2$light250)
noise <- quantile(fox2$noise250)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(fox2$urban250), mean(fox2$urban250), max(fox2$urban250)), times = 1),
                      wood250 = unique(fox2$wood250),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light250 = light[c("25%", "75%")],
                      noise250 = noise[c("25%", "75%")])
newdat$urban250 <- 0.9-newdat$wood250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox250pred <- predict(fox250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox250pred <- as.data.frame(fox250pred)

# add predictions to original data frame
newdat$preds <- fox250pred$fit
newdat$se <- fox250pred$se.fit

# urbanisation categories
newdat$woodcat <- cut(
  newdat$wood250,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
foxloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, woodcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxloc_binned)
foxloc_binned$scale <- "Local"
foxloc_binned$species <- "Red Fox"





# FOX1km - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(fox2$dog_RA)
area <- quantile(fox2$area)
light <- quantile(fox2$light1km)
noise <- quantile(fox2$noise1km)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(fox2$urban250), mean(fox2$urban250), max(fox2$urban250)), times = 1),
                      wood1km = unique(fox2$wood1km),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light1km = light[c("25%", "75%")],
                      noise1km = noise[c("25%", "75%")])
newdat$urban1km <- 0.9-newdat$wood1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox1kmpred <- predict(fox1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox1kmpred <- as.data.frame(fox1kmpred)

# add predictions to original data frame
newdat$preds <- fox1kmpred$fit
newdat$se <- fox1kmpred$se.fit

# urbanisation categories
newdat$woodcat <- cut(
  newdat$wood1km,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
foxlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, woodcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxlan_binned)
foxlan_binned$scale <- "Landscape"
foxlan_binned$species <- "Red Fox"




# DEER250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(deer2$dog_RA)
area <- quantile(deer2$area)
light <- quantile(deer2$light250)
noise <- quantile(deer2$noise250)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      wood250 = unique(deer2$wood250),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light250 = light[c("25%", "75%")],
                      noise250 = noise[c("25%", "75%")])
newdat$urban250 <- 0.9-newdat$wood250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer250pred <- predict(dee250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer250pred <- as.data.frame(deer250pred)

# add predictions to original data frame
newdat$preds <- deer250pred$fit
newdat$se <- deer250pred$se.fit

# urbanisation categories
newdat$woodcat <- cut(
  newdat$wood250,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
deerloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, woodcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerloc_binned)
deerloc_binned$scale <- "Local"
deerloc_binned$species <- "Roe Deer"



# DEER1km - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
dog <- quantile(deer2$dog_RA)
area <- quantile(deer2$area)
light <- quantile(deer2$light1km)
noise <- quantile(deer2$noise1km)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      wood1km = unique(deer2$wood1km),
                      dog_RA = dog[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      light1km = light[c("25%", "75%")],
                      noise1km = noise[c("25%", "75%")])
newdat$urban1km <- 0.9-newdat$wood1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer1kmpred <- predict(dee1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer1kmpred <- as.data.frame(deer1kmpred)

# add predictions to original data frame
newdat$preds <- deer1kmpred$fit
newdat$se <- deer1kmpred$se.fit

# urbanisation categories
newdat$woodcat <- cut(
  newdat$wood1km,
  breaks = c(0, 0.33, 0.67, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
deerlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, woodcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerlan_binned)
deerlan_binned$scale <- "Landscape"
deerlan_binned$species <- "Roe Deer"




# full data frame
wood_all_binned <- rbind(deerloc_binned, deerlan_binned,
                         squloc_binned, squlan_binned, 
                         foxloc_binned, foxlan_binned)

write.csv(wood_all_binned, file = "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/wood_activity_df.csv")

wood_all_binned <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/wood_activity_df.csv")

wood_all_binned$woodcat <- factor(wood_all_binned$woodcat, levels = c("Low", "Medium", "High"))

# plot
ggplot(wood_all_binned, aes(x = mean_time, y = mean_pred, color = woodcat)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_pred - se_pred, ymax = mean_pred + se_pred, fill = woodcat), alpha = 0.07) +
  labs(x = "Time", y = "Activity", color = "Woodland", fill = "Woodland") +
  theme_bw() +
  scale_color_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  scale_fill_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  #facet_grid(rows = vars(scale), cols = vars(species))
  facet_grid(factor(scale, levels = c("Local", "Landscape")) ~ species) +
  theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )




### DOG

# SQU100 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
noise <- quantile(squirrel2$noise100)
area <- quantile(squirrel2$area)
urban <- quantile(squirrel2$urban100)
light <- quantile(squirrel2$light100)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = month,
                      #urban100 = rep(c(min(squirrel2$urban100), mean(squirrel2$urban100), max(squirrel2$urban100)), times = 1),
                      dog_RA = unique(squirrel2$dog_RA),
                      noise100 = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban100 = urban[c("25%", "75%")],
                      light100 = light[c("25%", "75%")])
newdat$wood100 <- 0.9-newdat$urban100

# scaled values of predictors
newdat$urban100_s <- scale(newdat$urban100)
newdat$wood100_s <- scale(newdat$wood100)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise100_s <- scale(newdat$noise100)
newdat$light100_s <- scale(newdat$light100)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ100pred <- predict(squ100.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ100pred <- as.data.frame(squ100pred)

# add predictions to original data frame
newdat$preds <- squ100pred$fit
newdat$se <- squ100pred$se.fit

# urbanisation categories
newdat$dogcat <- cut(
  newdat$dog_RA,
  breaks = c(0, 1, 2, 20),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
squloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, dogcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squloc_binned)
squloc_binned$scale <- "Local"
squloc_binned$species <- "Grey Squirrel"



# SQU400 - TIME, urban, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
noise <- quantile(squirrel2$noise400)
area <- quantile(squirrel2$area)
urban <- quantile(squirrel2$urban400)
light <- quantile(squirrel2$light400)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = month,
                      #urban100 = rep(c(min(squirrel2$urban400), mean(squirrel2$urban400), max(squirrel2$urban400)), times = 1),
                      dog_RA = unique(squirrel2$dog_RA),
                      noise400 = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban400 = urban[c("25%", "75%")],
                      light400 = light[c("25%", "75%")])
newdat$wood400 <- 0.9 - newdat$urban400

# scaled values of predictors
newdat$urban400_s <- scale(newdat$urban400)
newdat$wood400_s <- scale(newdat$wood400)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise400_s <- scale(newdat$noise400)
newdat$light400_s <- scale(newdat$light400)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ400pred <- predict(squ400.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ400pred <- as.data.frame(squ400pred)

# add predictions to original data frame
newdat$preds <- squ400pred$fit
newdat$se <- squ400pred$se.fit

# urbanisation categories
newdat$dogcat <- cut(
  newdat$dog_RA,
  breaks = c(0, 1, 2, 20),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
squlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, dogcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squlan_binned)
squlan_binned$scale <- "Landscape"
squlan_binned$species <- "Grey Squirrel"



# FOX250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
noise <- quantile(fox2$noise250)
area <- quantile(fox2$area)
urban <- quantile(fox2$urban250)
light <- quantile(fox2$light250)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(fox2$urban250), mean(fox2$urban250), max(fox2$urban250)), times = 1),
                      dog_RA = unique(fox2$dog_RA),
                      noise250 = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban250 = urban[c("25%", "75%")],
                      light250 = light[c("25%", "75%")])
newdat$wood250 <- 0.9-newdat$urban250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox250pred <- predict(fox250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox250pred <- as.data.frame(fox250pred)

# add predictions to original data frame
newdat$preds <- fox250pred$fit
newdat$se <- fox250pred$se.fit

# urbanisation categories
newdat$dogcat <- cut(
  newdat$dog_RA,
  breaks = c(0, 1, 2, 20),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
foxloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, dogcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxloc_binned)
foxloc_binned$scale <- "Local"
foxloc_binned$species <- "Red Fox"



# FOX1KM - TIME, urban, wood, light noise, area, dog_RA, month, sitesession

# find quantile values for variables
noise <- quantile(fox2$noise1km)
area <- quantile(fox2$area)
urban <- quantile(fox2$urban1km)
light <- quantile(fox2$light1km)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(fox2$urban1km), mean(fox2$urban1km), max(fox2$urban1km)), times = 1),
                      dog_RA = unique(fox2$dog_RA),
                      noise1km = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban1km = urban[c("25%", "75%")],
                      light1km = light[c("25%", "75%")])
newdat$wood1km <- 0.9-newdat$urban1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox1kmpred <- predict(fox1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox1kmpred <- as.data.frame(fox1kmpred)

# add predictions to original data frame
newdat$preds <- fox1kmpred$fit
newdat$se <- fox1kmpred$se.fit

# urbanisation categories
newdat$dogcat <- cut(
  newdat$dog_RA,
  breaks = c(0, 1, 2, 20),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
foxlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, dogcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxlan_binned)
foxlan_binned$scale <- "Landscape"
foxlan_binned$species <- "Red Fox"



# DEER250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
noise <- quantile(deer2$noise250)
area <- quantile(deer2$area)
urban <- quantile(deer2$urban250)
light <- quantile(deer2$light250)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      dog_RA = unique(deer2$dog_RA),
                      noise250 = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban250 = urban[c("25%", "75%")],
                      light250 = light[c("25%", "75%")])
newdat$wood250 <- 0.9-newdat$urban250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer250pred <- predict(dee250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer250pred <- as.data.frame(deer250pred)

# add predictions to original data frame
newdat$preds <- deer250pred$fit
newdat$se <- deer250pred$se.fit

# urbanisation categories
newdat$dogcat <- cut(
  newdat$dog_RA,
  breaks = c(0, 1, 2, 20),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
deerloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, dogcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerloc_binned)
deerloc_binned$scale <- "Local"
deerloc_binned$species <- "Roe Deer"



# DEER1KM - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession 

# find quantile values for variables
noise <- quantile(deer2$noise1km)
area <- quantile(deer2$area)
urban <- quantile(deer2$urban1km)
light <- quantile(deer2$light1km)
month <- c(6,8)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = month,
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      dog_RA = unique(deer2$dog_RA),
                      noise1km = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban1km = urban[c("25%", "75%")],
                      light1km = light[c("25%", "75%")])
newdat$wood1km <- 0.9-newdat$urban1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer1kmpred <- predict(dee1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer1kmpred <- as.data.frame(deer1kmpred)

# add predictions to original data frame
newdat$preds <- deer1kmpred$fit
newdat$se <- deer1kmpred$se.fit

# urbanisation categories
newdat$dogcat <- cut(
  newdat$dog_RA,
  breaks = c(0, 1, 2, 20),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE,
  right = FALSE)
head(newdat)

# bin times and average predictions and standard errors
deerlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, dogcat) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerlan_binned)
deerlan_binned$scale <- "Landscape"
deerlan_binned$species <- "Roe Deer"



# full data frame
dog_all_binned <- rbind(deerloc_binned, deerlan_binned, 
                          squloc_binned, squlan_binned, 
                          foxloc_binned, foxlan_binned)

write.csv(dog_all_binned, file = "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/dog_activity_df.csv")

dog_all_binned <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/dog_activity_df.csv")

dog_all_binned$dogcat <- factor(dog_all_binned$dogcat, levels = c("Low", "Medium", "High"))
dog_all_binned$species <- factor(dog_all_binned$species, levels = c("Grey Squirrel", "Red Fox", "Roe Deer"))

# plot
ggplot(dog_all_binned, aes(x = mean_time, y = mean_pred, color = dogcat)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = mean_pred - se_pred, ymax = mean_pred + se_pred, fill = dogcat), alpha = 0.07) +
  labs(x = "Time", y = "Activity", color = "Dog R.A.", fill = "Dog R.A.") +
  theme_bw() +
  scale_color_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  scale_fill_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  #facet_grid(rows = vars(scale), cols = vars(species))
  facet_grid(factor(scale, levels = c("Local", "Landscape")) ~ species) +
  theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )




### MONTH

# SQU100 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
noise <- quantile(squirrel2$noise100)
area <- quantile(squirrel2$area)
urban <- quantile(squirrel2$urban100)
light <- quantile(squirrel2$light100)
dog <- quantile(squirrel2$dog_RA)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = unique(squirrel2$month2),
                      #urban100 = rep(c(min(squirrel2$urban100), mean(squirrel2$urban100), max(squirrel2$urban100)), times = 1),
                      dog_RA = dog[c("25%", "75%")],
                      noise100 = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban100 = urban[c("25%", "75%")],
                      light100 = light[c("25%", "75%")])
newdat$wood100 <- 0.9-newdat$urban100

# scaled values of predictors
newdat$urban100_s <- scale(newdat$urban100)
newdat$wood100_s <- scale(newdat$wood100)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise100_s <- scale(newdat$noise100)
newdat$light100_s <- scale(newdat$light100)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ100pred <- predict(squ100.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ100pred <- as.data.frame(squ100pred)

# add predictions to original data frame
newdat$preds <- squ100pred$fit
newdat$se <- squ100pred$se.fit

# bin times and average predictions and standard errors
squloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, month2) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squloc_binned)
squloc_binned$scale <- "Local"
squloc_binned$species <- "Grey Squirrel"



# SQU400 - TIME, urban, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
noise <- quantile(squirrel2$noise400)
area <- quantile(squirrel2$area)
urban <- quantile(squirrel2$urban400)
light <- quantile(squirrel2$light400)
dog <- quantile(squirrel2$dog_RA)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(squirrel2$SiteSession)),
                      month2 = unique(squirrel$month2),
                      #urban100 = rep(c(min(squirrel2$urban400), mean(squirrel2$urban400), max(squirrel2$urban400)), times = 1),
                      dog_RA = dog[c("25%", "75%")],
                      noise400 = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban400 = urban[c("25%", "75%")],
                      light400 = light[c("25%", "75%")])
newdat$wood400 <- 0.9 - newdat$urban400

# scaled values of predictors
newdat$urban400_s <- scale(newdat$urban400)
newdat$wood400_s <- scale(newdat$wood400)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise400_s <- scale(newdat$noise400)
newdat$light400_s <- scale(newdat$light400)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
squ400pred <- predict(squ400.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
squ400pred <- as.data.frame(squ400pred)

# add predictions to original data frame
newdat$preds <- squ400pred$fit
newdat$se <- squ400pred$se.fit

# bin times and average predictions and standard errors
squlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, month2) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(squlan_binned)
squlan_binned$scale <- "Landscape"
squlan_binned$species <- "Grey Squirrel"



# FOX250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
noise <- quantile(fox2$noise250)
area <- quantile(fox2$area)
urban <- quantile(fox2$urban250)
light <- quantile(fox2$light250)
dog <- quantile(fox2$dog_RA)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = unique(fox2$month2),
                      #urban1km = rep(c(min(fox2$urban250), mean(fox2$urban250), max(fox2$urban250)), times = 1),
                      dog_RA = dog[c("25%", "75%")],
                      noise250 = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban250 = urban[c("25%", "75%")],
                      light250 = light[c("25%", "75%")])
newdat$wood250 <- 0.9-newdat$urban250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox250pred <- predict(fox250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox250pred <- as.data.frame(fox250pred)

# add predictions to original data frame
newdat$preds <- fox250pred$fit
newdat$se <- fox250pred$se.fit

# bin times and average predictions and standard errors
foxloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, month2) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxloc_binned)
foxloc_binned$scale <- "Local"
foxloc_binned$species <- "Red Fox"



# FOX1KM - TIME, urban, wood, light noise, area, dog_RA, month, sitesession

# find quantile values for variables
noise <- quantile(fox2$noise1km)
area <- quantile(fox2$area)
urban <- quantile(fox2$urban1km)
light <- quantile(fox2$light1km)
dog <- quantile(fox2$dog_RA)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(fox2$SiteSession)),
                      month2 = unique(fox2$month2),
                      #urban1km = rep(c(min(fox2$urban1km), mean(fox2$urban1km), max(fox2$urban1km)), times = 1),
                      dog_RA = dog[c("25%", "75%")],
                      noise1km = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban1km = urban[c("25%", "75%")],
                      light1km = light[c("25%", "75%")])
newdat$wood1km <- 0.9-newdat$urban1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
fox1kmpred <- predict(fox1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
fox1kmpred <- as.data.frame(fox1kmpred)

# add predictions to original data frame
newdat$preds <- fox1kmpred$fit
newdat$se <- fox1kmpred$se.fit

# bin times and average predictions and standard errors
foxlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, month2) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(foxlan_binned)
foxlan_binned$scale <- "Landscape"
foxlan_binned$species <- "Red Fox"



# DEER250 - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession

# find quantile values for variables
noise <- quantile(deer2$noise250)
area <- quantile(deer2$area)
urban <- quantile(deer2$urban250)
light <- quantile(deer2$light250)
dog <- quantile(deer$dog_RA)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = unique(deer2$month2),
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      dog_RA = dog[c("25%", "75%")],
                      noise250 = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban250 = urban[c("25%", "75%")],
                      light250 = light[c("25%", "75%")])
newdat$wood250 <- 0.9-newdat$urban250

# scaled values of predictors
newdat$urban250_s <- scale(newdat$urban250)
newdat$wood250_s <- scale(newdat$wood250)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise250_s <- scale(newdat$noise250)
newdat$light250_s <- scale(newdat$light250)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer250pred <- predict(dee250.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer250pred <- as.data.frame(deer250pred)

# add predictions to original data frame
newdat$preds <- deer250pred$fit
newdat$se <- deer250pred$se.fit

# bin times and average predictions and standard errors
deerloc_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, month2) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerloc_binned)
deerloc_binned$scale <- "Local"
deerloc_binned$species <- "Roe Deer"



# DEER1KM - TIME, urban, wood, light, noise, area, dog_RA, month, sitesession 

# find quantile values for variables
noise <- quantile(deer2$noise1km)
area <- quantile(deer2$area)
urban <- quantile(deer2$urban1km)
light <- quantile(deer2$light1km)
dog <- quantile(deer2$dog_RA)

# new data frame
newdat <- expand.grid(Time = seq(0, 23, 1),
                      SiteSession = factor(unique(deer2$SiteSession)),
                      month2 = unique(deer2$month2),
                      #urban1km = rep(c(min(deer2$urban1km), mean(deer2$urban1km), max(deer2$urban1km)), times = 1),
                      dog_RA = dog[c("25%", "75%")],
                      noise1km = noise[c("25%", "75%")],
                      area = area[c("25%", "75%")],
                      urban1km = urban[c("25%", "75%")],
                      light1km = light[c("25%", "75%")])
newdat$wood1km <- 0.9-newdat$urban1km

# scaled values of predictors
newdat$urban1km_s <- scale(newdat$urban1km)
newdat$wood1km_s <- scale(newdat$wood1km)
newdat$area_s <- scale(newdat$area)
newdat$month2_s <- scale(newdat$month2)
newdat$dog_RA_s <- scale(newdat$dog_RA)
newdat$noise1km_s <- scale(newdat$noise1km)
newdat$light1km_s <- scale(newdat$light1km)
newdat$Time_s <- scale(newdat$Time)

# predict activity 
deer1kmpred <- predict(dee1km.final, newdata = newdat, se.fit = T, type = "response")
# extract as data frame
deer1kmpred <- as.data.frame(deer1kmpred)

# add predictions to original data frame
newdat$preds <- deer1kmpred$fit
newdat$se <- deer1kmpred$se.fit

# bin times and average predictions and standard errors
deerlan_binned <- newdat %>%
  mutate(time_bin = cut(Time, breaks = 24)) %>%
  group_by(time_bin, month2) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    mean_pred = mean(preds, na.rm = TRUE),
    se_pred = mean(se, na.rm = TRUE),
    .groups = 'drop'
  )

# add columns for facet grid
head(deerlan_binned)
deerlan_binned$scale <- "Landscape"
deerlan_binned$species <- "Roe Deer"



# full data frame
month_all_binned <- rbind(deerloc_binned, deerlan_binned, 
                        squloc_binned, squlan_binned, 
                        foxloc_binned, foxlan_binned)

write.csv(month_all_binned, file = "C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/month_activity_df.csv")

month_all_binned <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/month_activity_df.csv")

month_all_binned$species <- factor(month_all_binned$species, levels = c("Grey Squirrel", "Red Fox", "Roe Deer"))
month_all_binned$month2 <- factor(month_all_binned$month2, levels = c("4", "5", "6", "7", "8", "9"))

# plot
ggplot(month_all_binned, aes(x = mean_time, y = mean_pred, color = month2)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = mean_pred - se_pred, ymax = mean_pred + se_pred, fill = month2), alpha = 0.07) +
  labs(x = "Time", y = "Activity", color = "Month", fill = "Month") +
  theme_bw() +
  #scale_color_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  #scale_fill_manual(values = c("Low" = "#1b9e77", "Medium" = "#d95f02", "High" = "#7570b3")) +
  #facet_grid(rows = vars(scale), cols = vars(species))
  facet_grid(factor(scale, levels = c("Local", "Landscape")) ~ species) +
  theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )




###############################
### FOREST PLOT DATA FRAMES ###
###############################

# SQUIRREL - LOCAL
s <- summary(squ400.final)
# parametric coefficients
param_coefs <- as.data.frame(s$p.table)
# clean up
param_df_slan <- data.frame(
  term = rownames(param_coefs),
  estimate = param_coefs$Estimate,
  std.error = param_coefs$`Std. Error`,
  statistic = param_coefs$`z value`,   # or "t value" if Gaussian family
  p.value = param_coefs$`Pr(>|z|)`
)
# add columns
param_df_slan$conf.low  <- param_df_slan$estimate - 1.96 * param_df_slan$std.error
param_df_slan$conf.high <- param_df_slan$estimate + 1.96 * param_df_slan$std.error
param_df_slan$species <- "Grey Squirrel"
param_df_slan$scale <- "Landscape"
param_df_slan$model <- "Activity"

# SQUIRREL - LOCAL
s <- summary(squ100.final)
# parametric coefficients
param_coefs <- as.data.frame(s$p.table)
# clean up
param_df_sloc <- data.frame(
  term = rownames(param_coefs),
  estimate = param_coefs$Estimate,
  std.error = param_coefs$`Std. Error`,
  statistic = param_coefs$`z value`,   # or "t value" if Gaussian family
  p.value = param_coefs$`Pr(>|z|)`
)
# add columns
param_df_sloc$conf.low  <- param_df_sloc$estimate - 1.96 * param_df_sloc$std.error
param_df_sloc$conf.high <- param_df_sloc$estimate + 1.96 * param_df_sloc$std.error
param_df_sloc$species <- "Grey Squirrel"
param_df_sloc$scale <- "Local"
param_df_sloc$model <- "Activity"

# FOX - LANDSCAPE
s <- summary(fox1km.final)
# parametric coefficients
param_coefs <- as.data.frame(s$p.table)
# clean up
param_df_flan <- data.frame(
  term = rownames(param_coefs),
  estimate = param_coefs$Estimate,
  std.error = param_coefs$`Std. Error`,
  statistic = param_coefs$`z value`,   # or "t value" if Gaussian family
  p.value = param_coefs$`Pr(>|z|)`
)
# add columns
param_df_flan$conf.low  <- param_df_flan$estimate - 1.96 * param_df_flan$std.error
param_df_flan$conf.high <- param_df_flan$estimate + 1.96 * param_df_flan$std.error
param_df_flan$species <- "Red Fox"
param_df_flan$scale <- "Landscape"
param_df_flan$model <- "Activity"

# FOX - LOCAL
s <- summary(fox250.final)
# parametric coefficients
param_coefs <- as.data.frame(s$p.table)
# clean up
param_df_floc <- data.frame(
  term = rownames(param_coefs),
  estimate = param_coefs$Estimate,
  std.error = param_coefs$`Std. Error`,
  statistic = param_coefs$`z value`,   # or "t value" if Gaussian family
  p.value = param_coefs$`Pr(>|z|)`
)
# add columns
param_df_floc$conf.low  <- param_df_floc$estimate - 1.96 * param_df_floc$std.error
param_df_floc$conf.high <- param_df_floc$estimate + 1.96 * param_df_floc$std.error
param_df_floc$species <- "Red Fox"
param_df_floc$scale <- "Local"
param_df_floc$model <- "Activity"

# DEER - LOCAL
s <- summary(dee250.final)
# parametric coefficients
param_coefs <- as.data.frame(s$p.table)
# clean up
param_df_dloc <- data.frame(
  term = rownames(param_coefs),
  estimate = param_coefs$Estimate,
  std.error = param_coefs$`Std. Error`,
  statistic = param_coefs$`z value`,   # or "t value" if Gaussian family
  p.value = param_coefs$`Pr(>|z|)`
)
# add columns
param_df_dloc$conf.low  <- param_df_dloc$estimate - 1.96 * param_df_dloc$std.error
param_df_dloc$conf.high <- param_df_dloc$estimate + 1.96 * param_df_dloc$std.error
param_df_dloc$species <- "Roe Deer"
param_df_dloc$scale <- "Local"
param_df_dloc$model <- "Activity"

# DEER - LANDSCAPE
s <- summary(dee1km.final)
# parametric coefficients
param_coefs <- as.data.frame(s$p.table)
# clean up
param_df_dlan <- data.frame(
  term = rownames(param_coefs),
  estimate = param_coefs$Estimate,
  std.error = param_coefs$`Std. Error`,
  statistic = param_coefs$`z value`,   # or "t value" if Gaussian family
  p.value = param_coefs$`Pr(>|z|)`
)
# add columns
param_df_dlan$conf.low  <- param_df_dlan$estimate - 1.96 * param_df_dlan$std.error
param_df_dlan$conf.high <- param_df_dlan$estimate + 1.96 * param_df_dlan$std.error
param_df_dlan$species <- "Roe Deer"
param_df_dlan$scale <- "Landscape"
param_df_dlan$model <- "Activity"

# combine to one df
results <- rbind(param_df_slan, param_df_sloc, param_df_flan, param_df_floc, param_df_dlan, param_df_dloc)

# rename vars
results$term <- ifelse(results$term == "wood100_s", "Woodland", results$term)
results$term <- ifelse(results$term == "wood400_s", "Woodland", results$term)
results$term <- ifelse(results$term == "wood1km_s", "Woodland", results$term)
results$term <- ifelse(results$term == "wood250_s", "Woodland", results$term)
results$term <- ifelse(results$term == "urban100_s", "Urban", results$term)
results$term <- ifelse(results$term == "urban400_s", "Urban", results$term)
results$term <- ifelse(results$term == "urban1km_s", "Urban", results$term)
results$term <- ifelse(results$term == "urban250_s", "Urban", results$term)
results$term <- ifelse(results$term == "noise100_s", "Noise", results$term)
results$term <- ifelse(results$term == "noise400_s", "Noise", results$term)
results$term <- ifelse(results$term == "noise1km_s", "Noise", results$term)
results$term <- ifelse(results$term == "noise250_s", "Noise", results$term)
results$term <- ifelse(results$term == "light100_s", "Light", results$term)
results$term <- ifelse(results$term == "light400_s", "Light", results$term)
results$term <- ifelse(results$term == "light1km_s", "Light", results$term)
results$term <- ifelse(results$term == "light250_s", "Light", results$term)
results$term <- ifelse(results$term == "area_s", "Area", results$term)
results$term <- ifelse(results$term == "month2_s", "Month", results$term)
results$term <- ifelse(results$term == "dog_RA_s", "Dog RA", results$term)

write.csv(results, file = "~/GALLANT Technician/Camera Trap Analysis/forest.act.param.csv")

# remove intercept term
plot_data <- subset(results, term != "(Intercept)")
plot_data$sig <- ifelse(plot_data$p.value < 0.05, "sig", "ns")
plot_data$scale <- factor(plot_data$scale, levels = c("Local", "Landscape"))

# plot
ggplot(plot_data, aes(x = estimate, y = term, color = sig)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #scale_x_log10() +
  scale_color_manual(values = c("sig" = "red", "ns" = "black")) +
  labs(x = "Odds Ratio (95% CI)", y = "") + 
  theme_bw() +
  theme(legend.position = "none") +
  facet_nested(cols = vars(species, scale)) +
  theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )
