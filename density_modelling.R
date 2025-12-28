# REM DENSITY MODELLING

library(car)
library(ggplot2)
library(ggeffects)
library(dplyr)
library(broom.mixed)

foxREM <- read.csv("~/GALLANT Technician/Camera Trap Analysis/fox_REM_df.csv")
deerREM <- read.csv("~/GALLANT Technician/Camera Trap Analysis/deer_REM_df.csv")
squirrelREM <- read.csv("~/GALLANT Technician/Camera Trap Analysis/squirrel_REM_df.csv")

# add 1 to density value (can't log-transform 0)
foxREM$D_indv <- foxREM$D_indv + 1
deerREM$D_indv <- deerREM$D_indv + 1
squirrelREM$D_indv <- deerREM$D_indv + 1

# log-transform density value
foxREM$D_log <- log(foxREM$D_indv)
deerREM$D_log <- log(deerREM$D_indv)
squirrelREM$D_log <- log(squirrelREM$D_indv)

##################
### FOX MODELS ###
##################

### local scale ###

# full model
mod.fox250 <- lm(data = foxREM,
                 D_log ~ wood250 + urban250 + dog_RA + area + noise250 + light250)
vif(mod.fox250) # fine

# remove light
fox250.2 <- lm(data = foxREM,
               D_log ~ wood250 + urban250 + dog_RA + area + noise250)
AIC(mod.fox250, fox250.2) # FULL LOWER - KEEP LIGHT

# remove noise
fox250.3 <- lm(data = foxREM,
               D_log ~ wood250 + urban250 + dog_RA + area + light250)
AIC(mod.fox250, fox250.3) # DIFF <2 - KEEP NOISE

# remove area
fox250.4 <- lm(data = foxREM,
               D_log ~ wood250 + urban250 + dog_RA + noise250 + light250)
AIC(mod.fox250, fox250.4) # FULL LOWER - KEEP AREA

# remove dog_RA
fox250.5 <- lm(data = foxREM,
               D_log ~ wood250 + urban250 + area + noise250 + light250)
AIC(mod.fox250, fox250.5) # FULL LOWER - KEEP DOG_RA

# remove urban
fox250.6 <- lm(data = foxREM,
               D_log ~ wood250 + dog_RA + area + noise250 + light250)
AIC(mod.fox250, fox250.6) # FULL LOWER - KEEP URBAN

# remove woodland
fox250.7 <- lm(data = foxREM,
               D_log ~ urban250 + dog_RA + area + noise250 + light250)
AIC(mod.fox250, fox250.7) # FULL LOWER - KEEP WOODLAND


# FINAL LOCAL FOX
fox250.final <- lm(data = foxREM,
                   D_log ~ wood250 + urban250 + dog_RA + area + noise250 + light250)
summary(fox250.final)



### landscape scale ###

mod.fox1km <- lm(data = foxREM,
                 D_log ~ urban1km + dog_RA + noise1km + light1km)
vif(mod.fox1km) # remove woodland and area

# remove light
fox1km.2 <- lm(data = foxREM,
               D_log ~ urban1km + dog_RA + noise1km)
AIC(mod.fox1km, fox1km.2) # FULL LOWER - KEEP LIGHT

# remove noise
fox1km.3 <- lm(data = foxREM,
               D_log ~ urban1km + dog_RA + light1km)
AIC(mod.fox1km, fox1km.3) # DIFF <2 - KEEP NOISE

# remove dog_RA
fox1km.4 <- lm(data = foxREM,
               D_log ~ urban1km + noise1km + light1km)
AIC(mod.fox1km, fox1km.4) # FULL LOWER - KEEP DOG_RA

# remove urban
fox1km.5 <- lm(data = foxREM,
               D_log ~ dog_RA + noise1km + light1km)
AIC(mod.fox1km, fox1km.5) # FULL LOWER - KEEP URBAN



# FINAL LANDSCAPE FOX
fox1km.final <- lm(data = foxREM,
                   D_log ~ urban1km + dog_RA + noise1km + light1km)
summary(fox1km.final)

##################
### DEER MODEL ###
##################

### LOCAL ###

# full model
mod.deer250 <- lm(data = deerREM,
                  D_log ~ wood250 + urban250 + dog_RA + area + noise250 + light250)
vif(mod.deer250) # fine

# remove light
deer250.2 <- lm(data = deerREM,
                D_log ~ wood250 + urban250 + dog_RA + area + noise250)
AIC(mod.deer250, deer250.2) # FULL LOWER - KEEP LIGHT 

# remove noise
deer250.3 <- lm(data = deerREM,
                D_log ~ wood250 + urban250 + dog_RA + area + light250)
AIC(mod.deer250, deer250.3) # FULL LOWER - KEEP NOISE

# remove area
deer250.4 <- lm(data = deerREM,
                D_log ~ wood250 + urban250 + dog_RA + noise250 + light250)
AIC(mod.deer250, deer250.4) # DIFF <2 - KEEP AREA

# remove dog_RA
deer250.5 <- lm(data = deerREM,
                D_log ~ wood250 + urban250 + area + noise250 + light250)
AIC(mod.deer250, deer250.5) # DIFF <2 - KEEP DOG_RA

# remove urban
deer250.6 <- lm(data = deerREM,
                D_log ~ wood250 + dog_RA + area + noise250 + light250)
AIC(mod.deer250, deer250.6) # FULL LOWER - KEEP URBAN

# remove woodland
deer250.7 <- lm(data = deerREM,
                D_log ~ urban250 + dog_RA + area + noise250 + light250)
AIC(mod.deer250, deer250.7) # DIFF <1 - KEEP WOODLAND


# FINAL LOCAL DEER
deer250.final <- lm(data = deerREM,
                    D_log ~ wood250 + urban250 + dog_RA + area + noise250 + light250)
summary(deer250.final)



### LANDSCAPE ###

mod.deer1km <- lm(data = deerREM,
                 D_log ~ urban1km + dog_RA + noise1km + light1km)
vif(mod.deer1km) # remove area and woodland
cor(deerREM$wood1km, deerREM$urban1km, use = "complete.obs")

# remove light
deer1km.2 <- lm(data = deerREM,
                D_log ~ urban1km + dog_RA + noise1km)
AIC(mod.deer1km, deer1km.2) # DIFF <2 - KEEP LIGHT

# remove noise
deer1km.3 <- lm(data = deerREM,
                D_log ~ urban1km + dog_RA + light1km)
AIC(mod.deer1km, deer1km.3) # FULL LOWER - KEEP NOISE

# remove dog_RA
deer1km.4 <- lm(data = deerREM,
                D_log ~ urban1km + noise1km + light1km)
AIC(mod.deer1km, deer1km.4) # DIFF <2 - KEEP DOG_RA

# remove urban
deer1km.5 <- lm(data = deerREM,
                D_log ~ dog_RA + noise1km + light1km)
AIC(mod.deer1km, deer1km.5) # FULL LOWER - KEEP URBAN


# FINAL LANDSCAPE DEER
deer1km.final <- lm(data = deerREM,
                    D_log ~ urban1km + dog_RA + noise1km + light1km)
summary(deer1km.final)

######################
### SQUIRREL MODEL ###
######################

### LOCAL ###

mod.squ100 <- lm(data = squirrelREM,
                 D_log ~ wood100 + urban100 + dog_RA + area + noise100 + light100)
vif(mod.squ100) # fine 

# remove light
squ100.2 <- lm(data = squirrelREM,
               D_log ~ wood100 + urban100 + dog_RA + area + noise100)
AIC(mod.squ100, squ100.2) # FULL LOWER - KEEP LIGHT

# remove noise
squ100.3 <- lm(data = squirrelREM,
               D_log ~ wood100 + urban100 + dog_RA + area + light100)
AIC(mod.squ100, squ100.3) # DIFF <1 - KEEP NOISE

# remove area
squ100.4 <- lm(data = squirrelREM,
               D_log ~ wood100 + urban100 + dog_RA + noise100 + light100)
AIC(mod.squ100, squ100.4) # DIFF <2 - KEEP AREA

# remove dog_RA
squ100.5 <- lm(data = squirrelREM,
               D_log ~ wood100 + urban100 + area + noise100 + light100)
AIC(mod.squ100, squ100.5) # DIFF <2 - KEEP DOG_RA

# remove urban
squ100.6 <- lm(data = squirrelREM,
               D_log ~ wood100 + dog_RA + area + noise100 + light100)
AIC(mod.squ100, squ100.6) # FULL LOWER - KEEP URBAN

# remove woodland
squ100.7 <- lm(data = squirrelREM,
               D_log ~ urban100 + dog_RA + area + noise100 + light100)
AIC(mod.squ100, squ100.7) # DIFF <2 - KEEP WOODLAND


# FINAL LOCAL SQUIRREL
squ100.final <- lm(data = squirrelREM,
                   D_log ~ wood100 + urban100 + dog_RA + area + noise100 + light100)
summary(squ100.final)


### LANDSCAPE ###

mod.squ400 <- lm(data = squirrelREM,
                 D_log ~ wood400 + urban400 + dog_RA + noise400 + light400)
vif(mod.squ400) # remove area

# remove light
squ400.2 <- lm(data = squirrelREM,
               D_log ~ wood400 + urban400 + dog_RA + noise400)
AIC(mod.squ400, squ400.2) # DIFF <2 - KEEP LIGHT

# remove noise
squ400.3 <- lm(data = squirrelREM,
               D_log ~ wood400 + urban400 + dog_RA + light400)
AIC(mod.squ400, squ400.3) # FULL LOWER - KEEP NOISE

# remove dog_RA
squ400.4 <- lm(data = squirrelREM,
               D_log ~ wood400 + urban400 + noise400 + light400)
AIC(mod.squ400, squ400.4) # DIFF <2 - KEEP DOG_RA

# remove urban
squ400.5 <- lm(data = squirrelREM,
               D_log ~ wood400 + dog_RA + noise400 + light400)
AIC(mod.squ400, squ400.5) # FULL LOWER - KEEP URBAN

# remove woodland
squ400.6 <- lm(data = squirrelREM,
               D_log ~ urban400 + dog_RA + noise400 + light400)
AIC(mod.squ400, squ400.6) # FULL LOWER - KEEP WOODLAND


# FINAL LANDSCAPE SQUIRREL
squ400.final <- lm(data = squirrelREM,
                   D_log ~ wood400 + urban400 + dog_RA + noise400 + light400)
summary(squ400.final)

########################
### PLOTCRASTINATION ###
########################


# new data frame - deer local
newdat.d2 <- predict_response(deer250.final, terms = "urban250")
newdat.d2$predicted <- exp(newdat.d2$predicted) - 1
newdat.d2$predicted <- pmax(newdat.d2$predicted, 0)
newdat.d2$std.error <- exp(newdat.d2$std.error) - 1
newdat.d2$conf.low <- exp(newdat.d2$conf.low) - 1
newdat.d2$conf.high <- exp(newdat.d2$conf.high) - 1
newdat.d2$Species <- "Roe Deer"
newdat.d2$Scale <- "Local"

# new data frame - deer landscape
newdat.d <- predict_response(deer1km.final, terms = "urban1km")
newdat.d$predicted <- exp(newdat.d$predicted) - 1
newdat.d$predicted <- pmax(newdat.d$predicted, 0)
newdat.d$std.error <- exp(newdat.d$std.error) - 1
newdat.d$conf.low <- exp(newdat.d$conf.low) - 1
newdat.d$conf.high <- exp(newdat.d$conf.high) - 1
newdat.d$Species <- "Roe Deer"
newdat.d$Scale <- "Landscape"

# new data frame - fox local
newdat.f2 <- predict_response(fox250.final, terms = "urban250")
newdat.f2$predicted <- exp(newdat.f2$predicted) - 1
newdat.f2$predicted <- pmax(newdat.f2$predicted, 0)
newdat.f2$std.error <- exp(newdat.f2$std.error) - 1
newdat.f2$conf.low <- exp(newdat.f2$conf.low) - 1
newdat.f2$conf.high <- exp(newdat.f2$conf.high) - 1
newdat.f2$Species <- "Red Fox"
newdat.f2$Scale <- "Local"

# new data frame - fox landscape
newdat.f <- predict_response(fox1km.final, terms = "urban1km")
newdat.f$predicted <- exp(newdat.f$predicted) - 1
newdat.f$predicted <- pmax(newdat.f$predicted, 0)
newdat.f$std.error <- exp(newdat.f$std.error) - 1
newdat.f$conf.low <- exp(newdat.f$conf.low) - 1
newdat.f$conf.high <- exp(newdat.f$conf.high) - 1
newdat.f$Species <- "Red Fox"
newdat.f$Scale <- "Landscape"

# new data frame - squ local
newdat.s2 <- predict_response(squ100.final, terms = "urban100")
newdat.s2$predicted <- exp(newdat.s2$predicted) - 1
newdat.s2$predicted <- pmax(newdat.s2$predicted, 0)
newdat.s2$std.error <- exp(newdat.s2$std.error) - 1
newdat.s2$conf.low <- exp(newdat.s2$conf.low) - 1
newdat.s2$conf.high <- exp(newdat.s2$conf.high) - 1
newdat.s2$Species <- "Grey Squirrel"
newdat.s2$Scale <- "Local"

# new data frame - squ landscape
newdat.s <- predict_response(squ400.final, terms = "urban400")
newdat.s$predicted <- exp(newdat.s$predicted) - 1
newdat.s$predicted <- pmax(newdat.s$predicted, 0)
newdat.s$std.error <- exp(newdat.s$std.error) - 1
newdat.s$conf.low <- exp(newdat.s$conf.low) - 1
newdat.s$conf.high <- exp(newdat.s$conf.high) - 1
newdat.s$Species <- "Grey Squirrel"
newdat.s$Scale <- "Landscape"

# combine to one data frame
newdat <- rbind(newdat.d, newdat.s, newdat.f, newdat.d2, newdat.s2, newdat.f2)
newdat$Scale <- factor(newdat$Scale, levels = c("Local", "Landscape"))

ggplot(newdat, aes(x = x, y = predicted, colour = factor(Species, levels = c("Grey Squirrel", "Red Fox", "Roe Deer")))) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = predicted - std.error, ymax = predicted + std.error), alpha = 0.1) +
  theme_bw() +
  labs(x = "Proportion of Urban Cover", y = "Predicted Density") +
  facet_wrap(~Scale) +
  scale_color_manual(values = c("Grey Squirrel" = '#7FC97F', "Red Fox" = '#386CB0', "Roe Deer" = '#F0027F'), name = "Species") +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )






####



# new data frame - deer local
newdat.d2 <- predict_response(deer250.final, terms = "light250")
newdat.d2$predicted <- exp(newdat.d2$predicted) - 1
newdat.d2$predicted <- pmax(newdat.d2$predicted, 0)
newdat.d2$std.error <- exp(newdat.d2$std.error) - 1
newdat.d2$conf.low <- exp(newdat.d2$conf.low) - 1
newdat.d2$conf.high <- exp(newdat.d2$conf.high) - 1
newdat.d2$Species <- "Roe Deer"
newdat.d2$Scale <- "Local"

# new data frame - deer landscape
newdat.d <- predict_response(deer1km.final, terms = "light1km")
newdat.d$predicted <- exp(newdat.d$predicted) - 1
newdat.d$predicted <- pmax(newdat.d$predicted, 0)
newdat.d$std.error <- exp(newdat.d$std.error) - 1
newdat.d$conf.low <- exp(newdat.d$conf.low) - 1
newdat.d$conf.high <- exp(newdat.d$conf.high) - 1
newdat.d$Species <- "Roe Deer"
newdat.d$Scale <- "Landscape"

# new data frame - fox local
newdat.f2 <- predict_response(fox250.final, terms = "light250")
newdat.f2$predicted <- exp(newdat.f2$predicted) - 1
newdat.f2$predicted <- pmax(newdat.f2$predicted, 0)
newdat.f2$std.error <- exp(newdat.f2$std.error) - 1
newdat.f2$conf.low <- exp(newdat.f2$conf.low) - 1
newdat.f2$conf.high <- exp(newdat.f2$conf.high) - 1
newdat.f2$Species <- "Red Fox"
newdat.f2$Scale <- "Local"

# new data frame - fox landscape
newdat.f <- predict_response(fox1km.final, terms = "light1km")
newdat.f$predicted <- exp(newdat.f$predicted) - 1
newdat.f$predicted <- pmax(newdat.f$predicted, 0)
newdat.f$std.error <- exp(newdat.f$std.error) - 1
newdat.f$conf.low <- exp(newdat.f$conf.low) - 1
newdat.f$conf.high <- exp(newdat.f$conf.high) - 1
newdat.f$Species <- "Red Fox"
newdat.f$Scale <- "Landscape"

# new data frame - squ local
newdat.s2 <- predict_response(squ100.final, terms = "light100")
newdat.s2$predicted <- exp(newdat.s2$predicted) - 1
newdat.s2$predicted <- pmax(newdat.s2$predicted, 0)
newdat.s2$std.error <- exp(newdat.s2$std.error) - 1
newdat.s2$conf.low <- exp(newdat.s2$conf.low) - 1
newdat.s2$conf.high <- exp(newdat.s2$conf.high) - 1
newdat.s2$Species <- "Grey Squirrel"
newdat.s2$Scale <- "Local"

# new data frame - squ landscape
newdat.s <- predict_response(squ400.final, terms = "light400")
newdat.s$predicted <- exp(newdat.s$predicted) - 1
newdat.s$predicted <- pmax(newdat.s$predicted, 0)
newdat.s$std.error <- exp(newdat.s$std.error) - 1
newdat.s$conf.low <- exp(newdat.s$conf.low) - 1
newdat.s$conf.high <- exp(newdat.s$conf.high) - 1
newdat.s$Species <- "Grey Squirrel"
newdat.s$Scale <- "Landscape"

# combine to one data frame
newdat <- rbind(newdat.d, newdat.s, newdat.f, newdat.d2, newdat.s2, newdat.f2)
newdat$Scale <- factor(newdat$Scale, levels = c("Local", "Landscape"))

ggplot(newdat, aes(x = x, y = predicted, colour = factor(Species, levels = c("Grey Squirrel", "Red Fox", "Roe Deer")))) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = predicted - std.error, ymax = predicted + std.error), alpha = 0.1) +
  theme_bw() +
  labs(x = "Light Pollution", y = expression("Predicted Density (indv km"^-2*")")) +
  facet_wrap(~Scale) +
  scale_color_manual(values = c("Grey Squirrel" = '#7FC97F', "Red Fox" = '#386CB0', "Roe Deer" = '#F0027F'), name = "Species") +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
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

# extract coefficiants and add columns
den.slan <- tidy(squ400.final, effects = "fixed", conf.int = TRUE, exponentiate = F)
den.slan$species <- "Grey Squirrel"
den.slan$scale <- "Landscape"
den.slan$model <- "Density"
den.sloc <- tidy(squ100.final, effects = "fixed", conf.int = TRUE, exponentiate = F)
den.sloc$species <- "Grey Squirrel"
den.sloc$scale <- "Local"
den.sloc$model <- "Density"
den.flan <- tidy(fox1km.final, effects = "fixed", conf.int = TRUE, exponentiate = F) 
den.flan$species <- "Red Fox"
den.flan$scale <- "Landscape"
den.flan$model <- "Density"
den.floc <- tidy(fox250.final, effects = "fixed", conf.int = TRUE, exponentiate = F)
den.floc$species <- "Red Fox"
den.floc$scale <- "Local"
den.floc$model <- "Density"
den.dlan <- tidy(deer1km.final, effects = "fixed", conf.int = TRUE, exponentiate = F)
den.dlan$species <- "Roe Deer"
den.dlan$scale <- "Landscape"
den.dlan$model <- "Density"
den.dloc <- tidy(deer250.final, effects = "fixed", conf.int = TRUE, exponentiate = F)
den.dloc$species <- "Roe Deer"
den.dloc$scale <- "Local"
den.dloc$model <- "Density"
# combine to one df
results <- rbind(den.slan, den.sloc, den.flan, den.floc, den.dlan, den.dloc)

# rename vars
results$term <- ifelse(results$term == "wood100", "Woodland", results$term)
results$term <- ifelse(results$term == "wood400", "Woodland", results$term)
results$term <- ifelse(results$term == "wood1km", "Woodland", results$term)
results$term <- ifelse(results$term == "wood250", "Woodland", results$term)
results$term <- ifelse(results$term == "urban100", "Urban", results$term)
results$term <- ifelse(results$term == "urban400", "Urban", results$term)
results$term <- ifelse(results$term == "urban1km", "Urban", results$term)
results$term <- ifelse(results$term == "urban250", "Urban", results$term)
results$term <- ifelse(results$term == "noise100", "Noise", results$term)
results$term <- ifelse(results$term == "noise400", "Noise", results$term)
results$term <- ifelse(results$term == "noise1km", "Noise", results$term)
results$term <- ifelse(results$term == "noise250", "Noise", results$term)
results$term <- ifelse(results$term == "light100", "Light", results$term)
results$term <- ifelse(results$term == "light400", "Light", results$term)
results$term <- ifelse(results$term == "light1km", "Light", results$term)
results$term <- ifelse(results$term == "light250", "Light", results$term)
results$term <- ifelse(results$term == "dog_RA", "Dog RA", results$term)
results$term <- ifelse(results$term == "area", "Area", results$term)

write.csv(results, file = "~/GALLANT Technician/Camera Trap Analysis/forest.den.csv")
