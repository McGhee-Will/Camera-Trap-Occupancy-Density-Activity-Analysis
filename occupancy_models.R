# OCCUPANCY MODELS

library(lme4)
library(car)
library(MuMIn)
library(boot)      
library(ggplot2)    
library(pROC)
library(glmmTMB)
library(DHARMa)
library(performance)
library(ggeffects)

squ.data <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/squirrel_occupancy_df.csv")
dee.data <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/deer_occupancy_df.csv")
fox.data <- read.csv("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/fox_occupancy_df.csv")

# scale area, light and noise
# squ
squ.data$area <- scale(squ.data$area)
squ.data$light100 <- scale(squ.data$light100)
squ.data$light400 <- scale(squ.data$light400)
squ.data$noise100 <- scale(squ.data$noise100)
squ.data$noise400 <- scale(squ.data$noise400)
# deer
dee.data$area <- scale(dee.data$area)
dee.data$light250 <- scale(dee.data$light250)
dee.data$light1km <- scale(dee.data$light1km)
dee.data$noise250 <- scale(dee.data$noise250)
dee.data$noise1km <- scale(dee.data$noise1km)
# fox
fox.data$area <- scale(fox.data$area)
fox.data$light250 <- scale(fox.data$light250)
fox.data$light1km <- scale(fox.data$light1km)
fox.data$noise250 <- scale(fox.data$noise250)
fox.data$noise1km <- scale(fox.data$noise1km)

# make numeric
squ.data$dog_RA <- as.numeric(squ.data$dog_RA)
dee.data$dog_RA <- as.numeric(dee.data$dog_RA)
fox.data$dog_RA <- as.numeric(fox.data$dog_RA)

# change to factors
squ.data$Site <- as.factor(squ.data$Site)
squ.data$Session <- as.factor(squ.data$Session)
dee.data$Site <- as.factor(dee.data$Site)
dee.data$Session <- as.factor(dee.data$Session)
fox.data$Site <- as.factor(fox.data$Site)
fox.data$Session <- as.factor(fox.data$Session)

#########################
### MULTICOLLINEARITY ###
#########################

### SQUIRREL ###

# LOCAL #

# check correlation matrix
cor(squ.data[c("wood100", "urban100", "light100", "noise100", "dog_RA", "area")]) # 100m - no values for wetland, remove it

# linear model for multicollinearity
lm.squ100 <- lm(squ.data,
                formula = capt ~ wood100 + urban100 + area +
                  dog_RA + light100 + noise100)
vif(lm.squ100) # remove water and grass

# LANDSCAPE #

# correlation matrix
cor(squ.data[c("wood400", "urban400", "light400", "noise400", "dog_RA", "area")]) # 400m - no values for wetland, remove it

# linear model for multicollinearity
lm.squ400 <- lm(squ.data,
                formula = capt ~ wood400 + urban400 + area +
                  dog_RA + light400 + noise400)
vif(lm.squ400) # remove water and grass



### DEER/FOX ###

# LOCAL #

# correlation matrix
cor(dee.data[c("wood250", "urban250", "light250", "noise250", "dog_RA", "area")]) # 250m

summary(dee.data$wet250) # less than 1% cover in two sites, remove it

# linear model to check multicollinearity
lm.dee250 <- lm(dee.data, 
                formula = capt ~ wood250 + urban250 + area +
                  dog_RA + light250 + noise250)
vif(lm.dee250) # remove water and grass

# LANDSCAPE #

# correlation matrix
cor(dee.data[c("wood1km", "urban1km", "light1km", "noise1km", "dog_RA", "area")]) # 1km

summary(dee.data$wet1km) # < 1% cover in one site, remove it
summary(dee.data$water1km)
water1km <- subset(dee.data, dee.data$water1km > 0)
levels(as.factor(water1km$water1km)) # 9

# linear model to check multicollinearity
lm.dee1km <- lm(dee.data,
                formula = capt ~ wood1km + urban1km + area +
                  dog_RA + light1km + noise1km)
vif(lm.dee1km) # remove grass and water

# LOCAL #

lm.fox250 <- lm(fox.data,
                formula = capt ~ wood250 + urban250 + area +
                  dog_RA + light250 + noise250)
vif(lm.fox250)

# LANDSCAPE #

lm.fox1km <- lm(fox.data,
                formula = capt ~ wood1km + urban1km + area +
                  dog_RA + light1km + noise1km)
vif(lm.fox1km)

#####################
### MODEL FITTING ###
#####################

### SQUIRREL ###
# LOCAL #

# binomial
# nested r eff
squ100.binom <- glmer(data = squ.data,
                      capt ~ wood100 + urban100 + area + dog_RA + light100 + noise100 + 
                        (1|Session/Site), 
                      family = binomial,
                      control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B"))) #warning
# singular r eff
squ100.binom2 <- glmer(data = squ.data,
                       capt ~ wood100 + urban100 + area + dog_RA + light100 + noise100 + 
                         (1|Session) + (1|Site), 
                       family = binomial,
                       control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B")))
summary(squ100.binom2) # session variance minimal - remove it
# remove session
squ100.binom3 <- glmer(data = squ.data,
                       capt ~ wood100 + urban100 + area + dog_RA + light100 + noise100 + 
                         (1|Site), 
                       family = binomial,
                       control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B")))

# check model fit
check_overdispersion(squ100.binom3) # no overdispersion 

sim_res <- simulateResiduals(fittedModel = squ100.binom3)
plot(sim_res) # ok

plot(ggpredict(squ100.binom3))

summary(squ100.binom3)

check_singularity(squ100.binom3)
isSingular(squ100.binom3, tol = 1e-4)
anova(squ100.binom3, squ100.binom2, squ100.binom, test = "Chisq") # binom3 the best

# LANDSCAPE #

# binomial
# nested r eff
squ400.binom <- glmer(data = squ.data,
                      capt ~ wood400 + urban400 + area + dog_RA + light400 + noise400 + 
                        (1|Session/Site), 
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # warning
# singular r eff
squ400.binom2 <- glmer(data = squ.data,
                       capt ~ wood400 + urban400 + area + dog_RA + light400 + noise400 + 
                         (1|Session) + (1|Site), 
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(squ400.binom2) # session variance minimal - remove
# remove session
squ400.binom3 <- glmer(data = squ.data,
                       capt ~ wood400 + urban400 + area + dog_RA + light400 + noise400 + 
                         (1|Site), 
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

check_overdispersion(squ400.binom3) # no dispersion issues 

sim_res <- simulateResiduals(fittedModel = squ400.binom3)
plot(sim_res) # meh

check_singularity(squ400.binom3)
isSingular(squ400.binom3, tol = 1e-4)
anova(squ400.binom3, squ400.binom2, squ400.binom, test = "Chisq") # binom3

summary(squ400.binom3)


# BINOMIAL FOR LOCAL SQ AND LANDSCAPE SQ #

### DEER ###
# LOCAL #

# full occupancy model
# nested random effects
dee250.binomial <- glmer(data = dee.data,
                         capt ~ wood250 + urban250 + area + dog_RA + light250 + noise250 + 
                           (1|Session/Site),
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # warning
# singular random effects 
dee250.binom2 <- glmer(data = dee.data,
                       capt ~ wood250 + urban250 + area + dog_RA + light250 + noise250 + 
                         (1|Session) + (1|Site),
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # warning
# remove session
dee250.binom3 <- glmer(data = dee.data,
                       capt ~ wood250 + urban250 + area + dog_RA + light250 + noise250 + 
                         (1|Site),
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

check_overdispersion(dee250.binom3) # no overdispersion

sim_res <- simulateResiduals(fittedModel = dee250.binom3)
plot(sim_res) # fine

summary(dee250.binom3)

check_overdispersion(dee250.binom3) # no overdispersion

check_singularity(dee250.binom3)
isSingular(dee250.binom3, tol = 1e-4)
anova(dee250.binom3, dee250.binom4, dee250.binom5, test = "Chisq") # binom3


# LANDSCAPE #

# full occupancy model
# nested r eff
dee1km.binomial <- glmer(data = dee.data,
                         capt ~ wood1km + urban1km + area + dog_RA + light1km + noise1km + 
                           (1|Session/Site),
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # warning
# singular r eff
dee1km.binom2 <- glmer(data = dee.data,
                       capt ~ wood1km + urban1km + area + dog_RA + light1km + noise1km + 
                         (1|Session) + (1|Site),
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(dee1km.binom2) # session variance minimal - remove
# remove session
dee1km.binom3 <- glmer(data = dee.data,
                       capt ~ wood1km + urban1km + area + dog_RA + light1km + noise1km + 
                         (1|Site),
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

check_overdispersion(dee1km.binom3) # no overdispersion

sim_res <- simulateResiduals(fittedModel = dee1km.binom3)
plot(sim_res) # not great

check_singularity(dee1km.binom3)
isSingular(dee1km.binom3, tol = 1e-4)
anova(dee1km.binom3, dee1km.binom4, dee1km.binom5, test = "Chisq") # binom3

plot(allEffects(dee1km.binom3))


# BINOMIAL FOR LOCAL AND LANDSCAPE DEER #

### FOX ###
# LOCAL #

# full occupancy model
# nested r eff
fox250.binomial <- glmer(data = fox.data,
                         capt ~ wood250 + urban250 + area + dog_RA + light250 + noise250 + 
                           (1|Session/Site),
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # warning
# singular r eff
fox250.binom2 <- glmer(data = fox.data,
                       capt ~ wood250 + urban250 + area + dog_RA + light250 + noise250 + 
                         (1|Session) + (1|Site),
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # warning
# remove session
fox250.binom3 <- glmer(data = fox.data,
                       capt ~ wood250 + urban250 + area + dog_RA + light250 + noise250 + 
                         (1|Site),
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

sim_res <- simulateResiduals(fittedModel = fox250.binom3)
plot(sim_res) # good

check_singularity(fox250.binom3)
isSingular(fox250.binom3, tol = 1e-4)
anova(fox250.binom3, fox250.binom2, fox250.binomial, test = "Chisq") # binom3


# LANDSCAPE #

# full occupancy model
# nested r eff
fox1km.binomial <- glmer(data = fox.data,
                         capt ~ wood1km + urban1km + area + dog_RA + light1km + noise1km + 
                           (1|Session/Site),
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))) # warning
# singular r eff
fox1km.binom2 <- glmer(data = fox.data,
                       capt ~ wood1km + urban1km + area + dog_RA + light1km + noise1km + 
                         (1|Session) + (1|Site),
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(fox1km.binom2) # session variance minimal - remove
# remove session
fox1km.binom3 <- glmer(data = fox.data,
                       capt ~ wood1km + urban1km + area + dog_RA + light1km + noise1km + 
                         (1|Site),
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

check_overdispersion(fox1km.binom3) # fine

sim_res <- simulateResiduals(fittedModel = fox1km.binom3)
plot(sim_res) # ok

check_singularity(fox1km.binom3)
isSingular(fox1km.binom3, tol = 1e-4)
anova(fox1km.binom3, fox1km.binom2, fox1km.binomial, test = "Chisq") # binom3

#############################
### 100m MODEL - SQUIRREL ###
#############################

# full occupancy model
squ100.mod <- glmer(data = squ.data,
                    capt ~ wood100 + urban100 + area + dog_RA + light100 + noise100 + (1|Site), 
                    family = binomial,
                    control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B")))

# remove noise
squ100.2 <- glmer(data = squ.data,
                  capt ~ wood100 + urban100 + area + dog_RA + light100 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B")))
AIC(squ100.mod, squ100.2) # DIFF < 1 - KEEP NOISE

# remove light
squ100.3 <- glmer(data = squ.data,
                  capt ~ wood100 + urban100 + area + dog_RA + noise100 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer="optimx", optCtrl=list(method=c("nlminb","bobyqa","L-BFGS-B"))))
AIC(squ100.mod, squ100.3) # FULL LOWER - KEEP LIGHT

# remove dog_RA
squ100.4 <- glmer(data = squ.data,
                  capt ~ wood100 + urban100 + area + light100 + noise100 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B")))
AIC(squ100.mod, squ100.4) # FULL LOWER - KEEP DOG_RA

# remove area
squ100.5 <- glmer(data = squ.data,
                  capt ~ wood100 + urban100 + dog_RA + light100 + noise100 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B")))
AIC(squ100.mod, squ100.5) # FULL LOWER - KEEP AREA

# remove urban
squ100.6 <- glmer(data = squ.data,
                  capt ~ wood100 + area + dog_RA + light100 + noise100 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B")))
AIC(squ100.mod, squ100.6) # DIFF <2 - KEEP URBAN

# remove woodland
squ100.7 <- glmer(data = squ.data,
                  capt ~ urban100 + area + dog_RA + light100 + noise100 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B")))
AIC(squ100.mod, squ100.7) # FULL LOWER - KEEP WOODLAND


### FINAL MODEL FOR SQUIREL - BUFFER 100 ###
final.squ100 <- glmer(data = squ.data,
                      capt ~ wood100 + urban100 + area + dog_RA + light100 + noise100 + (1|Site), 
                      family = binomial,
                      control = glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B")))

summary(final.squ100)
# pseudo r-sq
r.squaredGLMM(final.squ100) # 0.235

#############################
### 400m MODEL - SQUIRREL ###
#############################

# full occupancy model
squ400.mod <- glmer(data = squ.data,
                    capt ~ wood400 + urban400 + area + dog_RA + light400 + noise400 + (1|Site), 
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# remove noise
squ400.2 <- glmer(data = squ.data,
                  capt ~ wood400 + urban400 + area + dog_RA + light400 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(squ400.mod, squ400.2) # DIFF <2 - KEEP NOISE

# remove light
squ400.3 <- glmer(data = squ.data,
                  capt ~ wood400 + urban400 + area + dog_RA + noise400 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(squ400.mod, squ400.3) # DIFF <2 - KEEP LIGHT

# remove dog_RA
squ400.4 <- glmer(data = squ.data,
                  capt ~ wood400 + urban400 + area + light400 + noise400 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(squ400.mod, squ400.4) # FULL LOWER - KEEP DOG_RA

# remove area
squ400.5 <- glmer(data = squ.data,
                  capt ~ wood400 + urban400 + dog_RA + light400 + noise400 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(squ400.mod, squ400.5) # DIFF <1 - KEEP AREA

# remove urban
squ400.6 <- glmer(data = squ.data,
                  capt ~ wood400 + area + dog_RA + light400 + noise400 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(squ400.mod, squ400.6) # DIFF <2 - KEEP URBAN

# remove woodland
squ400.7 <- glmer(data = squ.data,
                  capt ~ urban400 + area + dog_RA + light400 + noise400 + (1|Site), 
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(squ400.mod, squ400.7) # FULL LOWER - KEEP WOODLAND


### FINAL MODEL FOR SQUIRREL - BUFFER 400 ###
final.squ400 <- glmer(data = squ.data,
                      capt ~ wood400 + urban400 + area + dog_RA + light400 + noise400 + (1|Site), 
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(final.squ400)
# pseudo r-sq
r.squaredGLMM(final.squ400) # 0.135

#########################
### 250m MODEL - DEER ###
#########################

# full occupancy model
dee250.mod <- glmer(data = dee.data,
                    capt ~ wood250 + urban250 + area + dog_RA + light250 + noise250 + (1|Site),
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# remove noise
dee250.2 <- glmer(data = dee.data,
                  capt ~ wood250 + urban250 + area + dog_RA + light250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer="optimx", optCtrl=list(method=c("nlminb","bobyqa","L-BFGS-B"))))
AIC(dee250.mod, dee250.2) # DIFF < 2 - KEEP NOISE

# remove light
dee250.3 <- glmer(data = dee.data,
                  capt ~ wood250 + urban250 + area + dog_RA + noise250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(dee250.mod, dee250.3) # DIFF <2 - KEEP LIGHT

# remove dog_RA
dee250.4 <- glmer(data = dee.data,
                  capt ~ wood250 + urban250 + area + light250 + noise250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(dee250.mod, dee250.4) # DIFF <2 - KEEP DOG_RA

# remove area
dee250.5 <- glmer(data = dee.data,
                  capt ~ wood250 + urban250 + dog_RA + light250 + noise250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(dee250.mod, dee250.5) # FULL LOWER - KEEP AREA

# remove urban
dee250.6 <- glmer(data = dee.data,
                  capt ~ wood250 + area + dog_RA + light250 + noise250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(dee250.mod, dee250.6) # DIFF <2 - KEEP URBAN

# remove woodland
dee250.7 <- glmer(data = dee.data,
                  capt ~ urban250 + area + dog_RA + light250 + noise250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(dee250.mod, dee250.7) # FULL LOWER - KEEP WOODLAND


### FINAL DEER MODEL - BUFFER 250 ###
final.deer250 <- glmer(data = dee.data,
                       capt ~ wood250 + urban250 + area + dog_RA + light250 + noise250 + (1|Site),
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(final.deer250)
# pseudo r-sq
r.squaredGLMM(final.deer250) # 0.256

########################
### 1km MODEL - DEER ###
########################

# full occupancy model
dee1km.mod <- glmer(data = dee.data,
                    capt ~ wood1km + urban1km + area + dog_RA + light1km + noise1km + (1|Site),
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# remove noise
dee1km.2 <- glmer(data = dee.data,
                  capt ~ wood1km + urban1km + area + dog_RA + light1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(dee1km.mod, dee1km.2) # FULL LOWER - KEEP NOISE

# remove light
dee1km.3 <- glmer(data = dee.data,
                  capt ~ wood1km + urban1km + area + dog_RA + noise1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(dee1km.mod, dee1km.3) # FULL LOWER - KEEP LIGHT

# remove dog_RA
dee1km.4 <- glmer(data = dee.data,
                  capt ~ wood1km + urban1km + area + light1km + noise1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(dee1km.mod, dee1km.4) # FULL LOWER - KEEP DOG_RA

# remove area
dee1km.5 <- glmer(data = dee.data,
                  capt ~ wood1km + urban1km + dog_RA + light1km + noise1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(dee1km.mod, dee1km.5) # FULL LOWER - KEEP AREA

# remove urban
dee1km.6 <- glmer(data = dee.data,
                  capt ~ wood1km + area + dog_RA + light1km + noise1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(dee1km.mod, dee1km.6) # FULL LOWER - KEEP URBAN

# remove woodland
dee1km.7 <- glmer(data = dee.data,
                  capt ~ urban1km + area + dog_RA + light1km + noise1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(dee1km.mod, dee1km.7) # DIFF <2 - KEEP WOODLAND


### FINAL DEER MODEL - 1KM BUFFER ###
final.deer1km <- glmer(data = dee.data,
                       capt ~ wood1km + urban1km + area + dog_RA + light1km + noise1km + (1|Site),
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(final.deer1km)
# pseudo r-sq
r.squaredGLMM(final.deer1km) # 0.344

########################
### 250m MODEL - FOX ###
########################

# full model
fox250.mod <- glmer(data = fox.data,
                    capt ~ wood250 + urban250 + area + dog_RA + light250 + noise250 + (1|Site),
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# remove noise
fox250.2 <- glmer(data = fox.data,
                  capt ~ wood250 + urban250 + area + dog_RA + light250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox250.mod, fox250.2) # FULL LOWER - KEEP NOISE

# remove light
fox250.3 <- glmer(data = fox.data,
                  capt ~ wood250 + urban250 + area + dog_RA + noise250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox250.mod, fox250.3) # DIFF <2 - KEEP LIGHT

# remove dog_RA
fox250.4 <- glmer(data = fox.data,
                  capt ~ wood250 + urban250 + area + light250 + noise250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox250.mod, fox250.4) # FULL LOWER - KEEP DOG_RA

# remove area
fox250.5 <- glmer(data = fox.data,
                  capt ~ wood250 + urban250 + dog_RA + light250 + noise250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox250.mod, fox250.5) # FULL LOWER - KEEP AREA

# remove urban
fox250.6 <- glmer(data = fox.data,
                  capt ~ wood250 + area + dog_RA + light250 + noise250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox250.mod, fox250.6) # DIFF <2 - KEEP URBAN

# remove woodland
fox250.7 <- glmer(data = fox.data,
                  capt ~ urban250 + area + dog_RA + light250 + noise250 + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox250.mod, fox250.7) # FULL LOWER - KEEP WOODLAND


# FINAL MODEL 250 BUFFER FOR FOXES
final.fox250 <- glmer(data = fox.data,
                      capt ~ wood250 + urban250 + area + dog_RA + light250 + noise250 + (1|Site),
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(final.fox250)
# pseudo r-squared
r.squaredGLMM(final.fox250) # 0.119

#######################
### 1km MODEL - FOX ###
#######################

# full model
fox1km.mod <- glmer(data = fox.data,
                    capt ~ wood1km + urban1km + area + dog_RA + light1km + noise1km + (1|Site),
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# remove noise 
fox1km.2 <- glmer(data = fox.data,
                  capt ~ wood1km + urban1km + area + dog_RA + light1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox1km.mod, fox1km.2) # DIFF <2 - KEEP NOISE 

# remove light
fox1km.3 <- glmer(data = fox.data,
                  capt ~ wood1km + urban1km + area + dog_RA + noise1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox1km.mod, fox1km.3) # FULL LOWER - KEEP LIGHT

# remove dog_RA
fox1km.4 <- glmer(data = fox.data,
                  capt ~ wood1km + urban1km + area + light1km + noise1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox1km.mod, fox1km.4) # DIFF <2 - KEEP DOG_RA

# remove area
fox1km.5 <- glmer(data = fox.data,
                  capt ~ wood1km + urban1km + dog_RA + light1km + noise1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox1km.mod, fox1km.5) # DIFF <2 - KEEP AREA

# remove urban
fox1km.6 <- glmer(data = fox.data,
                  capt ~ wood1km + area + dog_RA + light1km + noise1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox1km.mod, fox1km.6) # FULL LOWER - KEEP URBAN

# remove woodland
fox1km.7 <- glmer(data = fox.data,
                  capt ~ urban1km + area + dog_RA + light1km + noise1km + (1|Site),
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
AIC(fox1km.mod, fox1km.7) # DIFF <1 - KEEP WOODLAND


# FINAL 1KM BUFFER MODEL FOR FOXES
final.fox1km <- glmer(data = fox.data,
                      capt ~ wood1km + urban1km + area + dog_RA + light1km + noise1km + (1|Site),
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(final.fox1km)
# pseudo r-sq
r.squaredGLMM(final.fox1km) # 0.066

########################
### PLOTCRASTINATION ###
########################

# new data frame
newdat.d <- expand.grid(wood1km = seq(0, 1, by = 0.01),
                       Site = unique(dee.data$Site),
                       Session = unique(dee.data$Session),
                       Species = "Roe Deer",
                       Scale = "Landscape",
                       dog_RA = mean(dee.data$dog_RA),
                       light1km = mean(dee.data$light1km),
                       noise1km = mean(dee.data$noise1km),
                       area = mean(dee.data$area))
newdat.d$urban1km <- 0.9-newdat.d$wood1km
# predict occupancy
deer.pred <- predict(final.deer1km, newdat.d, type = "response", se.fit = T, re.form = NA)
# add to df
newdat.d$preds <- deer.pred$fit
newdat.d$se <- deer.pred$se.fit
# remove unneccessary columns
newdat.d <- newdat.d[c("wood1km", "Scale", "preds", "se", "Species")]
# rename urban column
colnames(newdat.d)[colnames(newdat.d) == "wood1km"] <- "wood"

newdat.d2 <- expand.grid(wood250 = seq(0, 1, by = 0.01),
                        Site = unique(dee.data$Site),
                        Session = unique(dee.data$Session),
                        Species = "Roe Deer",
                        Scale = "Local",
                        dog_RA = mean(dee.data$dog_RA),
                        light250 = mean(dee.data$light250),
                        noise250 = mean(dee.data$noise250),
                        area = mean(dee.data$area))
newdat.d2$urban250 <- 0.9-newdat.d2$wood250
deer.pred2 <- predict(final.deer250, newdat.d2, type = "response", se.fit = T, re.form = NA)
newdat.d2$preds <- deer.pred2$fit
newdat.d2$se <- deer.pred2$se.fit
newdat.d2 <- newdat.d2[c("wood250", "Scale", "preds", "se", "Species")]
colnames(newdat.d2)[colnames(newdat.d2) == "wood250"] <- "wood"

# new data frame
newdat.f <- expand.grid(wood1km = seq(0, 1, by = 0.01),
                        Site = unique(fox.data$Site),
                        Session = unique(fox.data$Session),
                        Species = "Red Fox",
                        Scale = "Landscape",
                        dog_RA = mean(fox.data$dog_RA),
                        light1km = mean(fox.data$light1km),
                        noise1km = mean(fox.data$noise1km),
                        area = mean(fox.data$area))
newdat.f$urban1km <- 0.9-newdat.f$wood1km
# predict occupancy
fox.pred <- predict(final.fox1km, newdat.f, type = "response", se.fit = T, re.form = NA)
# add to df
newdat.f$preds <- fox.pred$fit
newdat.f$se <- fox.pred$se.fit
# remove unneccessary columns
newdat.f <- newdat.f[c("wood1km", "Scale", "preds", "se", "Species")]
# rename urban column
colnames(newdat.f)[colnames(newdat.f) == "wood1km"] <- "wood"

newdat.f2 <- expand.grid(wood250 = seq(0, 1, by = 0.01),
                         Site = unique(fox.data$Site),
                         Session = unique(fox.data$Session),
                         Species = "Red Fox",
                         Scale = "Local",
                         dog_RA = mean(fox.data$dog_RA),
                         light250 = mean(fox.data$light250),
                         noise250 = mean(fox.data$noise250),
                         area = mean(fox.data$area))
newdat.f2$urban250 <- 0.9-newdat.f2$wood250
fox.pred2 <- predict(final.fox250, newdat.f2, type = "response", se.fit = T, re.form = NA)
newdat.f2$preds <- fox.pred2$fit
newdat.f2$se <- fox.pred2$se.fit
newdat.f2 <- newdat.f2[c("wood250", "Scale", "preds", "se", "Species")]
colnames(newdat.f2)[colnames(newdat.f2) == "wood250"] <- "wood"

# new data frame
newdat.s <- expand.grid(wood400 = seq(0, 1, by = 0.01),
                        Site = unique(squ.data$Site),
                        Session = unique(squ.data$Session),
                        Species = "Grey Squirrel",
                        Scale = "Landscape",
                        dog_RA = mean(squ.data$dog_RA),
                        light400 = mean(squ.data$light400),
                        noise400 = mean(squ.data$noise400),
                        area = mean(squ.data$area))
newdat.s$urban400 <- 0.9-newdat.s$wood400
# predict occupancy
squ.pred <- predict(final.squ400, newdat.s, type = "response", se.fit = T, re.form = NA)
# add to df
newdat.s$preds <- squ.pred$fit
newdat.s$se <- squ.pred$se.fit
# remove unneccessary columns
newdat.s <- newdat.s[c("wood400", "Scale", "preds", "se", "Species")]
# rename urban column
colnames(newdat.s)[colnames(newdat.s) == "wood400"] <- "wood"

newdat.s2 <- expand.grid(wood100 = seq(0, 1, by = 0.01),
                         Site = unique(squ.data$Site),
                         Session = unique(squ.data$Session),
                         Species = "Grey Squirrel",
                         Scale = "Local",
                         dog_RA = mean(squ.data$dog_RA),
                         light100 = mean(squ.data$light100),
                         noise100 = mean(squ.data$noise100),
                         area = mean(squ.data$area))
newdat.s2$urban100 <- 0.9-newdat.s2$wood100
squ.pred2 <- predict(final.squ100, newdat.s2, type = "response", se.fit = T, re.form = NA)
newdat.s2$preds <- squ.pred2$fit
newdat.s2$se <- squ.pred2$se.fit
newdat.s2 <- newdat.s2[c("wood100", "Scale", "preds", "se", "Species")]
colnames(newdat.s2)[colnames(newdat.s2) == "wood100"] <- "wood"

plot <- rbind(newdat.d, newdat.d2, newdat.f, newdat.f2, newdat.s, newdat.s2)

ggplot(plot, aes(x = wood, y = preds, colour = factor(Species, levels = c("Grey Squirrel", "Red Fox", "Roe Deer")))) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = preds - se, ymax = preds + se), alpha = 0.1) +
  theme_bw() +
  labs(x = "Proportion of Woodand", y = "Predicted Occupancy") +
  facet_wrap(~factor(Scale, levels = c("Local", "Landscape"))) +
  scale_color_manual(values = c("Grey Squirrel" = '#7FC97F', "Red Fox" = '#386CB0', "Roe Deer" = '#F0027F'), name = "Species") +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_abline(intercept = 1, slope = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(-3, 3)) +
  theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )




newdat.d2 <- predict_response(final.deer250, terms = "urban250 [all]", bias_correction = T)
newdat.d2$Species <- "Roe Deer"
newdat.d2$Scale <- "Local"
newdat.d <- predict_response(final.deer1km, terms = "urban1km [all]", bias_correction = T)
newdat.d$Species <- "Roe Deer"
newdat.d$Scale <- "Landscape"
newdat.f2 <- predict_response(final.fox250, terms = "urban250 [all]", bias_correction = T)
newdat.f2$Species <- "Red Fox"
newdat.f2$Scale <- "Local"
newdat.f <- predict_response(final.fox1km, terms = "urban1km [all]", bias_correction = T)
newdat.f$Species <- "Red Fox"
newdat.f$Scale <- "Landscape"
newdat.s2 <- predict_response(final.squ100, terms = "urban100 [all]", bias_correction = T)
newdat.s2$Species <- "Grey Squirrel"
newdat.s2$Scale <- "Local"
newdat.s <- predict_response(final.squ400, terms = "urban400 [all]", bias_correction = T)
newdat.s$Species <- "Grey Squirrel"
newdat.s$Scale <- "Landscape"

plot <- rbind(newdat.d, newdat.d2, newdat.f, newdat.f2, newdat.s, newdat.s2)

ggplot(plot, aes(x = x, y = predicted, colour = factor(Species, levels = c("Grey Squirrel", "Red Fox", "Roe Deer")))) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = predicted - std.error, ymax = predicted + std.error), alpha = 0.1) +
  theme_bw() +
  labs(x = "Proportion of Woodand", y = "Predicted Occupancy") +
  facet_wrap(~factor(Scale, levels = c("Local", "Landscape"))) +
  scale_color_manual(values = c("Grey Squirrel" = '#7FC97F', "Red Fox" = '#386CB0', "Roe Deer" = '#F0027F'), name = "Species") +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_abline(intercept = 1, slope = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(-3, 3)) +
  theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )




newdat.d2 <- predict_response(final.deer250, terms = "wood250 [all]", bias_correction = T)
newdat.d2$Species <- "Roe Deer"
newdat.d2$Scale <- "Local"
newdat.d <- predict_response(final.deer1km, terms = "wood1km [all]", bias_correction = T)
newdat.d$Species <- "Roe Deer"
newdat.d$Scale <- "Landscape"
newdat.f2 <- predict_response(final.fox250, terms = "wood250 [all]", bias_correction = T)
newdat.f2$Species <- "Red Fox"
newdat.f2$Scale <- "Local"
newdat.f <- predict_response(final.fox1km, terms = "wood1km [all]", bias_correction = T)
newdat.f$Species <- "Red Fox"
newdat.f$Scale <- "Landscape"
newdat.s2 <- predict_response(final.squ100, terms = "wood100 [all]", bias_correction = T)
newdat.s2$Species <- "Grey Squirrel"
newdat.s2$Scale <- "Local"
newdat.s <- predict_response(final.squ400, terms = "wood400 [all]", bias_correction = T)
newdat.s$Species <- "Grey Squirrel"
newdat.s$Scale <- "Landscape"

plot <- rbind(newdat.d, newdat.d2, newdat.f, newdat.f2, newdat.s, newdat.s2)

ggplot(plot, aes(x = x, y = predicted, colour = factor(Species, levels = c("Grey Squirrel", "Red Fox", "Roe Deer")))) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = predicted - std.error, ymax = predicted + std.error), alpha = 0.1) +
  theme_bw() +
  labs(x = "Proportion of Woodland", y = "Predicted Occupancy") +
  facet_wrap(~factor(Scale, levels = c("Local", "Landscape"))) +
  scale_color_manual(values = c("Grey Squirrel" = '#7FC97F', "Red Fox" = '#386CB0', "Roe Deer" = '#F0027F'), name = "Species") +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_abline(intercept = 1, slope = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(-3, 3)) +
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
occ.slan <- tidy(final.squ400, effects = "fixed", conf.int = TRUE, exponentiate = F)
occ.slan$species <- "Grey Squirrel"
occ.slan$scale <- "Landscape"
occ.slan$model <- "Occupancy"
occ.sloc <- tidy(final.squ100, effects = "fixed", conf.int = TRUE, exponentiate = F)
occ.sloc$species <- "Grey Squirrel"
occ.sloc$scale <- "Local"
occ.sloc$model <- "Occupancy"
occ.flan <- tidy(final.fox1km, effects = "fixed", conf.int = TRUE, exponentiate = F) 
occ.flan$species <- "Red Fox"
occ.flan$scale <- "Landscape"
occ.flan$model <- "Occupancy"
occ.floc <- tidy(final.fox250, effects = "fixed", conf.int = TRUE, exponentiate = F)
occ.floc$species <- "Red Fox"
occ.floc$scale <- "Local"
occ.floc$model <- "Occupancy"
occ.dlan <- tidy(final.deer1km, effects = "fixed", conf.int = TRUE, exponentiate = F)
occ.dlan$species <- "Roe Deer"
occ.dlan$scale <- "Landscape"
occ.dlan$model <- "Occupancy"
occ.dloc <- tidy(final.deer250, effects = "fixed", conf.int = TRUE, exponentiate = F)
occ.dloc$species <- "Roe Deer"
occ.dloc$scale <- "Local"
occ.dloc$model <- "Occupancy"
# combine to one df
results <- rbind(occ.slan, occ.sloc, occ.flan, occ.floc, occ.dlan, occ.dloc)

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

write.csv(results, file = "~/GALLANT Technician/Camera Trap Analysis/forest.occ.csv")

