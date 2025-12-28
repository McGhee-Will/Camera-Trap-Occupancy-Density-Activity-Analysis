# Detection zone calculation

library(ggplot2)

data <- read.csv("camera_detection_zone.csv")

head(data)

squirrel <- subset(data, data$species == "grey_squirrel")
fox <- subset(data, data$species == "red_fox")
deer <- subset(data, data$species == "roe_deer")

#########################################################
### REMOVE 5% AT WIDEST ANGLES AND GREATEST DISTANCES ###
#########################################################

# identify widest angles
angle95s <- quantile(squirrel$angle_to_indv_rad, probs = c(0, 0.975))
squirrel$angle95 <- 1
squirrel$angle95 <- ifelse(squirrel$angle_to_indv_rad > angle95s[2],
                           squirrel$angle95 == '0',
                           squirrel$angle95 == '1')
angle95d <- quantile(deer$angle_to_indv_rad, probs = c(0, 0.975))
deer$angle95 <- 1
deer$angle95 <- ifelse(deer$angle_to_indv_rad > angle95d[2],
                       deer$angle95 == '0',
                       deer$angle95 == '1')
angle95f <- quantile(fox$angle_to_indv_rad, probs = c(0, 0.975))
fox$angle95 <- 1
fox$angle95 <- ifelse(fox$angle_to_indv_rad > angle95f[2],
                      fox$angle95 == '0',
                      fox$angle95 == '1')

# identify farthest distances
dist95s <- quantile(squirrel$distance_to_indv_m, probs = c(0, 0.975))
squirrel$dist95 <- 1
squirrel$dist95 <- ifelse(squirrel$distance_to_indv_m > dist95s[2],
                           squirrel$dist95 == '0',
                           squirrel$dist95 == '1')
dist95d <- quantile(deer$distance_to_indv_m, probs = c(0, 0.975))
deer$dist95 <- 1
deer$dist95 <- ifelse(deer$distance_to_indv_m > dist95d[2],
                       deer$dist95 == '0',
                       deer$dist95 == '1')
dist95f <- quantile(fox$distance_to_indv_m, probs = c(0, 0.975))
fox$dist95 <- 1
fox$dist95 <- ifelse(fox$distance_to_indv_m > dist95f[2],
                      fox$dist95 == '0',
                      fox$dist95 == '1')

# check
table(squirrel$angle95, squirrel$dist95)
table(deer$angle95, deer$dist95)
table(fox$angle95, fox$dist95)

# combine again
data2 <- rbind(squirrel, fox, deer)

summary(data2$angle95)
summary(data2$dist95)
table(data2$angle95, data2$dist95)

ggplot(data2, aes(x = lateral_distance_m, y = logitudinal_distance_m)) +
  geom_point(aes(shape = data2$angle95)) +
  scale_shape_manual(values = c(16, 1)) +
  theme_bw() +
  facet_wrap(~species)

ggplot(data2, aes(x = lateral_distance_m, y = logitudinal_distance_m)) +
  geom_point(aes(shape = data2$dist95)) +
  scale_shape_manual(values = c(16, 1)) +
  theme_bw() +
  facet_wrap(~species)

# combination column
data2$comb95 <- 1
data2$comb95 <- ifelse(data2$angle95 & data2$dist95 == T,
                       data2$comb95 <- T,
                       data2$comb95 <- F)

table(data2$comb95)

ggplot(data2, aes(x = lateral_distance_m, y = logitudinal_distance_m)) +
  geom_point(aes(shape = comb95)) +
  scale_shape_manual(values = c(16, 1)) +
  theme_bw() +
  facet_wrap(~species)

data2 <- subset(data2, data2$comb95 == T)
data2$angle_to_indv_deg <- data2$angle_to_indv_rad * (180/pi)

ggplot(data2, aes(x = lateral_distance_m, y = logitudinal_distance_m)) +
  geom_point(aes(shape = illumination)) +
  scale_shape_manual(values = c(16, 1)) +
  theme_bw() +
  facet_wrap(~species)

##########################
### MAX DIST AND ANGLE ###
##########################

squirrel <- subset(data2, data2$species == "grey_squirrel")
fox <- subset(data2, data2$species == "red_fox")
deer <- subset(data2, data2$species == "roe_deer")

# squirrel
max(squirrel$angle_to_indv_rad) # 0.548
max(squirrel$angle_to_indv_deg) # 31.389
max(squirrel$distance_to_indv_m) # 9.563
# deer
max(deer$angle_to_indv_rad) # 0.598
max(deer$angle_to_indv_deg) # 34.270
max(deer$distance_to_indv_m) # 13.803
# fox
max(fox$angle_to_indv_rad) # 0.625
max(fox$angle_to_indv_deg) # 35.791
max(fox$distance_to_indv_m) # 10.00

