### FOREST PLOT

library(ggplot2)
library(ggh4x)

# load csvs
occ <- read.csv("~/GALLANT Technician/Camera Trap Analysis/forest.occ.csv")
den <- read.csv("~/GALLANT Technician/Camera Trap Analysis/forest.den.csv")
act <- read.csv("~/GALLANT Technician/Camera Trap Analysis/forest.act.param.csv")
#noct <- read.csv("~/GALLANT Technician/Camera Trap Analysis/forest.noct.csv")

# occupancy
results <- occ

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


# density
results <- den

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
  facet_nested(cols = vars(species, scale)) +theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )


# activity
results <- act

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
  facet_nested(cols = vars(species, scale)) +theme(
    axis.title = element_text(size = 16),       # axis labels
    axis.text = element_text(size = 14),        # tick labels
    legend.title = element_text(size = 16),     # legend title
    legend.text = element_text(size = 14),      # legend entries
    strip.text = element_text(size = 15)        # facet labels
  )
