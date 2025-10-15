## Model selection Assignment 
# SSS 6 OCTOBER 2025

# ---- Set up ----
# libraries
library(ggplot2)
library(patchwork)
library(MuMIn)
library(easystats)
library(arm)
library(ggfortify)
library(grid)
library(readr)
library(dplyr)
library(AICcmodavg)
library(performance)
library(GGally)
library(olsrr)
library(lme4)
library(modelsummary)
library(broom.mixed)

# bring in my dataset
gm <- read.csv("data/data_gmocc_all-ut.csv")

# create a df to work with
  # selecting predictor variables: rockiness, pp, shrub density, air temp, and elevation (5)
  # create a transect-level df
    # median rock_ind, pp_total, shr_den, mean air_temp, median elev, "sum" of detection across col_points
gm_df <- gm %>%
  group_by(tran_id) %>%
  select(date, site_id, tran_id, col_point, rock_ind, shr_m_density, pp_total, elev, air_temp, detection) %>%
  mutate(m_air_temp = mean(air_temp, na.rm = TRUE),
         med_rock = median(rock_ind, na.rm = TRUE),
         med_elev = median(elev, na.rm = TRUE),
         detection = as.integer(sum(detection, na.rm = TRUE) > 0)) %>%
  ungroup()

gm_df <- gm_df %>%
  group_by(tran_id) %>%
  slice_head(n = 1) %>%
  select(date, tran_id, site_id, med_rock, med_elev, shr_m_density, pp_total, m_air_temp, detection) %>%
  ungroup()

gm_df <- gm_df %>%
  rename(
    rockiness = med_rock,
    elevation = med_elev,
    air_temp = m_air_temp,
    prey_presence = pp_total,
    shrub_density = shr_m_density,
    transect_id = tran_id) %>%
  arrange(date)

glimpse(gm_df)

# ---- Checking for colinearity ---
#check for co-linearity among the five predictor variables
pairs(gm_df[,4:8], lower.panel = NULL) 

ggpairs(gm_df, columns = 4:8) + theme_minimal()
## Elevation is highly correlated with shrub density and air temperature. Biologically this makes sense to me, with higher elevations being associated with more shrub density
# (vegetative diversity/communities), and cooler temperatures.

# look at coefficients of the Global Model 
model_global <- glm(detection ~ rockiness + elevation + air_temp + prey_presence + shrub_density, family = binomial, data = gm_df)

# Variance Inflation Factor
check_collinearity(model_global)
# All VIF scores are low (<5) which means there is some correlation but not too severe.

# ---- Establishing Hypotheses ----
# Global Model: model_global < - glm(detection ~ rockiness + elevation + air_temp + prey_presence + shrub_density, family = binomial, data = gm_df)

# Model 1: model_01 <- glmer(detection ~ rockiness + elevation + (1|site_id/transect_id), family = binomial, data = gm_df)
  # Detection probability increases on sandier transects due to tracks being more easily detected in loose substrates, and lower-elevation transects where thermal conditions and 
  # refugia availability increases Gila monster activity.

# Model 2: model_02 <- glmer(detection ~ rockiness + air_temp + (1|site_id/transect_id), family = binomial, data = gm_df)
  # Detection probability increases on sandier transects and during warmer air temperatures (with the caveat of this not being a continuous linear regression).

# Model 3: model_03 <- glmer(detection ~ rockiness + shrub_density + (1|site_id/transect_id), family = binomial, data = gm_df)
  # Detection probability increases on sandier, more sparsely vegetated transects where visibility and surface activity are greater, and declines on rockier, more shrub-dense transects.

# Model 4: model_04 <- glmer(detection ~ rockiness + prey_presence + (1|site_id/transect_id), family = binomial, data = gm_df)
  # Detection probability increases on sandy transects and where adult prey were observed the most. Foraging opportunities increase above-ground activity and activity is most easily detected
  # on sandier substrates.

# Model 5: model_05 <- glmer(detection ~ rockiness + shrub_density + prey_presence + (1|site_id/transect_id), family = binomial, data = gm_df)
  # Detection probabiltiy increases on sandier transects with prey present but declines with higher shrub density; rockier, shrub-dense habitats reduce detectability unless prey availability boosts surface activity.
  # Such as in creosote-white bursage habitat where tortoises and ground-nesting birds lay their eggs.

# Model 6: model_06 <- glmer(detection ~ shrub_density + prey_presence + (1|site_id/transect_id), family = binomial, data = gm_df)
  # Detection probability increases with prey presence as animals spend more time foraging above ground and decreases with shrub density due to visual obstruction, though more prey might be present.

options(na.action = "na.fail")   # ensure common rows

# ---- Writing Models ----
# GLMS
model_01 <- glm(detection ~ rockiness + elevation, family = binomial, data = gm_df)
model_02 <- glm(detection ~ rockiness + air_temp, family = binomial, data = gm_df)
model_03 <- glm(detection ~ rockiness + shrub_density, family = binomial, data = gm_df)
model_04 <- glm(detection ~ rockiness + prey_presence, family = binomial, data = gm_df)
model_05 <- glm(detection ~ rockiness + shrub_density + prey_presence, family = binomial, data = gm_df)
model_06 <- glm(detection ~ shrub_density + prey_presence, family = binomial, data = gm_df)
model_07 <- glm(detection ~ rockiness + elevation + air_temp, family = binomial, data = gm_df)

# GLMMS
gm_df <- gm_df %>%
  mutate(site_id = factor(site_id),
         transect_id = factor(transect_id))
model_08 <- glmer(detection ~ rockiness + elevation + (1|site_id/transect_id), family = binomial, data = gm_df)
model_09 <- glmer(detection ~ rockiness + air_temp + (1|site_id/transect_id), family = binomial, data = gm_df)
model_10 <- glmer(detection ~ rockiness + shrub_density + (1|site_id/transect_id), family = binomial, data = gm_df)
model_11 <- glmer(detection ~ rockiness + prey_presence + (1|site_id/transect_id), family = binomial, data = gm_df)
model_12 <- glmer(detection ~ rockiness + shrub_density + prey_presence + (1|site_id/transect_id), family = binomial, data = gm_df)
model_13 <- glmer(detection ~ shrub_density + prey_presence + (1|site_id/transect_id), family = binomial, data = gm_df)
model_14 <- glmer(detection ~ rockiness + elevation + air_temp + (1|site_id/transect_id), family = binomial, data = gm_df)


# glm_models: model_01, model_02, model_03, model_04, model_05, model_06, model_07
# glmer models: model_08, model_09, model_10, model_11, model_12, model_13, model_14

# ---- Model Selection ----
# Compare models
model_comparison <- model.sel(model_01, model_02, model_03, model_04, model_05, model_06, model_07)
model_comparison

# model_02 (rock + air temp) best describes the data; followed by model_07 (rock, air temp, elevation)
# which variables are most influential in these models? 

gm_models <- model.sel(model_01, model_02, model_03, model_04, model_05, model_06, model_07)
sw(gm_models)
# Looking at the output, there is plenty of evidence for *rockiness* and *air temp*, but much less for *elevation*, *shrub density*, or *prey presence*.

model.avg(gm_models, revised.var = TRUE)

# ---- Interpretation ----
# Can now report the model averaged coefficients for the predictor variables individual effects on detection probability.
# Used to average regression coefficients across multiple models with the ultimate goal of capturing a variable’s overall “effect.”
summary(model.avg(gm_models, subset = delta < 2)) 

# Intercept: high p-value most likely because of the low amount of detections
# Detection probability increases as the rockiness index increases; in this case the results suggest that detections are more likely on sandier transects
# Detection probability increases with air temperature; biologically this (mostly) makes sense as Gilas are ectotherms and have greater surface activity at
  # warmer temperatures; HOWEVER they to have a low tolerance for high temperatures so this relationship is not continuous in reality.
# Elevation has little influence on detection after accounting for the substrate texture and air temperture.
# Conditional average = only includes models where the variable appears.
  # Here nearly identical to the full average, so variable inclusion is consistent across top models.
# Thus the supported hypothesis is that Gila monster detection probability increases with warmer air temperatures and sandier substrates with elevation playing a minor role.

# ---- Variable Plots ----
plot_rockiness <-
  ggplot(gm_df, aes(rockiness, detection)) +    
  geom_jitter(size=2, width = 0.25) +                                  # jitter helps to see all data points; width for spread     
  geom_smooth(method="glm",
              fullrange = TRUE,                                        # force fit line past limits of data
              method.args=list(family=binomial(link="logit"))) +
  labs(title="Rockiness") +
  ylab ("Detection (0/1)") +
  xlab ("Rockiness")

plot_rockiness

plot_airtemp <-
  ggplot(gm_df, aes(air_temp, detection)) +    
  geom_jitter(size=2, width = 0.25) +                                  # jitter helps to see all data points; width for spread     
  geom_smooth(method="glm",
              fullrange = TRUE,                                        # force fit line past limits of data
              method.args=list(family=binomial(link="logit"))) +
  labs(title="Air Temperature") +
  ylab ("Detection (0/1)") +
  xlab ("Air Temperature (C)")

plot_airtemp

plot_elevation <-
  ggplot(gm_df, aes(elevation, detection)) +    
  geom_jitter(size=2, width = 0.25) +                                  # jitter helps to see all data points; width for spread     
  geom_smooth(method="glm",
              fullrange = TRUE,                                        # force fit line past limits of data
              method.args=list(family=binomial(link="logit"))) +
  labs(title="Elevation") +
  ylab ("Detection (0/1)") +
  xlab ("Elevation (m)")

plot_elevation

plot_shrubden <- 
  ggplot(gm_df, aes(shrub_density, detection)) +    
  geom_jitter(size=2, width = 0.25) +                                  # jitter helps to see all data points; width for spread     
  geom_smooth(method="glm",
              fullrange = TRUE,                                        # force fit line past limits of data
              method.args=list(family=binomial(link="logit"))) +
  labs(title="Shrub Density") +
  ylab ("Detection (0/1)") +
  xlab ("Shrub Density (m^2)")

plot_shrubden

plot_prey <-   
  ggplot(gm_df, aes(prey_presence, detection)) +    
  geom_jitter(size=2, width = 0.25) +                                  # jitter helps to see all data points; width for spread     
  geom_smooth(method="glm",
              fullrange = TRUE,                                        # force fit line past limits of data
              method.args=list(family=binomial(link="logit"))) +
  labs(title="Prey Presence") +
  ylab ("Detection (0/1)") +
  xlab ("Prey Count")

plot_prey
# ---- Adding in Mixed-Effects ----
model_comparison2 <- model.sel(model_01, model_02, model_03, model_04, model_05, model_06, model_07,
                               model_08, model_09, model_10, model_11, model_12, model_13, model_14)
model_comparison2

# Adding in the mixed-effects models changes our results completely. By taking in to account the random effects of site and transect, the only supported model is model 13:
# which uses shrub density and prey presence as the predictor variables. This leads us to conclude that 
# Detection probability of Gila monsters 
# Prey presence contributed little to detection, implying that Gila monster activity (and thus detectability) is more governed by thermal ecology than by immediate prey abundance cues.
# Temperature provides a major explanatory gain.
# Adding other predictors (rockiness, elevation, shrub density) increased AICc without improving fit — meaning they didn’t add useful information.
# The random intercepts (1 | site_id/transect_id) improved the model compared with non-mixed GLMs, capturing spatial structure among transects.