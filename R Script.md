# Mini-project-R-script
# Using non-tropical and tropical bird sister species pairs to investigate latitudinal variation in clutch size.
# Code was tested using R 3.2.3 on 15 Feb 2018.

# Clear workspace and load dataset
rm(list=ls())
project<-read.csv("bird_sister_species_data.csv", stringsAsFactors=FALSE)

# Run a paired t-test (main result in report)
with(project, t.test(nontropical_litter_or_clutch_size_n, tropical_litter_or_clutch_size_n, paired = TRUE))

# Create a dataframe for non-tropical and tropical clutch sizes
nontropical_clutch<-project$nontropical_litter_or_clutch_size_n
tropical_clutch<-project$tropical_litter_or_clutch_size_n
df<-data.frame(values = c(nontropical_clutch, tropical_clutch), tropical = rep(c('Non-tropical','Tropical'), each = 68))

# Plot this dataframe to visualise the t-test output
install.packages("ggplot2")
library(ggplot2)
ggplot(data=df, aes(x=values, group=tropical, fill=tropical)) + geom_histogram() + labs(x="Clutch size", y="Density") + theme_classic() + scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12, 14)) + scale_y_continuous(breaks=c(0, 5, 10, 15, 20, 25)) + theme(legend.title = element_blank())

# Run collinearity analysis plots
pairs(project)

# Further investigate any pairs with high collinearity and remove them from analysis
cor(multreg_complete$meanlogegg_mass, multreg_complete$meanlogbodymass)
# collinearity = 0.98 so remove one

# Run multiple regression
require(dplyr)

# For diet, use a categorical classification: is either of the pair carnivorous or omnivorous?
# For migratory status, use a categorical classification: is either of the pair partial or obligate migrant?
predtypes <- c('Invertebrate','Omnivore','VertFishScav')
multregdat <- project %>% mutate(interann_temp = nontropical_interannual_var_temp - tropical_interannual_var_temp,
                                 elev_cv = nontropical_elevation_cv - tropical_elevation_cv,
                                 predator = nontropical_Diet.5Cat %in% predtypes | tropical_Diet.5Cat %in% predtypes,
                                 migrant = nontropical_migrant_status %in% c('partial','obligate') | tropical_migrant_status %in% c('partial','obligate'),
                                 dist_phylo = dist_phy,
                                 lat = nontropical_lat - tropical_lat,
                                 interann_precip = nontropical_interannual_var_precip - tropical_interannual_var_precip,
                                 meanlogegg_mass = apply(cbind(log10(nontropical_egg_mass_g), log10(tropical_egg_mass_g)), 1, mean),
                                 clutch_size_diff = clutch_size_diff) %>%
  dplyr::select(interann_precip, meanlogegg_mass, clutch_size_diff, lat, dist_phylo, interann_temp, elev_cv, predator, migrant) 
multreg_complete <- filter(multregdat, complete.cases(multregdat)) %>% as.data.frame

# Backward stepwise model selection using AIC
install.packages("MASS")
require(MASS)
install.packages("MuMIn")
require(MuMIn)
min_model <- lm(clutch_size_diff ~ 1, data = multreg_complete)
full_model <- lm(clutch_size_diff ~ interann_precip + lat + meanlogegg_mass + interann_temp + dist_phylo + elev_cv + predator + migrant,  data = multreg_complete)
min_model_formula <- formula(min_model)
aic_step_result <- stepAIC(full_model, direction = 'backward', scope = min_model_formula)

# Standardise coefficients
best_model_data <- multreg_complete %>%
  transmute(clutch_size_diff = clutch_size_diff,
            meanlogegg_mass = meanlogegg_mass,
            interann_temp = interann_temp/sd(interann_temp),
            predator = predator)
best_model_std <- lm(clutch_size_diff ~ meanlogegg_mass + interann_temp + predator, data = best_model_data)

# Plot the model output for normality
plot(best_model_std)

# Plot the model summary
summary(best_model_std)

# Plot significant variables graphically
ggplot(data=best_model_data, aes(x=interann_temp, y=clutch_size_diff)) + geom_point() + geom_smooth(method="lm", se=FALSE) + labs(x="Interannual temperature difference (CV)", y="Clutch size difference") + scale_x_continuous(breaks=c(-0.5, 0, 0.5, 0, 1, 1.5, 2, 2.5, 3, 3.5)) + theme_classic()
ggplot(data=best_model_data, aes(x=meanlogegg_mass, y=clutch_size_diff)) + geom_point() + geom_smooth(method="lm", se=FALSE) + labs(x="log(Mean egg mass)", y="Clutch size difference") + theme_classic()
