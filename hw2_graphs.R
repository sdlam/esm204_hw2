# Attach Packages
library(tidyverse)
library(here)
library(effsize)
library(kableExtra)
library(broom)

# Read in the data
water_curves <- read_csv("Water_Districts.csv")
#individual plots
ggplot(data = water_curves, aes(x = Reduction, y = MC_Kern)) +
  geom_point(alpha = 0.5)
ggplot(data = water_curves, aes(x = Reduction, y = MC_Mojave)) +
  geom_point(alpha = 0.5)
ggplot(data = water_curves, aes(x = Reduction, y = MC_Antelope)) +
  geom_point(alpha = 0.5)
ggplot(data = water_curves, aes(x = Reduction, y = MC_Ventura)) +
  geom_point(alpha = 0.5)
ggplot(data = water_curves, aes(x = Reduction, y = MC_aggregate)) +
  geom_point(alpha = 0.5)
#combined plot 
ggplot() +
  geom_point(data = water_curves, aes(x = Reduction, y = MC_Kern, color = "Kern")) +
  geom_point(data = water_curves, aes(x = Reduction, y = MC_Mojave, color = "Mojave")) +
  geom_point(data = water_curves, aes(x = Reduction, y = MC_Antelope, color = "Antelope")) +
  geom_point(data = water_curves, aes(x = Reduction, y = MC_Ventura, color = "Ventura")) +
  labs(y = "$ / Acre Foot", 
       x = "Water Reduction (AF)", 
       title = "Marginal Cost of Water Reduction")

## Kern
mod_kern <- lm(MC_Kern ~ 0 + Reduction, data = water_curves) #regression line fit
summary(mod_kern)
kern_baseline <- 150
kern_data <- tibble(Reduction = 0:150) # creates a vector of equally spaced numbers
predicted_MC <- predict(mod_kern, newdata = kern_data)
kern_data <- kern_data %>% 
  mutate(predicted_MC = predicted_MC)
# Plotting with predicted line
ggplot(data = water_curves, aes(x = Reduction, y = MC_Kern)) +
  geom_point(alpha = 0.5) +
  geom_line(data = kern_data, aes(x=Reduction, y=predicted_MC))
#Define demand or use as Baseline (no policy) - Abatement
##### We use the data we already have, and mutate
kern_data <- kern_data %>% 
  mutate(water_demand = kern_baseline - Reduction)
# Plot the demand
ggplot(kern_data, aes(x = water_demand, y = predicted_MC)) +
  geom_line() + 
  labs(x = 'water demand (AF)', y = '$/AF')

##Mojave
mod_mojave <- lm(MC_Mojave ~ 0 + Reduction, data = water_curves)
mojave_baseline <- 140
mojave_data <- tibble(Reduction = 0:140) # creates a vector of equally spaced numbers
predicted_MC <- predict(mod_mojave, newdata = mojave_data)
mojave_data <- mojave_data %>% 
  mutate(predicted_MC = predicted_MC)
# Plotting with predicted line
ggplot(data = water_curves, aes(x = Reduction, y = MC_Mojave)) +
  geom_point(alpha = 0.5) +
  geom_line(data = mojave_data, aes(x=Reduction, y=predicted_MC))
#Define demand or use as Baseline (no policy) - Abatement
##### We use the data we already have, and mutate
mojave_data <- mojave_data %>% 
  mutate(water_demand = mojave_baseline - Reduction)
# Plot the demand
ggplot(mojave_data, aes(x = water_demand, y = predicted_MC)) +
  geom_line() + 
  labs(x = 'water demand', y = '$/AF')

##Antelope
mod_antelope <- lm(MC_Antelope ~ 0 + Reduction, data = water_curves)
antelope_baseline <- 220
antelope_data <- tibble(Reduction = 0:220) # creates a vector of equally spaced numbers
predicted_MC <- predict(mod_antelope, newdata = antelope_data)
antelope_data <- antelope_data %>% 
  mutate(predicted_MC = predicted_MC)
# Plotting with predicted line
ggplot(data = water_curves, aes(x = Reduction, y = MC_Antelope)) +
  geom_point(alpha = 0.5) +
  geom_line(data = antelope_data, aes(x=Reduction, y=predicted_MC))
#Define demand or use as Baseline (no policy) - Abatement
##### We use the data we already have, and mutate
antelope_data <- antelope_data %>% 
  mutate(water_demand = antelope_baseline - Reduction)
# Plot the demand
ggplot(antelope_data, aes(x = water_demand, y = predicted_MC)) +
  geom_line() + 
  labs(x = 'water demand (AF)', y = '$/AF')

##Ventrua 
mod_ventura <- lm(MC_Ventura ~ 0 + Reduction, data = water_curves)
ventura_baseline <- 245
ventura_data <- tibble(Reduction = 0:245) # creates a vector of equally spaced numbers
predicted_MC <- predict(mod_ventura, newdata = ventura_data)
ventura_data <- ventura_data %>% 
  mutate(predicted_MC = predicted_MC)
# Plotting with predicted line
ggplot(data = water_curves, aes(x = Reduction, y = MC_Ventura)) +
  geom_point(alpha = 0.5) +
  geom_line(data = ventura_data, aes(x=Reduction, y=predicted_MC))
#Define demand or use as Baseline (no policy) - Abatement
##### We use the data we already have, and mutate
ventura_data <- ventura_data %>% 
  mutate(water_demand = ventura_baseline - Reduction)
# Plot the demand
ggplot(ventura_data, aes(x = water_demand, y = predicted_MC)) +
  geom_line() + 
  labs(x = 'water demand (AF)', y = '$/AF')

#### We want the data for all districts to be in one plot now
joined_demand <- bind_rows('Mojave' = mojave_data, 
                           'Kern' = kern_data, 
                           'Antelope' = antelope_data, 
                           'Ventura' = ventura_data, 
                           'Aggregate' = agg_data, .id = 'Districts')
### plot the demand curves
ggplot(data = joined_demand, aes(x = water_demand, y = predicted_MC, color = Districts)) + 
  geom_line() +
  labs(title = "Predicted Marginal Demand Curves for Each District", 
       x = "Water Demand (AF)", 
       y = "$/AF")

# all the linear regression lines
ggplot(data = water_curves) +
  geom_line(data = kern_data, aes(x=Reduction, y=predicted_MC, color = "Kern")) +
  geom_line(data = mojave_data, aes(x=Reduction, y=predicted_MC, color = "Mojave")) +
  geom_line(data = antelope_data, aes(x=Reduction, y=predicted_MC, color = "Antelope")) +
  geom_line(data = ventura_data, aes(x=Reduction, y=predicted_MC, color = "Ventura")) +
  labs(title = "Predicted Marginal Cost of Water Abatemend", 
       x = "Water Reduction (AF)", 
       y = "$ / Acre Foot")
#regression lines and marginal cost plots 
ggplot() +
  geom_point(data = water_curves, aes(x = Reduction, y = MC_Kern, color = "Kern")) +
  geom_line(data = kern_data, aes(x=Reduction, y=predicted_MC, color = "Kern")) +
  geom_point(data = water_curves, aes(x = Reduction, y = MC_Mojave, color = "Mojave")) +
  geom_line(data = mojave_data, aes(x=Reduction, y=predicted_MC, color = "Mojave")) +
  geom_point(data = water_curves, aes(x = Reduction, y = MC_Antelope, color = "Antelope")) +
  geom_line(data = antelope_data, aes(x=Reduction, y=predicted_MC, color = "Antelope")) +
  geom_point(data = water_curves, aes(x = Reduction, y = MC_Ventura, color = "Ventura")) +
  geom_line(data = ventura_data, aes(x=Reduction, y=predicted_MC, color = "Ventura")) +
  labs(y = "$ / Acre Foot", 
       x = "Water Reduction (AF)", 
       title = "Marginal Cost of Water Reduction")

#### 3A, B, C

#All hinge on the demand curve. Once you have the final colorful plot you can tackle 3 A, B, and C.

### 3A
#We want a single number. 

baseline = c(150, 140, 220, 245)
# A single fraction multiplied by these numbers that sum to 755-500 = 255. We want to deduct by a total of 255. 
# Use wolfram alpha to solve: p * 150 + p * 140 + p * 220 + p * 245 = 255
# p = 0.338


# A function to compute the cost for a districts abatement given the abatement number

quantity_abate = 50
lm_mod = mod_kern
# defining the function
calc_cost <- function(lm_mod, quantity_abate) {
  estimated_cost = lm_mod$coefficients * quantity_abate
  total_cost = 0.5 * quantity_abate * estimated_cost
  return(total_cost[[1]])
}

# Now with the above code we can answer questions like: What is the total cost if Kern were able to abate 117 units?
calc_cost(mod_kern, quantity_abate = 117)
# we can fill in different numbers for the 117, and get the total coast answer

# Q3a, kern abated 0.338 * 150 = 50.7 units
calc_cost(mod_kern, quantity_abate = 50.7) 
calc_cost(mod_mojave, quantity_abate = 47.43) 
calc_cost(mod_antelope, quantity_abate = 74.36) 
calc_cost(mod_ventura, quantity_abate = 82.81) 
# these calculations are the same thing as if we calculated the area under the curve up to 50.7 by using the area of a triangle. The answer we got here was 2938.55








