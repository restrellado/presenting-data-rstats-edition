library(tidyverse)
library(palmerpenguins)

glimpse(penguins)

levels(penguins$species)
levels(penguins$island)
levels(penguins$sex) 

penguins |> distinct(year)

summary(penguins) 


# Average Measurements 

penguins |> 
  group_by(species) |> 
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE), 
            mean_bill_depth = mean(bill_depth_mm, na.rm = TRUE), 
            mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE), 
            mean_body_mass = mean(body_mass_g, na.rm = TRUE))


# Plots

ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, color = species)) + 
  geom_point() + 
  labs(title = "Bill Length By Species")

ggplot(penguins, aes(x = body_mass_g, y = bill_depth_mm, color = species)) + 
  geom_point() + 
  labs(title = "Bill Depth By Species")

ggplot(penguins, aes(x = flipper_length_mm, y = bill_depth_mm, color = species)) + 
  geom_point() + 
  labs(title = "Flipper Length By Species")