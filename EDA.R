library(tidyverse)
library(palmerpenguins)

glimpse(penguins)

levels(penguins$species)
levels(penguins$island)
levels(penguins$sex) 

summary(penguins) 

# Average length 

penguins |> 
  group_by(species) |> 
  summarize(mean_bill = mean(bill_length_mm, na.rm = TRUE))