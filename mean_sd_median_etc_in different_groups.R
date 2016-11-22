library(apaTables)
library(tidyverse)

#dataset
goggles

goggles$attractiveness
mean(goggles$attractiveness)

goggles_group_gender <- goggles %>% group_by(gender)

summarise(goggles_group_gender, mean(attractiveness), sd(attractiveness))

goggles_group_booze <- goggles %>% group_by(alcohol)
summarise(goggles_group_booze, mean(attractiveness),

goggles_group_both <- goggles %>% group_by(alcohol,gender)

summarise(goggles_group_both, mean(attractiveness),sd(attractiveness), median(attractiveness))
          