library(tidyverse)
data <- read.csv("vpa/cod_long.csv")
head(data)
unique(data$which_data)

#Northern Cod Virtual Population Analysis from 2J3KL
#baa = biomass at age
#naa = numbers at age
#waa = waa

#Sweet victory, got the data into R