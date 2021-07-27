library(tidyverse)

data <- read.csv("rec-reconstruction/data.csv")
str(data)

#----------------------------------------------------
#Notes on data
#Pacific Yellowfin Tuna, total catches over gear types
#CPUE from Japan longlines
#Catch (millions), Effort (million hooks)
#----------------------------------------------------
q <- 0.01145 #estimated from assessment models

#Back calculate Nt for all years
data$N <- data$mean_cpue / q

#Let's plot some stuff and see what's going on 
data %>%
  ggplot(aes(x=effort, y=catch)) + 
  geom_point() + 
  ylab("Fishery Catch (millions of critters)") + 
  xlab("Fishery Effort (millions of hooks)") +
  ggsidekick::theme_sleek()

ggsave("rec-reconstruction/catch_vs_effort.pdf", 
       width=5, height=3)

data %>%
  ggplot(aes(x=year, y=N)) + 
  geom_point(shape = 21, colour = "black", fill = "white", size = 3) + 
  geom_line() + 
  geom_point(aes(x=year, y=catch)) + 
  ylab("Abundance vs. Catch") + 
  xlab("Year") + 
  ggsidekick::theme_sleek()

ggsave("rec-reconstruction/N_vs_catch.pdf", 
       width=5, height=3)

#General model: 
#N[t+1] = S(N[t]-C[t]) + R[t]
#Solve for R[t]: 
#R[t] = N[t+1] - S(N[t] - C[t])

# S annual finite survival rate
#S = e^(-M), where M is instantaneous natural mortality
# What does fishbase say about M? e.g., 
# http://www.fishbase.org/Summary/SpeciesSummary.php?ID=143&AT=yellowfin+tuna
# See also Hampton 1992, M ranges from 0.3 to ~ 2.0?! 

#Set up leading parameters
S <- exp(-0.6)
n_years <- length(unique(data$year))

#Set up a vector to put recruitment predictions into
data$Rpred <- NA

#Do the recursion
for(t in (n_years-1):1){ 
  data$Rpred[t] <- data$N[t+1] - S*(data$N[t] - data$catch[t])
}

#TODO: Plot Rt vs. N[t-2], think about things

