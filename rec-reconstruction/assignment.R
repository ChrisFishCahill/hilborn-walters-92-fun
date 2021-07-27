data <- read.csv("rec-reconstruction/data.csv")
str(data)

#----------------------------------------------------
#Notes on data
#Pacific Yellowfin Tuna, total catches over gear types
#CPUE from Japan longlines
#Catch (millions), Effort (million hooks)
#----------------------------------------------------

# S annual finite survival rate
#S = e^(-M), where M is instantaneous natural mortality
# What does fishbase say? e.g., 
# http://www.fishbase.org/Summary/SpeciesSummary.php?ID=143&AT=yellowfin+tuna
# See also Hampton 1992, M ranges from 0.3 to ~ 2.0?! 

#Set up leading parameters
S <- exp(-0.6)
q <- 0.01145 #estimated from assessment models

#General model: 
#N[t+1] = S(N[t]-C[t]) + R[t]
#Solve for R[t]: 
#R[t] = N[t+1] - S(N[t] - C[t])

#Back calculate Nt for all years
data$N <- data$mean_cpue / q


