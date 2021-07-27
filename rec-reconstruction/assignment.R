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

S <- exp(-0.6)

