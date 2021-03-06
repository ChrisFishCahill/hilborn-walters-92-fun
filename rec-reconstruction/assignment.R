library(tidyverse)

data <- read.csv("rec-reconstruction/data.csv")
str(data)

#----------------------------------------------------
# Notes on data
# Pacific Yellowfin Tuna, total catches over gear types
# CPUE from Japan longlines
# Catch (millions), Effort (million hooks)

# General model:
# N[t+1] = S(N[t]-C[t]) + R[t]
# Solve for R[t]:
# R[t] = N[t+1] - S(N[t] - C[t])

# S annual finite survival rate
# S = e^(-M), where M is instantaneous natural mortality
# What does fishbase say about M? e.g.,
# http://www.fishbase.org/Summary/SpeciesSummary.php?ID=143&AT=yellowfin+tuna
# See also Hampton 1992, M ranges from 0.3 to ~ 2.0?!
#----------------------------------------------------

# Set up leading parameters
M <- 0.6
S <- exp(-M)
n_years <- length(unique(data$year))
q <- 0.01145 # catchability coefficient, estimated from assessment model

# Set up a vector to put recruitment predictions into
data$Rpred <- NA

# Back calculate Nt for all years
data$N <- data$mean_cpue / q

# Do the recursion (run the model)
for (t in (n_years - 1):1) {
  data$Rpred[t] <- data$N[t + 1] - S * (data$N[t] - data$catch[t])
}

#------------------------------------------------
# Plots
p1 <- data %>%
  ggplot(aes(x = effort, y = catch)) +
  geom_point() +
  ylab("Fishery Catch (millions of critters)") +
  xlab("Fishery Effort (millions of hooks)") +
  ggsidekick::theme_sleek()
p1

p2 <- data %>%
  ggplot(aes(x = year, y = N)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 3) +
  geom_line() +
  geom_point(aes(x = year, y = catch)) +
  ylab("Abundance (open) or Catch (filled)") +
  xlab("Year") +
  ggsidekick::theme_sleek()
p2

p3 <- data %>%
  ggplot(aes(y = Rpred, x = year)) +
  geom_point(shape = 21, colour = "black", fill = "black") +
  geom_line() +
  ylab("Recruits") +
  xlab("Year") +
  ggsidekick::theme_sleek()
p3

#Plot Rt vs. N[t-2]
data$stock <- NA
# set stock as N[t-2]:
data$stock[3:n_years] <- data$N[1:(n_years - 2)]

# plot stock recruit relationship
p4 <- data %>%
  ggplot(aes(y = Rpred, x = stock)) +
  geom_point() +
  ylab("Recruits") +
  xlab("Stock Size") +
  expand_limits(x = 0, y = 0) + 
  ggsidekick::theme_sleek()
p4

my_plot <- cowplot::plot_grid(p1, p2, p3, p4,
  nrow = 2
)

ggsave(
  filename = "rec-reconstruction/results.pdf",
  width = 8, height = 6, units = "in"
)
#----------------------------------------------------
