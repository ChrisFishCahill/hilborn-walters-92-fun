#----------------------------------------------------
# length-at-age data can be decomposed into three components
# 1) probability of surviving to age a (p_a)
# 2) probability of being selected given critter is age a (proxy for size) (s_l)
# 3) probability of being length x given you are age a (p_l_a)
#
# Thus, when you get a sample of length-age data, the total probability of observing
# said critters at age (Z_a) is simply the sum product of those three components, i.e.,
# sum product p_a*p_l_a*s_l across ages
#
# See also Nathan Taylor's paper in CJFAS
#---------------------------------------------------

#Declare some leading parameters:
S <- 0.8
Linf <- 100
k <- 0.35
to <- -0.1

#selectivity parameters
l50 <- 50
sig_sel <- 5

#sd length at age:
cv <- 0.1 # sig_a = cv*l_a

ages <- 1:10
n_ages <- length(ages)
lens <- 5:135
s_l <- rep(NA, length(lens))

#Set up leading vectors
#mean length at age as per von B
l_a <- Linf * (1 - exp(-k * (ages - to)))
sig_a <- l_a * cv

#Survivorship
lx <- rep(NA, length(ages))
lx[1] <- 1
for (a in 2:n_ages) {
  lx[a] = lx[a - 1] * S
}

#probability of surviving to age a (i.e., p(a))
p_a <- lx / sum(lx)

#probability of being selected by survey at age or length a/l
sel_a <- 1 / (1 + exp(-(l_a - mu_sel) / sig_sel))

#Calculate p(length|age) using normal distribution with length = La
#psi_l_a is total probablity of critter being age a given total probability from laa, lx, sel
p_l_a <- psi_l_a <- matrix(NA, nrow = length(lens), ncol = n_ages)

for (l in 1:length(lens)) {
  #calculate the selectivity at length l, i.e., s(x)
  s_l[l] = 1 / (1 + exp(-(lens[l] - mu_sel) / sig_sel))
  for (a in unique(ages)) {
    #calculate p(length | age) using normal pdf
    p_l_a[l, a] = (1 / (sig_a[a] * sqrt(2 * pi))) * exp(-0.5 * ((lens[l] -
                                                                   l_a[a]) / sig_a[a]) ^ 2)
    #calculate psi_l_a
    psi_l_a[l, a] = p_l_a[l, a] * s_l[l] * p_a[a]
  }
}

#normalize psi_l_a, maybe this is brett's length-age key?
#note brett calls this Z(x,a)
psi_l_a = psi_l_a / sum(psi_l_a)
#should try and figure out a v. sexy plot for this...

axis(2, at = seq(from = 1, to = 10, by = 1))
#Do some more calculations for Z_a, Z_a_cdf
Z_a <- Z_a_cdf <- rep(NA, nrow(p_l_a))

for (i in 1:length(Z_a)) {
  # calculate total probability of p_a, p_l_a, s_l
  Z_a[i] = sum(p_l_a[i, ] * p_a * s_l[i])
}

#normalize Z(a)
Z_a = Z_a / sum(Z_a)

#does it sum to 1?
sum(Z_a)

#cumulative Z(a), don't actually need this to generate fake lengths (see below)
Z_a_cdf[1] <- Z_a[1]
for (i in 2:length(Z_a)) {
  Z_a_cdf[i] = Z_a_cdf[i - 1] + Z_a[i]
}

#create 5000 fish of random lengths | lx, sel, length-at-age assumptions above
set.seed(6)
devs <- rmultinom(n = 1, size = 5000, prob = Z_a)

pdf("growth/growth_fig.pdf",
    width = 8, height = 6)
par(mfrow = c(2, 2))

matplot(
  lens,
  p_l_a,
  type = "l",
  las = 1,
  ylab = "p(l|a)",
  xlab = "length (cm)"
)

plot(
  s_l ~ lens,
  type = "l",
  las = 1,
  ylab = "relative selectivity",
  xlab = "length (cm)"
)

plot(lens,
     Z_a,
     ylab = "Pr(obs. fish of length l)",
     xlab = "length (cm)",
     type = "l")

barplot(devs ~ lens,
        type = "",
        xlab = "length (cm)",
        ylab = "Number critters at length")

dev.off()
