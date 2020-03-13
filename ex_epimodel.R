library(tidyverse)
library(EpiModel) 

# (http://epimodel.org/)
# Jenness SM, Goodreau SM and Morris M. EpiModel: An R Package for Mathematical 
# Modeling of Infectious Disease over Networks. Journal of Statistical Software. 
# 2018; 84(8): 1-47. doi: 10.18637/jss.v084.i08 
# (http://doi.org/10.18637/jss.v084.i08).

help(package = "EpiModel")

epiweb("dcm") # http://statnet.org/tut/BasicDCMs.html
epiweb("icm")
epiweb("net")


# Example 1: Independent SIS model ----------------------------------------

set.seed(12345)

# Step 1: Estimating network structure

nw <- network::network.initialize(n = 1000, directed = FALSE)
nw <- network::set.vertex.attribute(nw, "risk", rep(0:1, each = 500))

formation <- ~ edges + nodefactor("risk") + nodematch("risk") + concurrent

target.stats <- c(250, 375, 225, 100)

coef.diss <- dissolution_coefs(dissolution = ~ offset(edges),
                               duration = 80)
coef.diss

est1 <- netest(nw, formation, target.stats, coef.diss)

# Step 2: Diagnosing network fits

dx <- netdx(est1, nsims = 10, nsteps = 1000)

dx

plot(dx)

par(mfrow = c(1, 2))
plot(dx, type = "duration")
plot(dx, type = "dissolution")
par(mfrow = c(1, 1))

# Step 3: Epidemic model setup and simulation -----------------------------

init <- init.net(i.num = 50)

param <- param.net(inf.prob = 0.1, act.rate = 5, rec.rate = 0.02)

control <- control.net(type = "SIS", nsteps = 500, nsims = 10, epi.by = "risk")

sim1 <- netsim(est1, param, init, control)

sim1

summary(sim1, at = 500)

plot(sim1)

plot(sim1, y = c("si.flow", "is.flow"), legend = TRUE)

plot(sim1, y = c("i.num.risk0", "i.num.risk1"), legend = TRUE)

par(mfrow = c(1, 2), mar = c(0, 0, 1, 0))
plot(sim1, type = "network", at = 1, sims = "mean", col.status = TRUE,
     main = "Prevalence at t1")
plot(sim1, type = "network", at = 500, sims = "mean", col.status = TRUE,
     main = "Prevalence at t500")
par(mfrow = c(1, 1))
