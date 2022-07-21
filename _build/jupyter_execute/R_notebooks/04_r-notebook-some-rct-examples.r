# install.packages("remotes", quiet = T)

remotes::install_github("cran/ORCI")

NV = 200745 # number of vaccinated (treated)
NU = 201229 # number of unvaccinated (control)
RV= 33/NV  # average outcome for vaccinated
RU =115/NU  # average outcome for unvaccinated
VE = (RU - RV)/RU; # vaccine efficacy

# incidence per 100000 
Incidence.RV=RV*100000
Incidence.RU=RU*100000

print(paste("Incidence per 100000 among treated:", round(Incidence.RV,4)))

print(paste("Incidence per 100000 among controlled:", round(Incidence.RU,4)))

# treatment effect - estimated reduction in incidence per 100000 people
delta.hat = 100000*(RV-RU)

print(paste("Estimated ATE of occurances per 100,000 is",  round(delta.hat,4)))

# variance, standard deviation and confidence interval of ATE using that outcomes are Bernoulli
Var.RV = RV*(1-RV)/NV
Var.RU = RU*(1-RU)/NU
Var.delta.hat =  100000^2*(Var.RV + Var.RU)
Std.delta.hat = sqrt(Var.delta.hat)

print(paste("Standard deviation for ATE is", round(Std.delta.hat,4)))

CI.delta = c(delta.hat -1.96*sqrt(Var.delta.hat), 
             delta.hat +1.96*sqrt(Var.delta.hat))

print(paste("95% confidence interval of ATE is [", round(CI.delta[1],4), ",", 
            round(CI.delta[2],4), "]"   ))

print(paste("Overall VE is", round(VE,4) ))

# we use an approximate bootstrap to find the confidence interval of vaccine efficacy
# via Monte Carlo draws
set.seed(1)
B = 10000 # number of bootstraps
RVs = RV  + rnorm(B)*sqrt(Var.RV)
RUs = RU  + rnorm(B)*sqrt(Var.RU)
VEs= (RUs - RVs)/RUs

plot(density(VEs), col=2, main="Approximate Distribution of VE estimates")

CI.VE =  quantile(VEs, c(.025, .975)) # use the empirical quantiles from the bootstraps

print(paste("95% confidence interval of VE is [", round(CI.VE[1],4), ",", 
            round(CI.VE[2],4), "]"))

NV =  19965; # number vaccinated
NU =  20172; # number unvaccinated
RV = 9/NV; # average outcome for vaccinated
RU = 169/NU; # average outcome for unvaccinated
VE = (RU - RV)/RU; # vaccine efficacy

# incidence per 100000
Incidence.RV=RV*100000
Incidence.RU=RU*100000

print(paste("Incidence per 100000 among vaccinated:", round(Incidence.RV,4)))

print(paste("Incidence per 100000 among unvaccinated:", round(Incidence.RU,4)))

# treatment effect - estimated reduction in incidence per 100000 people
delta.hat = 100000*(RV-RU)
      
print(paste("Estimated ATE of occurances per 100,000 is",  round(delta.hat,4)))
      
# variance, standard deviation and confidence interval of ATE using that outcomes are Bernoulli
Var.RV = RV*(1-RV)/NV
Var.RU = RU*(1-RU)/NU
Var.delta.hat =  100000^2*(Var.RV + Var.RU)
Std.delta.hat = sqrt(Var.delta.hat)

print(paste("Standard deviation for ATE is", round(Std.delta.hat,4)))

CI.delta = c(delta.hat -1.96*sqrt(Var.delta.hat), 
             delta.hat +1.96*sqrt(Var.delta.hat))

print(paste("95% confidence interval of ATE is [", round(CI.delta[1],4), ",", 
            round(CI.delta[2],4), "]"   ))

print(paste("Overall VE is", round(VE,4) ))
      
# we use an approximate bootstrap to find the VE confidence interval
# using Monte Carlo draws as before
set.seed(1)
B = 10000
RVs = RV  + rnorm(B)*sqrt(Var.RV)
RUs = RU  + rnorm(B)*sqrt(Var.RU)
VEs= (RUs - RVs)/RUs

plot(density(VEs), col=2, main="Approximate Distribution of VE estimates")

CI.VE = quantile(VEs, c(.025, .975))

print(paste("95% confidence interval of VE is [", round(CI.VE[1],4), ",", 
            round(CI.VE[2],4), "]" ))

# Here we calculate the overall effectiveness of the vaccine for the two groups that are 65 or older
NV =  3239+805;
NU =  3255+812;
RV = 1/NV;
RU = (14+5)/NU;
VE = (RU - RV)/RU;

print(paste("Overall VE is", round(VE,4)) )

Var.RV = RV*(1-RV)/NV
Var.RU = RU*(1-RU)/NU

# As before, we use an approximate bootstrap to find the confidence intervals
# using Monte Carlo draws

set.seed(1)
B = 10000
    RVs = RV  + rnorm(B)*sqrt(Var.RV)+ 10^(-10)
    RUs = RU  + rnorm(B)*sqrt(Var.RU)+ 10^(-10)
    VEs= (RUs - RVs)/RUs

plot(density(VEs), col=2, main="Approximate Distribution of VE estimates")

CI.VE =  quantile(VEs, c(.025, .975))

print(paste("two-sided 95 % confidence interval is [", CI.VE[1], ",", 
            CI.VE[2], "]"   ))

OneSidedCI.VE =  quantile(VEs, c(.05))

print(paste("one-sided 95 % confidence interval is [", OneSidedCI.VE[1], ",", 
            1, "]"   ))

NV =  3239+805;
NU =  3255+812;
RV = 1/NV;
RU = (14+5)/NU;
VE = (RU - RV)/RU;

print(paste("Overall VE is", VE) )

set.seed(1)
B = 10000 #number of simulation draw
    RVs = rbinom(100000, size= NV, prob = RV) 
    RUs = rbinom(100000, size= NU, prob = RU)
    VEs= (RUs - RVs)/RUs

plot(density(VEs), col=2, main="Approximate Distribution of VE estimates")

CI.VE =  quantile(VEs, c(.025, .975))

print(paste("two-sided 95 % confidence interval is [", CI.VE[1], ",", 
            CI.VE[2], "]"   ))

OneSidedCI.VE =  quantile(VEs, c(.05))

print(paste("one-sided 95 % confidence interval is [", OneSidedCI.VE[1], ",",  1, "]"   ))

# Exact CI exploiting Bernoulli outcome using the Cornfield Procedure
library(ORCI)

NV =  19965;
NU =  20172;
RV = 9/NV;
RU = 169/NU;
VE = (RU - RV)/RU;

1- Cornfieldexact.CI(9, NV, 169, NU, conf = 0.95, interval = c(1e-08, 1e+08))

# Exact CI exploiting Bernoulli outcome for the two groups that are 65 or older
library(ORCI)
NV =  3239+805;
NU =  3255+812;
RV = 1/NV;
RU = (14+5)/NU;
VE = (RU - RV)/RU;

1- Cornfieldexact.CI(1, NV, 19, NU, conf = 0.95, interval = c(1e-08, 1e+08))
