# install.packages("librarian", quiet = T)
librarian::shelf(sandwich, lmtest, quiet = T)
# generate the simulated dataset
set.seed(123)        # set MC seed
n = 1000             # sample size
Z = rnorm(n)         # generate Z
Y0 = -Z + rnorm(n)   # conditional average baseline response is -Z
Y1 = Z + rnorm(n)    # conditional average treatment effect is +Z
D = (runif(n)<.2)    # treatment indicator; only 20% get treated 
Y = Y1*D + Y0*(1-D)  # observed Y
D = D - mean(D)      # demean D
Z = Z-mean(Z)        # demean Z

# implement each of the models on the simulated data
CL = lm(Y ~ D)          
CRA = lm(Y ~ D+ Z)      #classical
IRA = lm(Y ~ D+ Z+ Z*D) #interactive approach

# we are interested in the coefficients on variable "D".
# library(sandwich) # heterokedasticity robust standard errors
# library(lmtest) # coefficient testing
coeftest(CL, vcov = vcovHC(CL, type="HC1"))
coeftest(CRA, vcov = vcovHC(CRA, type="HC1"))
coeftest(IRA, vcov = vcovHC(IRA, type="HC1"))

summary(CL)
summary(CRA)
summary(IRA)

set.seed(123)
n = 1000
B= 1000

CLs = rep(0, B)
CRAs = rep(0, B)
IRAs = rep(0, B)

for ( i in 1:B){
  Z = rnorm(n)
  Y0 = -Z + rnorm(n)
  Y1 =  Z + rnorm(n)
  Z = Z - mean(Z)
  D = (runif(n)<.1)
  D = D- mean(D)
  Y = Y1*D + Y0*(1-D)
  CLs[i]= lm(Y ~ D)$coef[2]
  CRAs[i] = lm(Y ~ D+ Z)$coef[2]
  IRAs[i] = lm(Y ~ D+ Z+ Z*D)$coef[2]
  }

print("Standard deviations for estimators")

sqrt(mean(CLs^2))
sqrt(mean(CRAs^2))
sqrt(mean(IRAs^2))
