# Simulation Design
install.packages("librarian")
librarian::shelf(hdm)

set.seed(1)
B= 10000 # trials
IVEst = rep(0, B)
n=100
beta = .25   # .2 weak IV
#beta = 1   #   1 strong IV


U =  rnorm(n)  
Z = rnorm(n)  #generate instrument
D = beta*Z + U  #generate endogenougs variable
Y =  D+ U  # the true causal effect is 1


summary(lm(D~Z))  # first stage is very weak here

summary(tsls(x=NULL, d=D, y=Y, z=Z))  #


# Simulation Design

set.seed(1)
B= 10000 # trials
IVEst = rep(0, B)

for(i in 1:B){
U =  rnorm(n)  
Z = rnorm(n)  #generate instrument
D = beta*Z + U  #generate endogenougs variable
Y =  D+ U  # the true causal effect is 1
IVEst[i] = coef(tsls(x=NULL, d=D, y=Y, z=Z))[1,1]
}



plot(density(IVEst-1, n=1000, from=-5, to=5),col=4, xlim= c(-5, 5),  
     xlab= "IV Estimator -True Effect", main="Actual Distribution vs Gaussian")

val=seq(-5, 5, by=.05)
var = (1/beta^2)*(1/100) # theoretical variance of IV
sd = sqrt(var)
lines(val, dnorm(val, sd=sd), col=2, lty=2)

rejection.frequency = sum(( abs(IVEst-1)/sd > 1.96))/B

cat(c("Rejection Frequency is ", rejection.frequency, " while we expect it to be .05"))


# help(tsls)

# help(density)
