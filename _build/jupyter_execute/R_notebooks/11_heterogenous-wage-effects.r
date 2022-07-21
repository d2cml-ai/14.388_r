install.packages("librarian", quiet = T)
librarian::shelf(hdm, quiet = T)
data(cps2012)
str(cps2012)

# create the model matrix for the covariates
X <- model.matrix(~-1 + female + female:(widowed + divorced + separated + nevermarried +
hsd08 + hsd911 + hsg + cg + ad + mw + so + we + exp1 + exp2 + exp3) + +(widowed +
divorced + separated + nevermarried + hsd08 + hsd911 + hsg + cg + ad + mw + so +
we + exp1 + exp2 + exp3)^2, data = cps2012)
X <- X[, which(apply(X, 2, var) != 0)] # exclude all constant variables
demean<- function (x){ x- mean(x)}
X<- apply(X, 2, FUN=demean)
dim(X)

# target variables, index.gender specifices coefficients we are interested in
index.gender <- grep("female", colnames(X))
y <- cps2012$lnw

# this cell takes a minute to run

effects.female <- rlassoEffects(x = X, y = y, index = index.gender)


result=summary(effects.female)
result$coef

pointwise.CI <- confint(effects.female, level = 0.90)
pointwise.CI
plot(effects.female, level=0.90) # plot of the effects

joint.CI <- confint(effects.female, level = 0.90, joint = TRUE)
joint.CI
plot(effects.female, joint=TRUE, level=0.90) # plot of the effects
