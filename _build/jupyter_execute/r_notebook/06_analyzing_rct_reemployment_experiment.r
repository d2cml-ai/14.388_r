# install.packages("librarian", quiet = T)
librarian::shelf(tidyverse, lmtest, sandwich, broom, hdm, quiet = T)
## loading the data
Penn <- as_tibble(read.table("https://github.com/d2cml-ai/14.388_r/raw/main/Data/penn_jae.dat", header=T ))
n <- dim(Penn)[1]
p_1 <- dim(Penn)[2]
Penn <- filter(Penn, tg == 4 | tg == 0)# subset(Penn, tg==4 | tg==0)
attach(Penn)

T4 <- (Penn$tg==4)
summary(T4)

glimpse(Penn)


m <- lm(T4 ~ (female + black + othrace + factor(dep) + q2 +q3 + q4 +
        q5 + q6 + agelt35 + agegt54 + durable + lusd + husd)^2, data = Penn)
coeftest(m, vcov = vcovHC(m, type="HC1"))


# model specifications


# no adjustment (2-sample approach)
formula_cl <- log(inuidur1) ~ T4

# adding controls
formula_cra <- log(inuidur1) ~ T4 + (female + black + othrace + factor(dep) +
              q2 + q3 + q4 + q5 + q6 + agelt35 +agegt54 + durable +lusd + husd )^2
# Omitted dummies: q1, nondurable, muld


ols.cl <- lm(formula_cl, data = Penn)
ols.cra <- lm(formula_cra, data = Penn)


ols.cl = coeftest(ols.cl, vcov = vcovHC(ols.cl, type="HC1"))
ols.cra = coeftest(ols.cra, vcov = vcovHC(ols.cra, type="HC1"))

tidy(ols.cl)
tidy(ols.cra)


#interactive regression model;

X = model.matrix ( ~ (female + black + othrace + factor(dep) + 
                      q2 + q3 + q4 + q5 + q6 + agelt35 + agegt54 + durable + lusd + husd)^2)[,-1]

dim(X)

demean<- function(x){ x - mean(x)}

X = apply(X, 2, demean)

ols_ira = lm(log(inuidur1) ~ T4*X) 
ols_ira= coeftest(ols_ira, vcov = vcovHC(ols_ira, type="HC1"))
tidy(ols_ira)

# library(hdm)

T4 = demean(T4)

DX = model.matrix(~T4 * X)[ ,-1]

rlasso.ira = summary(rlassoEffects(DX, log(inuidur1), index = 1))

print(rlasso.ira)

# glimpse(ols_ira)
ols_ira[2,1]

# library(xtable)
table<- matrix(0, 2, 4)
table[1,1]<-  ols.cl[2,1]
table[1,2]<-  ols.cra[2,1]
table[1,3]<-  ols_ira[2,1]
table[1,4]<-  rlasso.ira[[1]][1]

table[2,1]<-  ols.cl[2,2]
table[2,2]<-  ols.cra[2,2]
table[2,3]<-  ols_ira[2,2]
table[2,4]<-  rlasso.ira[[1]][2]


colnames(table)<- c("CL","CRA","IRA", "IRA w Lasso")
rownames(table)<- c("estimate", "standard error")
# tab<- xtable(table, digits=5)
table

