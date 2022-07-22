install.packages("librarian", quiet = T)
librarian::shelf(tidyverse, sandwich, hdm, quiet = T)
data <- read_csv("https://github.com/d2cml-ai/14.388_R/raw/main/Data/wage2015_subsample_inference.csv"
        , show_col_types = F)
dim(data)
attach(data)

variables <- c("lwage","sex","shs","hsg","scl","clg","ad","ne","mw","so","we","exp1")

Z <- data |> select(all_of(variables))

data_female <- data |> filter(sex == 1)
data_male <- data |> filter(sex == 0)

Z_mean = Z |>
  mutate(sex = 3) |> # ALL 
  bind_rows(Z) |> # Sex
  group_by(sex) |>
  summarise(across(where(is.numeric), mean)) |>
  ungroup() |>
  mutate(sex = case_when(sex == 1 ~ "Female", sex == 0 ~ "Male", T ~ "All"))

colnames(Z_mean) <- c("Sex","Log Wage","Less then High School","High School Graduate","Some College","College Graduate","Advanced Degree", "Northeast","Midwest","South","West","Experience")
Z_mean |>
  pivot_longer(!Sex, names_to = "Variable") |>
  pivot_wider(names_from = Sex, values_from = value)

mean(data_female$lwage)-mean(data_male$lwage)

nocontrol_fit <- lm(lwage ~ sex, data = Z)
nocontrol_est <- summary(nocontrol_fit)$coef["sex",1]
HCV_coefs <- vcovHC(nocontrol_fit, type = 'HC'); # HC - "heteroskedasticity cosistent"
nocontrol_se <- sqrt(diag(HCV_coefs))[2] # Estimated std errors

# print unconditional effect of gender and the corresponding standard error
cat ("The estimated coefficient on the dummy for gender is",nocontrol_est,"\nand the corresponding robust standard error is", nocontrol_se) 

# ols regression with controls

flex <- lwage ~ sex + (exp1 + exp2 + exp3 + exp4) * (shs + hsg + scl + clg+ occ2 + ind2 + mw + so + we)

#   Note that ()*() operation in formula objects in R creates a formula of the sort:
#  (exp1+exp2+exp3+exp4)+ (shs+hsg+scl+clg+occ2+ind2+mw+so+we) + (exp1+exp2+exp3+exp4)*(shs+hsg+scl+clg+occ2+ind2+mw+so+we)
#  This is not intuitive at all, but that's what it does.

control_fit <- lm(flex, data = data)
control_est <- summary(control_fit)$coef[2,1]

summary(control_fit)

cat("Coefficient for OLS with controls", control_est)

HCV_coefs <- vcovHC(control_fit, type = 'HC');
control_se <- sqrt(diag(HCV_coefs))[2] # Estimated std errors

# Partialling-out using ols

# models
flex_y <- lwage ~ (exp1 + exp2 + exp3 + exp4) * (shs + hsg + scl +clg + occ2+ ind2 + mw + so + we) # model for Y
flex_d <- sex ~ (exp1 + exp2 + exp3 + exp4)*(shs + hsg + scl + clg + occ2 + ind2 + mw + so + we) # model for D

# partialling-out the linear effect of W from Y
t_Y <- lm(flex_y, data=data)$res
# partialling-out the linear effect of W from D
t_D <- lm(flex_d, data=data)$res

# regression of Y on D after partialling-out the effect of W
partial_fit <- lm(t_Y ~ t_D)
partial_est <- summary(partial_fit)$coef[2,1]

cat("Coefficient for D via partialling-out", partial_est)

# standard error
HCV_coefs <- vcovHC(partial_fit, type = 'HC')
partial_se <- sqrt(diag(HCV_coefs))[2]

# confidence interval
confint(partial_fit)[2,]

# Partialling-out using lasso

library(hdm)

# models
flex_y <- lwage ~ (exp1 + exp2 + exp3 + exp4) * (shs + hsg + scl + clg + occ2 + ind2 + mw + so + we) # model for Y
flex_d <- sex ~ (exp1 + exp2 + exp3 + exp4) * (shs + hsg + scl + clg + occ2 + ind2 + mw + so + we) # model for D

# partialling-out the linear effect of W from Y
t_Y <- rlasso(flex_y, data = data)$res
# partialling-out the linear effect of W from D
t_D <- rlasso(flex_d, data = data)$res

# regression of Y on D after partialling-out the effect of W
partial_lasso_fit <- lm(t_Y ~ t_D)
partial_lasso_est <- summary(partial_lasso_fit)$coef[2,1]

cat("Coefficient for D via partialling-out using lasso", partial_lasso_est)

# standard error
HCV_coefs <- vcovHC(partial_lasso_fit, type = 'HC')
partial_lasso_se <- sqrt(diag(HCV_coefs))[2]

table<- matrix(0, 4, 2)
table[1,1]<- nocontrol_est  
table[1,2]<- nocontrol_se   
table[2,1]<- control_est
table[2,2]<- control_se    
table[3,1]<- partial_est  
table[3,2]<- partial_se  
table[4,1]<-  partial_lasso_est
table[4,2]<- partial_lasso_se 
colnames(table)<- c("Estimate","Std. Error")
rownames(table)<- c("Without controls", "full reg", "partial reg", "partial reg via lasso")	
table

# extra flexible model

extraflex <- lwage ~ sex + (exp1 + exp2 + exp3 + exp4 + shs + hsg + scl + clg + occ2 + ind2 + mw + so + we)^2

control_fit <- lm(extraflex, data=data)
summary(control_fit)
control_est <- summary(control_fit)$coef[2,1]

cat("Number of Extra-Flex Controls", length(control_fit$coef) - 1, "\n")

cat("Coefficient for OLS with extra flex controls", control_est)

HCV_coefs <- vcovHC(control_fit, type = 'HC');

n = length(wage); p =length(control_fit$coef);

control_se <- sqrt(diag(HCV_coefs))[2] * sqrt( n / (n - p)) # Estimated std errors

# This is a crude adjustment for the effect of dimensionality on OLS standard errors, 
# motivated by Cattaneo, Jannson, and Newey (2018). For a more correct approach, we 
# would implement the approach of Cattaneo, Jannson, and Newey (2018)'s procedure.

library(hdm)

# models
extraflex_y <- lwage ~ (exp1 + exp2 + exp3 + exp4 + shs + hsg + scl + clg + occ2 + ind2 + mw + so + we)^2 # model for Y
extraflex_d <- sex ~ (exp1 + exp2 + exp3 + exp4 + shs + hsg + scl + clg + occ2 + ind2 + mw + so + we)^2 # model for D

# partialling-out the linear effect of W from Y
t_Y <- rlasso(extraflex_y, data=data)$res
# partialling-out the linear effect of W from D
t_D <- rlasso(extraflex_d, data=data)$res

# regression of Y on D after partialling-out the effect of W
partial_lasso_fit <- lm(t_Y~t_D)
partial_lasso_est <- summary(partial_lasso_fit)$coef[2,1]

cat("Coefficient for D via partialling-out using lasso", partial_lasso_est)

# standard error
HCV_coefs <- vcovHC(partial_lasso_fit, type = 'HC')
partial_lasso_se <- sqrt(diag(HCV_coefs))[2]

table<- matrix(0, 2, 2)
table[1,1]<- control_est
table[1,2]<- control_se    
table[2,1]<-  partial_lasso_est
table[2,2]<- partial_lasso_se 
colnames(table)<- c("Estimate","Std. Error")
rownames(table)<- c("full reg","partial reg via lasso")	
table
