install.packages("librarian", quiet = T)
librarian::shelf(tidyverse, broom, hdm, quiet = T)
data <- read_csv(
    "https://github.com/d2cml-ai/14.388_R/raw/main/Data/wage2015_subsample_inference.csv"
    , show_col_types = F) |> 
        rename(socl = scl, sohs = shs, sout = so)
dim(data)

glimpse(data)

# construct matrices for estimation from the data 

Y <- log(data$wage)
n <- length(Y)
Z <- data |> select(!c(wage, lwage)) #data[-which(colnames(data) %in% c("wage","lwage"))]
p <- dim(Z)[2]

cat(
  "Number of observations:", n, '\n'
   , "Number of raw regressors:", p
  )


# generate a table of means of variables 
variables <- c("lwage","sex","sohs","hsg","socl","clg","ad","mw","sout","we","ne","exp1")
data |> 
  select(all_of(variables)) |>
  relocate(all_of(variables)) |>
  pivot_longer(everything()) |>
  with_groups(name, summarise, "Sample mean" = mean(value)) |>
  mutate(
    Variable = c("Log Wage","Sex","Some High School","High School Graduate","Some College","College Graduate", "Advanced Degree","Midwest","South","West","Northeast","Experience") |> str_sort()
    , across(where(is.numeric), round, 2)
    )


# 1. basic model
basic <- lwage ~ (sex + exp1 + sohs + hsg+ socl + clg + mw + sout + we + occ2 + ind2)
regbasic <- lm(basic, data=data) # perform ols using the defined model
tidy(regbasic)
cat("\nNumber of regressors in the basic model:",length(regbasic$coef), '\n') # number of regressors in the Basic Model


# 2. flexible model
flex <- lwage ~ sex + sohs + hsg+ socl+ clg+ mw + sout + we + occ2 + ind2 + 
  (exp1 + exp2 + exp3 + exp4)*(sohs + hsg + socl + clg + occ2 + ind2 + mw + sout + we)
regflex <- lm(flex, data=data)
tidy(regflex)
cat("\n Number of regressors in the flexible model:",length(regflex$coef)) # number of regressors in the Flexible Model


# Flexible model using Lasso
# library(hdm)
lassoreg<- rlasso(flex, data=data) 
sumlasso<- summary(lassoreg)

# Assess predictive performance
sumbasic <- summary(regbasic)
sumflex <- summary(regflex)

# R-squared and adjusted R-squared
R2_1 <- sumbasic$r.squared
R2_adj1 <- sumbasic$adj.r.squared
cat(
    "R-squared for the basic model:\t", R2_1
    , "\nAdjusted R-squared for the basic model:\t", R2_adj1
    )

R2_2 <- sumflex$r.squared
R2_adj2 <- sumflex$adj.r.squared
cat(
    "R-squared for the flexible model:\t", R2_2
    , "\nAdjusted R-squared for the flexible model:\t", R2_adj2
    )

R2_3 <- sumlasso$r.squared
R2_adj3 <- sumlasso$adj.r.squared
cat(
    "\nR-squared for the lasso with flexible model:\t", R2_3
    , "\nAdjusted R-squared for the flexible model:\t", R2_adj3
    )

# MSE and adjusted MSE
MSE1 <- mean(sumbasic$res^2)
p1 <- sumbasic$df[1] # number of regressors
MSE_adj1 <- (n / (n - p1)) * MSE1

cat(
    "\nMSE for the basic model: ", MSE1
    , "\nAdjusted MSE for the basic model: ", MSE_adj1
    )

MSE2 <-mean(sumflex$res^2)
p2 <- sumflex$df[1]
MSE_adj2 <- (n / (n - p2)) * MSE2

cat(
    "\nMSE for the flexible model: ", MSE2
    , "\nAdjusted MSE for the lasso flexible model: ", MSE_adj2
)

MSEL <-mean(sumlasso$res^2)
pL <- length(sumlasso$coef)
MSE_adj3 <- (n / (n - pL)) * MSEL
cat(
    "\nMSE for the lasso flexible model: ", MSEL
    , "\nAdjusted MSE for the lasso flexible model: ", MSE_adj3
)

# Output the table
table <- matrix(0, 3, 5)
table[1,1:5]   <- c(p1,R2_1,MSE1,R2_adj1,MSE_adj1)
table[2,1:5]   <- c(p2,R2_2,MSE2,R2_adj2,MSE_adj2)
table[3,1:5]   <- c(pL,R2_3,MSEL,R2_adj3,MSE_adj3)
colnames(table)<- c("p","R^2_sample","MSE_sample","R^2_adjusted", "MSE_adjusted")
rownames(table)<- c("Basic reg","Flexible reg", "Lasso flex")
table |> as.data.frame() |> rownames_to_column("Model")

# splitting the data
set.seed(1) # to make the results replicable (we will generate random numbers)
random <- sample(1:n, floor(n*4/5))
# draw (4/5)*n random numbers from 1 to n without replacing them
train <- data[random,] # training sample
test <- data[-random,] # testing sample

# basic model
# estimating the parameters in the training sample
regbasic <- lm(basic, data=train)

# calculating the out-of-sample MSE
trainregbasic <- predict(regbasic, newdata=test)
y_test <- log(test$wage)
MSE_test1 <- sum((y_test - trainregbasic)^2) / length(y_test)
R2_test1 <- 1 - MSE_test1 / var(y_test)

cat(
  "Test MSE for the basic model: ", MSE_test1
  , "\nTest R2 for the basic model: ", R2_test1
  )



# flexible model
# estimating the parameters
options(warn=-1) # ignore warnings 
regflex <- lm(flex, data=train)

# calculating the out-of-sample MSE
trainregflex<- predict(regflex, newdata=test)
y_test <- log(test$wage)
MSE_test2 <- sum((y_test - trainregflex)^2) / length(y_test)
R2_test2 <- 1- MSE_test2/var(y_test)

cat(
  "Test MSE for the flexible model: ", MSE_test2
  , "\nTest R2 for the flexible model: ", R2_test2
  )

# flexible model using lasso
library(hdm) # a library for high-dimensional metrics
reglasso <- rlasso(flex, data=train, post=FALSE) # estimating the parameters

# calculating the out-of-sample MSE
trainreglasso<- predict(reglasso, newdata=test)
MSE_lasso <- sum((y_test - trainreglasso)^2) / length(y_test)
R2_lasso<- 1 - MSE_lasso / var(y_test)

cat(
  "Test MSE for the lasso on flexible model: ", MSE_lasso, 
  "\nTest R2 for the lasso flexible model: ", R2_lasso
  )

# Output the comparison table

tibble(
  "Model" = c("Basic reg", "Flexible reg", "Lasso Regression")
  , "MSE test" = c(MSE_test1, MSE_test2, MSE_lasso)
  , "R2_test" = c(R2_test1, R2_test2, R2_lasso)
)

