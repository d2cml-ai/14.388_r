install.packages("librarian")
librarian::shelf(
  hdm, ggplot2, 
  , DoubleML
  , mlr3learners
  , mlr3
  , data.table
  , randomForest
  , ranger
  , xgboost
)

library(hdm) 
library(ggplot2)
data(pension)
data <- pension 
dim(data)

# save(data, file = "../data/pension.RData")

# help(pension)

hist_e401 = ggplot(data, aes(x = e401, fill = factor(e401))) + geom_bar()
hist_e401

dens_net_tfa = ggplot(data, aes(x = net_tfa, color = factor(e401), fill = factor(e401)) ) + 
                    geom_density() + xlim(c(-20000, 150000)) + 
                    facet_wrap(.~e401) 
                    
dens_net_tfa

e1 <- data[data$e401==1,]
e0 <- data[data$e401==0,]
round(mean(e1$net_tfa)-mean(e0$net_tfa),0)

p1 <- data[data$p401==1,]
p0 <- data[data$p401==0,]
round(mean(p1$net_tfa)-mean(p0$net_tfa),0)

# installing Double ML
#remotes::install_github("DoubleML/doubleml-for-r",quiet=TRUE)


# loading the packages
library(DoubleML)
library(mlr3learners)
library(mlr3)
library(data.table)
library(randomForest)


# Constructing the data (as DoubleMLData)
formula_flex = "net_tfa ~ e401 + poly(age, 6, raw=TRUE) + poly(inc, 8, raw=TRUE) + poly(educ, 4, raw=TRUE) + poly(fsize, 2, raw=TRUE) + marr + twoearn + db + pira + hown"
model_flex = as.data.table(model.frame(formula_flex, pension))
x_cols = colnames(model_flex)[-c(1,2)]
data_ml = DoubleMLData$new(model_flex, y_col = "net_tfa", d_cols = "e401", x_cols=x_cols)


p <- dim(model_flex)[2]-2
p

# complex model with two-way interactions
data_interactions = fetch_401k(polynomial_features = TRUE, instrument = FALSE)


# Constructing the data (as DoubleMLData)
formula_flex = "net_tfa ~ e401 + poly(age, 6, raw=TRUE) + poly(inc, 6, raw=TRUE) + poly(educ, 4, raw=TRUE) + poly(fsize, 2, raw=TRUE) + marr + twoearn + db + pira + hown"
model_flex = as.data.table(model.frame(formula_flex, pension))
x_cols = colnames(model_flex)[-c(1,2)]
data_ml_aux = DoubleMLData$new(model_flex, y_col = "net_tfa", d_cols = "e401", x_cols=x_cols)


# Estimating the PLR
lgr::get_logger("mlr3")$set_threshold("warn") 
set.seed(123)
lasso <- lrn("regr.cv_glmnet",nfolds = 5, s = "lambda.min")
lasso_class <- lrn("classif.cv_glmnet", nfolds = 5, s = "lambda.min")

dml_plr <- DoubleMLPLR$new(data_ml_aux, ml_g = lasso, ml_m = lasso_class, n_folds=3)
dml_plr$fit(store_predictions=TRUE)
dml_plr$summary()
lasso_plr <- dml_plr$coef
lasso_std_plr <- dml_plr$se

dml_plr$params_names()
g_hat <- as.matrix(dml_plr$predictions$ml_l) # predictions of g_o
m_hat <- as.matrix(dml_plr$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
y <- as.matrix(pension$net_tfa) # true observations
theta <- as.numeric(dml_plr$coef) # estimated regression coefficient
d <- as.matrix(pension$e401) 
predictions_y <- as.matrix(d*theta)+g_hat # predictions for y
lasso_y_rmse <- sqrt(mean((y-predictions_y)^2)) 
lasso_y_rmse

# cross-fitted RMSE: treatment
d <- as.matrix(pension$e401) 
lasso_d_rmse <- sqrt(mean((d-m_hat)^2)) 
lasso_d_rmse

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)

# install.packages( "regr.ranger" )
# install.packages( "classif.ranger" )

# Random Forest
lgr::get_logger("mlr3")$set_threshold("warn") 
randomForest <- lrn("regr.ranger")
randomForest_class <- lrn("classif.ranger")

dml_plr <- DoubleMLPLR$new(data_ml_aux, ml_g = randomForest, ml_m = randomForest_class, n_folds=3)
dml_plr$fit(store_predictions=TRUE) # set store_predictions=TRUE to evaluate the model
dml_plr$summary()
forest_plr <- dml_plr$coef
forest_std_plr <- dml_plr$se

# # Random Forest
# lgr::get_logger("mlr3")$set_threshold("warn") 
# randomForest <- lrn("regr.ranger")
# randomForest_class <- lrn("classif.ranger")

# dml_plr <- DoubleMLPLR$new(data_ml, ml_g = randomForest, ml_m = randomForest_class, n_folds=3)
# dml_plr$fit(store_predictions=TRUE) # set store_predictions=TRUE to evaluate the model
# dml_plr$summary()
# forest_plr <- dml_plr$coef
# forest_std_plr <- dml_plr$se

# Evaluation predictions
g_hat <- as.matrix(dml_plr$predictions$ml_l) # predictions of g_o
m_hat <- as.matrix(dml_plr$predictions$ml_m) # predictions of m_o
theta <- as.numeric(dml_plr$coef) # estimated regression coefficient
predictions_y <- as.matrix(d*theta)+g_hat # predictions for y
forest_y_rmse <- sqrt(mean((y-predictions_y)^2)) 
forest_y_rmse

# cross-fitted RMSE: treatment
forest_d_rmse <- sqrt(mean((d-m_hat)^2)) 
forest_d_rmse

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)

library(rpart)

# Trees
lgr::get_logger("mlr3")$set_threshold("warn") 

trees <- lrn("regr.rpart")
trees_class <- lrn("classif.rpart")

dml_plr <- DoubleMLPLR$new(data_ml_aux, ml_l = trees, ml_m = trees_class, n_folds=3)
dml_plr$fit(store_predictions=TRUE)
dml_plr$summary()
tree_plr <- dml_plr$coef
tree_std_plr <- dml_plr$se

# Evaluation predictions
g_hat <- as.matrix(dml_plr$predictions$ml_l) # predictions of g_o
m_hat <- as.matrix(dml_plr$predictions$ml_m) # predictions of m_o
theta <- as.numeric(dml_plr$coef) # estimated regression coefficient
predictions_y <- as.matrix(d*theta)+g_hat # predictions for y
tree_y_rmse <- sqrt(mean((y-predictions_y)^2)) 
tree_y_rmse

# cross-fitted RMSE: treatment
tree_d_rmse <- sqrt(mean((d-m_hat)^2)) 
tree_d_rmse

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)

# Boosting
lgr::get_logger("mlr3")$set_threshold("warn") 
boost<- lrn("regr.xgboost",objective="reg:squarederror")
boost_class <- lrn("classif.xgboost",objective = "binary:logistic",eval_metric ="logloss")

dml_plr <- DoubleMLPLR$new(data_ml_aux, ml_l = boost, ml_m = boost_class, n_folds=3)
dml_plr$fit(store_predictions=TRUE)
dml_plr$summary()
boost_plr <- dml_plr$coef
boost_std_plr <- dml_plr$se

# Evaluation predictions
g_hat <- as.matrix(dml_plr$predictions$ml_l) # predictions of g_o
m_hat <- as.matrix(dml_plr$predictions$ml_m) # predictions of m_o
theta <- as.numeric(dml_plr$coef) # estimated regression coefficient
predictions_y <- as.matrix(d*theta)+g_hat # predictions for y
boost_y_rmse <- sqrt(mean((y-predictions_y)^2)) 
boost_y_rmse

# cross-fitted RMSE: treatment
boost_d_rmse <- sqrt(mean((d-m_hat)^2)) 
boost_d_rmse

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)

table <- matrix(0, 4, 4)
table[1,1:4]   <- c(lasso_plr,forest_plr,tree_plr,boost_plr)
table[2,1:4]   <- c(lasso_std_plr,forest_std_plr,tree_std_plr,boost_std_plr)
table[3,1:4]   <- c(lasso_y_rmse,forest_y_rmse,tree_y_rmse,boost_y_rmse)
table[4,1:4]   <- c(lasso_d_rmse,forest_d_rmse,tree_d_rmse,boost_d_rmse)
rownames(table) <- c("Estimate","Std.Error","RMSE Y","RMSE D")
colnames(table) <- c("Lasso","Random Forest","Trees","Boosting")
table

set.seed(123)
lgr::get_logger("mlr3")$set_threshold("warn") 
dml_irm = DoubleMLIRM$new(data_ml_aux, ml_g = lasso, 
                          ml_m = lasso_class, 
                          trimming_threshold = 0.01, n_folds=3)
dml_irm$fit(store_predictions=TRUE)
dml_irm$summary()
lasso_irm <- dml_irm$coef
lasso_std_irm <- dml_irm$se


# predictions
dml_irm$params_names()
g0_hat <- as.matrix(dml_irm$predictions$ml_g0) # predictions of g_0(D=0, X)
g1_hat <- as.matrix(dml_irm$predictions$ml_g1) # predictions of g_0(D=1, X)
g_hat <- d*g1_hat+(1-d)*g0_hat # predictions of g_0
m_hat <- as.matrix(dml_irm$predictions$ml_m) # predictions of m_o


# cross-fitted RMSE: outcome
y <- as.matrix(pension$net_tfa) # true observations
d <- as.matrix(pension$e401) 
lasso_y_irm <- sqrt(mean((y-g_hat)^2)) 
lasso_y_irm

# cross-fitted RMSE: treatment
lasso_d_irm <- sqrt(mean((d-m_hat)^2)) 
lasso_d_irm

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)

##### forest #####

dml_irm = DoubleMLIRM$new(data_ml_aux, ml_g = randomForest, 
                          ml_m = randomForest_class, 
                          trimming_threshold = 0.01, n_folds=3)
dml_irm$fit(store_predictions=TRUE)
dml_irm$summary()
forest_irm <- dml_irm$coef
forest_std_irm <- dml_plr$se

# predictions
g0_hat <- as.matrix(dml_irm$predictions$ml_g0) # predictions of g_0(D=0, X)
g1_hat <- as.matrix(dml_irm$predictions$ml_g1) # predictions of g_0(D=1, X)
g_hat <- d*g1_hat+(1-d)*g0_hat # predictions of g_0
m_hat <- as.matrix(dml_irm$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
y <- as.matrix(pension$net_tfa) # true observations
d <- as.matrix(pension$e401) 
forest_y_irm <- sqrt(mean((y-g_hat)^2)) 
forest_y_irm

# cross-fitted RMSE: treatment
forest_d_irm <- sqrt(mean((d-m_hat)^2)) 
forest_d_irm

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)



##### trees #####

dml_irm <- DoubleMLIRM$new(data_ml_aux, ml_g = trees, ml_m = trees_class, 
                           trimming_threshold = 0.01, n_folds=3)
dml_irm$fit(store_predictions=TRUE)
dml_irm$summary()
tree_irm <- dml_irm$coef
tree_std_irm <- dml_irm$se

# predictions
g0_hat <- as.matrix(dml_irm$predictions$ml_g0) # predictions of g_0(D=0, X)
g1_hat <- as.matrix(dml_irm$predictions$ml_g1) # predictions of g_0(D=1, X)
g_hat <- d*g1_hat+(1-d)*g0_hat # predictions of g_0
m_hat <- as.matrix(dml_irm$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
y <- as.matrix(pension$net_tfa) # true observations
d <- as.matrix(pension$e401) 
tree_y_irm <- sqrt(mean((y-g_hat)^2)) 
tree_y_irm

# cross-fitted RMSE: treatment
tree_d_irm <- sqrt(mean((d-m_hat)^2)) 
tree_d_irm

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)

##### boosting #####

dml_irm <- DoubleMLIRM$new(data_ml_aux, ml_g = boost, ml_m = boost_class,
                           trimming_threshold = 0.01, n_folds=3)
dml_irm$fit(store_predictions=TRUE)
dml_irm$summary()
boost_irm <- dml_irm$coef
boost_std_irm <- dml_irm$se

# predictions
g0_hat <- as.matrix(dml_irm$predictions$ml_g0) # predictions of g_0(D=0, X)
g1_hat <- as.matrix(dml_irm$predictions$ml_g1) # predictions of g_0(D=1, X)
g_hat <- d*g1_hat+(1-d)*g0_hat # predictions of g_0
m_hat <- as.matrix(dml_irm$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
y <- as.matrix(pension$net_tfa) # true observations
d <- as.matrix(pension$e401) 
boost_y_irm <- sqrt(mean((y-g_hat)^2)) 
boost_y_irm

# cross-fitted RMSE: treatment
boost_d_irm <- sqrt(mean((d-m_hat)^2)) 
boost_d_irm

# cross-fitted ce: treatment
mean(ifelse(m_hat > 0.5, 1, 0) != d)

table <- matrix(0, 4, 4)
table[1,1:4]   <- c(lasso_irm,forest_irm,tree_irm,boost_irm)
table[2,1:4]   <- c(lasso_std_irm,forest_std_irm,tree_std_irm,boost_std_irm)
table[3,1:4]   <- c(lasso_y_irm,forest_y_irm,tree_y_irm,boost_y_irm)
table[4,1:4]   <- c(lasso_d_irm,forest_d_irm,tree_d_irm,boost_d_irm)
rownames(table) <- c("Estimate","Std.Error","RMSE Y","RMSE D")
colnames(table) <- c("Lasso","Random Forest","Trees","Boosting")
table

set.seed(123)
lgr::get_logger("mlr3")$set_threshold("warn") 
dml_irm = DoubleMLIRM$new(data_ml_aux, ml_g = randomForest, 
                          ml_m = lasso_class, 
                          trimming_threshold = 0.01, n_folds=3)
dml_irm$fit(store_predictions=TRUE)
dml_irm$summary()
best_irm <- dml_irm$coef
best_std_irm <- dml_irm$se

# # Constructing the data (as DoubleMLData)
# formula_flex2 = "net_tfa ~ p401+ e401 + poly(age, 6, raw=TRUE) + poly(inc, 8, raw=TRUE) + poly(educ, 4, raw=TRUE) + poly(fsize, 2, raw=TRUE) + marr + twoearn + db + pira + hown"
# model_flex2 = as.data.table(model.frame(formula_flex2, data))
# x_cols = colnames(model_flex2)[-c(1,2,3)]
# data_IV = DoubleMLData$new(model_flex2, y_col = "net_tfa", d_cols = "p401", z_cols ="e401",x_cols=x_cols)

# Constructing the data (as DoubleMLData)
formula_flex2 = "net_tfa ~ p401+ e401 + poly(age, 6, raw=TRUE) + poly(inc, 6, raw=TRUE) + poly(educ, 4, raw=TRUE) + poly(fsize, 2, raw=TRUE) + marr + twoearn + db + pira + hown"
model_flex2 = as.data.table(model.frame(formula_flex2, data))
x_cols = colnames(model_flex2)[-c(1,2,3)]
data_IV_aux = DoubleMLData$new(model_flex2, y_col = "net_tfa", d_cols = "p401", z_cols ="e401",x_cols=x_cols)

set.seed(123)
lgr::get_logger("mlr3")$set_threshold("warn") 
dml_MLIIVM = DoubleMLIIVM$new(data_IV_aux, ml_g = lasso, 
                       ml_m = lasso_class, ml_r = lasso_class,n_folds=3, subgroups = list(always_takers = FALSE, 
                                         never_takers = TRUE))
dml_MLIIVM$fit(store_predictions=TRUE)
dml_MLIIVM$summary()
lasso_MLIIVM <- dml_MLIIVM$coef
lasso_std_MLIIVM <- dml_MLIIVM$se

dml_MLIIVM$confint(level = 0.95)

# variables
y <- as.matrix(pension$net_tfa) # true observations
d <- as.matrix(pension$p401) 
z <- as.matrix(pension$e401) 

# predictions
dml_MLIIVM$params_names()
g0_hat <- as.matrix(dml_MLIIVM$predictions$ml_g0) # predictions of g_0(z=0, X)
g1_hat <- as.matrix(dml_MLIIVM$predictions$ml_g1) # predictions of g_0(z=1, X)
g_hat <- z*g1_hat+(1-z)*g0_hat # predictions of g_0
r0_hat <- as.matrix(dml_MLIIVM$predictions$ml_r0) # predictions of r_0(z=0, X)
r1_hat <- as.matrix(dml_MLIIVM$predictions$ml_r1) # predictions of r_0(z=1, X)
r_hat <- z*r1_hat+(1-z)*r0_hat # predictions of r_0
m_hat <- as.matrix(dml_MLIIVM$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
lasso_y_MLIIVM <- sqrt(mean((y-g_hat)^2)) 
lasso_y_MLIIVM

# cross-fitted RMSE: treatment
lasso_d_MLIIVM <- sqrt(mean((d-r_hat)^2)) 
lasso_d_MLIIVM

# cross-fitted RMSE: instrument
lasso_z_MLIIVM <- sqrt(mean((z-m_hat)^2)) 
lasso_z_MLIIVM


### random forest ###

set.seed(123)
lgr::get_logger("mlr3")$set_threshold("warn") 
dml_MLIIVM = DoubleMLIIVM$new(data_IV_aux, ml_g = randomForest, 
                       ml_m = randomForest_class, ml_r = randomForest_class,n_folds=3, subgroups = list(always_takers = FALSE, 
                                         never_takers = TRUE))
dml_MLIIVM$fit(store_predictions=TRUE)
dml_MLIIVM$summary()
forest_MLIIVM <- dml_MLIIVM$coef
forest_std_MLIIVM <- dml_MLIIVM$se

# predictions
g0_hat <- as.matrix(dml_MLIIVM$predictions$ml_g0) # predictions of g_0(Z=0, X)
g1_hat <- as.matrix(dml_MLIIVM$predictions$ml_g1) # predictions of g_0(Z=1, X)
g_hat <- z*g1_hat+(1-z)*g0_hat # predictions of g_0
r0_hat <- as.matrix(dml_MLIIVM$predictions$ml_r0) # predictions of r_0(Z=0, X)
r1_hat <- as.matrix(dml_MLIIVM$predictions$ml_r1) # predictions of r_0(Z=1, X)
r_hat <- z*r1_hat+(1-z)*r0_hat # predictions of r_0
m_hat <- as.matrix(dml_MLIIVM$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
forest_y_MLIIVM <- sqrt(mean((y-g_hat)^2)) 
forest_y_MLIIVM

# cross-fitted RMSE: treatment
forest_d_MLIIVM <- sqrt(mean((d-r_hat)^2)) 
forest_d_MLIIVM

# cross-fitted RMSE: instrument
forest_z_MLIIVM <- sqrt(mean((z-m_hat)^2)) 
forest_z_MLIIVM


### trees ###

dml_MLIIVM = DoubleMLIIVM$new(data_IV_aux, ml_g = trees, 
                       ml_m = trees_class, ml_r = trees_class,n_folds=3, subgroups = list(always_takers = FALSE, 
                                         never_takers = TRUE))
dml_MLIIVM$fit(store_predictions=TRUE)
dml_MLIIVM$summary()
tree_MLIIVM <- dml_MLIIVM$coef
tree_std_MLIIVM <- dml_MLIIVM$se

# predictions
g0_hat <- as.matrix(dml_MLIIVM$predictions$ml_g0) # predictions of g_0(Z=0, X)
g1_hat <- as.matrix(dml_MLIIVM$predictions$ml_g1) # predictions of g_0(Z=1, X)
g_hat <- z*g1_hat+(1-z)*g0_hat # predictions of g_0
r0_hat <- as.matrix(dml_MLIIVM$predictions$ml_r0) # predictions of r_0(Z=0, X)
r1_hat <- as.matrix(dml_MLIIVM$predictions$ml_r1) # predictions of r_0(Z=1, X)
r_hat <- z*r1_hat+(1-z)*r0_hat # predictions of r_0
m_hat <- as.matrix(dml_MLIIVM$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
tree_y_MLIIVM <- sqrt(mean((y-g_hat)^2)) 
tree_y_MLIIVM

# cross-fitted RMSE: treatment
tree_d_MLIIVM <- sqrt(mean((d-r_hat)^2)) 
tree_d_MLIIVM

# cross-fitted RMSE: instrument
tree_z_MLIIVM <- sqrt(mean((z-m_hat)^2)) 
tree_z_MLIIVM




### boosting ###
dml_MLIIVM = DoubleMLIIVM$new(data_IV_aux, ml_g = boost, 
                       ml_m = boost_class, ml_r = boost_class,n_folds=3, subgroups = list(always_takers = FALSE, 
                                         never_takers = TRUE))
dml_MLIIVM$fit(store_predictions=TRUE)
dml_MLIIVM$summary()
boost_MLIIVM <- dml_MLIIVM$coef
boost_std_MLIIVM <- dml_MLIIVM$se

# predictions
g0_hat <- as.matrix(dml_MLIIVM$predictions$ml_g0) # predictions of g_0(Z=0, X)
g1_hat <- as.matrix(dml_MLIIVM$predictions$ml_g1) # predictions of g_0(Z=1, X)
g_hat <- z*g1_hat+(1-z)*g0_hat # predictions of g_0
r0_hat <- as.matrix(dml_MLIIVM$predictions$ml_r0) # predictions of r_0(Z=0, X)
r1_hat <- as.matrix(dml_MLIIVM$predictions$ml_r1) # predictions of r_0(Z=1, X)
r_hat <- z*r1_hat+(1-z)*r0_hat # predictions of r_0
m_hat <- as.matrix(dml_MLIIVM$predictions$ml_m) # predictions of m_o

# cross-fitted RMSE: outcome
boost_y_MLIIVM <- sqrt(mean((y-g_hat)^2)) 
boost_y_MLIIVM

# cross-fitted RMSE: treatment
boost_d_MLIIVM <- sqrt(mean((d-r_hat)^2)) 
boost_d_MLIIVM

# cross-fitted RMSE: instrument
boost_z_MLIIVM <- sqrt(mean((z-m_hat)^2)) 
boost_z_MLIIVM

# library(xtable)
table <- matrix(0, 5, 4)
table[1,1:4]   <- c(lasso_MLIIVM,forest_MLIIVM,tree_MLIIVM,boost_MLIIVM)
table[2,1:4]   <- c(lasso_std_MLIIVM,forest_std_MLIIVM,tree_std_MLIIVM,boost_std_MLIIVM)
table[3,1:4]   <- c(lasso_y_MLIIVM,forest_y_MLIIVM,tree_y_MLIIVM,boost_y_MLIIVM)
table[4,1:4]   <- c(lasso_d_MLIIVM,forest_d_MLIIVM,tree_d_MLIIVM,boost_d_MLIIVM)
table[5,1:4]   <- c(lasso_z_MLIIVM,forest_z_MLIIVM,tree_z_MLIIVM,boost_z_MLIIVM)
rownames(table) <- c("Estimate","Std.Error","RMSE Y","RMSE D","RMSE Z")
colnames(table) <- c("Lasso","Random Forest","Trees","Boosting")
# tab<- xtable(table, digits = 2)
table

set.seed(123)
lgr::get_logger("mlr3")$set_threshold("warn") 
dml_MLIIVM = DoubleMLIIVM$new(data_IV_aux, ml_g = randomForest, 
                       ml_m = lasso_class, ml_r = lasso_class,n_folds=3, subgroups = list(always_takers = FALSE, 
                                         never_takers = TRUE))
dml_MLIIVM$fit(store_predictions=TRUE)
dml_MLIIVM$summary()
best_MLIIVM <- dml_MLIIVM$coef
best_std_MLIIVM <- dml_MLIIVM$se
