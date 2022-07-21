install.packages("librarian", quiet = T)
librarian::shelf(
  tidyverse, 
  lfe, 
  hdm,
  glmnet,
  randomForest,
  quiet = T
)
data <- read_csv("https://raw.githubusercontent.com/d2cml-ai/14.388_R/main/Data/gun_clean.csv", show_col_types = F) 
dim(data)[1]

##################### Find Variable Names from Dataset ######################

varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {
  vars <- character(0)
  if (any(type %in% "numeric")) {
    vars <- c(vars,names(df)[sapply(df,is.numeric)])
  }
  if (any(type %in% "factor")) {
    vars <- c(vars,names(df)[sapply(df,is.factor)])
  }  
  if (any(type %in% "character")) {
    vars <- c(vars,names(df)[sapply(df,is.character)])
  }  
  vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
}

############################# Create Variables ##############################

# dummy variables for year and county fixed effects
fixed  <- grep("X_Jfips", names(data), value=TRUE, fixed=TRUE)
year   <- varlist(data, pattern="X_Tyear")

# census control variables
census     <- NULL
census_var <- c("^AGE", "^BN", "^BP", "^BZ", "^ED", "^EL","^HI", "^HS", "^INC", "^LF", "^LN", "^PI", "^PO", "^PP", "^PV", "^SPR", "^VS")

for(i in 1:length(census_var)){
    census  <- append(census, varlist(data, pattern=census_var[i])) 
}

################################ Variables ##################################
# treatment variable
d     <- "logfssl"

# outcome variable
y     <- "logghomr"

# other control variables
X1    <- c("logrobr", "logburg", "burg_missing", "robrate_missing")
X2    <- c("newblack", "newfhh", "newmove", "newdens", "newmal")

######################## Partial out Fixed Effects ##########################

# new dataset for partialled-out variables
rdata    <- as.data.frame(data$CountyCode) 
colnames(rdata) <- "CountyCode"

# variables to partial out
varlist <- c(y, d,X1, X2, census)

# partial out year and county fixed effect from variables in varlist
for(i in 1:length(varlist)){
  form <- as.formula(paste(varlist[i], "~", paste(paste(year,collapse="+"),  paste(fixed,collapse="+"), sep="+")))
  rdata[, varlist[i]] <- lm(form, data)$residuals
}

# treatment variable
D     <- rdata[which(colnames(rdata) == d)]

# outcome variable
Y     <- rdata[which(colnames(rdata) == y)]

# construct matrix Z
Z <- rdata[which(colnames(rdata) %in% c(X1,X2,census))]
dim(Z)

clu <- rdata[which(colnames(rdata) == "CountyCode")] # for clustering the standard errors
data <- data.frame(cbind(Y, D, Z,as.matrix(clu)))

library(lfe) # linear group fixed effects package

# baseline_formula <- as.formula(paste(y, "~", d ))
# baseline.ols <- lm(baseline_formula,data=rdata)

baseline.ols <- felm(logghomr ~ logfssl |0|0| CountyCode,data=data) # ols with clustered standard errors
est_baseline <- summary(baseline.ols)$coef[2,]
confint(baseline.ols)[2,]
est_baseline

control_formula <- as.formula(paste("logghomr", "~", paste("logfssl",paste(colnames(Z),collapse="+"),
                                                           sep="+"),"|0|0| CountyCode"))
control.ols <- felm(control_formula,data=data) # fixed effects lm function
est_ols <- summary(control.ols)$coef[2,]
confint(control.ols)[2,]
est_ols

DML2.for.PLM <- function(z, d, y, dreg, yreg, nfold=2, clu) {
  nobs <- nrow(z) # number of observations
  foldid <- rep.int(1:nfold,times = ceiling(nobs/nfold))[sample.int(nobs)] # define folds indices
  I <- split(1:nobs, foldid)  # split observation indices into folds  
  ytil <- dtil <- rep(NA, nobs)
  cat("fold: ")
  for(b in 1:length(I)){
    dfit <- dreg(z[-I[[b]],], d[-I[[b]]]) # take a fold out
    yfit <- yreg(z[-I[[b]],], y[-I[[b]]]) # take a fold out
    dhat <- predict(dfit, z[I[[b]],], type="response") # predict the left-out fold 
    yhat <- predict(yfit, z[I[[b]],], type="response") # predict the left-out fold  
    dtil[I[[b]]] <- (d[I[[b]]] - dhat) # record residual for the left-out fold
    ytil[I[[b]]] <- (y[I[[b]]] - yhat) # record residial for the left-out fold
    cat(b," ")
        }
  #rfit <- lm(ytil ~ dtil) # estimate the main parameter by regressing one residual on the other
  data <- data.frame(cbind(ytil, dtil, as.matrix(clu)))
  rfit <- felm(ytil ~ dtil|0|0|CountyCode,data=data) 
  coef.est <- coef(rfit)[2] # extract coefficient
  #HC <- vcovHC(rfit)
  se    <- summary(rfit,robust=T)$coefficients[2,2] # record robust standard error by county
  cat(sprintf("\ncoef (se) = %g (%g)\n", coef.est , se)) # print output
  return( list(coef.est =coef.est , se=se, dtil=dtil, ytil=ytil, rfit=rfit) ) # save output and residuals 
}

y <- as.matrix(Y)
d <- as.matrix(D)
z <- as.matrix(Z)
clu <- rdata[which(colnames(rdata) == "CountyCode")]
head(data.frame(cbind(y,d,as.matrix(clu))))

# DML with Lasso:
set.seed(123)
dreg <- function(z,d){ rlasso(z,d, post=FALSE) } # ML method= lasso from hdm 
yreg <- function(z,y){ rlasso(z,y, post=FALSE) } # ML method = lasso from hdm
DML2.lasso = DML2.for.PLM(z, d, y, dreg, yreg, nfold=10,clu)

# DML with Post-Lasso:
dreg <- function(z,d){ rlasso(z,d, post=T) } # ML method= lasso from hdm 
yreg <- function(z,y){ rlasso(z,y, post=T) } # ML method = lasso from hdm
DML2.post = DML2.for.PLM(z, d, y, dreg, yreg, nfold=10, clu)

# DML with cross-validated Lasso:
dreg <- function(z,d){ cv.glmnet(z,d,family="gaussian", alpha=1) } # ML method = lasso from glmnet 
yreg <- function(z,y){ cv.glmnet(z,y,family="gaussian", alpha=1) } # ML method = lasso from glmnet 
DML2.lasso.cv = DML2.for.PLM(z, d, y, dreg, yreg, nfold=10, clu)

dreg <- function(z,d){ cv.glmnet(z,d,family="gaussian", alpha=0.5) } # ML method = elastic net from glmnet 
yreg <- function(z,y){ cv.glmnet(z,y,family="gaussian", alpha=0.5) } # ML method = elastic net from glmnet 
DML2.elnet = DML2.for.PLM(z, d, y, dreg, yreg, nfold=10, clu)

dreg <- function(z,d){ cv.glmnet(z,d,family="gaussian", alpha=0) } # ML method = ridge from glmnet 
yreg <- function(z,y){ cv.glmnet(z,y,family="gaussian", alpha=0) } # ML method = ridge from glmnet 
DML2.ridge = DML2.for.PLM(z, d, y, dreg, yreg, nfold=10, clu)

dreg <- function(z,d){  glmnet(z,d,family="gaussian", lambda=0) } # ML method = ols from glmnet 
yreg <- function(z,y){  glmnet(z,y,family="gaussian", lambda=0) }  # ML method = ols from glmnet 
DML2.ols = DML2.for.PLM(z, d, y, dreg, yreg, nfold=10, clu)

# DML with Random Forest:
dreg <- function(z,d){ randomForest(z, d) } # ML method = random forest 
yreg <- function(z,y){ randomForest(z, y) } # ML method = random forest
set.seed(1)
DML2.RF = DML2.for.PLM(z, d, y, dreg, yreg, nfold=2, clu) # set folds to 2 to limit computation time

mods<- list(DML2.ols, DML2.lasso, DML2.post, DML2.lasso.cv, DML2.ridge, DML2.elnet, DML2.RF)

RMSE.mdl<- function(mdl) {
RMSEY <- sqrt(mean(mdl$ytil)^2) 
RMSED <- sqrt(mean(mdl$dtil)^2) 
return( list(RMSEY=RMSEY, RMSED=RMSED))
}

#RMSE.mdl(DML2.lasso)
#DML2.lasso$ytil

Res<- lapply(mods, RMSE.mdl)

prRes.Y<- c( Res[[1]]$RMSEY,Res[[2]]$RMSEY, Res[[3]]$RMSEY, Res[[4]]$RMSEY, Res[[5]]$RMSEY,  Res[[6]]$RMSEY, Res[[7]]$RMSEY)
prRes.D<- c( Res[[1]]$RMSED,Res[[2]]$RMSED, Res[[3]]$RMSED, Res[[4]]$RMSED, Res[[5]]$RMSED, Res[[6]]$RMSED, Res[[7]]$RMSED)

prRes<- rbind(prRes.Y, prRes.D); 
rownames(prRes)<- c("RMSE D", "RMSE Y");
colnames(prRes)<- c("OLS", "Lasso", "Post-Lasso", "CV Lasso", "CV Ridge", "CV Elnet", "RF")
print(prRes,digit=6)

dreg <- function(z,d){ rlasso(z,d, post=T) } # ML method = lasso from hdm 
yreg <- function(z,y){ cv.glmnet(z,y,family="gaussian", alpha=0) }  # ML method = ridge from glmnet 
DML2.best= DML2.for.PLM(z, d, y, dreg, yreg, nfold=10, clu)

# library(xtable)

table <- matrix(0,9,2)
table[1,1] <- as.numeric(est_baseline[1])
table[2,1] <- as.numeric(est_ols[1])
table[3,1]   <- as.numeric(DML2.lasso$coef.est)
table[4,1]   <- as.numeric(DML2.post$coef.est)
table[5,1]  <-as.numeric(DML2.lasso.cv$coef.est)
table[6,1] <-as.numeric(DML2.elnet$coef.est)
table[7,1] <-as.numeric(DML2.ridge$coef.est)
table[8,1] <-as.numeric(DML2.RF$coef.est)
table[9,1] <-as.numeric(DML2.best$coef.est)
table[1,2] <- as.numeric(est_baseline[2])
table[2,2] <- as.numeric(est_ols[2])
table[3,2]   <- as.numeric(DML2.lasso$se)
table[4,2]   <- as.numeric(DML2.post$se)
table[5,2]  <-as.numeric(DML2.lasso.cv$se)
table[6,2] <-as.numeric(DML2.elnet$se)
table[7,2] <-as.numeric(DML2.ridge$se)
table[8,2] <-as.numeric(DML2.RF$se)
table[9,2] <-as.numeric(DML2.best$se)

# print results
colnames(table) <- c("Estimate","Standard Error")
rownames(table) <- c("Baseline OLS", "Least Squares with controls", "Lasso", "Post-Lasso", "CV Lasso","CV Elnet", "CV Ridge", "Random Forest", 
                     "Best")
table
