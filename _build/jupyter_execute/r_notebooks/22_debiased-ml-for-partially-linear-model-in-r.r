install.packages("librarian")
librarian::shelf(
  AER, randomForest, hdm, glmnet
)


DML2.for.PLM <- function(x, d, y, dreg, yreg, nfold=2) {
  nobs <- nrow(x) #number of observations
  foldid <- rep.int(1:nfold,times = ceiling(nobs/nfold))[sample.int(nobs)] #define folds indices
  I <- split(1:nobs, foldid)  #split observation indices into folds  
  ytil <- dtil <- rep(NA, nobs)
  cat("fold: ")
  for(b in 1:length(I)){
    dfit <- dreg(x[-I[[b]],], d[-I[[b]]]) #take a fold out
    yfit <- yreg(x[-I[[b]],], y[-I[[b]]]) # take a foldt out
    dhat <- predict(dfit, x[I[[b]],], type="response") #predict the left-out fold 
    yhat <- predict(yfit, x[I[[b]],], type="response") #predict the left-out fold 
    dtil[I[[b]]] <- (d[I[[b]]] - dhat) #record residual for the left-out fold
    ytil[I[[b]]] <- (y[I[[b]]] - yhat) #record residial for the left-out fold
    cat(b," ")
        }
  rfit <- lm(ytil ~ dtil)    #estimate the main parameter by regressing one residual on the other
  coef.est <- coef(rfit)[2]  #extract coefficient
  se <- sqrt(vcovHC(rfit)[2,2]) #record robust standard error
  cat(sprintf("\ncoef (se) = %g (%g)\n", coef.est , se))  #printing output
  return( list(coef.est =coef.est , se=se, dtil=dtil, ytil=ytil) ) #save output and residuals 
}


# library(AER)  #applied econometrics library
# library(randomForest)  #random Forest library
# library(hdm) #high-dimensional econometrics library
# library(glmnet) #glm net

data(GrowthData)                     # Barro-Lee growth data
y= as.matrix(GrowthData[,1])         # outcome: growth rate
d= as.matrix(GrowthData[,3])         # treatment: initial wealth
x= as.matrix(GrowthData[,-c(1,2,3)]) # controls: country characteristics

cat(sprintf("\n length of y is %g \n", length(y) ))
cat(sprintf("\n num features x is %g \n", dim(x)[2] ))


#summary(y)
#summary(d)
#summary(x)

cat(sprintf("\n Naive OLS that uses all features w/o cross-fitting \n"))
lres=summary(lm(y~d +x))$coef[2,1:2]

cat(sprintf("\ncoef (se) = %g (%g)\n", lres[1] , lres[2]))



#DML with OLS


cat(sprintf("\n DML with OLS w/o feature selection \n"))

set.seed(1)
dreg <- function(x,d){ glmnet(x, d, lambda = 0) } #ML method= OLS using glmnet; using lm gives bugs
yreg <- function(x,y){ glmnet(x, y, lambda = 0) } #ML method = OLS

DML2.OLS = DML2.for.PLM(x, d, y, dreg, yreg, nfold=10)


#DML with Lasso:

cat(sprintf("\n DML with Lasso \n"))

set.seed(1)
dreg <- function(x,d){ rlasso(x,d, post=FALSE) } #ML method= lasso from hdm 
yreg <- function(x,y){ rlasso(x,y, post=FALSE) } #ML method = lasso from hdm
DML2.lasso = DML2.for.PLM(x, d, y, dreg, yreg, nfold=10)

cat(sprintf("\n DML with Random Forest \n"))

#DML with Random Forest:
dreg <- function(x,d){ randomForest(x, d) } #ML method=Forest 
yreg <- function(x,y){ randomForest(x, y) } #ML method=Forest
set.seed(1)
DML2.RF = DML2.for.PLM(x, d, y, dreg, yreg, nfold=10)


cat(sprintf("\n DML with Lasso/Random Forest \n"))

#DML MIX:
dreg <- function(x,d){ rlasso(x,d, post=FALSE) } #ML method=Forest 
yreg <- function(x,y){ randomForest(x, y) } #ML method=Forest
set.seed(1)
DML2.RF = DML2.for.PLM(x, d, y, dreg, yreg, nfold=10)


prRes.D<- c( mean((DML2.OLS$dtil)^2), mean((DML2.lasso$dtil)^2), mean((DML2.RF$dtil)^2));
prRes.Y<- c(mean((DML2.OLS$ytil)^2), mean((DML2.lasso$ytil)^2),mean((DML2.RF$ytil)^2));
prRes<- rbind(sqrt(prRes.D), sqrt(prRes.Y)); 
rownames(prRes)<- c("RMSE D", "RMSE Y");
colnames(prRes)<- c("OLS", "Lasso", "RF")
print(prRes,digit=2)

