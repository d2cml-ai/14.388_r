install.packages("librarian")
librarian::shelf(
  hdm, AER, randomForest, glmnet, carData, lfe
  , quiet = T
)



# library(AER)  #applied econometrics library
# library(randomForest)  #random Forest library
# library(hdm) #high-dimensional econometrics library
# library(glmnet) #glm net


# DML for PLIVM

DML2.for.PLIVM <- function(x, d, z, y, dreg, yreg, zreg, nfold=2) {
  # this implements DML2 algorithm, where there moments are estimated via DML, before constructing
  # the pooled estimate of theta randomly split data into folds
  nobs <- nrow(x)
  foldid <- rep.int(1:nfold,times = ceiling(nobs/nfold))[sample.int(nobs)]
  I <- split(1:nobs, foldid)
  # create residualized objects to fill
  ytil <- dtil <- ztil<- rep(NA, nobs)
  # obtain cross-fitted residuals
  cat("fold: ")
  for(b in 1:length(I)){
    dfit <- dreg(x[-I[[b]],], d[-I[[b]]])  #take a fold out
    zfit <- zreg(x[-I[[b]],], z[-I[[b]]])  #take a fold out
    yfit <- yreg(x[-I[[b]],], y[-I[[b]]])  # take a folot out
    dhat <- predict(dfit, x[I[[b]],], type="response")  #predict the fold out
    zhat <- predict(zfit, x[I[[b]],], type="response")  #predict the fold out
    yhat <- predict(yfit, x[I[[b]],], type="response")  #predict the fold out
    dtil[I[[b]]] <- (d[I[[b]]] - dhat) #record residual
    ztil[I[[b]]] <- (z[I[[b]]] - zhat) #record residual
    ytil[I[[b]]] <- (y[I[[b]]] - yhat) #record residial
    cat(b," ")
  }
  ivfit= tsls(y=ytil,d=dtil, x=NULL, z=ztil, intercept=FALSE)
  coef.est <-  ivfit$coef          #extract coefficient 
  se <-  ivfit$se                  #record standard error
  cat(sprintf("\ncoef (se) = %g (%g)\n", coef.est , se))
  return( list(coef.est =coef.est , se=se, dtil=dtil, ytil=ytil, ztil=ztil) )
}




data(AJR); 

y = AJR$GDP; 
d = AJR$Exprop; 
z = AJR$logMort
xraw= model.matrix(~ Latitude+ Africa+Asia + Namer + Samer, data=AJR)
x = model.matrix(~ -1 + (Latitude + Latitude2 + Africa + 
                           Asia + Namer + Samer)^2, data=AJR)
dim(x)

# DML with Random Forest
cat(sprintf("\n DML with Random Forest \n"))

dreg <- function(x,d){ randomForest(x, d) }  #ML method=Forest
yreg <- function(x,y){ randomForest(x, y) }  #ML method=Forest
zreg<-  function(x,z){ randomForest(x, z)}  #ML method=Forest 
  
set.seed(1)
DML2.RF = DML2.for.PLIVM(xraw, d, z, y, dreg, yreg, zreg, nfold=20)

# DML with PostLasso
cat(sprintf("\n DML with Post-Lasso \n"))

dreg <- function(x,d){ rlasso(x, d) }  #ML method=lasso
yreg <- function(x,y){ rlasso(x, y) }  #ML method=lasso
zreg<-  function(x,z){ rlasso(x, z)}  #ML method=lasso 

set.seed(1)
DML2.lasso = DML2.for.PLIVM(x, d, z, y, dreg, yreg, zreg, nfold=20)


# Compare Forest vs Lasso

comp.tab= matrix(NA, 3, 2)
comp.tab[1,] = c( sqrt(mean((DML2.RF$ytil)^2)),  sqrt(mean((DML2.lasso$ytil)^2)) )
comp.tab[2,] = c( sqrt(mean((DML2.RF$dtil)^2)),  sqrt(mean((DML2.lasso$dtil)^2)) )
comp.tab[3,] = c( sqrt(mean((DML2.RF$ztil)^2)),  sqrt(mean((DML2.lasso$ztil)^2)) )
rownames(comp.tab) = c("RMSE for Y:", "RMSE for D:", "RMSE for Z:")
colnames(comp.tab) = c("RF", "LASSO")
print(comp.tab, digits=3)

# install.packages("lfe")
# library(lfe)
summary(felm(DML2.lasso$dtil~DML2.lasso$ztil), robust=T)
summary(felm(DML2.RF$dtil~DML2.RF$ztil), robust=T)

# DML-AR (DML with Anderson-Rubin) 

DML.AR.PLIV<- function(rY, rD, rZ, grid, alpha=.05){
    n = length(rY)
    Cstat = rep(0, length(grid))
    for (i in 1:length(grid)) {
    Cstat[i] <-  n* (mean( (rY - grid[i]*rD)*rZ)  )^2/var ( (rY - grid[i]*rD) * rZ )
    };
    LB<- min(grid[ Cstat < qchisq(1-alpha,1)]);
    UB <- max(grid[ Cstat < qchisq(1-alpha,1)]); 
    plot(range(grid),range(c( Cstat)) , type="n",xlab="Effect of institutions", ylab="Statistic", main=" ");  
    lines(grid, Cstat,   lty = 1, col = 1);       
    abline(h=qchisq(1-alpha,1), lty = 3, col = 4);
    abline(v=LB, lty = 3, col = 2);
    abline(v=UB, lty = 3, col = 2);
    return(list(UB=UB, LB=LB))
    }


DML.AR.PLIV(rY = DML2.lasso$ytil, rD= DML2.lasso$dtil, rZ= DML2.lasso$ztil,
           grid = seq(-2, 2, by =.01))


DML.AR.PLIV(rY = DML2.RF$ytil, rD= DML2.RF$dtil, rZ= DML2.RF$ztil,
           grid = seq(-2, 2, by =.01))

