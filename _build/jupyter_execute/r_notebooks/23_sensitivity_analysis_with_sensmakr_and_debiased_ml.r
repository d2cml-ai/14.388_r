# loads package

install.packages("librarian")
librarian::shelf(
  sensemakr, hdm, lfe, randomForest, zoo
  , survival
)

# loads data
data("darfur")


#get rid of village fixed effects

attach(darfur)
# library(lfe)

peacefactorR<- lm(peacefactor~village)$res
directlyharmedR<-  lm(directlyharmed~village)$res
femaleR<-  lm(female~village)$res
ageR<-     lm(age~village)$res
farmerR<-  lm(farmer_dar~village)$res
herderR<-  lm(herder_dar~village)$res
pastvotedR<- lm(pastvoted~village)$res
hhsizeR<-   lm(hhsize_darfur~village)$res


# Preliminary linear model analysis

summary(felm(peacefactorR~ directlyharmedR+ femaleR +
                     ageR + farmerR+ herderR + pastvotedR + hhsizeR |0|0|village))

# here we are clustering standard errors at the village level


summary(felm(peacefactorR~ femaleR +
                     ageR + farmerR+ herderR + pastvotedR + hhsizeR |0|0|village))

# here we are clustering standard errors at the village level



summary(felm(directlyharmedR~ femaleR +
                     ageR + farmerR+ herderR + pastvotedR + hhsizeR |0|0|village))

# here we are clustering standard errors at the village level



# library(hdm)


resY =  rlasso(peacefactorR ~  (femaleR +
                     ageR + farmerR+ herderR + pastvotedR + hhsizeR)^3, post=F)$res

resD =  rlasso(directlyharmedR ~  (femaleR +
                     ageR + farmerR + herderR + pastvotedR + hhsizeR)^3 , post=F)$res


print(c("Controls explain the following fraction of variance of Outcome", 1-var(resY)/var(peacefactorR)))


print(c("Controls explain the following fraction of variance of Treatment", 1-var(resD)/var(directlyharmedR)))



# library(lfe)


dml.darfur.model= felm(resY ~ resD|0|0|village)   # cluster SEs by village

summary(dml.darfur.model,robust=T)  #culster SE by village

dml.darfur.model= lm(resY ~ resD)  #lineaer model to use as input in sensemakr   





# Main estimate

beta = dml.darfur.model$coef[2]

# Hypothetical values of partial R2s 

R2.YC = .16; R2.DC = .01

# Elements of the formal

kappa<-  (R2.YC * R2.DC)/(1- R2.DC)

varianceRatio<- mean(dml.darfur.model$res^2)/mean(resD^2)

# Compute square bias 

BiasSq <-  kappa*varianceRatio

# Compute absolute value of the bias

print(sqrt(BiasSq))


# plotting 

gridR2.DC<- seq(0,.3, by=.001) 

gridR2.YC<- kappa*(1 - gridR2.DC)/gridR2.DC

gridR2.YC<- ifelse(gridR2.YC> 1, 1, gridR2.YC);



plot(gridR2.DC, gridR2.YC, type="l", col=4, xlab="Partial R2 of Treatment with Confounder", 
     ylab="Partial R2 of Outcome with Confounder",
    main= c("Combo of R2 such that |Bias|< ", round(sqrt(BiasSq), digits=4))
)







 
dml.darfur.sensitivity <- sensemakr(model = dml.darfur.model, 
                                treatment = "resD")
summary(dml.darfur.sensitivity)

plot(dml.darfur.sensitivity, nlevels = 15)


DML2.for.PLM <- function(x, d, y, dreg, yreg, nfold=2, clusterID) {
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
  rfit <- felm(ytil ~ dtil |0|0|clusterID)    #get clustered standard errors using felm
  rfitSummary<- summary(rfit)
  coef.est <-  rfitSummary$coef[2] #extract coefficient
  se <- rfitSummary$coef[2,2]  #record robust standard error
  cat(sprintf("\ncoef (se) = %g (%g)\n", coef.est , se))  #printing output
  return( list(coef.est =coef.est , se=se, dtil=dtil, ytil=ytil) ) #save output and residuals 
}


x= model.matrix(~  femaleR + ageR + farmerR + herderR + pastvotedR + hhsizeR)

dim(x)

d= directlyharmedR

y = peacefactorR;

#DML with Random Forest:
dreg <- function(x,d){ randomForest(x, d) } #ML method=Forest 
yreg <- function(x,y){ randomForest(x, y) } #ML method=Forest
set.seed(1)
DML2.RF = DML2.for.PLM(x, d, y, dreg, yreg, nfold=10, clusterID=village)


resY =  DML2.RF$ytil

resD =  DML2.RF$dtil


print(c("Controls explain the following fraction of variance of Outcome", max(1-var(resY)/var(peacefactorR),0)))


print(c("Controls explain the following fraction of variance of Treatment", max(1-var(resD)/var(directlyharmedR),0)))



dml.darfur.model= lm(resY~resD) 


dml.darfur.sensitivity <- sensemakr(model = dml.darfur.model, 
                                treatment = "resD")
summary(dml.darfur.sensitivity)

plot(dml.darfur.sensitivity,nlevels = 15)


