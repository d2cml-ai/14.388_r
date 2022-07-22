install.packages("librarian", quiet = T)
librarian::shelf(rpart, randomForest, keras, gbm, quiet = T)
set.seed(1)
X_train <- matrix(runif(1000),1000,1)
Y_train <- exp(4*X_train)  #Noiseless case  Y=g(X)
dim(X_train)

# library(rpart)

# shallow tree
TreeModel<- rpart(Y_train~X_train, cp=.01)  #cp is penalty level
pred.TM<- predict(TreeModel, newx=X_train)
plot(X_train, Y_train, type="p", pch=19, xlab="z", ylab="g(z)")
points(X_train, pred.TM, col=3, pch=19)

set.seed(1)
X_train <- matrix(runif(1000),1000,1)
Y_train <- exp(4*X_train)  #Noiseless case  Y=g(X)
dim(X_train)

# library(rpart)

TreeModel<- rpart(Y_train~X_train, cp=.0005)  #cp is penalty level
pred.TM<- predict(TreeModel, newx=X_train)
plot(X_train, Y_train, type="p", pch=19, xlab="z", ylab="g(z)")
points(X_train, pred.TM, col=3, pch=19)

# library(randomForest)

RFmodel<- randomForest(Y_train~X_train)
pred.RF<- predict(RFmodel, newdata=X_train)
plot(X_train, Y_train, type="p", pch=19, xlab="z", ylab="g(z)")
points(X_train, pred.RF, col=4,  pch=19,)


# library(gbm)

data_train = as.data.frame(cbind(X_train, Y_train))
BoostTreemodel<- gbm(Y_train~X_train, distribution= "gaussian",  n.trees=100, shrinkage=.01, interaction.depth
=4) 
 #shrinkage is "learning rate"
 # n.trees is the number of boosting steps 
pred.BT<- predict(BoostTreemodel, newdata=data_train, n.trees=100)
plot(X_train, Y_train, type="p", pch=19, xlab="z", ylab="g(z)")
points(X_train, pred.BT, col=4,  pch=19,)

# library(gbm)

data_train = as.data.frame(cbind(X_train, Y_train))
BoostTreemodel<- gbm(Y_train~X_train, distribution= "gaussian",  n.trees=1000, shrinkage=.01, interaction.depth
=4) 
 # shrinkage is "learning rate"
 # n.trees is the number of boosting steps 
pred.BT<- predict(BoostTreemodel, newdata=data_train, n.trees=1000)
plot(X_train, Y_train, type="p", pch=19, xlab="z", ylab="g(z)")
points(X_train, pred.BT, col=4,  pch=19,)

# library(keras)

build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 200, activation = "relu", 
                input_shape = 1)%>% 
   layer_dense(units = 20, activation = "relu") %>% 
    layer_dense(units = 1) 
  
  model %>% compile(
    optimizer = optimizer_adam(lr = 0.01), 
    loss = "mse", 
    metrics = c("mae"),
  )
}



model <- build_model()
summary(model)

num_epochs <- 1
model %>% fit(X_train, Y_train,
                    epochs = num_epochs, batch_size = 10, verbose = 0)
pred.NN <- model %>% predict(X_train)
plot(X_train, Y_train, type="p", pch=19, xlab="z", ylab="g(z)")
points(X_train, pred.NN, col=4,  pch=19,)



num_epochs <- 100
model %>% fit(X_train, Y_train,
                    epochs = num_epochs, batch_size = 10, verbose = 0)
pred.NN <- model %>% predict(X_train)
plot(X_train, Y_train, type="p", pch=19, xlab="z", ylab="g(z)")
points(X_train, pred.NN, col=4,  pch=19,)



