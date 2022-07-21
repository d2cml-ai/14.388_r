install.packages("librarian", quiet = T)
librarian::shelf(keras, tidyverse)
data = read_csv("https://raw.githubusercontent.com/d2cml-ai/14.388_R/main/Data/wage2015_subsample_inference.csv", show_col_types = F)
Z <- data |> select(-c(lwage, wage)) # regressors

# split the data into training and testing sets
set.seed(1234)
training <- sample(nrow(data), nrow(data)*(3/4), replace=FALSE)

data_train <- data[training,1:16]
data_test <- data[-training,1:16]

# normalize the data
mean <- apply(data_train, 2, mean)
std <- apply(data_train, 2, sd)
data_train <- scale(data_train, center = mean, scale = std)
data_test <- scale(data_test, center = mean, scale = std)
data_train <- as.data.frame(data_train)
data_test <- as.data.frame(data_test)

X_basic <-  "sex + exp1 + shs + hsg+ scl + clg + mw + so + we"
formula_basic <- as.formula(paste("lwage", "~", X_basic))
model_X_basic_train <- model.matrix(formula_basic,data_train)
model_X_basic_test <- model.matrix(formula_basic,data_test)

Y_train <- data_train$lwage
Y_test <- data_test$lwage

library(keras)

build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 20, activation = "relu", # ReLU activation function
                input_shape = dim(model_X_basic_train)[2])%>% 
    layer_dense(units = 10, activation = "relu") %>% 
    layer_dense(units = 1) 
  
  model %>% compile(
    optimizer = optimizer_adam(lr = 0.005), # Adam optimizer
    loss = "mse", 
    metrics = c("mae")
  )
}

model <- build_model()
summary(model)

# training the network 
num_epochs <- 1000
model %>% fit(model_X_basic_train, Y_train,
                    epochs = num_epochs, batch_size = 100, verbose = 0)

# evaluating performance
model %>% evaluate(model_X_basic_test, Y_test, verbose = 0)

# calculating the performance measures
pred.nn <- model %>% predict(model_X_basic_test)
MSE.nn = summary(lm((Y_test-pred.nn)^2~1))$coef[1:2]
R2.nn <- 1-MSE.nn[1]/var(Y_test)
# printing R^2
cat("R^2 of the neural network:",R2.nn)
