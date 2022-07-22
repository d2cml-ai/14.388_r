install.packages("librarian")
librarian::shelf(
    tidyverse
    , keras
    , broom
    , hdm
    , sandwich
    , lmtest
    , h2o
    , glmnet
    , randomForest
    , rpart
    , nnet
    , gbm
    , rpart.plot
    , keras
    , dagitty
#     , ggdag
    , ORCI
    , lfe
#     , ggraph
    , DoubleML
    , mlr3learners
    , mlr3
    , data.table
    , AER
    , xgboost
    , zoo
    , survival
    , car
    , carData
    , h2o
    , Rcpp
    , regr.ranger
    , classif.ranger
    , tensorflow
)

install.packages("remotes")
install.packages("dagitty")
# install.packages("ggdag")

remotes::install_github("IRkernel/IRkernel")
remotes::install_github("cran/ORCI")
remotes::install_github("malcolmbarrett/ggdag")
IRkernel::installspec()
tensorflow::install_tensorflow()
