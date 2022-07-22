# load the H2O package
install.packages("librarian", quiet = T)
librarian::shelf(h2o, tidyverse, quiet = T)
# library(h2o)

# load the data set

data = read_csv("https://github.com/alexanderquispe/14.38_Causal_ML/raw/main/data/wage2015_subsample_inference.csv", show_col_types = F)

# split the data
set.seed(1234)
training <- sample(nrow(data), nrow(data)*(3/4), replace=FALSE)

train <- data[training,]
test <- data[-training,]

# start h2o cluster
h2o.init()

# convert data as h2o type
train_h = as.h2o(train)
test_h = as.h2o(test)

# have a look at the data
h2o.describe(train_h)

# define the variables
y = 'lwage'
x = setdiff(names(data), c('wage','occ2', 'ind2'))
            
# run AutoML for 10 base models and a maximal runtime of 100 seconds
aml = h2o.automl(x=x,y = y,
                  training_frame = train_h,
                  leaderboard_frame = test_h,
                  max_models = 10,
                  seed = 1,
                  max_runtime_secs = 100
                 )
# AutoML Leaderboard
lb = aml@leaderboard
print(lb, n = nrow(lb))

aml@leaderboard$mse[1]

aml@leader

model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
# Get the "All Models" Stacked Ensemble model
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
# Get the Stacked Ensemble metalearner model
metalearner <- se@model$metalearner_model
h2o.varimp(metalearner)

h2o.varimp_plot(metalearner)

pred <- as.matrix(h2o.predict(aml@leader,test_h)) # make prediction using x data from the test sample
head(pred)

y_test <- as.matrix(test_h$lwage)
summary(lm((y_test-pred)^2~1))$coef[1:2]

h2o.shutdown(prompt = F)
