install.packages(
    "librarian"
    , quiet = T) # package installation and management
librarian::shelf(
    tidyverse, broom, glue
    , quiet = T)
set.seed(123)
n = 1000

p = n
X <- matrix(rnorm(n*p), n, p)
Y <- rnorm(n)

mdl_0 <- lm(Y ~ X)


cat(
  "- p/n is: " , p/n
  , "\n- R2 is: ", summary(mdl_0)$r.squared
  , "\n- Adjusted r2 is: ", summary(mdl_0)$adj.r.squared
)

broom::tidy(mdl_0) |> 
  slice(1:5, 995:1000)


set.seed(123)
n = 1000

p = n/2
X <- matrix(rnorm(n*p), n, p)
Y <- rnorm(n)

mdl_1 <- lm(Y ~ X)


cat(
  "- p/n is: " , p/n
  , "\n- R2 is: ", summary(mdl_1)$r.squared
  , "\n- Adjusted r2 is: ", summary(mdl_1)$adj.r.squared
)


set.seed(123)
n = 1000

p = .05*n
X <- matrix(rnorm(n*p), n, p)
Y <- rnorm(n)

mld_2 <- lm(Y ~ X)

cat(
  "- p/n is: " , p/n
  , "\n- R2 is: ", summary(mdl_1)$r.squared
  , "\n- Adjusted r2 is: ", summary(mdl_1)$adj.r.squared
)

