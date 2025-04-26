rm(list = ls())

library(ordinalForest)

source("./code/fit_rf_func.R") # load function

# Load in training and test data and put into the right format
x <- read.csv(file = "./data/derived/df_v1_TRAIN.csv")[, -1]
y_multi <- x$num |> factor(levels = sort(unique(x$num)))
y_binary <- as.numeric(x$num > 0) |> factor(levels = c(0, 1))
x <- x[, -which(names(x) == "num")]

x_test <- read.csv(file = "./data/derived/df_v1_TEST.csv")[, -1]
y_multi_test <- x_test$num |> factor(levels = sort(unique(x_test$num)))
y_binary_test <- as.numeric(x_test$num > 0) |> factor(levels = c(0, 1))
x_test <- x_test[, -which(names(x_test) == "num")]


meta <- read.csv(file = "./data/derived/var_info_upd.csv")


# Fit ordinal model (takes a while)
res_multi <- fit_rf(x, y_multi)

# Fit ordinal model but instead let's classify into the binary output (0, > 0)
res_binary <- fit_rf(x, y_binary)

save.image(file = "./results/randomforest_results.RData")
