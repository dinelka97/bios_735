
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

df_multi_train <- cbind(x, y_multi) |> as.data.frame()
names(df_multi_train)[ncol(df_multi_train)] <- "Class"
pred_multi_train <- predict(object = res_multi$fit, newdata = df_multi_train)
table("prediction" = pred_multi_train$ypred, "true label" = df_multi_train$Class)


df_multi_test <- cbind(x_test, y_multi_test) |> as.data.frame()
names(df_multi_test)[ncol(df_multi_test)] <- "Class"
pred_multi_test <- predict(object = res_multi$fit, newdata = df_multi_test)
table("prediction" = pred_multi_test$ypred, "true label" = df_multi_test$Class)

# Fit ordinal model but instead let's classify into the binary output (0, > 0)
res_binary <- fit_rf(x, y_binary)


df_binary_train <- cbind(x, y_binary) |> as.data.frame()
names(df_binary_train)[ncol(df_binary_train)] <- "Class"
pred_binary_train <- predict(object = res_binary$fit, newdata = df_binary_train)
table("prediction" = pred_binary_train$ypred, "true label" = df_binary_train$Class)


df_binary_test <- cbind(x_test, y_binary_test) |> as.data.frame()
names(df_binary_test)[ncol(df_binary_test)] <- "Class"
pred_binary_test <- predict(object = res_binary$fit, newdata = df_binary_test)
table("prediction" = pred_binary_test$ypred, "true label" = df_binary_test$Class)


save.image(file = "./results/randomforest_results.RData")
# Seeing that it performs much better on the binary output than the multi-class.