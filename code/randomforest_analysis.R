rm(list = ls())

load(file = "./results/randomforest_results.RData")

library(ordinalForest)
library(ComplexHeatmap)
library(viridis)
library(circlize)

# Heatmap of hyperparameter tuning results
Heatmap(res_multi$mean_mae, col = rev(viridis(n = 100)), 
		cluster_rows = FALSE, cluster_columns = FALSE, name = "MAE", 
		row_title = "min. number of trees/set", row_labels = c(10, 50, 100, 500, 1000), row_names_side = "left", 
		column_title = "min. node size", column_labels = c(2, 3, 4, 6, 8, 12, 16, 24, 32), column_names_side = "top")


# Ordinal outcome measurement (0, 1, 2, 3, 4) -----

df_multi_train <- cbind(x, y_multi) |> as.data.frame()
names(df_multi_train)[ncol(df_multi_train)] <- "Class"
pred_multi_train <- predict(object = res_multi$fit, newdata = df_multi_train)

pred_multi_train_table <- table("prediction" = pred_multi_train$ypred, "true label" = df_multi_train$Class) |> as.matrix()
pred_multi_train_table_prop <- t(apply(X = pred_multi_train_table, MARGIN = 1, FUN = function(i) i / colSums(pred_multi_train_table)))

# Heatmap of confusion matrix for TRAINING data with all classes
Heatmap(pred_multi_train_table_prop, 
		col = colorRamp2(breaks = c(0, 1), colors = c("white", "red")), 
		name = "proportion", 
		cluster_rows = FALSE, cluster_columns = FALSE, 
		row_title = "prediction", row_names_side = "left", 
		column_title = "true label", column_names_side = "top", column_names_rot = 0, 
		cell_fun = function(j, i, x, y, width, height, fill) {
			grid.text(sprintf("%.2f", pred_multi_train_table_prop[i, j]), x, y, gp = gpar(fontsize = 10))
		})

df_multi_test <- cbind(x_test, y_multi_test) |> as.data.frame()
names(df_multi_test)[ncol(df_multi_test)] <- "Class"
pred_multi_test <- predict(object = res_multi$fit, newdata = df_multi_test)

pred_multi_test_table <- table("prediction" = pred_multi_test$ypred, "true label" = df_multi_test$Class) |> as.matrix()
pred_multi_test_table_prop <- t(apply(X = pred_multi_test_table, MARGIN = 1, FUN = function(i) i / colSums(pred_multi_test_table)))

# Heatmap of confusion matrix for TEST data with all classes
Heatmap(pred_multi_test_table_prop, 
		col = colorRamp2(breaks = c(0, 1), colors = c("white", "red")), 
		name = "proportion", 
		cluster_rows = FALSE, cluster_columns = FALSE, 
		row_title = "prediction", row_names_side = "left", 
		column_title = "true label", column_names_side = "top", column_names_rot = 0, 
		cell_fun = function(j, i, x, y, width, height, fill) {
			grid.text(sprintf("%.2f", pred_multi_test_table_prop[i, j]), x, y, gp = gpar(fontsize = 10))
		})


# Binary outcome measurement (0, 1+) -----

df_binary_train <- cbind(x, y_binary) |> as.data.frame()
names(df_binary_train)[ncol(df_binary_train)] <- "Class"
pred_binary_train <- predict(object = res_binary$fit, newdata = df_binary_train)
table("prediction" = pred_binary_train$ypred, "true label" = df_binary_train$Class)

pred_binary_train_table <- table("prediction" = pred_binary_train$ypred, "true label" = df_binary_train$Class) |> as.matrix()
pred_binary_train_table_prop <- t(apply(X = pred_binary_train_table, MARGIN = 1, FUN = function(i) i / colSums(pred_binary_train_table)))

# Heatmap of confusion matrix for TRAINING data with binary outcome
Heatmap(pred_binary_train_table_prop, 
		col = colorRamp2(breaks = c(0, 1), colors = c("white", "red")), 
		name = "proportion", 
		cluster_rows = FALSE, cluster_columns = FALSE, 
		row_title = "prediction", row_names_side = "left", 
		column_title = "true label", column_names_side = "top", column_names_rot = 0, 
		cell_fun = function(j, i, x, y, width, height, fill) {
			grid.text(sprintf("%.2f", pred_binary_train_table_prop[i, j]), x, y, gp = gpar(fontsize = 10))
		})

df_binary_test <- cbind(x_test, y_binary_test) |> as.data.frame()
names(df_binary_test)[ncol(df_binary_test)] <- "Class"
pred_binary_test <- predict(object = res_binary$fit, newdata = df_binary_test)
table("prediction" = pred_binary_test$ypred, "true label" = df_binary_test$Class)


pred_binary_test_table <- table("prediction" = pred_binary_test$ypred, "true label" = df_binary_test$Class) |> as.matrix()
pred_binary_test_table_prop <- t(apply(X = pred_binary_test_table, MARGIN = 1, FUN = function(i) i / colSums(pred_binary_test_table)))

# Heatmap of confusion matrix for TEST data with binary outcome
Heatmap(pred_binary_test_table_prop, 
		col = colorRamp2(breaks = c(0, 1), colors = c("white", "red")), 
		name = "proportion", 
		cluster_rows = FALSE, cluster_columns = FALSE, 
		row_title = "prediction", row_names_side = "left", 
		column_title = "true label", column_names_side = "top", column_names_rot = 0, 
		cell_fun = function(j, i, x, y, width, height, fill) {
			grid.text(sprintf("%.2f", pred_binary_test_table_prop[i, j]), x, y, gp = gpar(fontsize = 10))
		})

