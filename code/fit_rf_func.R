#' Fit an ordinal random forest model
#'
#' This function fits a random forest model for ordinala classification. It uses the `ordfor` function from the `ordinalForest` R package. It performs a grid search on combinations of the minimum node size and number of trees hyperparameters, using cross-validation and mean absolute error (MAE) to compare combinations. Finally, ,it fits and returns a model on the full dataset using the best parameter combination.
#'
#' @param x A numeric matrix or data frame of predictors (samples x features).
#' @param y A factor vector of class labels, with at least two ordered levels.
#' @param ntree_grid A numeric vector for the number of trees to evaluate
#'   in the grid search (default: `round(10^seq(1, 3, by = 0.5))`).
#' @param nodesize_grid A numeric vector for the minimum node size
#'   to evaluate (default: `round(10^seq(1, 2, by = 0.25))`).
#' @param nfolds An integer specifying the number of cross-validation folds (default: `5`).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{fit}{The final fitted `ordinalForest` model using the optimal hyperparameters.}
#'   \item{mae}{A 3D array of MAE values across all folds and grid combinations.}
#'   \item{mean_mae}{A matrix of average MAE values across folds for each parameter combination.}
#'   \item{best_ntree_nodesize}{A 2-element vector indicating the indices of the best ntree and nodesize.}
#'   \item{df}{The full training data with fold assignments.}
#' }
#'
#' @importFrom ordinalForest ordfor
#' @export
#' 
fit_rf <- function(x, y, 
				   ntree_grid = round(10^seq(1, 3, by = 0.5)), 
				   nodesize_grid = round(10^(seq(1, 2, by = 0.25))), 
				   nfolds = 5) {
	if (nrow(x) != length(y)) {
		stop("number of rows of x (samples) should equal length of y")
	}
	if (!is.factor(y)) {
		stop("y (class labels) should be a factor of 2 or more levels")
	}
	if (nlevels(y) < 2) {
		stop("y (class labels) should be a factor of 2 or more levels")
	}
	
	df <- cbind(x, y) |> as.data.frame()
	names(df)[ncol(df)] <- "Class"
	df$Class <- factor(df$Class, levels = sort(unique(df$Class)))

	df$foldid <- sample(1:nfolds, size = nrow(df), replace = TRUE)
	
	mae <- array(dim = c(length(ntree_grid), length(nodesize_grid), nfolds))
	# Grid search
	for (i_fold in 1:nfolds) {
		message(paste("Fold", i_fold, "of", nfolds, "..."))
		df_fold <- df[df$foldid != i_fold, ]
		df_fold <- df_fold[, -ncol(df_fold)]
		
		df_test <- df[df$foldid == i_fold, ]
		df_test <- df_test[, -ncol(df_test)]
		
		for (i_ntree in seq_along(ntree_grid)) {
			for (i_nodesize in seq_along(nodesize_grid)) {

				fit <- ordfor(depvar = "Class", 
							  data = df_fold,
							  perffunction = "probability", 
							  ntreeperdiv = ntree_grid[i_ntree], 
							  ntreefinal = ntree_grid[i_ntree], 
							  min.node.size = nodesize_grid[i_nodesize])
				
				pred <- predict(fit, newdata = df_test)
				mae[i_ntree, i_nodesize, i_fold] <- mean(abs(as.numeric(pred$ypred) - as.numeric(df_test$Class)))
				
			}
		}
	}
	
	mean_mae <- apply(X = mae, MARGIN = c(1, 2), FUN = mean)
	
	best_ntree_nodesize <- which(mean_mae == min(mean_mae), arr.ind = TRUE)
	
	# If there is a non-unique optimum
	if (sum(mean_mae == min(mean_mae)) > 1) {
		best_ntree_nodesize <- best_ntree_nodesize[order(best_ntree_nodesize[, 1], best_ntree_nodesize[, 2]), ][1, ]
	}

	message("Fitting final model...")
	fit <- ordfor(depvar = "Class",
				  data = df[, -which(names(df) == "foldid")],
				  perffunction = "probability",
				  ntreeperdiv = ntree_grid[best_ntree_nodesize[1]],
				  ntreefinal = ntree_grid[best_ntree_nodesize[1]],
				  min.node.size = nodesize_grid[best_ntree_nodesize[2]])
	
	return(list("fit" = fit, 
				"mae" = mae, 
				"mean_mae" = mean_mae, 
				"best_ntree_nodesize" = best_ntree_nodesize, 
				"df" = df
	))
}
