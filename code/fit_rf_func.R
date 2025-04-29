#' Fit an ordinal random forest model
#'
#' This function fits a random forest model for ordinal classification. It uses the `ordfor` function from the `ordinalForest` R package. It performs a grid search on combinations of the minimum node size and number of trees hyperparameters, using cross-validation and mean absolute error (MAE) to compare combinations. Finally, it fits and returns a model on the full dataset using the best parameter combination.
#'
#' @param x A numeric matrix or data frame of predictors (samples x features).
#' @param y A factor vector of class labels, with at least two ordered levels.
#' @param ntree_grid A numeric vector for the number of trees to evaluate
#'   in the grid search (default: `c(10, 50, 100, 500, 1000)`).
#' @param nodesize_grid A numeric vector for the minimum node size
#'   to evaluate (default: `c(2, 3, 4, 6, 8, 12, 16, 24, 32)`).
#' @param nsets An integer specifying the number of score sets to use. Currently uses default for ordinalForest (default: `1000`).
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
#' @importFrom parallel mclapply
#' @export
#' 
fit_rf <- function(x, y, 
				   ntree_grid = c(10, 50, 100, 500, 1000), 
				   nodesize_grid = c(2, 3, 4, 6, 8, 12, 16, 24, 32), 
				   nsets = 1000, 
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
	
	# Create data.frame with formatting required by `ordfor`
	df <- cbind(x, y) |> as.data.frame()
	names(df)[ncol(df)] <- "Class"
	df$Class <- factor(df$Class, levels = sort(unique(df$Class)))
	
	# Create folds
	df$foldid <- sample(1:nfolds, size = nrow(df), replace = TRUE)
	
	# Run grid search + cross-validation in parallel
	mae <- mclapply(X = 1:nfolds, FUN = function(i_fold) {
		df_fold <- df[df$foldid != i_fold, ]
		df_fold <- df_fold[, -ncol(df_fold)]
		
		df_test <- df[df$foldid == i_fold, ]
		df_test <- df_test[, -ncol(df_test)]
		
		mae_fold <- array(dim = c(length(ntree_grid), length(nodesize_grid)))
		for (i_ntree in seq_along(ntree_grid)) {
			for (i_nodesize in seq_along(nodesize_grid)) {
				
				fit <- ordfor(depvar = "Class", 
							  data = df_fold, 
							  nsets = nsets, 
							  perffunction = ifelse(nlevels(df$Class) > 2, "probability", "equal"),  
							  ntreeperdiv = ntree_grid[i_ntree], 
							  ntreefinal = 50 * ntree_grid[i_ntree], 
							  min.node.size = nodesize_grid[i_nodesize])
				
				# Evaluate combination on withheld set
				pred <- predict(fit, newdata = df_test)
				mae_fold[i_ntree, i_nodesize] <- mean(abs(as.numeric(pred$ypred) - as.numeric(df_test$Class)))
				
			}
		}
		
		return(mae_fold)
	})
	
	mae <- simplify2array(mae)
	
	# Calculate average MAE across the folds
	mean_mae <- apply(X = mae, MARGIN = c(1, 2), FUN = mean)
	
	best_ntree_nodesize <- which(mean_mae == min(mean_mae), arr.ind = TRUE)
	
	# If there is a non-unique optimum
	if (sum(mean_mae == min(mean_mae)) > 1) {
		best_ntree_nodesize <- best_ntree_nodesize[order(best_ntree_nodesize[, 1], best_ntree_nodesize[, 2]), ][1, ]
	}
	
	message("Fitting final model...")
	fit <- ordfor(depvar = "Class", 
				  data = df[, -which(names(df) == "foldid")],
				  perffunction = ifelse(nlevels(df$Class) > 2, "probability", "equal"), 
				  importance = ifelse(nlevels(df$Class) > 2, "rps", "accuracy"), 
				  ntreeperdiv = ntree_grid[best_ntree_nodesize[1]],
				  ntreefinal = 50 * ntree_grid[best_ntree_nodesize[1]],
				  min.node.size = nodesize_grid[best_ntree_nodesize[2]])
	
	return(list("fit" = fit, 
				"mae" = mae, 
				"mean_mae" = mean_mae, 
				"best_ntree_nodesize" = best_ntree_nodesize, 
				"df" = df
	))
}
