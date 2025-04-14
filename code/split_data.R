split_data <- function(dataset, training_fraction = 0.8) {
	# Random split
	samples_train <- sample(1:nrow(dataset), 
							   size = round(training_fraction * nrow(dataset)))
	samples_test <- setdiff(1:nrow(dataset), samples_train)
	
	dataset_train <- dataset[samples_train, ]
	dataset_test <- dataset[samples_test, ]
	
	return(list("train" = dataset_train, 
				"test" = dataset_test))
}

x <- read.csv(file = "./data/derived/df_v1.csv")
x <- x[complete.cases(x), ]

dataset_split <- split_data(dataset = x)

write.csv(x = dataset_split$train, file = "./data/derived/df_v1_TRAIN.csv", row.names = FALSE)
write.csv(x = dataset_split$test, file = "./data/derived/df_v1_TEST.csv", row.names = FALSE)
