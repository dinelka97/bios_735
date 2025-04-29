# create a small test dataset
set.seed(22)
n <- 100
data <- data.frame(
  age=rnorm(n, 50, 10),
  fbs=factor(sample(0:1, n, replace=TRUE)),
  sex=factor(sample(0:1, n, replace=TRUE)),
  cp=sample(1:4, n, replace=TRUE),
  chol=rnorm(n, 200, 50),
  exang=sample(0:1, n, replace=TRUE),
  thalach=rnorm(n, 150, 20),
  slope=factor(sample(1:3, n, replace=TRUE))
)

# create binary outcome
logit <- -2 + 0.02*data$age + 0.5*as.numeric(data$sex) - 0.3*data$cp +
  0.003*data$chol + 0.7*data$exang - 0.01*data$thalach
prob <- 1/(1 + exp(-logit))
data$y_bin <- factor(rbinom(n, 1, prob))

# create multi-category outcome (0 through 4)
data$y_mult <- factor(sample(0:4, n, replace=TRUE,
                             prob=c(0.4, 0.3, 0.15, 0.1, 0.05)))

# test 1: check if glm_v2 correctly handles logistic regression
test_that("glm_v2 correctly fits logistic regression model", {
  # fit model using glm_v2
  formula_bin <- y_bin ~ age + fbs + sex + cp + chol + exang + thalach + slope
  result <- glm_v2(formula_bin, df=data, method="lr")

  # check that basic structure is correct
  expect_true(is.list(result))
  expect_true(all(c("beta", "ll", "time") %in% names(result)))
})

# test 2: check if glm_v2 correctly handles proportional odds logistic regression
test_that("glm_v2 correctly fits proportional odds logistic regression model", {
  # fit model using glm_v2
  formula_mult <- y_mult ~ age + fbs + sex + cp + chol + exang + thalach + slope
  result <- glm_v2(formula_mult, df=data, method="polr")

  # check basic structure
  expect_true(is.list(result))
  expect_true(all(c("beta", "theta", "ll", "time") %in% names(result)))

  # check that we have the right number of thresholds
  expected_thetas <- length(levels(data$y_mult)) - 1
  expect_equal(length(result$theta), expected_thetas)
})

# test 3: check error handling for misspecified inputs
test_that("glm_v2 correctly handles errors in input", {
  # try to fit logistic regression with multi-category outcome
  formula_mult <- y_mult ~ age + sex + cp
  expect_error(glm_v2(formula_mult, df=data, method="lr"),
               "Make sure that the response variable has 2 categories")

  # try with non-existent variable
  formula_bad <- y_bin ~ age + nonexistent_var
  expect_error(glm_v2(formula_bad, df=data, method="lr"))

  # try without outcome
  formula_bad <- ~ age + cp
  expect_error(glm_v2(formula_bad, df=data, method="lr"))
})

# test 4: check that function validates method parameter
test_that("glm_v2 validates the method parameter", {
  formula_bin <- y_bin ~ age + sex

  # test with invalid method parameter
  expect_error(glm_v2(formula_bin, df=data, method="invalid_method"))
})

# test 5: check that function handles empty data frame
test_that("glm_v2 handles empty or tiny datasets appropriately", {
  # test with empty data frame
  empty_df <- data[0, ]
  formula_bin <- y_bin ~ age + sex
  expect_error(glm_v2(formula_bin, df=empty_df, method="lr"))
})
