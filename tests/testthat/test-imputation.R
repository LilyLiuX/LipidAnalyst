library(data.table)
test_that("mean imputation replaces NA with column mean", {
  df <- data.frame(
    PC = c(10, 20, NA, 30),
    PE = c(2, NA, 6, 10)
  )
  
  result <- impute_missing_values(df, method = "mean")
  
  expect_equal(result$PC, c(10, 20, 20, 30))
  expect_equal(result$PE, c(2, 6, 6, 10))
  expect_false(anyNA(result))
})

test_that("median imputation uses column median", {
  df <- data.frame(
    PC = c(10, 20, 100, NA),
    PE = c(1, 2, 3, NA)
  )
  
  result <- impute_missing_values(df, method = "median")
  
  expect_equal(result$PC[4], 20)
  expect_equal(result$PE[4], 2)
})


test_that("LoD imputation uses minimum observed value", {
  df <- data.frame(
    PC = c(10, 20, NA),
    PE = c(5, NA, 15)
  )
  
  result_5 <- impute_missing_values(
    df, method = "LoD 1/5 minimum value"
  )
  
  result_2 <- impute_missing_values(
    df, method = "LoD 1/2 minimum value"
  )
  
  expect_equal(result_5$PC[3], 1)
  expect_equal(result_5$PE[2], 1)
  
  expect_equal(result_2$PC[3], 2.5)
  expect_equal(result_2$PE[2], 2.5)
})

test_that("KNN samplewise is correct",{
  df <- data.frame(
    feature_A = c(10, 20, 30, 40),
    feature_B = c(5, 10, 15, 50),
    feature_C = c(5, 12, 15, 18),
    feature_D = c(1, 2, 3, NA)
  )
  result_k_1 <- impute_missing_values(df,
                                      method = "knn-samplewise",
                                      k = 1)
  result_k_2 <- impute_missing_values(df,
                                      method = "knn-samplewise",
                                      k = 2)
  result_k_3 <- impute_missing_values(df,
                                      method = "knn-samplewise",
                                      k = 3)
  expect_equal(result_k_1$feature_D[4], 3)
  expect_equal(result_k_2$feature_D[4], 2.5)
  expect_equal(result_k_3$feature_D[4], 2)
  
})

test_that("KNN featurewise is correct",{
  df <- data.frame(
    feature_A = c(10, 20, 30),
    feature_B = c(5, NA, 15),
    feature_C = c(5, 12, 15),
    feature_D = c(100, 200, 300)
  )
  result_k_1 <- impute_missing_values(df,
                                      method = "knn-featurewise",
                                      k = 1)
  result_k_2 <- impute_missing_values(df,
                                      method = "knn-featurewise",
                                      k = 2)
  result_k_3 <- impute_missing_values(df,
                                      method = "knn-featurewise",
                                      k = 3)
  expect_equal(result_k_1$feature_B[2], 12)
  expect_equal(result_k_2$feature_B[2], 16)
  expect_equal(result_k_3$feature_B[2], 232/3)
  
})


# testthat::test_file("tests/testthat/test-imputation.R")
# testthat::test_dir("tests/testthat")