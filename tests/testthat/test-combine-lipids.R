# test-combine-lipids
test_that("Combines all the duplicates successfully", {
  df <- data.frame(
    "PC 14:1" = c(10, 20, 10, 30),
    "PE 14:2" = c(2, 5, 6, 5),
    "PC 14:1" = c(5, 15, 25, 18),
    "PC 14:1" = c(1, 1, 1, 1),
    "PE 14:2" = c(1, 1, 1, 1),
    row.names = c("Sample1", "Sample2", "Sample3", "Sample4"),
    check.names = FALSE
  )
  
  combined_df <-combine_duplicated_lipids(df,"sum")$data
  testthat::expect_equal(
    combined_df[["PC 14:1"]],
    c(16, 36, 36, 49)
  )
  testthat::expect_equal(
    combined_df[["PE 14:2"]],
    c(3, 6, 7, 6)
  )
  # 确认合并后只剩一列 PC 14:1
  testthat::expect_equal(
    sum(colnames(combined_df) == "PC 14:1"),
    1L
  )
  
})

test_that("Combines all the duplicates successfully", {
  df <- data.frame(
    "PC 14:1" = c(NA, 20, 10, 30),
    "PE 14:2" = c(2, 5, 6, 5),
    "PC 14:1" = c(5, 15, 25, 18),
    "PC 14:1" = c(1, 1, 1, 1),
    "PE 14:2" = c(1, 1, 1, 1),
    row.names = c("Sample1", "Sample2", "Sample3", "Sample4"),
    check.names = FALSE
  )
  
  combined_df <-combine_duplicated_lipids(df,"sum")$data
  testthat::expect_equal(
    combined_df[["PC 14:1"]],
    c(6, 36, 36, 49)
  )
  testthat::expect_equal(
    combined_df[["PE 14:2"]],
    c(3, 6, 7, 6)
  )
  # 确认合并后只剩一列 PC 14:1
  testthat::expect_equal(
    sum(colnames(combined_df) == "PC 14:1"),
    1L
  )
  
})
# testthat::test_file("tests/testthat/test-combine-lipids.R")