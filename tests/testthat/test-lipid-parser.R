# test-lipid-parser.R
test_that("PC class, total carbon number, and total double bonds are parsed correctly", {
  result <- parse_name("PC 16:0_18:1")
  expect_equal(result$Lipid.class, "PC")
  expect_equal(result$Total.carbon, 34)
  expect_equal(result$Total.unsaturation, 1)
})

test_that("Lipids with TAG-FFA style are parsed correctly", {
  result <- parse_name("TAG 55:3 FFA 18:1")
  expect_equal(result$Lipid.class, "TG")
  expect_equal(result$Total.carbon, 55)
  expect_equal(result$Total.unsaturation, 3)
})

test_that("PE O- class, total carbon number, and total double bonds are parsed correctly", {
  result <- parse_name("PE O- 18:0_18:1")
  expect_equal(result$Lipid.class, "PE O-")
  expect_equal(result$Total.carbon, 36)
  expect_equal(result$Total.unsaturation, 1)
})

test_that("DhCer18:0/16:0 class, total carbon number, and total double bonds are parsed correctly", {
  result <- parse_name("DhCer18:0/16:0")
  expect_equal(result$Lipid.class, "DhCer")
  expect_equal(result$Total.carbon, 34)
  expect_equal(result$Total.unsaturation, 0)
})