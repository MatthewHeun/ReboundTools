test_that("sample_eeu_data_path() works as expected", {
  sample_data_path <- sample_eeu_data_path()
  expect_true(endsWith(sample_data_path, "/extdata/example_eeu_data.xlsx"))
})
