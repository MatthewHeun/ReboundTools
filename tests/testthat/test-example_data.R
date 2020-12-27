test_that("sample_eeu_data_path() works as expected", {
  sample_data_path <- sample_eeu_data_path()
  expect_true(endsWith(sample_data_path, "/extdata/example_eeu_data.xlsx"))
})


test_that("load_eeu_data() works as expected", {
  eeu_data <- load_eeu_data()
  # Verify that all expected column names are present.
  cnames <- colnames(eeu_data)
  cols_present <- ReboundTools::eeu_base_params %in% cnames
  expect_true(all(cols_present))
  
  # Check if one column is missing
  cols_present_2 <- ReboundTools::eeu_base_params %in% cnames[1:(length(cnames)-1)]
  expect_false(all(cols_present_2))
})
