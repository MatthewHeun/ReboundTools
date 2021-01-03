test_that("stages_table() works as expected", {
  t1 <- stages_table()
  expect_true(!is.null(t1))
  expect_true("Case" %in% colnames(t1))
  # We should have units. I.e., every item in the name column should contain a "[".
  expect_true(all(grepl(pattern = "\\[", t1$name)))
  
  # Try without the case column
  analysis_data <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    dplyr::filter(Case == "Car") %>% 
    dplyr::mutate(
      Case = NULL
    )
  t2 <- stages_table(analysis_data)
  expect_true(!is.null(t2))
  expect_true(!("Case" %in% colnames(t2)))
  
  # Check that the name of the "name" column is empty.
  cnames <- colnames(t2)
  expect_equal(cnames[[1]], " ")
})
