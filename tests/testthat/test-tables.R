test_that("stages_table() works as expected", {
  t1 <- stages_table()
  expect_true(!is.null(t1))
  expect_true("Case" %in% colnames(t1))
  
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
})
