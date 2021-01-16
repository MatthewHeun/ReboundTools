test_that("stages_table() works as expected", {
  t1 <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    stages_table()
  expect_true(!is.null(t1))
  expect_true("Case" %in% colnames(t1))
  # We should have units. I.e., every item in the name column should contain a "[".
  expect_true(all(grepl(pattern = "\\[", t1$name)))
  expect_true(! ".unit_col" %in% names(t1))
  
  # Try without the case column
  analysis_data <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    dplyr::filter(Case == "Car") %>% 
    dplyr::mutate(
      Case = NULL
    )
  t2 <- stages_table(analysis_data)
  expect_true(!is.null(t2))
  expect_true(!(ReboundTools::eeu_base_params$case %in% colnames(t2)))
  
  # Check that the name of the "name" column is empty.
  cnames <- colnames(t2)
  expect_equal(cnames[[1]], " ")
})


test_that("rebound_results_table() works as expected", {
  t1 <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    rebound_results_table()
  expect_true(!is.null(t1))
  expect_true("Case" %in% colnames(t1))
  expect_true("Value [\\%]" %in% colnames(t1))

  # Try without the case column
  analysis_data <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    dplyr::filter(Case == "Car") %>% 
    dplyr::mutate(
      Case = NULL
    )
  t2 <- rebound_results_table(analysis_data)
  expect_true(!is.null(t2))
  expect_true(!(ReboundTools::eeu_base_params$case %in% colnames(t2)))
  expect_equal(colnames(t2)[[2]], "Value [\\%]")
})


test_that("percentage works as expected", {
  t1 <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    rebound_results_table(as_percent = FALSE)
  expect_true("Value [-]" %in% colnames(t1))
})


test_that("including subtotals works as expected", {
  t1 <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    rebound_results_table(escape_latex = FALSE,
                          include_subtotals = FALSE)
  Re_types <- t1$`Rebound term`
  expect_true(! ReboundTools::rebound_terms$Re_empl %in% Re_types)
  expect_true(! ReboundTools::rebound_terms$Re_sub %in% Re_types)
  expect_true(! ReboundTools::rebound_terms$Re_inc %in% Re_types)
  expect_true(! ReboundTools::rebound_terms$Re_prod %in% Re_types)
  expect_true(! ReboundTools::rebound_terms$Re_i %in% Re_types)
  expect_true(! ReboundTools::rebound_terms$Re_d %in% Re_types)
  
  expect_true(ReboundTools::rebound_terms$Re_tot %in% Re_types)
})


test_that("not including total works as expected", {
  t1 <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    rebound_results_table(escape_latex = FALSE,
                          include_total = FALSE)
  Re_types <- t1$`Rebound term`

  expect_true(ReboundTools::rebound_terms$Re_empl %in% Re_types)
  expect_true(ReboundTools::rebound_terms$Re_sub %in% Re_types)
  expect_true(ReboundTools::rebound_terms$Re_inc %in% Re_types)
  expect_true(ReboundTools::rebound_terms$Re_prod %in% Re_types)
  expect_true(ReboundTools::rebound_terms$Re_i %in% Re_types)
  expect_true(ReboundTools::rebound_terms$Re_d %in% Re_types)
  
  expect_true(! ReboundTools::rebound_terms$Re_tot %in% Re_types)
})
