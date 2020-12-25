test_that("calc_rebound_data() works as expected", {
  res <- load_eeu_data() %>% 
    calc_rebound_data()
  
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_ratio]][[1]], 1.68)
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_ratio]][[2]], 9.29545454545454497008)
})
