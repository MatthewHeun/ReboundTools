test_that("calc_rebound_data() works as expected", {
  res <- load_eeu_data() %>% 
    calc_rebound_data()

  
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_orig]][[1]], 0.19743862087385857795)
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_orig]][[2]], 2453.7037037)
  
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_tilde]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_tilde]][[2]], 22722.222222222)
  
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_ratio]][[1]], 1.68)
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_ratio]][[2]], 9.260377359)
  
  expect_equal(res[[ReboundTools::eeu_derived_data$E_dot_s_orig]][[1]], 73060.68051000000559724867)
  expect_equal(res[[ReboundTools::eeu_derived_data$E_dot_s_orig]][[2]], 236.52)
  
  expect_equal(res[[ReboundTools::eeu_derived_data$S_dot_dev]][[1]], 29572.18)
  expect_equal(res[[ReboundTools::eeu_derived_data$S_dot_dev]][[2]], 210.97892420537905877609)

  
  
  
  
})
