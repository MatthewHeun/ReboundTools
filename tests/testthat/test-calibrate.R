test_that("calibrate_k() works as expected", {
  new_k <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    calibrate_k(0.63)
  expect_equal(new_k[[1]], 5.77357)
  expect_equal(new_k[[2]], 1.7783998)
  expect_equal(names(new_k)[[1]], "Car")
  expect_equal(names(new_k)[[2]], "Lamp")
  
  # Add calibration with 2 Re_tot_target values.
  new_k_2 <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    calibrate_k(c(0.4, 0.7))
  expect_equal(new_k_2[[1]], 2.9321855)
  expect_equal(new_k_2[[2]], 2.1459482)
})
