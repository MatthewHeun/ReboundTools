test_that("calibrate_k() works as expected", {
  new_k <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    calibrate_k(0.63)
  expect_equal(new_k[[1]], 6.2555914)
  expect_equal(new_k[[2]], 2.3800704986831138)
  expect_equal(new_k[[3]], 6.376461)
  expect_equal(names(new_k)[[1]], "Car")
  expect_equal(names(new_k)[[2]], "Lamp")
  expect_equal(names(new_k)[[3]], "Car, r = 0")
  
  # Add calibration with 2 Re_tot_target values.
  new_k_2 <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    calibrate_k(c(0.4, 0.7, 1.0))
  expect_equal(new_k_2[[1]], 3.2246189747379455)
  expect_equal(new_k_2[[2]], 2.8718661404585002)
  expect_equal(new_k_2[[3]], 11.327695)
})
