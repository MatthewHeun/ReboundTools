test_that("calibrate_k() works as expected", {
  new_k <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    calibrate_k(0.63)
  expect_equal(new_k[[1]], 5.77357)
  expect_equal(new_k[[2]], 1.7783998)
  expect_equal(names(new_k)[[1]], "Car")
  expect_equal(names(new_k)[[2]], "Lamp")
})
