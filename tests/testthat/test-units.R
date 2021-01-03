test_that("units() works as expected", {
  su <- "service"
  eu <- "energy"
  expect_equal(units("Delta_C_dot_o_hat", service_unit = su, energy_engr_unit = eu), 
               c(Delta_C_dot_o_hat = "[$/year]"))
  expect_equal(units("Delta_C_dot_o_hat", service_unit = su, energy_engr_unit = eu, surround_left = NULL), 
               c(Delta_C_dot_o_hat = "$/year"))
  expect_equal(units("Delta_C_dot_o_hat", service_unit = su, energy_engr_unit = eu, surround_right = NULL), 
               c(Delta_C_dot_o_hat = "$/year"))
})
