test_that("units() works as expected", {
  su <- "service"
  eu <- "energy"
  expect_equal(units("Delta_C_dot_o_hat", service_unit = su, energy_engr_unit = eu), 
               c(Delta_C_dot_o_hat = "[$/year]"))
  expect_equal(units("Delta_C_dot_o_hat", service_unit = su, energy_engr_unit = eu, surround_left = NULL), 
               c(Delta_C_dot_o_hat = "$/year"))
  expect_equal(units("Delta_C_dot_o_hat", service_unit = su, energy_engr_unit = eu, surround_right = NULL), 
               c(Delta_C_dot_o_hat = "$/year"))
  
  expect_equal(units("C_dot_s_orig", service_unit = su, energy_engr_unit = eu), 
               c(C_dot_s_orig = "[$/year]"))
  expect_equal(units("C_dot_s_orig", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(C_dot_s_orig = "[\\$/year]"))

  expect_equal(units("C_cap_orig", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(C_cap_orig = "[\\$]"))
  
  expect_equal(units("E_dot_s_orig", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(E_dot_s_orig = "[MJ/year]"))
  
  expect_equal(units("E_emb_orig", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(E_emb_orig = "[MJ]"))
  
  expect_equal(units("p_E_engr_units", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(p_E_engr_units = "[\\$/energy]"))
  expect_equal(units("p_E", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(p_E = "[\\$/energy]"))
  
  expect_equal(units("I_E", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(I_E = "[MJ/\\$]"))
  
})
