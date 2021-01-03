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
  
  expect_equal(units("Delta_E_emb_orig", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(Delta_E_emb_orig = "[MJ]"))

  expect_equal(units("p_E_engr_units", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(p_E_engr_units = "[\\$/energy]"))
  expect_equal(units("p_E", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(p_E = "[\\$/MJ]"))
  
  expect_equal(units("I_E", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(I_E = "[MJ/\\$]"))
  
  expect_equal(units("e_qs_ps_UC", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(e_qs_ps_UC = "[--]"))
  expect_equal(units("e_qs_ps", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(e_qs_ps = "[--]"))
  expect_equal(units("e_qo_ps", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(e_qo_ps = "[--]"))
  expect_equal(units("e_qs_M", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(e_qs_M = "[--]"))
  expect_equal(units("e_qo_M", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(e_qo_M = "[--]"))
  
  expect_equal(units("eta_engr_units_orig", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(eta_engr_units_orig = "[service/energy]"))
  
  expect_equal(units("eta", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(eta = "[service/MJ]"))
  
  expect_equal(units("t_own_orig", service_unit = su, energy_engr_unit = eu, escape_latex = TRUE), 
               c(t_own_orig = "[year]"))
  
})

