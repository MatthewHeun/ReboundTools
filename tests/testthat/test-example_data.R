test_that("sample_eeu_data_path() works as expected", {
  sample_data_path <- sample_eeu_data_path()
  expect_true(endsWith(sample_data_path, "/extdata/example_eeu_data.xlsx"))
})


test_that("load_eeu_data() works as expected", {
  eeu_data <- load_eeu_data()
  # Verify that all required columns are present.
  cnames <- colnames(eeu_data)
  required_cols <- c(eeu_base_params, 
                     orig_vars[c("eta_orig_engr_units", "q_dot_s_orig", "M_dot_orig", "C_cap_orig", "t_own_orig", "C_dot_md_orig", "E_emb_orig", "t_life_orig")], 
                     star_vars[c("eta_star_engr_units", "C_cap_star", "t_own_star", "C_dot_md_star", "E_emb_star", "t_life_star")])
  cols_present <- required_cols %in% cnames
  expect_true(all(cols_present))
})
