test_that("sample_eeu_data_path() works as expected", {
  sample_data_path <- sample_eeu_data_path()
  expect_true(endsWith(sample_data_path, "/extdata/example_eeu_data.xlsx"))
})


test_that("load_eeu_data() works as expected", {
  eeu_data <- load_eeu_data()
  # Verify that all required columns are present.
  cnames <- colnames(eeu_data)
  required_cols <- c(eeu_base_params, 
                     orig_vars[c("eta_orig_engr_units", "q_dot_s_orig", "M_dot_orig", "C_cap_orig", "t_orig", "C_dot_md_orig", "E_emb_orig")], 
                     star_vars[c("eta_star_engr_units", "C_cap_star", "t_star", "C_dot_md_star", "E_emb_star")])
  cols_present <- required_cols %in% cnames
  expect_true(all(cols_present))
  
  # Check if one column is missing
  # cols_present_2 <- ReboundTools::eeu_base_params %in% cnames[1:(length(cnames)-1)]
  # expect_false(all(cols_present_2))
})
