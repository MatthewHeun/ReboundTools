test_that("parametric_studies() works when 1 case is requested", {
  one_case <- load_eeu_data() 
  params <- list(Lamp = list(k = seq(0, 2, by = 0.5)))
  res <- parametric_analysis(one_case, params, include_orig_point = FALSE)
  expect_equal(unique(res$Case), "Lamp")
  expect_equal(unique(res[[ReboundTools::parametric_analysis_point_types$point_type_colname]]), 
               ReboundTools::parametric_analysis_point_types$sweep)
})


test_that("parametric_studies() fails when wrong variables are sent", {
  lamp_case <- load_eeu_data()
  params <- list(Lamp = list(wrong_variable = seq(0, 2, by = 0.5)))
  expect_error(parametric_analysis(lamp_case, params, include_orig_point = FALSE),
               "Elements 1 of params %in% names\\(original_cases\\) are not true")
})


test_that("parametric_studies() uses expand.grid", {
  car_case <- load_eeu_data() 
  params <- list(Car = list(k = seq(0, 2, by = 0.5), 
                            p_E_engr_units = seq(1.5, 2.5, by = 0.25)))
  res <- parametric_analysis(car_case, params, include_orig_point = FALSE)
  expect_equal(res %>% 
                 dplyr::select(c("k", "p_E_engr_units")) %>% 
                 as.data.frame(),
               expand.grid(params$Car), ignore_attr = TRUE)
  
  # Check that all rows are same (except for the k and p_E_engr_units columns)
  res_no_param_vars <- res %>% 
    dplyr::select(names(car_case)) %>% 
    dplyr::select(-c(ReboundTools::eeu_base_params$k, ReboundTools::eeu_base_params$p_E_engr_units))
  expected <- car_case %>% 
    dplyr::filter(Case == "Car") %>% 
    dplyr::select(-c(ReboundTools::eeu_base_params$k, ReboundTools::eeu_base_params$p_E_engr_units))
  for (i in 1:nrow(res)) {
    expect_equal(dplyr::slice(res_no_param_vars, i), expected)
  }
})


test_that("parametric_studies() works for more than 1 case", {
  cases <- load_eeu_data()
  # Make parameters that don't vary
  params_1 <- list(Car = list(k = rep(1, times = 10), p_E_engr_units = rep(1.5, times = 2)), 
                   Lamp = list(k = rep(1.5, times = 3), p_E_engr_units = rep(0.05, times = 5)))
  res_1 <- parametric_analysis(cases, params_1, include_orig_point = FALSE)
  # Every row in res should be the same (per case)
  for (case in unique(res_1$Case)) {
    res_per_case <- res_1 %>% dplyr::filter(.data[["Case"]] == case)
    unique_rows <- unique(res_per_case)
    expect_equal(nrow(unique_rows), 1)
  }
  
  # Now test with different parameters on each row
  params_2 <- list(Car = list(k = seq(1, 2, by = 0.5), p_E_engr_units = seq(1.5, 2.5, by = 0.25)), 
                   Lamp = list(k = seq(0.5, 1.5, by = 0.5), p_E_engr_units = seq(0.05, 0.15, by = 0.05)))
  res_2 <- parametric_analysis(cases, params_2, include_orig_point = FALSE)

  # Check that we have something.  
  expect_true(!is.null(res_2))
  # Check that we have both cases in the result
  expect_equal(unique(res_2$Case), c("Car", "Lamp"))
  # Ensure that each row is different
  unique_rows_2 <- unique(res_2)
  expect_equal(nrow(unique_rows_2), nrow(res_2))
})


test_that("parametric_studies() works with orig point.", {
  one_case <- load_eeu_data() 
  params <- list(Lamp = list(k = seq(0, 2, by = 0.5)))
  res <- parametric_analysis(one_case, params)
  expect_equal(unique(res[[ReboundTools::parametric_analysis_point_types$point_type_colname]]), 
               c(ReboundTools::parametric_analysis_point_types$orig, 
                 ReboundTools::parametric_analysis_point_types$sweep))
})


test_that("parametric_studies() works with single k values.", {
  k_vals <- list(Car = list(k = 3),
                 Lamp = list(k = 3))
  
  rebound_results_with_k_3 <- load_eeu_data() %>% 
    ReboundTools::parametric_analysis(parameterization = k_vals,
                                      include_orig_point = FALSE)
  
  # Re_macro should be triple the avlues from the default analysis (with k = 1).
  rebound_results <- load_eeu_data() %>% 
    rebound_analysis()
  
  expect_equal(rebound_results_with_k_3[[ReboundTools::rebound_terms$Re_macro]], 
               rebound_results[[ReboundTools::rebound_terms$Re_macro]] * 3)
})