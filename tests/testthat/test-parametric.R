test_that("parametric_studies() fails when >1 case is sent", {
  two_cases <- load_eeu_data()
  params <- list(k = seq(0, 2, by = 0.5))
  expect_error(parametric_studies(two_cases, params), "nrow\\(original_cases\\) not equal to 1")
})

test_that("parametric_studies() fails when wrong variables are sent", {
  lamp_case <- load_eeu_data() %>% 
    dplyr::filter(.data[[ReboundTools::eeu_base_params$case]] == "Lamp")
  params <- list(wrong_variable = seq(0, 2, by = 0.5))
  expect_error(parametric_studies(lamp_case, params),
               "Elements 1 of names\\(parameterization\\) %in% names\\(original_cases\\) are not true")
})

test_that("parametric_studies() uses expand.grid", {
  car_case <- load_eeu_data() %>% 
    dplyr::filter(.data[[ReboundTools::eeu_base_params$case]] == "Car")
  params <- list(seq(0, 2, by = 0.5), seq(1.5, 2.5, by = 0.25)) %>% 
    magrittr::set_names(c(ReboundTools::eeu_base_params$k, 
                          ReboundTools::eeu_base_params$p_E_engr_units))
  res <- parametric_studies(car_case, params)
  expect_equal(res %>% 
                 dplyr::select(c(ReboundTools::eeu_base_params$k,
                                 ReboundTools::eeu_base_params$p_E_engr_units)) %>% 
                 as.data.frame(),
               expand.grid(params), ignore_attr = TRUE)
  
  # Check that all rows are same (except for the k and p_E_engr_units columns)
  res_no_param_vars <- res %>% 
    dplyr::select(names(car_case)) %>% 
    dplyr::select(-c(ReboundTools::eeu_base_params$k, ReboundTools::eeu_base_params$p_E_engr_units))
  expected <- car_case %>% 
    dplyr::select(-c(ReboundTools::eeu_base_params$k, ReboundTools::eeu_base_params$p_E_engr_units))
  for (i in 1:nrow(res)) {
    expect_equal(dplyr::slice(res_no_param_vars, i), expected)
  }
})
