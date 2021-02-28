test_that("parametric uses expand.grid", {
  car_case <- load_eeu_data() %>% 
    dplyr::filter(.data[[ReboundTools::eeu_base_params$case]] == "Car")
  params <- list(k = seq(0.1, 2, by = 0.1), p_E_engr_units = seq(1.5, 2.5, by = 0.1))
  res <- parametric_studies(car_case, params)
  expect_equal(res[["k", "p_E_engr_units"]], expand.grid(params))
})
