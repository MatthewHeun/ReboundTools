test_that("iso_cost_lines() works as expected", {
  iso_cost_lines_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    iso_cost_lines()
  
    # Check values on the car absolute iso line.
    car_cost_iso_lines_abs <- iso_cost_lines_abs %>%
      dplyr::filter(Case == "Car", graph_type == "Cost", line_name == "orig")
    expect_equal(car_cost_iso_lines_abs$slope, -1)
    expect_equal(car_cost_iso_lines_abs$intercept, 27401.27769302945671370253)

    # Calculate the indexed iso cost line.
    car_cost_iso_lines_indexed <- load_eeu_data() %>%
      rebound_analysis() %>%
      iso_cost_lines(indexed = TRUE)
    car_cost_iso_lines_indexed <- car_cost_iso_lines_indexed %>%
      dplyr::filter(Case == "Car", graph_type == "Cost", line_name == "orig")
    expect_equal(car_cost_iso_lines_indexed$slope, -0.04880826547079648164)
    expect_equal(car_cost_iso_lines_indexed$intercept, 1.04880826547079641919)
})

test_that("iso_budget_lines_prefs() works as expected", {
  iso_budget_lines <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    iso_budget_lines_prefs() %>% 
    dplyr::filter(Case == "Lamp")
  expect_equal(iso_budget_lines$intercept[[1]], 1.00032500637362797846)
  expect_equal(iso_budget_lines$intercept[[2]], 1.00003509645028976216)
  expect_equal(iso_budget_lines$intercept[[3]], 0.99979635239197450769)
  expect_equal(iso_budget_lines$intercept[[4]], 1.00035871939210463388)
  expect_equal(iso_budget_lines$intercept[[5]], 0.00000000000000000000)
})


test_that("add_iso() works as expected", {
  meta <- tibble::tibble(Case = "Test case")
  res <- add_iso(indexed = FALSE, meta = meta, graph_type = "Test type", 
                 iso_name = "Test iso", 
                 x_orig = 10, y_orig = 10, 
                 x = 20, y = 30)
  expect_equal(res$Case, "Test case")
  expect_equal(res$graph_type, "Test type")
  expect_equal(res$line_name, "Test iso")
  expect_equal(res$colour, "gray")
  expect_equal(res$size, 0.5)
  expect_equal(res$linetype, "solid")
  expect_equal(res$slope, -1)
  expect_equal(res$intercept, 50)

  # Now try with vectors, adding to the previous data frame.
  meta2 <- tibble::tibble(Case = c("Test case 1", "Test case 2"))
  res2 <- res %>% 
    add_iso(indexed = FALSE, meta = meta2, graph_type = "Test type 2", 
            iso_name = "Test iso 2", 
            x_orig = 10, y_orig = 10, 
            x = c(20, 30), y = c(30, 40))
  expect_equal(res2$slope, c(-1, -1, -1))
  expect_equal(res2$intercept, c(50, 50, 70))

  # Now try indexing
  res3 <- res %>% 
    add_iso(indexed = TRUE, meta = meta2, graph_type = "Test type 3", 
            iso_name = "Test iso 3", 
            x_orig = 10, y_orig = 10, 
            x = c(20, 30), y = c(30, 40))
  expect_equal(res3$slope, c(-1, -1, -1))
  expect_equal(res3$intercept, c(50, 5, 7))
})


test_that("indifference_func() works as expected", {
  expect_equal(indifference_func(qs_qs0 = 1, qs1_qs0 = 1, Co1_Co0 = 1, f_Cs = 0.25, sigma = 0.3), 1)
  # At point (1, 1), the result is independent of f_Cs and sigma.
  expect_equal(indifference_func(qs_qs0 = 1, qs1_qs0 = 1, Co1_Co0 = 1, f_Cs = 0.5, sigma = 0.3), 1)
  expect_equal(indifference_func(qs_qs0 = 1, qs1_qs0 = 1, Co1_Co0 = 1, f_Cs = 0.25, sigma = 0.8), 1)
  
  # Get a consistent set of parameters
  qs1_qs0 <- 1
  Co1_Co0 <- 1
  sigma <- 0.3
  f_Cs_orig <- 0.01
  rho <- (sigma-1)/sigma
  u1_u0 <- (f_Cs_orig*(qs1_qs0)^rho + (1 - f_Cs_orig)*(Co1_Co0)^rho)^(1/rho)
  
  x <- 1
  y <- indifference_func(qs_qs0 = x, qs1_qs0 = qs1_qs0, Co1_Co0 = Co1_Co0, f_Cs_orig = f_Cs_orig, sigma = sigma)
  expect_equal(y, 1)
  x <- 0.5
  y <- indifference_func(qs_qs0 = x, qs1_qs0 = qs1_qs0, Co1_Co0 = Co1_Co0, f_Cs_orig = f_Cs_orig, sigma = sigma)
  expect_equal(y, 1.01801496409513392294)
  
  # Make sure the function is vectorized
  x <- c(1, 0.5)
  y <- indifference_func(qs_qs0 = x, qs1_qs0 = qs1_qs0, Co1_Co0 = Co1_Co0, f_Cs_orig = f_Cs_orig, sigma = sigma)
  expect_equal(y, c(1, 1.01801496409513392294))
  
  # Try with different qs1_qs0 and Co1_Co0
  qs1_qs0 <- 20
  Co1_Co0 <- 1.1
  u1_u0 <- (f_Cs_orig*(qs1_qs0)^rho + (1 - f_Cs_orig)*(Co1_Co0)^rho)^(1/rho)
  x <- 1
  y <- indifference_func(qs_qs0 = x, qs1_qs0 = qs1_qs0, Co1_Co0 = Co1_Co0, f_Cs_orig = f_Cs_orig, sigma = sigma)
  expect_equal(y, 1.10599647717356)
})


test_that("add_indifference_curve() works as expected", {
  meta <- tibble::tibble(Case = "Test case")
  res <- add_indifference_curve(meta = meta, graph_type = "Test type", 
                 line_name = "Test indiff", 
                 qs1_qs0 = 5, Co1_Co0 = 10, f_Cs_orig = 0.1, sigma = 0.04)
  expect_equal(res$Case, "Test case")
  expect_equal(res$graph_type, "Test type")
  expect_equal(res$line_name, "Test indiff")
  expect_equal(res$colour, "gray")
  expect_equal(res$size, 0.5)
  expect_equal(res$linetype, "solid")
  expect_equal(res$qs1_qs0, 5)
  expect_equal(res$Co1_Co0, 10)
  expect_equal(res$f_Cs_orig, 0.1)
  expect_equal(res$sigma, 0.04)
})


test_that("indifference_lines() works as expected", {
  indiff_lines <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    indifference_lines()
  expect_equal(indiff_lines$line_name[[1]], ReboundTools::rebound_stages$orig)
  expect_equal(indiff_lines$graph_type[[1]], ReboundTools::graph_types$preferences)
  expect_equal(indiff_lines$qs1_qs0[[1]], 1)
  
  
  
  
  
})
