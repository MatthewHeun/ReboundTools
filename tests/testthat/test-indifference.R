

test_that("indifference_lines() works as expected", {
  indiff_lines <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    indifference_lines()
  expect_equal(indiff_lines$line_name[[1]], ReboundTools::rebound_stages$orig)
  expect_equal(indiff_lines$graph_type[[1]], ReboundTools::graph_types$consumption)
  expect_equal(indiff_lines$qs1_qs0[[1]], 1)
})


test_that("add_indifference_curve() works as expected", {
  meta <- tibble::tibble(Case = "Test case")
  res <- add_indifference_curve(meta = meta, graph_type = "Test type", 
                                line_name = "Test indiff", 
                                qs1_qs0 = 5, Co1_Co0 = 10, qs2_qs0 = 7, f_Cs_orig = 0.1, sigma = 0.04)
  expect_equal(res$Case %>% unique(), "Test case")
  expect_equal(res$graph_type %>% unique(), "Test type")
  expect_equal(res$line_name %>% unique(), "Test indiff")
  expect_equal(res$colour %>% unique(), "black")
  expect_equal(res$size %>% unique(), 0.5)
  expect_equal(res$linetype %>% unique(), "solid")
  expect_equal(res$qs1_qs0 %>% unique(), 5)
  expect_equal(res$Co1_Co0 %>% unique(), 10)
  expect_equal(res$f_Cs_orig %>% unique(), 0.1)
  expect_equal(res$sigma %>% unique(), 0.04)
  
  # Try with two cases
  meta2 <- tibble::tibble(Case = c("Case 1", "Case 2"))
  res2 <- add_indifference_curve(meta = meta2, graph_type = c("Preferences", "Preferences"), 
                                 line_name = c("Test indiff1", "Test indiff2"), 
                                 qs1_qs0 = c(1.5,1.6), Co1_Co0 = c(0.999,0.998), 
                                 qs2_qs0 = c(7,8), f_Cs_orig = c(0.1,0.2), sigma = c(0.04,0.05))
  expect_equal(res2[[eeu_base_params$case]] %>% unique, c("Case 1", "Case 2"))
  # Check the last rows for each case.
  res2 %>% 
    dplyr::filter(.data[[ReboundTools::eeu_base_params$case]] == "Case 1") %>% 
    magrittr::extract2(graph_df_colnames$x_col) %>% 
    magrittr::extract2(length(.)) %>% 
    expect_equal(16)
  res2 %>% 
    dplyr::filter(.data[[ReboundTools::eeu_base_params$case]] == "Case 2") %>% 
    magrittr::extract2(graph_df_colnames$x_col) %>% 
    magrittr::extract2(length(.)) %>% 
    expect_equal(80)
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


test_that("geom_seq() works as expected", {
  s <- geom_seq(from = 0.01, to = 10.01, n = 100)
  expect_equal(s[[1]], 0.01)
  expect_equal(s[[100]], 10.01)
  
  # Try with from == to
  expect_equal(geom_seq(from = 10, to = 10, n = 100), rep(10, times = 100))

  # Try with to < from
  s2 <- geom_seq(from = 10, to = 0.42, n = 100)
  expect_equal(s2[[1]], 10)
  expect_equal(s2[[100]], 0.42)
  
  # Try with n = 0
  expect_error(geom_seq(from = 10, to = 0.42, n = 0), "n >= 2 required in geom_seq")
  
  # Try with n = -1
  expect_error(geom_seq(from = 10, to = 0.42, n = -1), "n >= 2 required in geom_seq")

  # Try with n = 1
  expect_error(geom_seq(from = 10, to = 0.42, n = 1), "n >= 2 required in geom_seq")
})







