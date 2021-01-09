test_that("indifference_fun() works as expected", {
  qs1_qs0 <- 1
  Co1_Co0 <- 1
  sigma <- 0.5
  f_Cs <- 0.01
  
  g <- ggplot2::ggplot() +
    ggplot2::stat_function(data = data.frame(x = c(0.1, 10)),
                           mapping = ggplot2::aes(x = x), 
                           fun = indifference_func, 
                           args = c(qs1_qs0 = qs1_qs0, Co1_Co0 = Co1_Co0,
                                    f_Cs = f_Cs, sigma = sigma))
  expect_true(!is.null(g))
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
  expect_equal(res$size, 0.1)
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


# test_that("indifference curves graph properly", {
#   rebound_data <- load_eeu_data() %>% 
#     rebound_analysis() %>% 
#     dplyr::filter(Case == "Lamp")
#   prefs_paths <- rebound_data %>% prefs_paths()
#   prefs_grid <- rebound_data %>% iso_budget_lines_prefs()
#   prefs_indiff <- rebound_data %>% indifference_lines()
#   
#   rebound_graphs(prefs_paths, NULL, prefs_indiff) + 
#     ggplot2::facet_grid(rows = ggplot2::vars(Case), 
#                         cols = ggplot2::vars(graph_type), 
#                         scales = "free") + 
#     ggplot2::scale_x_continuous(name = "q_dot_s/q_dot_s_orig") +
#     ggplot2::scale_y_continuous(name = "C_dot_o/C_dot_o_orig")+
#     MKHthemes::xy_theme()
# })