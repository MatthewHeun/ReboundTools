
test_that("path_graphs() works as expected", {
  # This is a mess, because all graphs are on the same plot.
  graphs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs()
  expect_true(!is.null(graphs))
  
  # Try with only one type of graph
  graphs_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(graph_types = "Energy")
  expect_true(!is.null(graphs_energy))
  expect_equal(graphs_energy$plot_env$.path_data$graph_type %>% unique() %>% as.character(), "Energy")
  
  # Try with only one case, Car Energy
  graphs_car_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Car", 
                   graph_types = "Energy")
  expect_true(!is.null(graphs_car_energy))
  expect_equal(graphs_car_energy$plot_env$.path_data$Case %>% unique(), "Car")
  expect_equal(graphs_car_energy$plot_env$.path_data$graph_type %>% unique() %>% as.character(), "Energy")

  # Try Car Cost
  graphs_car_cost <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Car", 
                   graph_types = "Cost")
  expect_true(!is.null(graphs_car_cost))
  expect_equal(graphs_car_cost$plot_env$.path_data$Case %>% unique(), "Car")
  expect_equal(graphs_car_cost$plot_env$.path_data$graph_type %>% unique() %>% as.character(), "Cost")
  
  # Try indexed Car Cost
  graphs_car_cost <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(indexed = TRUE,
                   cases = "Car", 
                   graph_types = "Cost")
  expect_true(!is.null(graphs_car_cost))
  expect_equal(graphs_car_cost$plot_env$.path_data$Case %>% unique(), "Car")
  expect_equal(graphs_car_cost$plot_env$.path_data$graph_type %>% unique() %>% as.character(), "Cost")

  # Eliminate the grids for Car Cost graph.
  graphs_car_cost_no_grids <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Car", 
                   graph_types = "Cost", 
                   grid_types = NULL)
  expect_true(!is.null(graphs_car_cost_no_grids))
  expect_equal(graphs_car_cost_no_grids$plot_env$.grid_data %>% nrow(), 0)
  
  # Try an Energy graph for lamps
  graphs_lamp_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Lamp", 
                   graph_types = "Energy")
  expect_true(!is.null(graphs_lamp_energy))
  expect_equal(graphs_lamp_energy$plot_env$.path_data$Case %>% unique(), "Lamp")
  expect_equal(graphs_lamp_energy$plot_env$.path_data$graph_type %>% unique() %>% as.character(), "Energy")
  
  # Try an indexed Energy graph for lamps
  graphs_indexed_lamp_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(indexed = TRUE, 
                   cases = "Lamp", 
                   graph_types = "Energy")
  expect_true(!is.null(graphs_indexed_lamp_energy))
  expect_equal(graphs_indexed_lamp_energy$plot_env$.path_data$Case %>% unique(), "Lamp")
  expect_equal(graphs_indexed_lamp_energy$plot_env$.path_data$graph_type %>% unique() %>% as.character(), "Energy")
  
  # Try two cases
  graphs_two_cases_indexed_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(indexed = TRUE,
                   graph_types = "Energy") +
    ggplot2::facet_wrap(facets = "Case")
  expect_true(!is.null(graphs_two_cases_indexed_energy))
  
  # Request both cases
  graphs_two_cases_indexed_energy_2 <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = c("Car", "Lamp"), 
                   indexed = TRUE,
                   graph_types = "Energy") +
    ggplot2::facet_wrap(facets = "Case")
  expect_true(!is.null(graphs_two_cases_indexed_energy_2))

  # Try a preferences graph for lamps
  graphs_lamp_prefs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Lamp", 
                   graph_types = "Preferences")
  expect_true(!is.null(graphs_lamp_prefs))
  
  # Try a preferences graph for lamps with fewer indifference curve points
  graph_prefs <- ReboundTools::path_graph_params
  graph_prefs$n_indiff_curve_points <- 200
  graphs_lamp_prefs_2 <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Lamp", 
                   graph_types = "Preferences", graph_params = graph_prefs) +  
    ggplot2::xlim(0.9, 2.5)
    ggplot2::ylim(0.99, 1.003)
  expect_true(!is.null(graphs_lamp_prefs_2))
  
})


test_that("rebound_graphs_helper() works as expected", {
  
  abs_energy_paths <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_paths()
  abs_energy_graph <- abs_energy_paths %>% 
    rebound_graphs_helper() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), cols = ggplot2::vars(graph_type))
  expect_true(!is.null(abs_energy_graph))
  
  indexed_energy_paths <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_paths(indexed = TRUE)
  indexed_energy_graph <- indexed_energy_paths %>% 
    rebound_graphs_helper() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(indexed_energy_graph))

  abs_cost_paths <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    cost_paths()
  abs_cost_graph <- abs_cost_paths %>% 
    rebound_graphs_helper() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), cols = ggplot2::vars(graph_type))
  expect_true(!is.null(abs_cost_graph))

  indexed_cost_paths <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    cost_paths(indexed = TRUE)  
  indexed_cost_graph <- indexed_cost_paths %>% 
    rebound_graphs_helper() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(indexed_cost_graph))
  
  abs_graph <- dplyr::bind_rows(abs_energy_paths, abs_cost_paths) %>% 
    dplyr::mutate(
      graph_type = factor(graph_type, levels = ReboundTools::graph_types)
    ) %>% 
    rebound_graphs_helper() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), cols = ggplot2::vars(graph_type))
  expect_true(!is.null(abs_graph))
  
  indexed_graph <- dplyr::bind_rows(indexed_energy_paths, indexed_cost_paths) %>%  
    rebound_graphs_helper() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(indexed_graph))
})


test_that("rebound_graphs_helper() works with grids", {
  rebound_data <- load_eeu_data() %>% 
    rebound_analysis()
  paths <- dplyr::bind_rows(rebound_data %>% energy_paths(), 
                            rebound_data %>% cost_paths())
  points_data <- extract_points(paths)
  abs_iso_grids <- rebound_data %>% 
    iso_cost_lines()
  abs_graph <- rebound_graphs_helper(paths, points_data, abs_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(abs_graph))
})


test_that("rebound_graphs_helper() works with a energy-only graph with grids", {
  rebound_data <- load_eeu_data() %>% 
    dplyr::filter(Case == "Car") %>% 
    rebound_analysis()
  paths <- rebound_data %>% 
    energy_paths()
  abs_iso_grids <- rebound_data %>%
    iso_energy_lines()
  abs_car_energy_graph <- rebound_graphs_helper(.path_data = paths, 
                                                .grid_data = abs_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(abs_car_energy_graph))
  
  # Now try with indexed data
  indexed_paths <- rebound_data %>% 
    energy_paths(indexed = TRUE)
  indexed_iso_grids <- rebound_data %>% 
    iso_energy_lines(indexed = TRUE)
  indexed_car_energy_graph <- rebound_graphs_helper(.path_data = indexed_paths, 
                                                    .grid_data = indexed_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(indexed_car_energy_graph))
  
})


test_that("rebound_graphs_helper() works with a cost-only graph with grids", {
  rebound_data <- load_eeu_data() %>% 
    dplyr::filter(Case == "Car") %>% 
    rebound_analysis()
  paths <- rebound_data %>% 
    cost_paths()
  abs_iso_grids <- rebound_data %>% 
    iso_cost_lines()
  abs_car_cost_graph <- rebound_graphs_helper(.path_data = paths, 
                                              .grid_data = abs_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") 
  expect_true(!is.null(abs_car_cost_graph))
  
  # Now try with indexed data
  indexed_paths <- rebound_data %>% 
    cost_paths(indexed = TRUE)
  indexed_iso_grids <- rebound_data %>% 
    iso_cost_lines(indexed = TRUE)
  indexed_car_cost_graph <- rebound_graphs_helper(.path_data = indexed_paths, 
                                                  .grid_data = indexed_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(indexed_car_cost_graph))
  
})


test_that("rebound_graphs_helper() works with a preferences graph with grids for car example", {
  rebound_data <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    dplyr::filter(Case == "Car")
  prefs_paths <- rebound_data %>% prefs_paths()
  prefs_grid <- rebound_data %>% iso_budget_lines_prefs()
  indiff_curve <- rebound_data %>% indifference_lines()
  
  graph <- rebound_graphs_helper(.path_data = prefs_paths, 
                                 .grid_data = prefs_grid,
                                 .indifference_data = indiff_curve) + 
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") + 
    ggplot2::xlim(0.99, 1.07) + 
    ggplot2::ylim(0.99, 1.05) +
    ggplot2::facet_wrap(facets = "Case")
  expect_true(!is.null(graph))
})


test_that("rebound_graphs_helper() works with a preferences graph with grids for lamp example", {
  rebound_data <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    dplyr::filter(Case == "Lamp")
  prefs_paths <- rebound_data %>% prefs_paths()
  prefs_grid <- rebound_data %>% iso_budget_lines_prefs()
  indiff_curve <- rebound_data %>% indifference_lines()
  
  graph <- rebound_graphs_helper(.path_data = prefs_paths, 
                                 .grid_data = prefs_grid, 
                                 .indifference_data = indiff_curve) + 
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") + 
    ggplot2::xlim(0.5, 3) +
    ggplot2::ylim(0.999, 1.001) +
    ggplot2::facet_wrap(facets = "Case")
  expect_true(!is.null(graph))
})


test_that("graphs work without arrows", {
  no_arrows <- ReboundTools::path_graph_params
  no_arrows$show_arrows <- FALSE
  # Try with only one case, Car Energy
  graphs_car_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Car", 
                   graph_types = "Energy", 
                   graph_params = no_arrows)
  expect_true(!is.null(graphs_car_energy))
  expect_equal(graphs_car_energy$plot_env$.path_data$Case %>% unique(), "Car")
  expect_equal(graphs_car_energy$plot_env$.path_data$graph_type %>% unique() %>% as.character(), "Energy")
})


test_that("points_atop_paths works as expected", {
  points_beneath_paths <- ReboundTools::path_graph_params
  points_beneath_paths$points_atop_paths <- FALSE
  # Try with only one case, Car Energy
  graphs_car_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Car", 
                   graph_types = "Energy", 
                   graph_params = points_beneath_paths)
  expect_true(!is.null(graphs_car_energy))
})


test_that("sensitivity_graphs() works as expected", {
  orig_data <- load_eeu_data()
  sens_params <- list(Car = list(k = seq(0.5, 1.5, by = 0.5)), 
                      Lamp = list(k = seq(0, 2, by = 1)))
  g <- sensitivity_graphs(rebound_data = orig_data, parameterization = sens_params, 
                          x_var = "k", y_vars = "Re_tot") +
    ggplot2::scale_colour_manual(values = c(Car = "black", Lamp = "black")) + 
    ggplot2::scale_size_manual(values = c(Car = 0.5, Lamp = 0.5)) + 
    ggplot2::scale_linetype_manual(values = c(Car = "solid", Lamp = "dashed")) + 
    ggplot2::labs(colour = ggplot2::element_blank(), 
                  size = ggplot2::element_blank(),
                  linetype = ggplot2::element_blank())
  expect_true(!is.null(g))
})


test_that("sensitivity_graphs() works with more than 1 line variation", {
  orig_data <- load_eeu_data()
  sens_params <- list(Car = list(k = seq(0, 2, by = 0.5), 
                                 I_E = seq(2, 5, by = 1), 
                                 e_qs_ps_UC = seq(-0.5, -0.1, by = 0.1)), 
                      Lamp = list(k = seq(0, 2, by = 0.5),
                                  I_E = seq(2, 5, by = 1), 
                                  e_qs_ps_UC = seq(-0.5, -0.1, by = 0.1)))
  g <- sensitivity_graphs(rebound_data = orig_data, parameterization = sens_params, 
                          x_var = "I_E", y_vars = "Re_tot") +
    ggplot2::facet_grid(rows = ggplot2::vars(k), 
                        cols = ggplot2::vars(e_qs_ps_UC)) +
    ggplot2::scale_colour_manual(values = c(Car = "darkgreen", Lamp = "black")) + 
    ggplot2::scale_size_manual(values = c(Car = 0.5, Lamp = 1)) + 
    ggplot2::scale_linetype_manual(values = c(Car = "solid", Lamp = "dotted")) + 
    ggplot2::labs(colour = ggplot2::element_blank(), 
                  size = ggplot2::element_blank(),
                  linetype = ggplot2::element_blank())
  expect_true(!is.null(g))
})
