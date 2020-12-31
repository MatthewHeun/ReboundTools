




test_that("rebound_graphs() graphs works as expected", {
  
  abs_energy_paths <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_paths()
  abs_energy_graph <- abs_energy_paths %>% 
    rebound_graphs() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), cols = ggplot2::vars(graph_type))
  expect_true(!is.null(abs_energy_graph))
  
  indexed_energy_paths <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_paths(indexed = TRUE)
  indexed_energy_graph <- indexed_energy_paths %>% 
    rebound_graphs() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(indexed_energy_graph))

  abs_cost_paths <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    cost_paths()
  abs_cost_graph <- abs_cost_paths %>% 
    rebound_graphs() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), cols = ggplot2::vars(graph_type))
  expect_true(!is.null(abs_cost_graph))

  indexed_cost_paths <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    cost_paths(indexed = TRUE)  
  indexed_cost_graph <- indexed_cost_paths %>% 
    rebound_graphs() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(indexed_cost_graph))
  
  abs_graph <- dplyr::bind_rows(abs_energy_paths, abs_cost_paths) %>% 
    dplyr::mutate(
      graph_type = factor(graph_type, levels = ReboundTools::graph_types)
    ) %>% 
    rebound_graphs() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), cols = ggplot2::vars(graph_type))
  expect_true(!is.null(abs_graph))
  
  indexed_graph <- dplyr::bind_rows(indexed_energy_paths, indexed_cost_paths) %>%  
    rebound_graphs() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") + 
    MKHthemes::xy_theme()
  expect_true(!is.null(indexed_graph))
})


test_that("rebound_graphs() works with grids", {
  rebound_data <- load_eeu_data() %>% 
    rebound_analysis()
  paths <- dplyr::bind_rows(rebound_data %>% energy_paths(), 
                            rebound_data %>% cost_paths())
  abs_iso_grids <- rebound_data %>% 
    iso_cost_lines()
  abs_graph <- rebound_graphs(paths, abs_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") + 
    MKHthemes::xy_theme()
  expect_true(!is.null(abs_graph))
})


test_that("rebound_graphs() works with a energy-only graph with grids", {
  rebound_data <- load_eeu_data() %>% 
    dplyr::filter(Case == "Car") %>% 
    rebound_analysis()
  paths <- rebound_data %>% 
    energy_paths()
  abs_iso_grids <- rebound_data %>%
    iso_energy_lines()
  abs_car_energy_graph <- rebound_graphs(paths, abs_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") + 
    MKHthemes::xy_theme()
  expect_true(!is.null(abs_car_energy_graph))
  
  # Now try with indexed data
  indexed_paths <- rebound_data %>% 
    energy_paths(indexed = TRUE)
  indexed_iso_grids <- rebound_data %>% 
    iso_energy_lines(indexed = TRUE)
  indexed_car_energy_graph <- rebound_graphs(indexed_paths, indexed_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") + 
    MKHthemes::xy_theme()
  expect_true(!is.null(indexed_car_energy_graph))
  
})


test_that("rebound_graphs() works with a cost-only graph with grids", {
  rebound_data <- load_eeu_data() %>% 
    dplyr::filter(Case == "Car") %>% 
    rebound_analysis()
  paths <- rebound_data %>% 
    cost_paths()
  abs_iso_grids <- rebound_data %>% 
    iso_cost_lines()
  abs_car_cost_graph <- rebound_graphs(paths, abs_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") + 
    MKHthemes::xy_theme()
  expect_true(!is.null(abs_car_cost_graph))
  
  # Now try with indexed data
  indexed_paths <- rebound_data %>% 
    cost_paths(indexed = TRUE)
  indexed_iso_grids <- rebound_data %>% 
    iso_cost_lines(indexed = TRUE)
  indexed_car_cost_graph <- rebound_graphs(indexed_paths, indexed_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") + 
    MKHthemes::xy_theme()
  expect_true(!is.null(indexed_car_cost_graph))
  
})


test_that("rebound_graphs() works with a preferences graph with grids", {
  graph <- rebound_data <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    dplyr::filter(Case == "Car")
  prefs_paths <- rebound_data %>% prefs_paths()
  prefs_grid <- rebound_data %>% iso_budget_lines_prefs()
  
  rebound_graphs(prefs_paths, prefs_grid) + 
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") + 
    ggplot2::scale_x_continuous(name = "q_dot_s/q_dot_s_orig") +
    ggplot2::scale_y_continuous(name = "C_dot_o/C_dot_o_orig")+
    MKHthemes::xy_theme()
  
  expect_true(!is.null(graph))
})



