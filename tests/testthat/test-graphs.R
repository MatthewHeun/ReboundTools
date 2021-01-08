
test_that("rebound_graphs() works as expected", {
  # This is a mess, because all graphs are on the same plot.
  graphs <- load_eeu_data() %>% 
    rebound_graphs()
  expect_true(!is.null(graphs))
  
  # Try with only one type of graph
  graphs_energy <- load_eeu_data() %>% 
    rebound_graphs(graph_types = "Energy")
  expect_true(!is.null(graphs_energy))
  expect_equal(graphs_energy$plot_env$.path_data$graph_type %>% unique(), "Energy")
  
  # Try with only one case, Car Energy
  graphs_car_energy <- load_eeu_data() %>% 
    rebound_graphs(cases = "Car", 
                   graph_types = "Energy")
  expect_true(!is.null(graphs_car_energy))
  expect_equal(graphs_car_energy$plot_env$.path_data$Case %>% unique(), "Car")
  expect_equal(graphs_car_energy$plot_env$.path_data$graph_type %>% unique(), "Energy")

  # Try Car Cost
  graphs_car_cost <- load_eeu_data() %>% 
    rebound_graphs(cases = "Car", 
                   graph_types = "Cost")
  expect_true(!is.null(graphs_car_cost))
  expect_equal(graphs_car_cost$plot_env$.path_data$Case %>% unique(), "Car")
  expect_equal(graphs_car_cost$plot_env$.path_data$graph_type %>% unique(), "Cost")
  
  # Try indexed Car Cost
  graphs_car_cost <- load_eeu_data() %>% 
    rebound_graphs(indexed = TRUE,
                   cases = "Car", 
                   graph_types = "Cost")
  expect_true(!is.null(graphs_car_cost))
  expect_equal(graphs_car_cost$plot_env$.path_data$Case %>% unique(), "Car")
  expect_equal(graphs_car_cost$plot_env$.path_data$graph_type %>% unique(), "Cost")

  # Eliminate the grids for Car Cost graph.
  graphs_car_cost_no_grids <- load_eeu_data() %>% 
    rebound_graphs(cases = "Car", 
                   graph_types = "Cost", 
                   grid_types = NULL)
  expect_true(!is.null(graphs_car_cost_no_grids))
  expect_equal(graphs_car_cost_no_grids$plot_env$.grid_data %>% nrow(), 0)
  
  # Try an Energy graph for lamps
  graphs_lamp_energy <- load_eeu_data() %>% 
    rebound_graphs(cases = "Lamp", 
                   graph_types = "Energy")
  expect_true(!is.null(graphs_lamp_energy))
  expect_equal(graphs_lamp_energy$plot_env$.path_data$Case %>% unique(), "Lamp")
  expect_equal(graphs_lamp_energy$plot_env$.path_data$graph_type %>% unique(), "Energy")
  
  # Try an indexed Energy graph for lamps
  graphs_indexed_lamp_energy <- load_eeu_data() %>% 
    rebound_graphs(indexed = TRUE, 
                   cases = "Lamp", 
                   graph_types = "Energy")
  expect_true(!is.null(graphs_indexed_lamp_energy))
  expect_equal(graphs_indexed_lamp_energy$plot_env$.path_data$Case %>% unique(), "Lamp")
  expect_equal(graphs_indexed_lamp_energy$plot_env$.path_data$graph_type %>% unique(), "Energy")
  
  # Try two cases
  graphs_two_cases_indexed_energy <- load_eeu_data() %>% 
    rebound_graphs(indexed = TRUE,
                   graph_types = "Energy") +
    ggplot2::facet_wrap(facets = "Case")
  expect_true(!is.null(graphs_two_cases_indexed_energy))

  # Try a preferences graph for lamps
  graphs_lamp_prefs <- load_eeu_data() %>% 
    rebound_graphs(cases = "Lamp", 
                   graph_types = "Preferences")
  expect_true(!is.null(graphs_lamp_prefs))
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
  abs_iso_grids <- rebound_data %>% 
    iso_cost_lines()
  abs_graph <- rebound_graphs_helper(paths, abs_iso_grids) +
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
  abs_car_energy_graph <- rebound_graphs_helper(paths, abs_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(abs_car_energy_graph))
  
  # Now try with indexed data
  indexed_paths <- rebound_data %>% 
    energy_paths(indexed = TRUE)
  indexed_iso_grids <- rebound_data %>% 
    iso_energy_lines(indexed = TRUE)
  indexed_car_energy_graph <- rebound_graphs_helper(indexed_paths, indexed_iso_grids) +
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
  abs_car_cost_graph <- rebound_graphs_helper(paths, abs_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") 
  expect_true(!is.null(abs_car_cost_graph))
  
  # Now try with indexed data
  indexed_paths <- rebound_data %>% 
    cost_paths(indexed = TRUE)
  indexed_iso_grids <- rebound_data %>% 
    iso_cost_lines(indexed = TRUE)
  indexed_car_cost_graph <- rebound_graphs_helper(indexed_paths, indexed_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(indexed_car_cost_graph))
  
})


test_that("rebound_graphs_helper() works with a preferences graph with grids", {
  rebound_data <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    dplyr::filter(Case == "Car")
  prefs_paths <- rebound_data %>% prefs_paths()
  prefs_grid <- rebound_data %>% iso_budget_lines_prefs()
  
  graph <- rebound_graphs_helper(prefs_paths, prefs_grid) + 
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") + 
    ggplot2::scale_x_continuous(name = "q_dot_s/q_dot_s_orig") +
    ggplot2::scale_y_continuous(name = "C_dot_o/C_dot_o_orig")
  
  expect_true(!is.null(graph))
})


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

