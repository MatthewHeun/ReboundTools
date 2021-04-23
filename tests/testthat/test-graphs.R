
test_that("path_graphs() works as expected", {
  # This is a mess, because all graphs are on the same plot.
  graphs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs()
  expect_true(!is.null(graphs))
  
  # Try with only one type of graph
  graphs_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(graph_types = ReboundTools::graph_types$energy)
  expect_true(!is.null(graphs_energy))
  expect_equal(graphs_energy$plot_env$.path_data$graph_type %>% unique() %>% as.character(), 
               ReboundTools::graph_types$energy)
  
  # Try with only one case, Car energy
  graphs_car_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Car", 
                   graph_types = ReboundTools::graph_types$energy)
  expect_true(!is.null(graphs_car_energy))
  expect_equal(graphs_car_energy$plot_env$.path_data$Case %>% unique(), "Car")
  expect_equal(graphs_car_energy$plot_env$.path_data$graph_type %>% unique() %>% as.character(), 
               ReboundTools::graph_types$energy)

  # Try Car expenditures
  graphs_car_expenditures <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Car", 
                   graph_types = ReboundTools::graph_types$expenditure)
  expect_true(!is.null(graphs_car_expenditures))
  expect_equal(graphs_car_expenditures$plot_env$.path_data$Case %>% unique(), "Car")
  expect_equal(graphs_car_expenditures$plot_env$.path_data$graph_type %>% unique() %>% as.character(), 
               ReboundTools::graph_types$expenditure)
  
  # Try indexed Car expenditures
  graphs_car_expenditures <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(indexed = TRUE,
                   cases = "Car", 
                   graph_types = ReboundTools::graph_types$expenditure)
  expect_true(!is.null(graphs_car_expenditures))
  expect_equal(graphs_car_expenditures$plot_env$.path_data$Case %>% unique(), "Car")
  expect_equal(graphs_car_expenditures$plot_env$.path_data$graph_type %>% unique() %>% as.character(), 
               ReboundTools::graph_types$expenditure)

  # Eliminate the grids for Car expenditures graph.
  graphs_car_expenditure_no_grids <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Car", 
                   graph_types = ReboundTools::graph_types$expenditure, 
                   grid_types = NULL)
  expect_true(!is.null(graphs_car_expenditure_no_grids))
  expect_equal(graphs_car_expenditure_no_grids$plot_env$.grid_data %>% nrow(), 0)
  
  # Try an energy graph for lamps
  graphs_lamp_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Lamp", 
                   graph_types = ReboundTools::graph_types$energy)
  expect_true(!is.null(graphs_lamp_energy))
  expect_equal(graphs_lamp_energy$plot_env$.path_data$Case %>% unique(), "Lamp")
  expect_equal(graphs_lamp_energy$plot_env$.path_data$graph_type %>% unique() %>% as.character(), 
               ReboundTools::graph_types$energy)
  
  # Try an indexed energy graph for lamps
  graphs_indexed_lamp_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(indexed = TRUE, 
                   cases = "Lamp", 
                   graph_types = ReboundTools::graph_types$energy)
  expect_true(!is.null(graphs_indexed_lamp_energy))
  expect_equal(graphs_indexed_lamp_energy$plot_env$.path_data$Case %>% unique(), "Lamp")
  expect_equal(graphs_indexed_lamp_energy$plot_env$.path_data$graph_type %>% unique() %>% as.character(), 
               ReboundTools::graph_types$energy)
  
  # Try two cases
  graphs_two_cases_indexed_energy <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(indexed = TRUE,
                   graph_types = ReboundTools::graph_types$energy) +
    ggplot2::facet_wrap(facets = "Case")
  expect_true(!is.null(graphs_two_cases_indexed_energy))
  
  # Request both cases
  graphs_two_cases_indexed_energy_2 <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = c("Car", "Lamp"), 
                   indexed = TRUE,
                   graph_types = ReboundTools::graph_types$energy) +
    ggplot2::facet_wrap(facets = "Case")
  expect_true(!is.null(graphs_two_cases_indexed_energy_2))

  # Try a preferences graph for lamps
  graphs_lamp_prefs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Lamp", 
                graph_types = ReboundTools::graph_types$preferences)
  expect_true(!is.null(graphs_lamp_prefs))
  
  # Try a preferences graph for lamps with fewer indifference curve points
  graph_prefs <- ReboundTools::path_graph_params
  graph_prefs$n_indiff_curve_points <- 200
  graphs_lamp_prefs_2 <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(cases = "Lamp", graph_types = ReboundTools::graph_types$preferences, graph_params = graph_prefs) +  
    ggplot2::xlim(0.9, 2.5)
    ggplot2::ylim(0.99, 1.003)
  expect_true(!is.null(graphs_lamp_prefs_2))
})


test_that("path_graphs() works with grid_types = NULL", {
  graphs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    path_graphs(grid_types = NULL)
  expect_true(!is.null(graphs))
})


test_that("path_graphs() works with show_indifference_curves = FALSE", {
  pgp <- ReboundTools::path_graph_params
  pgp$show_indifference_curves <- FALSE
  graphs <- load_eeu_data() %>% 
    dplyr::filter(.data[[ReboundTools::eeu_base_params$case]] == "Lamp") %>% 
    rebound_analysis() %>% 
    path_graphs(graph_types = ReboundTools::graph_types$preferences, 
                graph_params = pgp)
  expect_true(!is.null(graphs))
})


test_that("path_graphs() works when the Reference column is missing", {
  graphs <- load_eeu_data() %>% 
    dplyr::select(-Reference) %>% 
    rebound_analysis() %>% 
    path_graphs()
  expect_true(!is.null(graphs))
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

  abs_expenditure_paths <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    expenditure_paths()
  abs_expenditure_graph <- abs_expenditure_paths %>% 
    rebound_graphs_helper() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), cols = ggplot2::vars(graph_type))
  expect_true(!is.null(abs_expenditure_graph))

  indexed_expenditure_paths <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    expenditure_paths(indexed = TRUE)  
  indexed_expenditure_graph <- indexed_expenditure_paths %>% 
    rebound_graphs_helper() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(indexed_expenditure_graph))
  
  abs_graph <- dplyr::bind_rows(abs_energy_paths, abs_expenditure_paths) %>% 
    dplyr::mutate(
      graph_type = factor(graph_type, levels = ReboundTools::graph_types)
    ) %>% 
    rebound_graphs_helper() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), cols = ggplot2::vars(graph_type))
  expect_true(!is.null(abs_graph))
  
  indexed_graph <- dplyr::bind_rows(indexed_energy_paths, indexed_expenditure_paths) %>%  
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
                            rebound_data %>% expenditure_paths())
  points_data <- extract_points(paths)
  abs_iso_grids <- rebound_data %>% 
    iso_expenditure_lines()
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


test_that("rebound_graphs_helper() works with a expenditure-only graph with grids", {
  rebound_data <- load_eeu_data() %>% 
    dplyr::filter(Case == "Car") %>% 
    rebound_analysis()
  paths <- rebound_data %>% 
    expenditure_paths()
  abs_iso_grids <- rebound_data %>% 
    iso_expenditure_lines()
  abs_car_expenditure_graph <- rebound_graphs_helper(.path_data = paths, 
                                              .grid_data = abs_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") 
  expect_true(!is.null(abs_car_expenditure_graph))
  
  # Now try with indexed data
  indexed_paths <- rebound_data %>% 
    expenditure_paths(indexed = TRUE)
  indexed_iso_grids <- rebound_data %>% 
    iso_expenditure_lines(indexed = TRUE)
  indexed_car_expenditure_graph <- rebound_graphs_helper(.path_data = indexed_paths, 
                                                  .grid_data = indexed_iso_grids) +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free")
  expect_true(!is.null(indexed_car_expenditure_graph))
  
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
                          x_var = "k", y_var = "Re_tot") +
    ggplot2::facet_wrap(facets = "Case", scales = "free_x") +
    ggplot2::scale_colour_manual(values = c(Re_tot = "black"), guide = FALSE) + 
    ggplot2::scale_size_manual(values = c(Re_tot = 0.5), guide = FALSE) + 
    ggplot2::scale_linetype_manual(values = c(Re_tot = "solid"), guide = FALSE) +
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
                     x_var = "I_E", y_var = "Re_tot", line_var = "Case") +
    ggplot2::facet_grid(rows = ggplot2::vars(k), 
                        cols = ggplot2::vars(e_qs_ps_UC), scales = "free_y") +
    ggplot2::scale_colour_manual(values = c(Car = "black", Lamp = "red")) + 
    ggplot2::scale_size_manual(values = c(Car = 0.5, Lamp = 1.0)) + 
    ggplot2::scale_linetype_manual(values = c(Car = "solid", Lamp = "dashed")) + 
    ggplot2::labs(colour = ggplot2::element_blank(), 
                  size = ggplot2::element_blank(),
                  linetype = ggplot2::element_blank())
  expect_true(!is.null(g))
})


test_that("rebound_terms_graph() works as expected", {
  df <- load_eeu_data()
  sens_params <- list(Car = list(eta_engr_units_star = seq(35, 50, by = 0.5)), 
                      Lamp = list(eta_engr_units_star = seq(70, 90, by = 5)))
  g <- rebound_terms_graph(rebound_data = df, parameterization = sens_params, 
                      x_var = "eta_engr_units_tilde") +
    ggplot2::facet_wrap(facets = "Case", scales = "free_x")
  expect_true(!is.null(g))
  g2 <- rebound_terms_graph(rebound_data = df, parameterization = sens_params, 
                           x_var = "eta_engr_units_tilde") +
    ggplot2::facet_wrap(facets = "Case", scales = "free_x")
  expect_true(!is.null(g2))
})


test_that("sensitivity graphs correctly order points", {
  df <- load_eeu_data()
  eta_sens_params = list(Car = list(eta_engr_units_star = seq(35, 50, by = 0.5)), 
                         Lamp = list(eta_engr_units_star = seq(70, 90, by = 5)))
  
  # Red dashes should lie atop the black line for the Lamp.
  g <- sensitivity_graphs(rebound_data = df, parameterization = eta_sens_params,
                     x_var = "eta_engr_units_star", y_var = c(ReboundTools::rebound_terms$Re_macro, 
                                                              ReboundTools::rebound_terms$Re_iinc)) +
    ggplot2::facet_wrap(facets = "Case", scales = "free_x") +
    ggplot2::scale_colour_manual(values = c(Re_macro = "black", Re_iinc = "red"), guide = FALSE) + 
    ggplot2::scale_size_manual(values = c(Re_macro = 0.5, Re_iinc = 0.5), guide = FALSE) + 
    ggplot2::scale_linetype_manual(values = c(Re_macro = "solid", Re_iinc = "dashed"), guide = FALSE) +
    ggplot2::labs(x = expression(tilde(eta)*" [mpg (Car) or lm/W (Lamp)]"),
                  y = expression(Re[tot]*" [-]"),
                  colour = ggplot2::element_blank(),
                  size = ggplot2::element_blank(),
                  linetype = ggplot2::element_blank())
  expect_true(!is.null(g))
})


test_that("a simple rebound_terms_graph works", {
  df <- load_eeu_data()
  sens_params <- list(Car = list(eta_engr_units_star = seq(35, 50, by = 0.5)), 
                      Lamp = list(eta_engr_units_star = seq(70, 90, by = 5)))
  g <- rebound_terms_graph(rebound_data = df, parameterization = sens_params, 
                           x_var = "eta_engr_units_tilde",
                           Re_terms = c(Re_tot = "Re_tot")) +
    ggplot2::facet_wrap(facets = "Case", scales = "free_x")
  expect_true(!is.null(g))
  g2 <- rebound_terms_graph(rebound_data = df, parameterization = sens_params, 
                            x_var = "eta_engr_units_tilde",
                            Re_terms = c(Re_dempl = "Re_dempl", "Re_emb", "Re_cap", "Re_md", "Re_empl",
                                         "Re_dsub", "Re_isub", "Re_sub",
                                         "Re_dinc", "Re_iinc", "Re_inc",
                                         "Re_macro",
                                         "Re_dir", "Re_indir",
                                         "Re_tot")
                      ) +
    ggplot2::facet_wrap(facets = "Case", scales = "free_x")
  expect_true(!is.null(g2))
})


test_that("points_atop_paths works for rebound_terms_graph()", {
  df <- load_eeu_data()
  sens_params <- list(Car = list(eta_engr_units_star = seq(35, 50, by = 0.5)), 
                      Lamp = list(eta_engr_units_star = seq(70, 90, by = 5)))
  graph_params <- ReboundTools::sens_graph_params
  graph_params[["points_atop_paths"]] <- FALSE
  
  g <- rebound_terms_graph(rebound_data = df, parameterization = sens_params, 
                           x_var = "eta_engr_units_tilde", 
                           graph_params = graph_params) +
    ggplot2::facet_wrap(facets = "Case", scales = "free_x")
  expect_true(!is.null(g))
})


test_that("LaTeX legends works as expected.", {
  Re_graph_params <- ReboundTools::sens_graph_params
  Re_graph_params[["include_x_axis"]] <- TRUE
  Re_graph_params[["use_latex_legend"]] <- TRUE
  
  df <- load_eeu_data()
  sens_params <- list(Car = list(eta_engr_units_star = seq(35, 50, by = 0.5)), 
                      Lamp = list(eta_engr_units_star = seq(70, 90, by = 5)))
  g <- rebound_terms_graph(rebound_data = df, parameterization = sens_params, 
                           graph_params = Re_graph_params,
                           x_var = "eta_engr_units_tilde",
                           Re_terms = c("Re_tot", "Re_md")) +
    ggplot2::facet_wrap(facets = "Case", scales = "free_x")
  expect_true(!is.null(g))
})

