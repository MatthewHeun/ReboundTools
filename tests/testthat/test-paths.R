test_that("extract_meta() works as expected.", {
  m <- load_eeu_data() %>% 
    extract_meta()
  expect_equal(colnames(m), c(ReboundTools::eeu_base_params$reference,
                              ReboundTools::eeu_base_params$case, 
                              ReboundTools::eeu_base_params$original, 
                              ReboundTools::eeu_base_params$upgrade))
})


test_that("energy_paths() works as expected", {
  # Calculate the absolute paths
  energy_paths_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_paths()
  
  # Check values on the car absolute energy path.
  car_energy_path_abs <- energy_paths_abs %>% 
    dplyr::filter(Case == "Car",
                  graph_type == "Energy",
                  line_name == ReboundTools::rebound_segments$dempl)
  expect_equal(car_energy_path_abs$x, 73060.68051000000559724867)
  expect_equal(car_energy_path_abs$xend, 43488.50030357141804415733)
  expect_equal(car_energy_path_abs$y, 77316.81880725323571823537)
  expect_equal(car_energy_path_abs$yend, 77316.81880725323571823537)
})


test_that("expenditure_paths() works as expected", {
  # Calculate the absolute paths
  expenditure_paths_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    expenditure_paths()
  
  # Check values on the car absolute cost path.
  car_cost_path_abs <- expenditure_paths_abs %>% 
    dplyr::filter(Case == "Car", graph_type == ReboundTools::graph_types$expenditure, 
                  line_name == ReboundTools::rebound_segments$dempl)
  expect_equal(car_cost_path_abs$x, 1275.17)
  expect_equal(car_cost_path_abs$xend, 759.02976190476181272970)
  expect_equal(car_cost_path_abs$y, 26126.10769302945845993236)
  expect_equal(car_cost_path_abs$yend, 26126.10769302945845993236)
  
  # Calculate the indexed paths
  expenditure_paths_indexed <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    expenditure_paths(indexed = TRUE)
  car_expenditure_paths_indexed <- expenditure_paths_indexed %>% 
    dplyr::filter(Case == "Car", graph_type == ReboundTools::graph_types$expenditure,
                  line_name == ReboundTools::rebound_segments$dempl)
  expect_equal(car_expenditure_paths_indexed$x, 1)
  expect_equal(car_expenditure_paths_indexed$xend, 0.59523809523809512179)
  expect_equal(car_expenditure_paths_indexed$y, 1)
  expect_equal(car_expenditure_paths_indexed$yend, 1)
})


test_that("prefs_paths() works as expected with approximated hat", {
  prefs_paths <- load_eeu_data() %>% 
    rebound_analysis(use_sub_approx = TRUE) %>% 
    prefs_paths()
  
  # Check values on the lighting graph.
  lighting_prefs_path <- prefs_paths %>% 
    dplyr::filter(Case == "Lamp", graph_type == "Preferences", line_name == ReboundTools::rebound_segments$isub)
  expect_equal(lighting_prefs_path$x, 1)
  expect_equal(lighting_prefs_path$y, 1)
  expect_equal(lighting_prefs_path$xend, 1)
  expect_equal(lighting_prefs_path$yend, 0.99971092431922392585)
  
  lighting_prefs_path2 <- prefs_paths %>% 
    dplyr::filter(Case == "Lamp", graph_type == "Preferences", line_name == ReboundTools::rebound_segments$dsub)
  expect_equal(lighting_prefs_path2$x, 1)
  expect_equal(lighting_prefs_path2$y, 0.99971092431922392585)
  expect_equal(lighting_prefs_path2$xend, 2.43409438974531644462)
  expect_equal(lighting_prefs_path2$yend, 0.99971092431922392585)
})


test_that("add_segment() works as expected", {
  meta <- tibble::tibble(Case = "Test case")
  res <- add_segment(indexed = FALSE, meta = meta, graph_type = "Test type", 
                     segment_name = "Test segment", 
                     x_orig = 10, y_orig = 10, 
                     x = 20, y = 30, xend = 40, yend = 50)
  expect_equal(res$Case, "Test case")
  expect_equal(res$graph_type, "Test type")
  expect_equal(res$line_name, "Test segment")
  expect_equal(res$colour, "black")
  expect_equal(res$linetype, "solid")
  expect_equal(res$x, 20)
  expect_equal(res$y, 30)
  expect_equal(res$xend, 40)
  expect_equal(res$yend, 50)
  
  # Now try with vectors, adding to the previous data frame
  meta2 <- tibble::tibble(Case = c("Test case 1", "Test case 2"))
  res2 <- res %>% 
    add_segment(indexed = FALSE, meta = meta2, graph_type = "Test type 2", 
                segment_name = "Test segment 2", 
                x_orig = 10, y_orig = 10, 
                x = c(20, 30), y = c(30, 40), xend = c(40, 50), yend = c(50, 60))
  expect_equal(res2$x, c(20, 20, 30))
  expect_equal(res2$y, c(30, 30, 40))
  expect_equal(res2$xend, c(40, 40, 50))
  expect_equal(res2$yend, c(50, 50, 60))
  
  # Now try indexing
  res3 <- res %>% 
    add_segment(indexed = TRUE, meta = meta2, graph_type = "Test type 3", 
                segment_name = "Test segment 3", 
                x_orig = 10, y_orig = 10, 
                x = c(20, 30), y = c(30, 40), xend = c(40, 50), yend = c(50, 60))
  expect_equal(res3$x, c(20, 2, 3))
  expect_equal(res3$y, c(30, 3, 4))
  expect_equal(res3$xend, c(40, 4, 5))
  expect_equal(res3$yend, c(50, 5, 6))
})


test_that("energy path creation works with reverse drawing order", {
  reverse_order <- ReboundTools::path_graph_params
  expect_false(reverse_order$reverse_path_drawing_order)
  reverse_order$reverse_path_drawing_order <- TRUE
  
  ep <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_paths()
  
  expected <- ReboundTools::rebound_segments
  expected$cap <- NULL
  expected <- expected %>% unlist() %>% unname()
  expect_equal(ep$line_name %>% unique(), expected)
  
  
  ep_rev <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_paths(graph_params = reverse_order)
  expect_equal(ep_rev$line_name %>% unique(), rev(expected))
})


test_that("cost path creation works with reverse drawing order", {
  reverse_order <- ReboundTools::path_graph_params
  expect_false(reverse_order$reverse_path_drawing_order)
  reverse_order$reverse_path_drawing_order <- TRUE
  
  cp <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    expenditure_paths()
  
  expected <- ReboundTools::rebound_segments
  expected$emb <- NULL
  expected$macro <- NULL
  expected <- expected %>% unlist() %>% unname()
  expect_equal(cp$line_name %>% unique(), expected)
  
  
  cp_rev <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    expenditure_paths(graph_params = reverse_order)
  expect_equal(cp_rev$line_name %>% unique(), rev(expected))
})


test_that("prefs path creation works with reverse drawing order", {
  reverse_order <- ReboundTools::path_graph_params
  expect_false(reverse_order$reverse_path_drawing_order)
  reverse_order$reverse_path_drawing_order <- TRUE
  
  pp <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    prefs_paths()
  
  expected <- ReboundTools::rebound_segments
  expected$dempl <- NULL
  expected$cap <- NULL
  expected$md <- NULL
  expected$emb <- NULL
  expected$macro <- NULL
  expected <- expected %>% unlist() %>% unname()
  expect_equal(pp$line_name %>% unique(), expected)
  
  
  pp_rev <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    prefs_paths(graph_params = reverse_order)
  expect_equal(pp_rev$line_name %>% unique(), rev(expected))
})

