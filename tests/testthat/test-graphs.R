test_that("rebound_paths() works as expected", {
  # Calculate the absolute paths
  paths_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    rebound_paths()
  
  # Check values on the car absolute cost path.
  car_cost_path_abs <- paths_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Cost", segment_name == "G_dot")
  expect_equal(car_cost_path_abs$x, 1275.17)
  expect_equal(car_cost_path_abs$xend, 759.02976190476181272970)
  expect_equal(car_cost_path_abs$y, 26126.10769302945845993236)
  expect_equal(car_cost_path_abs$yend, 26126.10769302945845993236)
  
  # Calculate the indexed paths
  paths_indexed <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    rebound_paths(indexed = TRUE)
  car_cost_path_indexed <- paths_indexed %>% 
    dplyr::filter(Case == "Car", graph_type == "Cost", segment_name == "G_dot")
  expect_equal(car_cost_path_indexed$x, 1)
  expect_equal(car_cost_path_indexed$xend, 0.59523809523809512179)
  expect_equal(car_cost_path_indexed$y, 1)
  expect_equal(car_cost_path_indexed$yend, 1)
})


test_that("graphs works as expected", {
  
  abs_graph <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    rebound_paths() %>% 
    dplyr::mutate(
      graph_type = factor(graph_type, levels = c("Energy", "Cost"))
    ) %>% 
    rebound_graphs() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), cols = ggplot2::vars(graph_type))
  
  expect_true(!is.null(abs_graph))
  
  indexed_graph <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    rebound_paths(indexed = TRUE) %>% 
    rebound_graphs() +
    ggplot2::facet_grid(rows = ggplot2::vars(Case), 
                        cols = ggplot2::vars(graph_type), 
                        scales = "free") + 
    # ggplot2::scale_x_continuous(limits = c(0, 2)) +
    MKHthemes::xy_theme()
  
  
  expect_true(!is.null(indexed_graph))
})



test_that("add_segment() works as expected", {
  meta <- tibble::tibble(Case = "Test case")
  res <- add_segment(indexed = FALSE, meta = meta, graph_type = "Test type", 
                     segment_name = "Test segment", 
                     x_orig = 10, y_orig = 10, 
                     x = 20, y = 30, xend = 40, yend = 50)
  expect_equal(res$Case, "Test case")
  expect_equal(res$graph_type, "Test type")
  expect_equal(res$segment_name, "Test segment")
  expect_equal(res$colour, "black")
  expect_equal(res$linetype, "solid")
  expect_equal(res$x, 20)
  expect_equal(res$y, 30)
  expect_equal(res$xend, 40)
  expect_equal(res$yend, 50)
  
  # Now try with vectors, adding 
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

  
  # Now try normalizing
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
