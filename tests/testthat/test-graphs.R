test_that("rebound_paths() works as expected", {
  paths <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    rebound_paths()
  
  
  expect_equal(paths)
  
  
})


test_that("graphs works as expected", {
  load_eeu_data() %>% 
    rebound_analysis() %>% 
    rebound_paths() %>% 
    rebound_graphs() +
    ggplot2::facet_grid(rows = ggplot2::vars(graph_type), cols = ggplot2::vars(Case))
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
