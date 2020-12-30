test_that("iso_cost_lines() works as expected", {
  iso_cost_lines_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    iso_cost_lines()
  
    # Check values on the car absolute iso line.
    car_cost_iso_lines_abs <- iso_cost_lines_abs %>%
      dplyr::filter(Case == "Car", graph_type == "Cost", iso_name == "orig")
    expect_equal(car_cost_iso_lines_abs$m, -1)
    expect_equal(car_cost_iso_lines_abs$b, 27401.27769302945671370253)

    # Calculate the indexed iso cost line.
    car_cost_iso_lines_indexed <- load_eeu_data() %>%
      rebound_analysis() %>%
      iso_cost_lines(indexed = TRUE)
    car_cost_iso_lines_indexed <- car_cost_iso_lines_indexed %>%
      dplyr::filter(Case == "Car", graph_type == "Cost", iso_name == "orig")
    expect_equal(car_cost_iso_lines_indexed$m, -1)
    expect_equal(car_cost_iso_lines_indexed$b, 2)
})


test_that("add_iso() works as expected", {
  meta <- tibble::tibble(Case = "Test case")
  res <- add_iso(indexed = FALSE, meta = meta, graph_type = "Test type", 
                 iso_name = "Test iso", 
                 x_orig = 10, y_orig = 10, 
                 x = 20, y = 30)
  expect_equal(res$Case, "Test case")
  expect_equal(res$graph_type, "Test type")
  expect_equal(res$iso_name, "Test iso")
  expect_equal(res$colour, "gray")
  expect_equal(res$size, 0.5)
  expect_equal(res$linetype, "solid")
  expect_equal(res$m, -1)
  expect_equal(res$b, 50)

  # Now try with vectors, adding to the previous data frame.
  meta2 <- tibble::tibble(Case = c("Test case 1", "Test case 2"))
  res2 <- res %>% 
    add_iso(indexed = FALSE, meta = meta2, graph_type = "Test type 2", 
            iso_name = "Test iso 2", 
            x_orig = 10, y_orig = 10, 
            x = c(20, 30), y = c(30, 40))
  expect_equal(res2$m, c(-1, -1, -1))
  expect_equal(res2$b, c(50, 50, 70))

  # Now try indexing
  res3 <- res %>% 
    add_iso(indexed = TRUE, meta = meta2, graph_type = "Test type 3", 
            iso_name = "Test iso 3", 
            x_orig = 10, y_orig = 10, 
            x = c(20, 30), y = c(30, 40))
  expect_equal(res3$m, c(-1, -1, -1))
  expect_equal(res3$b, c(50, 5, 7))
})
