test_that("add_points() works as expected", {
  meta <- tibble::tibble(Case = "Test case")
  res <- add_point(indexed = FALSE, meta = meta, graph_type = "Test type", 
                   point_name = "Test point", 
                   x_orig = 10, y_orig = 10, 
                   x = 20, y = 30)
  expect_equal(res$x, 20)
  expect_equal(res$y, 30)
  expect_equal(nrow(res), 1)
})


test_that("energy_points() works as expected", {
  # Calculate the absolute paths
  energy_points_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_points()
  
  # Check values on the car absolute energy path.
  car_energy_points_abs <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == "orig")
  expect_equal(car_energy_points_abs$x, 73060.68051000000559724867)
  expect_equal(car_energy_points_abs$y, 77316.81880725323571823537)
})
