
test_that("energy_points() works as expected", {
  # Calculate the absolute paths
  energy_points_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_paths() %>% 
    energy_points()
  
  # Check values on the car absolute energy path.
  car_energy_points_orig <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == "orig")
  expect_equal(car_energy_points_orig$x, 73060.68051000000559724867)
  expect_equal(car_energy_points_orig$y, 77316.81880725323571823537)
  
  # Add more checks!
  car_energy_points_star <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == "star")
  expect_equal(car_energy_points_star$x, 43488.5)
  expect_equal(car_energy_points_star$y, 77452.327183507833979)
  

  
})
