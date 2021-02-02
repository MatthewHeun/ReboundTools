
test_that("energy_points() works as expected", {
  # Calculate the absolute paths
  energy_points_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_paths() %>% 
    energy_points()
  
  # Check values on the car absolute energy path.
  car_energy_points_abs <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == "orig")
  expect_equal(car_energy_points_abs$x, 73060.68051000000559724867)
  expect_equal(car_energy_points_abs$y, 77316.81880725323571823537)
  
  # Add more checks!

  
  
  
  # Fix failing test.
  
  # Create constants for segment_names and point_names
  # Use those constants in the *_paths and *_points functions.
  
  
})
