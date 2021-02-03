
test_that("extract_points() works as expected", {
  # Calculate absolute energy paths
  energy_points_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_paths() %>% 
    extract_points()
  
  # Orig point.
  car_energy_points_orig <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == ReboundTools::rebound_stages$orig)
  expect_equal(car_energy_points_orig$x, 73060.68051000000559724867)
  expect_equal(car_energy_points_orig$y, 77316.81880725323571823537)
  
  # Star point (after emplacement effect)
  car_energy_points_star <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == ReboundTools::rebound_stages$star)
  expect_equal(car_energy_points_star$x, 43488.5)
  expect_equal(car_energy_points_star$y, 77452.327183507833979)

  # Hat point (after substitution effect)
  car_energy_points_star <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == ReboundTools::rebound_stages$hat)
  expect_equal(car_energy_points_star$x, 44362.00012)
  expect_equal(car_energy_points_star$y, 77384.727788596195751)
  
  # Bar point (after income effect)
  car_energy_points_bar <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == ReboundTools::rebound_stages$bar)
  expect_equal(car_energy_points_bar$x, 45929.46762)
  expect_equal(car_energy_points_bar$y, 79685.765757904940983)
  
  # Tilde point (after productivity effect)
  car_energy_points_tilde <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == ReboundTools::rebound_stages$tilde)
  expect_equal(car_energy_points_tilde$x, 45929.46762)
  expect_equal(car_energy_points_tilde$y, 82079.528960735886358)
  
  
  # Calculate absolute cost paths
  cost_points_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    cost_paths() %>% 
    extract_points()
  
  
  # Calculate preferences paths
  prefs_points <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    prefs_paths() %>% 
    extract_points()
  
  # Star point.
  lamp_prefs_points_star <- prefs_points %>% 
    dplyr::filter(Case == "Lamp", graph_type == "Preferences", point_name == ReboundTools::rebound_stages$star)
  expect_equal(lamp_prefs_points_star$x, 1)
  expect_equal(lamp_prefs_points_star$y, 1)
  
  
  
})

