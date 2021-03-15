
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
  car_energy_points_hat <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == ReboundTools::rebound_stages$hat)
  expect_equal(car_energy_points_hat$x, 44362.00012)
  expect_equal(car_energy_points_hat$y, 77384.727788596195751)
  
  # Bar point (after income effect)
  car_energy_points_bar <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == ReboundTools::rebound_stages$bar)
  expect_equal(car_energy_points_bar$x, 45929.46762)
  expect_equal(car_energy_points_bar$y, 79685.765757904940983)
  
  # Tilde point (after productivity effect)
  car_energy_points_tilde <- energy_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Energy", point_name == ".last_point")
  expect_equal(car_energy_points_tilde$x, 45929.46762)
  expect_equal(car_energy_points_tilde$y, 82079.528960735886358)
  
  # Check point colours
  expect_equal(car_energy_points_orig$colour %>% unique(), 
               ReboundTools::path_graph_params$dempl_colour)
  expect_equal(car_energy_points_star$colour %>% unique(), 
               ReboundTools::path_graph_params$isub_colour)
  expect_equal(car_energy_points_hat$colour %>% unique(), 
               ReboundTools::path_graph_params$dinc_colour)
  expect_equal(car_energy_points_bar$colour %>% unique(), 
               ReboundTools::path_graph_params$prod_colour)
  # There are no tilde points in this version.
  # If there were a tilde point, it should have this colour
  # expect_true(is.na(car_energy_points_tilde$colour %>% unique()))
  
  # Calculate absolute cost paths
  cost_points_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    cost_paths() %>% 
    extract_points()
  
  # Orig point.
  car_cost_points_orig <- cost_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Cost", point_name == ReboundTools::rebound_stages$orig)
  expect_equal(car_cost_points_orig$x, 1275.17)
  expect_equal(car_cost_points_orig$y, 26126.10769302945846)
  
  # Star point.
  car_cost_points_star <- cost_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Cost", point_name == ReboundTools::rebound_stages$star)
  expect_equal(car_cost_points_star$x, 759.02976190476181273)
  expect_equal(car_cost_points_star$y, 25940.684414189981908)
  
  # Hat point.
  car_cost_points_hat <- cost_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Cost", point_name == ReboundTools::rebound_stages$hat)
  expect_equal(car_cost_points_hat$x, 774.27545561799774987)
  expect_equal(car_cost_points_hat$y, 25920.739701178415999)
  
  # Bar point.
  car_cost_points_bar <- cost_points_abs %>% 
    dplyr::filter(Case == "Car", graph_type == "Cost", point_name == ".last_point")
  # There is not a bar point.  There is a "last_point"
  expect_equal(car_cost_points_bar$x, 801.63336575829112007)
  expect_equal(car_cost_points_bar$y, 26599.644327271169459)
  

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

  # Hat point
  lamp_prefs_points_hat <- prefs_points %>% 
    dplyr::filter(Case == "Lamp", graph_type == "Preferences", point_name == ReboundTools::rebound_stages$hat)
  expect_equal(lamp_prefs_points_hat$x, 2.4344098152813411495)
  expect_equal(lamp_prefs_points_hat$y, 0.99984047291445865557)
  
  # bar point
  lamp_prefs_points_bar <- prefs_points %>% 
    dplyr::filter(Case == "Lamp", graph_type == "Preferences", point_name == ".last_point")
  # There is not a bar point.  There is a "last_point"
  expect_equal(lamp_prefs_points_bar$x, 2.4354635233051604715)
  expect_equal(lamp_prefs_points_bar$y, 1.0002732430759311288)
  
})

