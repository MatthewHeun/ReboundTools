
test_that("extract_points() works as expected", {
  # Calculate absolute energy paths
  energy_points_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    energy_paths() %>% 
    extract_points()
  
  # Orig point.
  car_energy_points_orig <- energy_points_abs %>% 
    dplyr::filter(Case == "Car",
                  graph_type == ReboundTools::graph_types$energy,
                  point_name == ReboundTools::rebound_stages$orig)
  expect_equal(car_energy_points_orig$x, 73060.68051000000559724867)
  expect_equal(car_energy_points_orig$y, 82759.2731666039326228)
  
  # Star point (after emplacement effect)
  car_energy_points_star <- energy_points_abs %>% 
    dplyr::filter(Case == "Car",
                  graph_type == ReboundTools::graph_types$energy,
                  point_name == ReboundTools::rebound_stages$star)
  expect_equal(car_energy_points_star$x, 43488.5)
  expect_equal(car_energy_points_star$y, 82894.9741308264492545)

  # Hat point (after substitution effect)
  car_energy_points_hat <- energy_points_abs %>% 
    dplyr::filter(Case == "Car",
                  graph_type == ReboundTools::graph_types$energy,
                  point_name == ReboundTools::rebound_stages$hat)
  expect_equal(car_energy_points_hat$x, 44466.0599973140051588)
  expect_equal(car_energy_points_hat$y, 82819.3230997071950696)
  
  # Bar point (after income effect)
  car_energy_points_bar <- energy_points_abs %>% 
    dplyr::filter(Case == "Car",
                  graph_type == ReboundTools::graph_types$energy,
                  point_name == ReboundTools::rebound_stages$bar)
  expect_equal(car_energy_points_bar$x, 45840.6306701721041463)
  expect_equal(car_energy_points_bar$y, 84999.8641219453711528)
  
  # Tilde point (after productivity effect)
  car_energy_points_tilde <- energy_points_abs %>% 
    dplyr::filter(Case == "Car",
                  graph_type == ReboundTools::graph_types$energy,
                  point_name == ".last_point")
  expect_equal(car_energy_points_tilde$x, 45840.6306701721041463)
  expect_equal(car_energy_points_tilde$y, 87261.7193574736884329)
  
  # Check point colours
  expect_equal(car_energy_points_orig$colour %>% unique(), 
               ReboundTools::path_graph_params$dempl_colour)
  expect_equal(car_energy_points_star$colour %>% unique(), 
               ReboundTools::path_graph_params$isub_colour)
  expect_equal(car_energy_points_hat$colour %>% unique(), 
               ReboundTools::path_graph_params$dinc_colour)
  expect_equal(car_energy_points_bar$colour %>% unique(), 
               ReboundTools::path_graph_params$macro_colour)
  # There are no tilde points in this version.
  # If there were a tilde point, it should have this colour
  # expect_true(is.na(car_energy_points_tilde$colour %>% unique()))
  
  # Calculate absolute expenditure paths
  expenditure_points_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    expenditure_paths() %>% 
    extract_points()
  
  # Orig point.
  car_expenditure_points_orig <- expenditure_points_abs %>% 
    dplyr::filter(Case == "Car",
                  graph_type == ReboundTools::graph_types$expenditure,
                  point_name == ReboundTools::rebound_stages$orig)
  expect_equal(car_expenditure_points_orig$x, 1275.17)
  expect_equal(car_expenditure_points_orig$y, 26126.1076930294584599)
  
  # Star point.
  car_expenditure_points_star <- expenditure_points_abs %>% 
    dplyr::filter(Case == "Car", 
                  graph_type == ReboundTools::graph_types$expenditure,
                  point_name == ReboundTools::rebound_stages$star)
  expect_equal(car_expenditure_points_star$x, 759.02976190476181273)
  expect_equal(car_expenditure_points_star$y, 25980.1622670666984050)
  
  # Hat point.
  car_expenditure_points_hat <- expenditure_points_abs %>% 
    dplyr::filter(Case == "Car", 
                  graph_type == ReboundTools::graph_types$expenditure,
                  point_name == ReboundTools::rebound_stages$hat)
  expect_equal(car_expenditure_points_hat$x, 776.0916724422514790)
  expect_equal(car_expenditure_points_hat$y, 25957.8419769712781999)
  
  # Bar point.
  car_expenditure_points_bar <- expenditure_points_abs %>% 
    dplyr::filter(Case == "Car",
                  graph_type == ReboundTools::graph_types$expenditure,
                  point_name == ".last_point")
  # There is not a bar point.  There is a "last_point"
  expect_equal(car_expenditure_points_bar$x, 800.0828435163911081)
  expect_equal(car_expenditure_points_bar$y, 26601.1948495130709489)
  

  # Calculate consumption paths
  cons_points <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    consumption_paths() %>% 
    extract_points()
  
  # Star point.
  lamp_cons_points_star <- cons_points %>% 
    dplyr::filter(Case == "Lamp", 
                  graph_type == ReboundTools::graph_types$consumption,
                  point_name == ReboundTools::rebound_stages$star)
  expect_equal(lamp_cons_points_star$x, 1)
  expect_equal(lamp_cons_points_star$y, 1)

  # Hat point
  lamp_cons_points_hat <- cons_points %>% 
    dplyr::filter(Case == "Lamp", 
                  graph_type == ReboundTools::graph_types$consumption,
                  point_name == ReboundTools::rebound_stages$hat)
  expect_equal(lamp_cons_points_hat$x, 2.4344098152813411495)
  expect_equal(lamp_cons_points_hat$y, 0.99984047291445865557)
  
  # bar point
  lamp_cons_points_bar <- cons_points %>% 
    dplyr::filter(Case == "Lamp", graph_type == ReboundTools::graph_types$consumption,
                  point_name == ".last_point")
  # There is not a bar point.  There is a "last_point"
  expect_equal(lamp_cons_points_bar$x, 2.4354631373868929)
  expect_equal(lamp_cons_points_bar$y, 1.0002730832628564)
})

