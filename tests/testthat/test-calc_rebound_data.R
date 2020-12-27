
  
test_that("calc_orig() works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig()

  expect_equal(res[[ReboundTools::eeu_derived_data$eta_orig]][[1]], 0.19743862087385857795)
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_orig]][[2]], 2453.7037037)

  expect_equal(res[[ReboundTools::eeu_derived_data$E_dot_s_orig]][[1]], 73060.68051000000559724867)
  expect_equal(res[[ReboundTools::eeu_derived_data$E_dot_s_orig]][[2]], 236.52)
  
  expect_equal(res[[ReboundTools::eeu_derived_data$C_dot_cap_orig]][[1]], 4030.87142857142816865235)
  expect_equal(res[[ReboundTools::eeu_derived_data$C_dot_cap_orig]][[2]], 1.04444444444444428655)
  
  expect_equal(res[[ReboundTools::eeu_derived_data$p_s_orig]][[1]], 0.0884)
  expect_equal(res[[ReboundTools::eeu_derived_data$p_s_orig]][[2]], 0.00001533963491320755)
})



test_that("calc_star() works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star()
  
  
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_tilde]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_tilde]][[2]], 22722.222222222)

    expect_equal(res[[ReboundTools::eeu_derived_data$eta_ratio]][[1]], 1.68)
  expect_equal(res[[ReboundTools::eeu_derived_data$eta_ratio]][[2]], 9.260377359)
  
  expect_equal(res[[ReboundTools::eeu_derived_data$C_dot_cap_star]][[1]], 3931.91428571428605209803)
  expect_equal(res[[ReboundTools::eeu_derived_data$C_dot_cap_star]][[2]], 0.121)

  expect_equal(res[[ReboundTools::eeu_derived_data$p_s_star]][[1]], 0.05261904761904762085)
  expect_equal(res[[ReboundTools::eeu_derived_data$p_s_star]][[2]], 0.00000165648054278729)

  expect_equal(res[[ReboundTools::eeu_derived_data$q_dot_s_star]][[1]], 14425)
  expect_equal(res[[ReboundTools::eeu_derived_data$q_dot_s_star]][[2]], 580350)

  expect_equal(res[[ReboundTools::eeu_derived_data$S_dot_dev]][[1]], 29572.18)
  expect_equal(res[[ReboundTools::eeu_derived_data$S_dot_dev]][[2]], 210.97892420537905877609)
  
  expect_equal(res[[ReboundTools::eeu_derived_data$G_dot]][[1]], 516.14023809523826002987)
  expect_equal(res[[ReboundTools::eeu_derived_data$G_dot]][[2]], 7.94101863887340275738)
  
    
})



test_that("calc_hat() works as expected", {
  expect_equal(res[[ReboundTools::eeu_derived_data$q_dot_s_hat]][[1]], )
  expect_equal(res[[ReboundTools::eeu_derived_data$q_dot_s_hat]][[2]], )
  
  
})