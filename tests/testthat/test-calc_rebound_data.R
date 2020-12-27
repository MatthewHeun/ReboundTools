
  
test_that("calc_orig() works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig()

  expect_equal(res[[ReboundTools::orig_vars$eta_orig]][[1]], 0.19743862087385857795)
  expect_equal(res[[ReboundTools::orig_vars$eta_orig]][[2]], 2453.7037037)

  expect_equal(res[[ReboundTools::orig_vars$E_dot_s_orig]][[1]], 73060.68051000000559724867)
  expect_equal(res[[ReboundTools::orig_vars$E_dot_s_orig]][[2]], 236.52)
  
  expect_equal(res[[ReboundTools::orig_vars$C_dot_cap_orig]][[1]], 4030.87142857142816865235)
  expect_equal(res[[ReboundTools::orig_vars$C_dot_cap_orig]][[2]], 1.04444444444444428655)
  
  expect_equal(res[[ReboundTools::orig_vars$p_s_orig]][[1]], 0.0884)
  expect_equal(res[[ReboundTools::orig_vars$p_s_orig]][[2]], 0.00001533963491320755)
  
  expect_equal(res[[ReboundTools::orig_vars$C_dot_s_orig]][[1]], 1275.17)
  expect_equal(res[[ReboundTools::orig_vars$C_dot_s_orig]][[2]], 8.902357)
  
  expect_equal(res[[ReboundTools::orig_vars$C_dot_o_orig]][[1]], 19234.10200768475260701962)
  expect_equal(res[[ReboundTools::orig_vars$C_dot_o_orig]][[2]], 27391.33089146313432138413)
  
  expect_equal(res[[ReboundTools::orig_vars$f_Cs_orig]][[1]], 0.06217529318067448879)
  expect_equal(res[[ReboundTools::orig_vars$f_Cs_orig]][[2]], 0.00032490077880412691)

  expect_equal(res[[ReboundTools::orig_vars$e_qs_ps]][[1]], -0.03782470681932551676)
  expect_equal(res[[ReboundTools::orig_vars$e_qs_ps]][[2]], -0.39967509922119587307)
  
  expect_equal(res[[ReboundTools::orig_vars$e_qo_ps]][[1]], 0.00250767784092693468)
  expect_equal(res[[ReboundTools::orig_vars$e_qo_ps]][[2]], 0.00012989695462730592)
  
  expect_equal(res[[ReboundTools::orig_vars$E_dot_emb_orig]][[1]], 4857.14285714285688300151)
  expect_equal(res[[ReboundTools::orig_vars$E_dot_emb_orig]][[2]], 1.22222222222222232091)
  
  expect_equal(res[[ReboundTools::orig_vars$N_dot_orig]][[1]], 0)
  expect_equal(res[[ReboundTools::orig_vars$N_dot_orig]][[2]], 0)
  
})


test_that("calc_star() works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star()
  
  expect_equal(res[[ReboundTools::star_vars$eta_star]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::star_vars$eta_star]][[2]], 22722.222222222)

  expect_equal(res[[ReboundTools::star_vars$eta_ratio]][[1]], 1.68)
  expect_equal(res[[ReboundTools::star_vars$eta_ratio]][[2]], 9.260377359)
  
  expect_equal(res[[ReboundTools::star_vars$S_dot_dev]][[1]], 29572.18)
  expect_equal(res[[ReboundTools::star_vars$S_dot_dev]][[2]], 210.97892420537905877609)
  
  expect_equal(res[[ReboundTools::star_vars$G_dot]][[1]], 516.14023809523826002987)
  expect_equal(res[[ReboundTools::star_vars$G_dot]][[2]], 7.94101863887340275738)
  
  expect_equal(res[[ReboundTools::star_vars$p_s_star]][[1]], 0.05261904761904762085)
  expect_equal(res[[ReboundTools::star_vars$p_s_star]][[2]], 0.00000165648054278729)
  
  expect_equal(res[[ReboundTools::star_vars$q_dot_s_star]][[1]], 14425)
  expect_equal(res[[ReboundTools::star_vars$q_dot_s_star]][[2]], 580350)
  
  expect_equal(res[[ReboundTools::star_vars$C_dot_cap_star]][[1]], 3931.91428571428605209803)
  expect_equal(res[[ReboundTools::star_vars$C_dot_cap_star]][[2]], 0.121)
  
  expect_equal(res[[ReboundTools::star_vars$E_dot_emb_star]][[1]], 5714.28571428571467549773)
  expect_equal(res[[ReboundTools::star_vars$E_dot_emb_star]][[2]], 0.65)

  expect_equal(res[[ReboundTools::star_vars$C_dot_s_star]][[1]], 759.02976190476192641654)
  expect_equal(res[[ReboundTools::star_vars$C_dot_s_star]][[2]], 0.96133848300660162955)

  expect_equal(res[[ReboundTools::star_vars$M_dot_star]][[1]], 27401.27769302945671370253)
  expect_equal(res[[ReboundTools::star_vars$M_dot_star]][[2]], 27401.27769302945671370253)
  
  expect_equal(res[[ReboundTools::star_vars$N_dot_star]][[1]], 701.56351693471481212327)
  expect_equal(res[[ReboundTools::star_vars$N_dot_star]][[2]], 8.86446308331784749157)

  expect_equal(res[[ReboundTools::star_vars$C_dot_o_star]][[1]], 19234.10200768475260701962)
  expect_equal(res[[ReboundTools::star_vars$C_dot_o_star]][[2]], 27391.33089146313432138413)
  
  expect_equal(res[[ReboundTools::star_vars$E_dot_s_star]][[1]], 43488.50030357143259607255)
  expect_equal(res[[ReboundTools::star_vars$E_dot_s_star]][[2]], 25.54107579462102606271)
})


test_that("calc_hat() works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star() %>% 
    calc_hat()
  
  expect_equal(res[[ReboundTools::hat_vars$eta_hat]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::hat_vars$eta_hat]][[2]], 22722.222222222)
  
  expect_equal(res[[ReboundTools::hat_vars$p_s_hat]][[1]], 0.05261904761904762085)
  expect_equal(res[[ReboundTools::hat_vars$p_s_hat]][[2]], 0.00000165648054278729)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_cap_hat]][[1]], 3931.91428571428605209803)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_cap_hat]][[2]], 0.121)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_md_hat]][[1]], 2774.66812079094552245806)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_md_hat]][[2]], 0)
  
  expect_equal(res[[ReboundTools::hat_vars$E_dot_emb_hat]][[1]], 5714.28571428571467549773)
  expect_equal(res[[ReboundTools::hat_vars$E_dot_emb_hat]][[2]], 0.65)
  
  expect_equal(res[[ReboundTools::hat_vars$q_dot_s_hat]][[1]], 14710.86057365263695828617)
  expect_equal(res[[ReboundTools::hat_vars$q_dot_s_hat]][[2]], 1412626.67908869450911879539)

})
