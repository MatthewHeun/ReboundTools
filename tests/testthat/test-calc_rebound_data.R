
  
test_that("calc_orig() works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig()

  expect_equal(res[[ReboundTools::orig_vars$p_E_orig]],
               res[[ReboundTools::orig_vars$p_E]])
  
  expect_equal(res[[ReboundTools::orig_vars$tau_alpha_orig]][[1]], 1.20327063)
  expect_equal(res[[ReboundTools::orig_vars$tau_alpha_orig]][[2]], 1.01181118)

  expect_equal(res[[ReboundTools::orig_vars$tau_omega_orig]][[1]], 0.7955036367662678)
  expect_equal(res[[ReboundTools::orig_vars$tau_omega_orig]][[2]], 0.9593839991000015)
  
  expect_equal(res[[ReboundTools::orig_vars$eta_orig]][[1]], 0.19743862087385857795)
  expect_equal(res[[ReboundTools::orig_vars$eta_orig]][[2]], 2453.7037037)

  expect_equal(res[[ReboundTools::orig_vars$E_dot_s_orig]][[1]], 73060.68051000000559724867)
  expect_equal(res[[ReboundTools::orig_vars$E_dot_s_orig]][[2]], 236.52)
  
  expect_equal(res[[ReboundTools::orig_vars$C_dot_cap_orig]][[1]], 2015.4357)
  expect_equal(res[[ReboundTools::orig_vars$C_dot_cap_orig]][[2]], 1.04444444444444428655)
  
  expect_equal(res[[ReboundTools::orig_vars$p_s_orig]][[1]], 0.0884)
  expect_equal(res[[ReboundTools::orig_vars$p_s_orig]][[2]], 0.00001533962264150944)
  
  expect_equal(res[[ReboundTools::orig_vars$C_dot_s_orig]][[1]], 1275.17)
  expect_equal(res[[ReboundTools::orig_vars$C_dot_s_orig]][[2]], 8.90235000000000198384)

  expect_equal(res[[ReboundTools::orig_vars$C_dot_d_orig]][[1]], 7.1428571428571432)
  expect_equal(res[[ReboundTools::orig_vars$C_dot_d_orig]][[2]], 0)
  
  expect_equal(res[[ReboundTools::orig_vars$C_dot_omd_orig]][[1]], 2866.8164256073246179)
  expect_equal(res[[ReboundTools::orig_vars$C_dot_omd_orig]][[2]], 0)
  
  expect_equal(res[[ReboundTools::orig_vars$C_dot_g_orig]][[1]], 20834.1766716994206945)
  expect_equal(res[[ReboundTools::orig_vars$C_dot_g_orig]][[2]], 27391.3186)
  
  expect_equal(res[[ReboundTools::orig_vars$f_Cs_orig]][[1]], 0.0576756074675084)
  expect_equal(res[[ReboundTools::orig_vars$f_Cs_orig]][[2]], 0.0003249006663864)

  expect_equal(res[[ReboundTools::orig_vars$e_qs_ps_C]][[1]], -0.0423243925324916)
  expect_equal(res[[ReboundTools::orig_vars$e_qs_ps_C]][[2]], -0.39967509922119587307)
  
  expect_equal(res[[ReboundTools::orig_vars$e_qg_ps_C]][[1]], 0.0025904933262359)
  expect_equal(res[[ReboundTools::orig_vars$e_qg_ps_C]][[2]], 0.0001298969097041)
  
  expect_equal(res[[ReboundTools::orig_vars$e_qs_ps_UC]][[1]], -0.10)
  expect_equal(res[[ReboundTools::orig_vars$e_qs_ps_UC]][[2]], -0.4)
  
  expect_equal(res[[ReboundTools::orig_vars$e_qg_ps_UC]][[1]], -0.0550851141412725)
  expect_equal(res[[ReboundTools::orig_vars$e_qg_ps_UC]][[2]], -0.0001950037566824)

  expect_equal(res[[ReboundTools::orig_vars$sigma]][[1]], 0.0449148858587275)
  expect_equal(res[[ReboundTools::orig_vars$sigma]][[2]], 0.39980499617582315741)
  
  expect_equal(res[[ReboundTools::orig_vars$rho]][[1]], -21.2643335473530648)
  expect_equal(res[[ReboundTools::orig_vars$rho]][[2]], -1.50121937)

  expect_equal(res[[ReboundTools::orig_vars$E_dot_emb_orig]][[1]], 2428.57142857142844150076)
  expect_equal(res[[ReboundTools::orig_vars$E_dot_emb_orig]][[2]], 1.22222222222222232091)
  
  expect_equal(res[[ReboundTools::orig_vars$N_dot_orig]][[1]], 0)
  expect_equal(res[[ReboundTools::orig_vars$N_dot_orig]][[2]], 0)
})


test_that("calc_star() works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star()
  
  expect_equal(res[[ReboundTools::star_vars$p_E_star]], 
               res[[ReboundTools::orig_vars$p_E]])
  
  expect_equal(res[[ReboundTools::star_vars$tau_alpha_star]][[1]], 1.20327063)
  expect_equal(res[[ReboundTools::star_vars$tau_alpha_star]][[2]], 1.1381602583025194)
  
  expect_equal(res[[ReboundTools::star_vars$tau_omega_star]][[1]], 0.7955036367662678)
  expect_equal(res[[ReboundTools::star_vars$tau_omega_star]][[2]], 0.8468981223801895)
  
  expect_equal(res[[ReboundTools::star_vars$eta_star]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::star_vars$eta_star]][[2]], 22722.222222222)

  expect_equal(res[[ReboundTools::star_vars$eta_ratio]][[1]], 1.68)
  expect_equal(res[[ReboundTools::star_vars$eta_ratio]][[2]], 9.260377359)
  
  expect_equal(res[[ReboundTools::star_vars$S_dot_dev]][[1]], 29572.18)
  expect_equal(res[[ReboundTools::star_vars$S_dot_dev]][[2]], 210.97892420537905877609)
  
  expect_equal(res[[ReboundTools::star_vars$G_dot]][[1]], 516.14023809523826002987)
  expect_equal(res[[ReboundTools::star_vars$G_dot]][[2]], 7.94101228606357256723)
  
  expect_equal(res[[ReboundTools::star_vars$p_s_star]][[1]], 0.05261904761904762085)
  expect_equal(res[[ReboundTools::star_vars$p_s_star]][[2]], 0.00000165647921760391)
  
  expect_equal(res[[ReboundTools::star_vars$q_dot_s_star]][[1]], 14425)
  expect_equal(res[[ReboundTools::star_vars$q_dot_s_star]][[2]], 580350)
  
  expect_equal(res[[ReboundTools::star_vars$C_dot_cap_star]][[1]], 1965.9571428571430260)
  expect_equal(res[[ReboundTools::star_vars$C_dot_cap_star]][[2]], 0.121)
  
  expect_equal(res[[ReboundTools::star_vars$E_dot_emb_star]][[1]], 2857.14285714285733774886)
  expect_equal(res[[ReboundTools::star_vars$E_dot_emb_star]][[2]], 0.65)

  expect_equal(res[[ReboundTools::star_vars$C_dot_s_star]][[1]], 759.02976190476192641654)
  expect_equal(res[[ReboundTools::star_vars$C_dot_s_star]][[2]], 0.96133771393643030478)

  expect_equal(res[[ReboundTools::star_vars$M_dot_star]][[1]], 27401.27769302945671370253)
  expect_equal(res[[ReboundTools::star_vars$M_dot_star]][[2]], 27401.27769302945671370253)
  
  expect_equal(res[[ReboundTools::star_vars$N_dot_star]][[1]], 662.0856640579992245)
  expect_equal(res[[ReboundTools::star_vars$N_dot_star]][[2]], 8.8600754979050258)

  expect_equal(res[[ReboundTools::star_vars$C_dot_d_star]][[1]], 7.2142857142857144)
  expect_equal(res[[ReboundTools::star_vars$C_dot_d_star]][[2]], 0)
  
  expect_equal(res[[ReboundTools::star_vars$C_dot_omd_star]][[1]], 2780.4071113133309154)
  expect_equal(res[[ReboundTools::star_vars$C_dot_omd_star]][[2]], 0)

  expect_equal(res[[ReboundTools::star_vars$C_dot_g_star]][[1]], 20834.1766716994206945)
  expect_equal(res[[ReboundTools::star_vars$C_dot_g_star]][[2]], 27391.3185624263605860)
  
  expect_equal(res[[ReboundTools::star_vars$f_Cs_star]][[1]], 0.0351513224420219)
  expect_equal(res[[ReboundTools::star_vars$f_Cs_star]][[2]], 0.0000350952062932)
  
  expect_equal(res[[ReboundTools::star_vars$e_qs_ps_C_star]][[1]], -0.0423243925324916)
  expect_equal(res[[ReboundTools::star_vars$e_qs_ps_C_star]][[2]], -0.39967509922119587307)
  
  expect_equal(res[[ReboundTools::star_vars$e_qg_ps_C_star]][[1]], 0.0025904933262359)
  expect_equal(res[[ReboundTools::star_vars$e_qg_ps_C_star]][[2]], 0.0001298969097041)
  
  expect_equal(res[[ReboundTools::star_vars$e_qs_ps_UC_star]][[1]], -0.10)
  expect_equal(res[[ReboundTools::star_vars$e_qs_ps_UC_star]][[2]], -0.4)
  
  expect_equal(res[[ReboundTools::star_vars$e_qg_ps_UC_star]][[1]], -0.0550851141412725)
  expect_equal(res[[ReboundTools::star_vars$e_qg_ps_UC_star]][[2]], -0.0001950037566824)
  
  expect_equal(res[[ReboundTools::star_vars$E_dot_s_star]][[1]], 43488.50030357143259607255)
  expect_equal(res[[ReboundTools::star_vars$E_dot_s_star]][[2]], 25.54107579462102606271)
})


test_that("calc_hat() works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star() %>% 
    calc_hat()
  
  expect_equal(res[[ReboundTools::hat_vars$p_E_hat]], 
               res[[ReboundTools::orig_vars$p_E]])
  
  expect_equal(res[[ReboundTools::hat_vars$tau_alpha_hat]][[1]], 
               res[[ReboundTools::star_vars$tau_alpha_star]][[1]])
  expect_equal(res[[ReboundTools::hat_vars$tau_alpha_hat]][[2]], 
               res[[ReboundTools::star_vars$tau_alpha_star]][[2]])
  
  expect_equal(res[[ReboundTools::hat_vars$tau_omega_hat]][[1]], 
               res[[ReboundTools::star_vars$tau_omega_star]][[1]])
  expect_equal(res[[ReboundTools::hat_vars$tau_omega_hat]][[2]], 
               res[[ReboundTools::star_vars$tau_omega_star]][[2]])
  
  expect_equal(res[[ReboundTools::hat_vars$eta_engr_units_hat]][[1]], 42)
  expect_equal(res[[ReboundTools::hat_vars$eta_engr_units_hat]][[2]], 81.8)
  
  expect_equal(res[[ReboundTools::hat_vars$eta_hat]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::hat_vars$eta_hat]][[2]], 22722.222222222)
  
  expect_equal(res[[ReboundTools::hat_vars$p_s_hat]][[1]], 0.05261904761904762085)
  expect_equal(res[[ReboundTools::hat_vars$p_s_hat]][[2]], 0.00000165647921760391)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_cap_hat]][[1]], 1965.9571428571430260)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_cap_hat]][[2]], 0.121)
  
  expect_equal(res[[ReboundTools::hat_vars$C_d_hat]][[1]], 101)
  expect_equal(res[[ReboundTools::hat_vars$C_d_hat]][[2]], 0)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_d_hat]][[1]], 7.2142857142857144)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_d_hat]][[2]], 0)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_om_hat]][[1]], 2774.6681207909455225)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_om_hat]][[2]], 0)

  expect_equal(res[[ReboundTools::hat_vars$C_dot_omd_hat]][[1]], 2780.4071113133309154)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_omd_hat]][[2]], 0)
  
  expect_equal(res[[ReboundTools::hat_vars$E_dot_emb_hat]][[1]], 2857.14285714285733774886)
  expect_equal(res[[ReboundTools::hat_vars$E_dot_emb_hat]][[2]], 0.65)
  
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat]][[1]], 27401.27769302945671370253)
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat]][[2]], 27401.27769302945671370253)
  
  expect_equal(res[[ReboundTools::hat_vars$q_dot_s_hat]][[1]], 14749.2535034274060308)
  expect_equal(res[[ReboundTools::hat_vars$q_dot_s_hat]][[2]], 1412809.7362985264044)
  
  expect_equal(res[[ReboundTools::hat_vars$E_dot_s_hat]][[1]], 44466.0599973140197108)
  expect_equal(res[[ReboundTools::hat_vars$E_dot_s_hat]][[2]], 62.177445607270108496)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_s_hat]][[1]], 776.0916724422515927)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_s_hat]][[2]], 2.340289966606972083)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_g_hat]][[1]], 20811.8563816040004895)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_g_hat]][[2]], 27386.9489032208657591)

  expect_equal(res[[ReboundTools::hat_vars$f_Cs_hat]][[1]], 0.0359502288267174)
  expect_equal(res[[ReboundTools::hat_vars$f_Cs_hat]][[2]], 0.0000854454437645)
  
  expect_equal(res[[ReboundTools::hat_vars$e_qs_ps_C_hat]][[1]], -0.0433001854343803)
  expect_equal(res[[ReboundTools::hat_vars$e_qs_ps_C_hat]][[2]], -0.39977083)
  
  expect_equal(res[[ReboundTools::hat_vars$e_qg_ps_C_hat]][[1]], 0.0016147004243471)
  expect_equal(res[[ReboundTools::hat_vars$e_qg_ps_C_hat]][[2]], 0.0000341615153233)
  
  expect_equal(res[[ReboundTools::hat_vars$e_qs_ps_UC_hat]][[1]], -0.0792504142610977)
  expect_equal(res[[ReboundTools::hat_vars$e_qs_ps_UC_hat]][[2]], -0.39985628)
  
  expect_equal(res[[ReboundTools::hat_vars$e_qg_ps_UC_hat]][[1]], -0.0343355284023703)
  expect_equal(res[[ReboundTools::hat_vars$e_qg_ps_UC_hat]][[2]], -0.0000512839284412)

  expect_equal(res[[ReboundTools::hat_vars$N_dot_hat]][[1]], 667.3440436159297633)
  expect_equal(res[[ReboundTools::hat_vars$N_dot_hat]][[2]], 11.8507824478295323)
  
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat_prime]][[1]], 21587.9480540462500358)
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat_prime]][[2]], 27389.2891931903723162)
})


test_that("calc_bar() works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star() %>% 
    calc_hat() %>% 
    calc_bar()

  expect_equal(res[[ReboundTools::bar_vars$p_E_bar]], 
               res[[ReboundTools::orig_vars$p_E]])
  
  expect_equal(res[[ReboundTools::bar_vars$tau_alpha_bar]][[1]], 
               res[[ReboundTools::star_vars$tau_alpha_star]][[1]])
  expect_equal(res[[ReboundTools::bar_vars$tau_alpha_bar]][[2]], 
               res[[ReboundTools::star_vars$tau_alpha_star]][[2]])
  
  expect_equal(res[[ReboundTools::bar_vars$tau_omega_bar]][[1]], 
               res[[ReboundTools::star_vars$tau_omega_star]][[1]])
  expect_equal(res[[ReboundTools::bar_vars$tau_omega_bar]][[2]], 
               res[[ReboundTools::star_vars$tau_omega_star]][[2]])
  
  expect_equal(res[[ReboundTools::bar_vars$eta_engr_units_bar]][[1]], 42)
  expect_equal(res[[ReboundTools::bar_vars$eta_engr_units_bar]][[2]], 81.8)
  
  expect_equal(res[[ReboundTools::bar_vars$eta_bar]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::bar_vars$eta_bar]][[2]], 22722.222222222)
  
  expect_equal(res[[ReboundTools::bar_vars$p_s_bar]][[1]], 0.05261904761904762085)
  expect_equal(res[[ReboundTools::bar_vars$p_s_bar]][[2]], 0.00000165647921760391)
  
  expect_equal(res[[ReboundTools::bar_vars$C_dot_cap_bar]][[1]], 1965.9571428571430260)
  expect_equal(res[[ReboundTools::bar_vars$C_dot_cap_bar]][[2]], 0.121)
  
  expect_equal(res[[ReboundTools::bar_vars$C_d_bar]][[1]], 101)
  expect_equal(res[[ReboundTools::bar_vars$C_d_bar]][[2]], 0)
  
  expect_equal(res[[ReboundTools::bar_vars$C_dot_d_bar]][[1]], 7.2142857142857144)
  expect_equal(res[[ReboundTools::bar_vars$C_dot_d_bar]][[2]], 0)
  
  expect_equal(res[[ReboundTools::bar_vars$C_dot_om_bar]][[1]], 2774.6681207909455225)
  expect_equal(res[[ReboundTools::bar_vars$C_dot_om_bar]][[2]], 0)
  
  expect_equal(res[[ReboundTools::bar_vars$C_dot_omd_bar]][[1]], 2780.4071113133309154)
  expect_equal(res[[ReboundTools::bar_vars$C_dot_omd_bar]][[2]], 0)
  
  expect_equal(res[[ReboundTools::bar_vars$E_dot_emb_bar]][[1]], 2857.14285714285733774886)
  expect_equal(res[[ReboundTools::bar_vars$E_dot_emb_bar]][[2]], 0.65)
  
  expect_equal(res[[ReboundTools::bar_vars$M_dot_bar]][[1]], 27401.27769302945671370253)
  expect_equal(res[[ReboundTools::bar_vars$M_dot_bar]][[2]], 27401.27769302945671370253)
  
  expect_equal(res[[ReboundTools::bar_vars$q_dot_s_bar]][[1]], 15205.1943111712353129)
  expect_equal(res[[ReboundTools::bar_vars$q_dot_s_bar]][[2]], 1413421.0317824832163751)
  
  expect_equal(res[[ReboundTools::bar_vars$E_dot_s_bar]][[1]], 45840.6306701721186982)
  expect_equal(res[[ReboundTools::bar_vars$E_dot_s_bar]][[2]], 62.2043485870041479)
  
  expect_equal(res[[ReboundTools::bar_vars$C_dot_s_bar]][[1]], 800.0828435163912218)
  expect_equal(res[[ReboundTools::bar_vars$C_dot_s_bar]][[2]], 2.3413025648719619)
  
  expect_equal(res[[ReboundTools::bar_vars$C_dot_g_bar]][[1]], 21455.2092541457932384)
  expect_equal(res[[ReboundTools::bar_vars$C_dot_g_bar]][[2]], 27398.7986730733282457)
  
  expect_equal(res[[ReboundTools::bar_vars$f_Cs_bar]][[1]], 0.0359502288267174)
  expect_equal(res[[ReboundTools::bar_vars$f_Cs_bar]][[2]], 0.0000854454437645)
  
  expect_equal(res[[ReboundTools::bar_vars$e_qs_ps_C_bar]][[1]], -0.0433001854343803)
  expect_equal(res[[ReboundTools::bar_vars$e_qs_ps_C_bar]][[2]], -0.39977083)
  
  expect_equal(res[[ReboundTools::bar_vars$e_qg_ps_C_bar]][[1]], 0.0016147004243471)
  expect_equal(res[[ReboundTools::bar_vars$e_qg_ps_C_bar]][[2]], 0.0000341615153233)
  
  expect_equal(res[[ReboundTools::bar_vars$e_qs_ps_UC_bar]][[1]], -0.0792504142610977)
  expect_equal(res[[ReboundTools::bar_vars$e_qs_ps_UC_bar]][[2]], -0.39985628)
  
  expect_equal(res[[ReboundTools::bar_vars$e_qg_ps_UC_bar]][[1]], -0.0343355284023703)
  expect_equal(res[[ReboundTools::bar_vars$e_qg_ps_UC_bar]][[2]], -0.0000512839284412)
  
  expect_equal(res[[ReboundTools::bar_vars$N_dot_bar]][[1]], 0)
  expect_equal(res[[ReboundTools::bar_vars$N_dot_bar]][[2]], 0)
})


test_that("calc_tilde() works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star() %>% 
    calc_hat() %>% 
    calc_bar() %>% 
    calc_tilde()

  expect_equal(res[[ReboundTools::tilde_vars$p_E_tilde]], 
               res[[ReboundTools::orig_vars$p_E]])
  
  expect_equal(res[[ReboundTools::tilde_vars$tau_alpha_tilde]][[1]], 
               res[[ReboundTools::star_vars$tau_alpha_star]][[1]])
  expect_equal(res[[ReboundTools::tilde_vars$tau_alpha_tilde]][[2]], 
               res[[ReboundTools::star_vars$tau_alpha_star]][[2]])
  
  expect_equal(res[[ReboundTools::tilde_vars$tau_omega_tilde]][[1]], 
               res[[ReboundTools::star_vars$tau_omega_star]][[1]])
  expect_equal(res[[ReboundTools::tilde_vars$tau_omega_tilde]][[2]], 
               res[[ReboundTools::star_vars$tau_omega_star]][[2]])
  
  expect_equal(res[[ReboundTools::tilde_vars$eta_engr_units_tilde]][[1]], 42)
  expect_equal(res[[ReboundTools::tilde_vars$eta_engr_units_tilde]][[2]], 81.8)
  
  expect_equal(res[[ReboundTools::tilde_vars$eta_tilde]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::tilde_vars$eta_tilde]][[2]], 22722.222222222)
  
  expect_equal(res[[ReboundTools::tilde_vars$p_s_tilde]][[1]], 0.05261904761904762085)
  expect_equal(res[[ReboundTools::tilde_vars$p_s_tilde]][[2]], 0.00000165647921760391)
  
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_cap_tilde]][[1]], 1965.9571428571430260)
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_cap_tilde]][[2]], 0.121)
  
  expect_equal(res[[ReboundTools::tilde_vars$C_d_tilde]][[1]], 101)
  expect_equal(res[[ReboundTools::tilde_vars$C_d_tilde]][[2]], 0)
  
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_d_tilde]][[1]], 7.2142857142857144)
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_d_tilde]][[2]], 0)
  
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_om_tilde]][[1]], 2774.6681207909455225)
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_om_tilde]][[2]], 0)
  
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_omd_tilde]][[1]], 2780.4071113133309154)
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_omd_tilde]][[2]], 0)
  
  expect_equal(res[[ReboundTools::tilde_vars$E_dot_emb_tilde]][[1]], 2857.14285714285733774886)
  expect_equal(res[[ReboundTools::tilde_vars$E_dot_emb_tilde]][[2]], 0.65)
  
  expect_equal(res[[ReboundTools::tilde_vars$M_dot_tilde]][[1]], 27401.27769302945671370253)
  expect_equal(res[[ReboundTools::tilde_vars$M_dot_tilde]][[2]], 27401.27769302945671370253)
  
  expect_equal(res[[ReboundTools::tilde_vars$q_dot_s_tilde]][[1]], 15205.1943111712353129)
  expect_equal(res[[ReboundTools::tilde_vars$q_dot_s_tilde]][[2]], 1413421.0317824832163751)
  
  expect_equal(res[[ReboundTools::tilde_vars$E_dot_s_tilde]][[1]], 45840.6306701721186982)
  expect_equal(res[[ReboundTools::tilde_vars$E_dot_s_tilde]][[2]], 62.2043485870041479)
  
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_s_tilde]][[1]], 800.0828435163912218)
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_s_tilde]][[2]], 2.3413025648719619)
  
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_g_tilde]][[1]], 21455.2092541457932384)
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_g_tilde]][[2]], 27398.7986730733282457)
  
  expect_equal(res[[ReboundTools::tilde_vars$f_Cs_tilde]][[1]], 0.0359502288267174)
  expect_equal(res[[ReboundTools::tilde_vars$f_Cs_tilde]][[2]], 0.0000854454437645)
  
  expect_equal(res[[ReboundTools::tilde_vars$e_qs_ps_C_tilde]][[1]], -0.0433001854343803)
  expect_equal(res[[ReboundTools::tilde_vars$e_qs_ps_C_tilde]][[2]], -0.39977083)
  
  expect_equal(res[[ReboundTools::tilde_vars$e_qg_ps_C_tilde]][[1]], 0.0016147004243471)
  expect_equal(res[[ReboundTools::tilde_vars$e_qg_ps_C_tilde]][[2]], 0.0000341615153233)
  
  expect_equal(res[[ReboundTools::tilde_vars$e_qs_ps_UC_tilde]][[1]], -0.0792504142610977)
  expect_equal(res[[ReboundTools::tilde_vars$e_qs_ps_UC_tilde]][[2]], -0.3998562801717589)
  
  expect_equal(res[[ReboundTools::tilde_vars$e_qg_ps_UC_tilde]][[1]], -0.0343355284023703)
  expect_equal(res[[ReboundTools::tilde_vars$e_qg_ps_UC_tilde]][[2]], -0.0000512839284412)
  
  expect_equal(res[[ReboundTools::tilde_vars$N_dot_tilde]][[1]], 0)
  expect_equal(res[[ReboundTools::tilde_vars$N_dot_tilde]][[2]], 0)
})
  

test_that("compensated and uncompensated elasticities are present in calculated data", {
  res <- load_eeu_data() %>% 
    rebound_analysis()

  # Compensated elasticities
  expect_true(ReboundTools::orig_vars$e_qs_ps_C_orig %in% names(res))
  expect_true(ReboundTools::star_vars$e_qs_ps_C_star %in% names(res))
  expect_true(ReboundTools::hat_vars$e_qs_ps_C_hat %in% names(res))
  expect_true(ReboundTools::bar_vars$e_qs_ps_C_bar %in% names(res))
  expect_true(ReboundTools::tilde_vars$e_qs_ps_C_tilde %in% names(res))
  expect_true(ReboundTools::orig_vars$e_qg_ps_C_orig %in% names(res))
  expect_true(ReboundTools::star_vars$e_qg_ps_C_star %in% names(res))
  expect_true(ReboundTools::hat_vars$e_qg_ps_C_hat %in% names(res))
  expect_true(ReboundTools::bar_vars$e_qg_ps_C_bar %in% names(res))
  expect_true(ReboundTools::tilde_vars$e_qg_ps_C_tilde %in% names(res))

  # Uncompensated elasticities
  expect_true(ReboundTools::orig_vars$e_qs_ps_UC_orig %in% names(res))
  expect_true(ReboundTools::star_vars$e_qs_ps_UC_star %in% names(res))
  expect_true(ReboundTools::hat_vars$e_qs_ps_UC_hat %in% names(res))
  expect_true(ReboundTools::bar_vars$e_qs_ps_UC_bar %in% names(res))
  expect_true(ReboundTools::tilde_vars$e_qs_ps_UC_tilde %in% names(res))
  expect_true(ReboundTools::orig_vars$e_qg_ps_UC_orig %in% names(res))
  expect_true(ReboundTools::star_vars$e_qg_ps_UC_star %in% names(res))
  expect_true(ReboundTools::hat_vars$e_qg_ps_UC_hat %in% names(res))
  expect_true(ReboundTools::bar_vars$e_qg_ps_UC_bar %in% names(res))
  expect_true(ReboundTools::tilde_vars$e_qg_ps_UC_tilde %in% names(res))
})

