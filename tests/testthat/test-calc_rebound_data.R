
  
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
  expect_equal(res[[ReboundTools::orig_vars$p_s_orig]][[2]], 0.00001533962264150944)
  
  expect_equal(res[[ReboundTools::orig_vars$C_dot_s_orig]][[1]], 1275.17)
  expect_equal(res[[ReboundTools::orig_vars$C_dot_s_orig]][[2]], 8.90235000000000198384)
  
  expect_equal(res[[ReboundTools::orig_vars$C_dot_o_orig]][[1]], 19234.10200768475260701962)
  expect_equal(res[[ReboundTools::orig_vars$C_dot_o_orig]][[2]], 27391.33089146313432138413)
  
  expect_equal(res[[ReboundTools::orig_vars$f_Cs_orig]][[1]], 0.06217529318067448879)
  expect_equal(res[[ReboundTools::orig_vars$f_Cs_orig]][[2]], 0.00032490051888371175)

  expect_equal(res[[ReboundTools::orig_vars$e_qs_ps_C]][[1]], -0.03782470681932551676)
  expect_equal(res[[ReboundTools::orig_vars$e_qs_ps_C]][[2]], -0.39967509922119587307)
  
  expect_equal(res[[ReboundTools::orig_vars$e_qo_ps]][[1]], 0.00250767784092693468)
  expect_equal(res[[ReboundTools::orig_vars$e_qo_ps]][[2]], 0.00012989685076052724)
  
  expect_equal(res[[ReboundTools::orig_vars$sigma]][[1]], 0.04033238466025244884)
  expect_equal(res[[ReboundTools::orig_vars$sigma]][[2]], 0.39980499617582315741)
  
  expect_equal(res[[ReboundTools::orig_vars$rho]][[1]], -23.7939716)
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
  
  expect_equal(res[[ReboundTools::star_vars$C_dot_cap_star]][[1]], 3931.91428571428605209803)
  expect_equal(res[[ReboundTools::star_vars$C_dot_cap_star]][[2]], 0.121)
  
  expect_equal(res[[ReboundTools::star_vars$E_dot_emb_star]][[1]], 2857.14285714285733774886)
  expect_equal(res[[ReboundTools::star_vars$E_dot_emb_star]][[2]], 0.65)

  expect_equal(res[[ReboundTools::star_vars$C_dot_s_star]][[1]], 759.02976190476192641654)
  expect_equal(res[[ReboundTools::star_vars$C_dot_s_star]][[2]], 0.96133771393643030478)

  expect_equal(res[[ReboundTools::star_vars$M_dot_star]][[1]], 27401.27769302945671370253)
  expect_equal(res[[ReboundTools::star_vars$M_dot_star]][[2]], 27401.27769302945671370253)
  
  expect_equal(res[[ReboundTools::star_vars$N_dot_star]][[1]], 701.56351693471481212327)
  expect_equal(res[[ReboundTools::star_vars$N_dot_star]][[2]], 8.86445673050801730142)

  expect_equal(res[[ReboundTools::star_vars$C_dot_o_star]][[1]], 19234.10200768475260701962)
  expect_equal(res[[ReboundTools::star_vars$C_dot_o_star]][[2]], 27391.33089146313432138413)
  
  expect_equal(res[[ReboundTools::star_vars$E_dot_s_star]][[1]], 43488.50030357143259607255)
  expect_equal(res[[ReboundTools::star_vars$E_dot_s_star]][[2]], 25.54107579462102606271)
})


test_that("calc_hat(use_sub_approx = TRUE) works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star() %>% 
    calc_hat(use_sub_approx = TRUE)

  expect_equal(res[[ReboundTools::hat_vars$eta_engr_units_hat]][[1]], 42)
  expect_equal(res[[ReboundTools::hat_vars$eta_engr_units_hat]][[2]], 81.8)
    
  expect_equal(res[[ReboundTools::hat_vars$eta_hat]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::hat_vars$eta_hat]][[2]], 22722.222222222)
  
  expect_equal(res[[ReboundTools::hat_vars$p_s_hat]][[1]], 0.05261904761904762085)
  expect_equal(res[[ReboundTools::hat_vars$p_s_hat]][[2]], 0.00000165647921760391)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_cap_hat]][[1]], 3931.91428571428605209803)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_cap_hat]][[2]], 0.121)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_md_hat]][[1]], 2774.66812079094552245806)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_md_hat]][[2]], 0)
  
  expect_equal(res[[ReboundTools::hat_vars$E_dot_emb_hat]][[1]], 2857.14285714285733774886)
  expect_equal(res[[ReboundTools::hat_vars$E_dot_emb_hat]][[2]], 0.65)
  
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat]][[1]], 27401.27769302945671370253)
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat]][[2]], 27401.27769302945671370253)

  expect_equal(res[[ReboundTools::hat_vars$q_dot_s_hat]][[1]], 14710.86057365263695828617)
  expect_equal(res[[ReboundTools::hat_vars$q_dot_s_hat]][[2]], 1412626.67908869450911879539)

  expect_equal(res[[ReboundTools::hat_vars$E_dot_s_hat]][[1]], 44350.31296520552132278681)
  expect_equal(res[[ReboundTools::hat_vars$E_dot_s_hat]][[2]], 62.16938929974694616476)

  expect_equal(res[[ReboundTools::hat_vars$C_dot_s_hat]][[1]], 774.07147304219824945903)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_s_hat]][[2]], 2.33998673749697339019)

  expect_equal(res[[ReboundTools::hat_vars$C_dot_o_hat]][[1]], 19209.09533220446610357612)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_o_hat]][[2]], 27383.41272383832256309688)
  
  expect_equal(res[[ReboundTools::hat_vars$N_dot_hat]][[1]], 711.52848127756499252428)
  expect_equal(res[[ReboundTools::hat_vars$N_dot_hat]][[2]], 15.40396900329500873283)
  
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat_prime]][[1]], 19983.16680524666298879310)
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat_prime]][[2]], 27385.75271244645409751683)
})


test_that("calc_hat(use_sub_approx = FALSE) works as expected", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star() %>% 
    calc_hat()
  
  expect_equal(res[[ReboundTools::hat_vars$eta_engr_units_hat]][[1]], 42)
  expect_equal(res[[ReboundTools::hat_vars$eta_engr_units_hat]][[2]], 81.8)
  
  expect_equal(res[[ReboundTools::hat_vars$eta_hat]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::hat_vars$eta_hat]][[2]], 22722.222222222)
  
  expect_equal(res[[ReboundTools::hat_vars$p_s_hat]][[1]], 0.05261904761904762085)
  expect_equal(res[[ReboundTools::hat_vars$p_s_hat]][[2]], 0.00000165647921760391)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_cap_hat]][[1]], 3931.91428571428605209803)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_cap_hat]][[2]], 0.121)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_md_hat]][[1]], 2774.66812079094552245806)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_md_hat]][[2]], 0)
  
  expect_equal(res[[ReboundTools::hat_vars$E_dot_emb_hat]][[1]], 2857.14285714285733774886)
  expect_equal(res[[ReboundTools::hat_vars$E_dot_emb_hat]][[2]], 0.65)
  
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat]][[1]], 27401.27769302945671370253)
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat]][[2]], 27401.27769302945671370253)
  
  expect_equal(res[[ReboundTools::hat_vars$q_dot_s_hat]][[1]], 14714.737165590910081)
  expect_equal(res[[ReboundTools::hat_vars$q_dot_s_hat]][[2]], 1412809.7362985264044)
  
  expect_equal(res[[ReboundTools::hat_vars$E_dot_s_hat]][[1]], 44362.000117350027722)
  expect_equal(res[[ReboundTools::hat_vars$E_dot_s_hat]][[2]], 62.177445607270108496)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_s_hat]][[1]], 774.27545561799786356)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_s_hat]][[2]], 2.340289966606972083)
  
  expect_equal(res[[ReboundTools::hat_vars$C_dot_o_hat]][[1]], 19214.157294673186698)
  expect_equal(res[[ReboundTools::hat_vars$C_dot_o_hat]][[2]], 27386.961239397664031)
  
  expect_equal(res[[ReboundTools::hat_vars$N_dot_hat]][[1]], 706.26253623304467055)
  expect_equal(res[[ReboundTools::hat_vars$N_dot_hat]][[2]], 11.855163665186063682)
  
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat_prime]][[1]], 19988.432750291183766)
  expect_equal(res[[ReboundTools::hat_vars$M_dot_hat_prime]][[2]], 27389.301529364271119)
})


test_that("calc_bar() works as expected with approximated hat", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star() %>% 
    calc_hat(use_sub_approx = TRUE) %>% 
    calc_bar()
  
  expect_equal(res[[ReboundTools::bar_vars$eta_engr_units_bar]][[1]], 42)
  expect_equal(res[[ReboundTools::bar_vars$eta_engr_units_bar]][[2]], 81.8)

  expect_equal(res[[ReboundTools::bar_vars$eta_bar]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::bar_vars$eta_bar]][[2]], 22722.222222222)

  expect_equal(res[[ReboundTools::bar_vars$p_s_bar]][[1]], 0.05261904761904762085)
  expect_equal(res[[ReboundTools::bar_vars$p_s_bar]][[2]], 0.00000165647921760391)

  expect_equal(res[[ReboundTools::bar_vars$C_dot_cap_bar]][[1]], 3931.91428571428605209803)
  expect_equal(res[[ReboundTools::bar_vars$C_dot_cap_bar]][[2]], 0.121)

  expect_equal(res[[ReboundTools::bar_vars$C_dot_md_bar]][[1]], 2774.66812079094552245806)
  expect_equal(res[[ReboundTools::bar_vars$C_dot_md_bar]][[2]], 0)
  
  expect_equal(res[[ReboundTools::bar_vars$E_dot_emb_bar]][[1]], 2857.14285714285733774886)
  expect_equal(res[[ReboundTools::bar_vars$E_dot_emb_bar]][[2]], 0.65)
  
  expect_equal(res[[ReboundTools::bar_vars$M_dot_bar]][[1]], 27401.27769302945671370253)
  expect_equal(res[[ReboundTools::bar_vars$M_dot_bar]][[2]], 27401.27769302945671370253)

  expect_equal(res[[ReboundTools::bar_vars$q_dot_s_bar]][[1]], 15234.66124970508462865837)
  expect_equal(res[[ReboundTools::bar_vars$q_dot_s_bar]][[2]], 1413421.25553010916337370872)

  expect_equal(res[[ReboundTools::bar_vars$E_dot_s_bar]][[1]], 45929.46761751178564736620)
  expect_equal(res[[ReboundTools::bar_vars$E_dot_s_bar]][[2]], 62.20435843408792919718)
  
  expect_equal(res[[ReboundTools::bar_vars$C_dot_s_bar]][[1]], 801.63336575829134744708)
  expect_equal(res[[ReboundTools::bar_vars$C_dot_s_bar]][[2]], 2.34130293586974636000)
  
  expect_equal(res[[ReboundTools::bar_vars$C_dot_o_bar]][[1]], 19893.06192076593652018346)
  expect_equal(res[[ReboundTools::bar_vars$C_dot_o_bar]][[2]], 27398.81538822091170004569)
  
  expect_equal(res[[ReboundTools::bar_vars$N_dot_bar]][[1]], 0)
  expect_equal(res[[ReboundTools::bar_vars$N_dot_bar]][[2]], 0)
})


test_that("calc_tilde() works as expected with approximated hat", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star() %>% 
    calc_hat(use_sub_approx = TRUE) %>% 
    calc_bar() %>% 
    calc_tilde()

  expect_equal(res[[ReboundTools::tilde_vars$eta_engr_units_tilde]][[1]], 42)
  expect_equal(res[[ReboundTools::tilde_vars$eta_engr_units_tilde]][[2]], 81.8)
  
  expect_equal(res[[ReboundTools::tilde_vars$eta_tilde]][[1]], 0.33169688306808242650)
  expect_equal(res[[ReboundTools::tilde_vars$eta_tilde]][[2]], 22722.222222222)
  
  expect_equal(res[[ReboundTools::tilde_vars$p_s_tilde]][[1]], 0.05261904761904762085)
  expect_equal(res[[ReboundTools::tilde_vars$p_s_tilde]][[2]], 0.00000165647921760391)
  
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_cap_tilde]][[1]], 3931.91428571428605209803)
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_cap_tilde]][[2]], 0.121)
  
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_md_tilde]][[1]], 2774.66812079094552245806)
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_md_tilde]][[2]], 0)
  
  expect_equal(res[[ReboundTools::tilde_vars$E_dot_emb_tilde]][[1]], 2857.14285714285733774886)
  expect_equal(res[[ReboundTools::tilde_vars$E_dot_emb_tilde]][[2]], 0.65)
  
  expect_equal(res[[ReboundTools::tilde_vars$M_dot_tilde]][[1]], 27401.27769302945671370253)
  expect_equal(res[[ReboundTools::tilde_vars$M_dot_tilde]][[2]], 27401.27769302945671370253)
  
  expect_equal(res[[ReboundTools::tilde_vars$q_dot_s_tilde]][[1]], 15234.66124970508462865837)
  expect_equal(res[[ReboundTools::tilde_vars$q_dot_s_tilde]][[2]], 1413421.25553010916337370872)
  
  expect_equal(res[[ReboundTools::tilde_vars$E_dot_s_tilde]][[1]], 45929.46761751178564736620)
  expect_equal(res[[ReboundTools::tilde_vars$E_dot_s_tilde]][[2]], 62.20435843408792919718)
  
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_s_tilde]][[1]], 801.63336575829134744708)
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_s_tilde]][[2]], 2.34130293586974636000)
  
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_o_tilde]][[1]], 19893.06192076593652018346)
  expect_equal(res[[ReboundTools::tilde_vars$C_dot_o_tilde]][[2]], 27398.81538822091170004569)
  
  expect_equal(res[[ReboundTools::tilde_vars$N_dot_tilde]][[1]], 0)
  expect_equal(res[[ReboundTools::tilde_vars$N_dot_tilde]][[2]], 0)
})


test_that("calc_Deltas() works as expected with approximated hat", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star() %>% 
    calc_hat(use_sub_approx = TRUE) %>% 
    calc_bar() %>% 
    calc_tilde() %>% 
    calc_Deltas()
  
  # Check a couple Delta values. 
  # Note that there are over 40 values calculated, so this is just a sample.
  expect_equal(res[[ReboundTools::Delta_vars$Delta_p_s_star]][[1]], -0.03578095238095238551)
  expect_equal(res[[ReboundTools::Delta_vars$Delta_p_s_star]][[2]], -0.00001368314342390552)

  expect_equal(res[[ReboundTools::Delta_vars$Delta_C_dot_o_bar]][[1]], 683.96658856147041660734)
  expect_equal(res[[ReboundTools::Delta_vars$Delta_C_dot_o_bar]][[2]], 15.40265280491803423502)

  expect_equal(res[[ReboundTools::Delta_vars$Delta_E_dot_s_star]][[1]], -29572.18020642857300117612)
  expect_equal(res[[ReboundTools::Delta_vars$Delta_E_dot_s_star]][[2]], -210.97892420537903035438)
  
  expect_equal(res[[ReboundTools::Delta_vars$Delta_N_dot_bar]][[1]], -711.52848127756499252428)
  expect_equal(res[[ReboundTools::Delta_vars$Delta_N_dot_bar]][[2]], -15.40396900329500873283)
})


test_that("calc_rebound() works as expected with approximated hat", {
  res <- load_eeu_data() %>% 
    calc_orig() %>% 
    calc_star() %>% 
    calc_hat(use_sub_approx = TRUE) %>% 
    calc_bar() %>% 
    calc_tilde() %>% 
    calc_Deltas() %>% 
    calc_rebound()

  expect_equal(res[[ReboundTools::rebound_terms$Re_dempl]][[1]], 0)
  expect_equal(res[[ReboundTools::rebound_terms$Re_dempl]][[2]], 0)
  
  expect_equal(res[[ReboundTools::rebound_terms$Re_emb]][[1]], 0.01449238526141076108)
  expect_equal(res[[ReboundTools::rebound_terms$Re_emb]][[2]], -0.00271222457113862387)
  
  expect_equal(res[[ReboundTools::rebound_terms$Re_md]][[1]], -0.00991009287347477917)
  expect_equal(res[[ReboundTools::rebound_terms$Re_md]][[2]], 0)
  
  expect_equal(res[[ReboundTools::rebound_terms$Re_empl]][[1]], 0.00458229238793599059)
  expect_equal(res[[ReboundTools::rebound_terms$Re_empl]][[2]], -0.00271222457113862300)
  
  expect_equal(res[[ReboundTools::rebound_terms$Re_dsub]][[1]], 0.02914268260298064420)
  expect_equal(res[[ReboundTools::rebound_terms$Re_dsub]][[2]], 0.17361124407606612352)
  
  expect_equal(res[[ReboundTools::rebound_terms$Re_isub]][[1]], -0.00286607552946620226)
  expect_equal(res[[ReboundTools::rebound_terms$Re_isub]][[2]], -0.12720385929734354113)
  
  expect_equal(res[[ReboundTools::rebound_terms$Re_sub]][[1]], 0.02627660707351444150)
  expect_equal(res[[ReboundTools::rebound_terms$Re_sub]][[2]], 0.04640738494919466328)

  expect_equal(res[[ReboundTools::rebound_terms$Re_dinc]][[1]], 0.05340000775333344357)
  expect_equal(res[[ReboundTools::rebound_terms$Re_dinc]][[2]], 0.00016574692562592113)

  expect_equal(res[[ReboundTools::rebound_terms$Re_iinc]][[1]], 0.07839106417780371261)
  expect_equal(res[[ReboundTools::rebound_terms$Re_iinc]][[2]], 0.24744089023629611823)

  expect_equal(res[[ReboundTools::rebound_terms$Re_macro]][[1]], 0.08154999933180497040)
  expect_equal(res[[ReboundTools::rebound_terms$Re_macro]][[2]], 0.24746203473015923602)

  expect_equal(res[[ReboundTools::rebound_terms$Re_dir]][[1]], 0.08254269035631409124)
  expect_equal(res[[ReboundTools::rebound_terms$Re_dir]][[2]], 0.17377699112626410205)

  expect_equal(res[[ReboundTools::rebound_terms$Re_indir]][[1]], 0.16165728036807847090)
  expect_equal(res[[ReboundTools::rebound_terms$Re_indir]][[2]], 0.36498684109797319142)

  expect_equal(res[[ReboundTools::rebound_terms$Re_tot]][[1]], 0.24419997072439253438)
  expect_equal(res[[ReboundTools::rebound_terms$Re_tot]][[2]], 0.53876383227013735500)
  
})
  
