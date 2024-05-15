test_that("iso_expenditure_lines() works as expected", {
  iso_expenditure_lines_abs <- load_eeu_data() %>% 
    rebound_analysis() %>% 
    iso_expenditure_lines()
  
    # Check values on the car absolute iso line.
    car_expenditure_iso_lines_abs <- iso_expenditure_lines_abs %>%
      dplyr::filter(Case == "Car",
                    graph_type == ReboundTools::graph_types$expenditure,
                    line_name == "orig")
    expect_equal(car_expenditure_iso_lines_abs$slope, -1)
    expect_equal(car_expenditure_iso_lines_abs$intercept, 27401.27769302945671370253)

    # Calculate the indexed iso expenditure line.
    car_expenditure_iso_lines_indexed <- load_eeu_data() %>%
      rebound_analysis() %>%
      iso_expenditure_lines(indexed = TRUE)
    car_expenditure_iso_lines_indexed <- car_expenditure_iso_lines_indexed %>%
      dplyr::filter(Case == "Car",
                    graph_type == ReboundTools::graph_types$expenditure,
                    line_name == "orig")
    expect_equal(car_expenditure_iso_lines_indexed$slope, -0.04880826547079648164)
    expect_equal(car_expenditure_iso_lines_indexed$intercept, 1.04880826547079641919)
})


test_that("iso_budget_lines_cons() works as expected with approximated hat", {
  iso_budget_lines <- load_eeu_data() %>% 
    rebound_analysis(use_sub_approx = TRUE) %>% 
    iso_budget_lines_cons() %>% 
    dplyr::filter(Case == "Lamp")
  expect_equal(iso_budget_lines$intercept[[1]], 1.00032500637362797846)
  expect_equal(iso_budget_lines$intercept[[2]], 1.00003509645028976216)
  expect_equal(iso_budget_lines$intercept[[3]], 0.99979635239197450769)
  expect_equal(iso_budget_lines$intercept[[4]], 1.0003585593438833)
  expect_equal(iso_budget_lines$intercept[[5]], 0.00000000000000000000)
})


test_that("add_iso() works as expected", {
  meta <- tibble::tibble(Case = "Test case")
  res <- add_iso(indexed = FALSE, meta = meta, graph_type = "Test type", 
                 iso_name = "Test iso", 
                 x_orig = 10, y_orig = 10, 
                 x = 20, y = 30)
  expect_equal(res$Case, "Test case")
  expect_equal(res$graph_type, "Test type")
  expect_equal(res$line_name, "Test iso")
  expect_equal(res$colour, "black")
  expect_equal(res$linewidth, 0.1)
  expect_equal(res$linetype, "solid")
  expect_equal(res$slope, -1)
  expect_equal(res$intercept, 50)

  # Now try with vectors, adding to the previous data frame.
  meta2 <- tibble::tibble(Case = c("Test case 1", "Test case 2"))
  res2 <- res %>% 
    add_iso(indexed = FALSE, meta = meta2, graph_type = "Test type 2", 
            iso_name = "Test iso 2", 
            x_orig = 10, y_orig = 10, 
            x = c(20, 30), y = c(30, 40))
  expect_equal(res2$slope, c(-1, -1, -1))
  expect_equal(res2$intercept, c(50, 50, 70))

  # Now try indexing
  res3 <- res %>% 
    add_iso(indexed = TRUE, meta = meta2, graph_type = "Test type 3", 
            iso_name = "Test iso 3", 
            x_orig = 10, y_orig = 10, 
            x = c(20, 30), y = c(30, 40))
  expect_equal(res3$slope, c(-1, -1, -1))
  expect_equal(res3$intercept, c(50, 5, 7))
})



