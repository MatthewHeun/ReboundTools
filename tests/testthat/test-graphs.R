test_that("cost_graph() works as expected", {
  load_eeu_data() %>% 
    rebound_analysis() %>% 
    cost_graph()
})
