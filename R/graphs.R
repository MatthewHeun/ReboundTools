#' Title
#'
#' @param .rebound_data 
#'
#' @return
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   cost_graph()
cost_graph <- function(.rebound_data) {
  # Make a data frame to be plotted.
  DF <- .rebound_data %>% 
    dplyr::select(eeu_base_params$case, eeu_base_params$original, eeu_base_params$upgrade,
                  starts_with("C_dot"), starts_with("Delta_C"))
  
  
}