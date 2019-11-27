
#' Inflation adjust a cost to present value
#'
#' @param from_year base year to inflate from
#' @param to_year destination year to inflate to
#' @param from_cost base year cost 
#' @param inflation_df inflation annual rate data. dataframe of annual inflation data, in the format
#' "year" = year (yyyy), "rate" = annual inflation rate (1/100th fraction)
#'
#' @export
#' 
#' @examples 
#' inflation_df <- data.frame("year" = 2008:2010, "rate" = c(0.02, 0.03, 0.025))
#' inflation_adjust_cost_custom(2008, 2009, 100, inflation_df)
#' 100*1.02 # 102 
#'
inflation_adjust_cost_custom <- function(from_year,
                                         to_year,
                                         from_cost,
                                         inflation_df) {
  
  if (from_year %% 1 != 0) stop("From date must be an integer valued whole year")
  if (to_year %% 1 != 0) stop("To date must be an integer valued whole year")
  if (from_cost < 0) stop("Cost must be non-negative")
  if(!from_year %in% inflation_df$year) stop("from year not in look-up table")
  if(!to_year %in% inflation_df$year) stop("to year not in look-up table")
  
  to_cost <- from_cost
  
  for (i in from_year:(to_year - 1)) {
    
    rate <- inflation_df[inflation_df$year == i, "rate"]
    to_cost <- to_cost * (1 + rate)
  }
  
  return(to_cost)
}