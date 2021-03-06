

#' Calculate annual inflation adjusted costs
#'
#' Up to a present value (PV) inflated upwards.
#'
#' Option to use the following datasets:
#'
#' \itemize{
#'   \item \href{https://www.pssru.ac.uk/pub/uc/uc2017/sources-of-information.pdf}{PSSRU annual inflation hospital and community health services (HCHS)}
#'   \item \link{CPI}
#' }
#'
#' \deqn{(1 + i_{1})(1 + i_{2}) \cdots (1 + i_{n}) \times C}
#'
#' Clearly, for the same \code{i} each year this is simply
#' \deqn{C(1 + i)^n}
#'
#' @param from_year Date of cost to convert from
#' @param to_year Date to convert cost to
#' @param from_cost Cost at \code{from_year}
#' @param inflation_df_nm Source of data (string) \code{\link{HCHS}}, or \code{\link{CPI}}.
#'
#' @return Inflated (to) cost (scalar), with attributes used to generate the return value:
#' \itemize{
#'   \item \code{from_year}
#'   \item \code{to_year}
#'   \item \code{from_cost}
#'   \item \code{reference} Which inflation adjustment dataset used.
#'  }
#' @export
#'
#' @examples
#' from_year <- 2012
#' to_year <- 2015
#' from_cost <- 96.140
#'
# inflation_adjust_cost_data(from_year,
#                            to_year,
#                            from_cost)
#' 1*(1+0.035)^6
#'
#' inflation_adjust_cost_data(from_year = 2014,
#'                            to_year = 2016,
#'                            from_cost = 1,
#'                            inflation_df_nm = "HCHS")
#' 1*1.004*1.01
#'
inflation_adjust_cost_data <- function(from_year,
                                       to_year,
                                       from_cost,
                                       inflation_df_nm){
  
  data_sources <- utils::data(package = "inflately")
  data_sources <- data_sources$results[ ,"Item"]
  
  if (!(inflation_df_nm %in% data_sources)) {
    
    stop("inflation_data name not available")
  }

  # load data to workspace
  eval(parse(text = paste0("utils::data(", inflation_df_nm, ")")))
  inflation_df <- eval(parse(text = inflation_df_nm))
  
  to_cost <- 
    inflation_adjust_cost_custom(from_year,
                                 to_year,
                                 from_cost,
                                 inflation_df)   
  
  attr(to_cost, "from_year") <- from_year
  attr(to_cost, "to_year")   <- to_year
  attr(to_cost, "from_cost") <- from_cost
  attr(to_cost, "reference") <- inflation_df_nm
  
  return(to_cost)
}
