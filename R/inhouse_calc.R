#' Inhouse Customer Score Calculation
#'
#' Calculating customer score from our 5 key customer qualities, weighted by importance.
#'
#' @param data Dataset; must have our predefined column names: customer_qual1 to customer_qual5.
#'
#' @return Vector of customer_score values, in a range between 0 and 100.
#' @export
#'
#' @examples inhouse_data$customer_score <- inhouse_calc()
#' @examples inhouse_data$customer_score <- inhouse_calc(data = inhouse_data)
#'
inhouse_calc <- function(data = inhouse_data){
  data$customer_score = 5 * scale(data$customer_qual1, center = FALSE) +
    4 * scale(data$customer_qual2, center = FALSE) +
    3 * scale(data$customer_qual3, center = FALSE) +
    2 * scale(data$customer_qual4, center = FALSE) +
    1 * scale(data$customer_qual5, center = FALSE)
  customer_score <- (data$customer_score / max(data$customer_score)) * 100
  attributes(customer_score) <- NULL
  return(customer_score)
}
