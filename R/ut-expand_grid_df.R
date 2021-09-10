#' Expand on two data.frames 
#'
#' @description An extension of expand.grid() to expand on two `data.frame`s
#' @param data_1 first data.frame
#' @param data_2 second data.frame
#' @export
#' @examples

expand_grid_df <- function(data_1, data_2) {

  expanded_data <- cbind(
    data.table(data_1)[rep(1:nrow(data_1), each = nrow(data_2)), ],
    data.table(data_2)[rep(1:nrow(data_2), nrow(data_1)), ]
  )

  return(expanded_data)

}
