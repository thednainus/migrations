#' teste
#'
#' @param region_n teste
#' @param exog_n teste
#'
#' @return  tesre
#' @export
#'
#' @examples
#' teste
create_tip_names <- function(demes_matrix, origin){

  #get index of matrix in which values == 1 to ge the tip names
  indexes <- which(demes_matrix == 1, arr.ind = TRUE)

  #order index by row
  indexes <- indexes[order(indexes[,1],decreasing=FALSE),]


  tip_names <- NULL

  for(i in 1:nrow(indexes)){

    rowindex <- unname(indexes[i,]["row"])
    colindex <- unname(indexes[i,]["col"])

    tipname <- names(demes_matrix[rowindex, colindex])
    tip_names <- c(tip_names, tipname)

  }

    region_tip <- seq(1:length(tip_names))
    tip_names <- paste(tip_names, region_tip, sep = "_")


  return(tip_names)

}
