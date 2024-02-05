#' teste
#'
#' @param demes_matrix teste
#'
#' @return  teste
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

  if(origin == "region"){
    region_tip <- seq(1:length(tip_names))
    tip_names <- paste(origin, tip_names, region_tip, sep = "_")

  }

  if(origin == "src"){
    src_tip <- seq(1:length(tip_names))
    tip_names <- paste(tip_names, src_tip, sep = "_")
  }



  return(tip_names)

}

#' Get metadata of tip to use as deme in the phydynR model
#'
#' @param tip_name character
#'
#' @return character with the deme name that can be "I", "Tr", or "src". "I"
#'    corresponds to infected individuals; Tr corresponds to individuals
#'    on treatment and "src" corresponds to source sequences that account for
#'    importation in the phylodynamic analysis.
#' @export
get_metadata <- function(tip_name){

  #origin will be region or source
  origin <- str_split(tip_name, "_")[[1]]

  if(origin[2] == "src"){
    deme <- "src"
  }

  if(origin[2] == "region"){

    #care <- str_split(origin[3], "\\.")[[1]][3]

    #if(care == "care3"){
    #  deme <- "Tr"
    #}else{
    #  deme <- "I"
    #}
    deme <- "I"

  }

  return(deme)
}
