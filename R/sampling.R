#' Random sample states
#'
#' This function will take the total number of compartments (or demes) in the
#' model and random select the number of diagnosed individuals not on ART.
#'
#' @param n_compartments Integer for the total number of compartments.
#' @param n_size Integer for the total number of individuals to random sample
#'    their states.
#' @param compartment_names Vector with the string names for each deme or
#'    compartment.
#'
#' @return Matrix with 0 and 1s. Value of 1 correspond to the sampled
#'    deme for each individual. Each colunm in the matrix is a deme in the model;
#'    and each row correspond to an individual.
#' @export
#'
#' @examples
#' ss <- sample_states(3, 10, c("care1", "care2", "care3"))
sample_states <- function(n_compartments, n_size, compartment_names){

  #index to sample (compartment names that contains care2 or care3)
  care2_index <- which(grepl(pattern = "care2", x = compartment_names))
  #care3_index <- which(grepl(pattern = "care3", x = compartment_names))

  #care_index <- c(care2_index, care3_index)
  care_index <- care2_index

  colindex <- sample(care_index,
                     replace = TRUE,
                     size = n_size)

  compartments <- matrix(0, nrow = n_size,
                         ncol = n_compartments)

  colnames(compartments) <- compartment_names

  for(i in 1:length(colindex)){

    compartments[i, colindex[i]] <- 1

  }

  return(compartments)


}

#' Random sample states
#'
#' This function will take the total number of compartments (or demes) in the
#' model and random select the total number of individuals in each compartment.
#'
#' @param n_compartments Integer for the total number of compartments.
#' @param n_size Integer for the total number of individuals to random sample
#'    their states.
#' @param compartment_names Vector with the string names for each deme or
#'    compartment.
#'
#' @return Matrix with 0 and 1s. Value of 1 correspond to the sampled
#'    deme for each individual. Each colunm in the matrix is a deme in the model;
#'    and each row correspond to an individual.
#' @export
#'
#' @examples
#' ss <- sample_states(3, 10, c("care1", "care2", "care3"))
sample_states_old <- function(n_compartments, n_size, compartment_names){

  #index to sample (compartment names that contains care2 or care3)
  care2_index <- which(grepl(pattern = "care2", x = compartment_names))
  care3_index <- which(grepl(pattern = "care3", x = compartment_names))

  care_index <- c(care2_index, care3_index)

  colindex <- sample(care_index,
                     replace = TRUE,
                     size = n_size)

  compartments <- matrix(0, nrow = n_size,
                         ncol = n_compartments)

  colnames(compartments) <- compartment_names

  for(i in 1:length(colindex)){

    compartments[i, colindex[i]] <- 1

  }

  return(compartments)


}

#' @export
sample_states2 <- function(n_compartments, n_size, compartment_names){


  colindex <- sample(1:120,
                     replace = TRUE,
                     size = n_size)

  compartments <- matrix(0, nrow = n_size,
                         ncol = n_compartments)

  colnames(compartments) <- compartment_names

  for(i in 1:length(colindex)){

    compartments[i, colindex[i]] <- 1

  }

  return(compartments)


}

