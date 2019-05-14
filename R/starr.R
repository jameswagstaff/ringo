# starr.R - functions for ringo package

#' Single Table to Tibble
#'
#' Turns a single table (as a vector of lines) from the STAR file into a tibble.
#'
#' @param z A Vector of all the lines that make one STAR table
#'
#' @return A tibble
single_table_to_df <- function(z){
  if (stringr::str_detect(z[[2]], pattern = "^loop_")) {
    if (length(z) < 3) {return(NA)} # Some lists are of 0 length...
    names_cols <- z[grepl("^_",z)]
    first_data_row <- max(which(grepl("^_",z) == T))
    z <- z[-(1:first_data_row)]
    df <- readr::read_delim(as.vector(z),
                            delim = " ",
                            trim_ws = T,
                            col_names = names_cols)
  } else { # For tables without a "loop_"
    z <- z[-1]
    df <- dplyr::tibble("data" = z)
    df <- tidyr::separate(df, "data", into = c("key", "value"), sep = "[:blank:]+")
    df <- tidyr::spread(df, "key", "value")
  }

  return(df)
}


#' starr
#'
#' The main function in the package, for reading in a STAR file and loading
#'  into memory as a list of tibbles.
#'
#' @param path_to_star The path to the STAR file.
#'
#' @return A list of tibbles, one for each of the tables in the STAR file.
#'
#' @export
starr <- function(path_to_star){
  x <- readr::read_lines(path_to_star)
  x <- trimws(x, which = "both")
  x <- x[x != ""]
  x <- x[!stringr::str_detect(x,"#")]
  x <- split(x, cumsum(grepl("data_", x)))
  y <- purrr::map(x, single_table_to_df)
  names(y) <- purrr::map(x, ~ .x[[1]])
  return(y)
}
