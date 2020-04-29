# starr.R - functions for ringo package

#' Single Table to Tibble
#'
#' Turns a single table (as a vector of lines) from the STAR file into a tibble.
#'
#' @param z A Vector of all the lines that make one STAR table
#'
#' @return A tibble
single_table_to_df <- function(z, clean_relion_names = TRUE){
  if (stringr::str_detect(z[[2]], pattern = "^loop_")) {
    if (length(z) < 3) {return(NA)} # Some lists are of 0 length...
    names_cols <- z[grepl("^_",z)]
    if (clean_relion_names){
      names_cols <- stringr::str_extract(
        string  = names_cols,
        pattern = "(?<=_).*(?= #)"
      )
    }
    first_data_row <- max(which(grepl("^_",z) == T))
    z <- z[-(1:first_data_row)]
    #return(z)
    #z <- paste0(z,"\n")
    df <- readr::read_delim(paste0(as.vector(z),"\n"),
                            # read delim requires newline to recognise as raw data (not as path)
                            delim = " ",
                            trim_ws = T,
                            col_names = names_cols)
  } else { # For tables without a "loop_"
    z <- z[-1]
    df <- dplyr::tibble("data" = z)
    df <- tidyr::separate(df, "data", into = c("key", "value"), sep = "[:blank:]+")
    if (clean_relion_names){
      df <- dplyr::mutate(df, key = stringr::str_extract(key,"(?<=_).*"))
    }
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
#' @return A tibble with two variables:
#' table_name - a character vector of table names.
#' table_tbl - a list of tibbles, one for each of the tables in the STAR file.
#'
#' @export
starr <- function(path_to_star, clean_relion_names = TRUE){
    x <- readr::read_lines(path_to_star)
    x <- trimws(x, which = "both")               # trim white space
    x <- x[substr(x,0,1)!= "#"]                  # remove comment lines
    x <- x[x != ""]                              # remove empty rows
    x <- unname(split(x, cumsum(grepl("data_", x))))
    x <- tibble::tibble("table_raw" = x)
    x <- dplyr::mutate(
      x, "table_name" = as.vector(purrr::map(table_raw, ~ .x[[1]])) )
    x <- dplyr::mutate(
      x, "table_tbl" = purrr::map(
        table_raw, single_table_to_df, clean_relion_names = clean_relion_names))
    x <- dplyr::select(x,table_name,table_tbl)
    return(x)
}
