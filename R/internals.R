#' @title Convert Emplty List Elements to NA
#' 
#' @param x a list.
#'
#' @return The list `x` with the empty elements tranformed.
#' 
#' @keywords internal
#'
#' @noRd
#' 
.null_to_na <- function(x) {
  
  classes <- sapply(x, class)
  
  if ("data.frame" %in% classes) {
    
    df.names <- unique(unlist(sapply(x, colnames)))
    rep <-
      as.data.frame(matrix(NA, 1, length(df.names), dimnames = list(NULL, df.names)))
    replace_these <- sapply(x, function(x) length(x) == 0L) & 
                        classes %in% "data.frame"
    x[replace_these] <- lapply(x[replace_these], function(x) x <- rep)
  }
   
  if ("character" %in% classes) {
    
    x <- lapply(x, paste, collapse = ", ")
    replace_these <- x %in% "" & classes %in% "character"
    x[replace_these] <- lapply(x[replace_these], function(x) x <- NA)
  }
  
  if ("list" %in% classes) {
    
    replace_these <- sapply(x, function(x) length(x) == 0L) & 
                      classes %in% "list"
    x[replace_these] <- lapply(x[replace_these], function(x) x <- NA)
  }
   
  return(x)
}
#' @title Remove Special Characters
#'
#' @description Simple function imported and adapted from package `plantR` to
#'   replace accents by the same or corresponding letters without accents.
#'   Currently, the function basically replaces special characters corresponding
#'   to the Latin-1 Supplement and very few Latin Extended-A.
#'
#' @param x a vector with characters to be replaced
#'
#' @return the input vector with the special characters replaced
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'   nomes <- c("Thom\u00e9", "Mu\u00f1oz", "\u0153uf")
#'   nomes
#'
#'   .remove_latin(nomes)
#' }
#'
.remove_latin <- function(x) {
  
  #Getting the special characters to be replaced
  unwanted_latin <- names(unwantedLatin)
  replace_latin <- unwantedLatin
  
  #Single-letter replacements
  replace_latin1 <- replace_latin[nchar(replace_latin) == 1]
  unwanted_latin1 <- unwanted_latin[nchar(replace_latin) == 1]
  x <- chartr(
    paste(unwanted_latin1, collapse = ''),
    paste(replace_latin1, collapse = ''),
    x)
  
  #Double-letter replacements
  replace_latin2 <- replace_latin[nchar(replace_latin) == 2]
  names(replace_latin2) <- unwanted_latin[nchar(replace_latin) == 2]
  for (i in seq_along(replace_latin2))
    x <- gsub(names(replace_latin2)[i],
              replace_latin2[i],
              x, fixed = TRUE)
  
  return(x)
}

