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
