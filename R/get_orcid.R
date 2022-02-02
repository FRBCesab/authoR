#' @title Get ORCID
#'
#' @description Get ORCID based on people's names.
#'
#' @param x a data frame containing at least two columns: first name and last
#'   name
#' @param first.name character. The name of the column containing the first
#'   name. Defaults to "FirstName".
#' @param last.name character. The name of the column containing the last name.
#'   Defaults to "LastName".
#' @param middle.name character. The name of the column containing the middle
#'   name (optional). Defaults to "MiddleName_Initials"
#' @param orcid.name character. The name of the column containing the ORCID
#'   number (optional). Defaults to "ORCID".
#' @param keywords character. one or more keywords to filter the search output,
#'   in the case of two or more ORCIDs
#' @param rows integer. The number of ORCID records to return, to be passed to
#'   function rorcid::orcid_search
#' @param clean logical. Should the function return only the exact name search?
#'   Defaults to TRUE
#'    
#'
#' @return the same input data frame with the missing (i.e. NA) ORCID, if found.
#'
#' @details The function ...
#' 
#' If there are two or more ORCID found, all ORCID are returned separated by a
#' pipe (i.e. "|").
#'
#' @author Renato A. F. de Lima
#' 
#' @importFrom rorcid orcid_search
#' @importFrom dplyr bind_rows
#'
#' @examples
#'
#' \dontrun{
#' # Simple names
#'   df <- data.frame(FirstName = "Nicolas", LastName = "Casajus")
#'   get_orcid(df)
#'   
#' @export get_orcid   
#'
get_orcid <- function(x,
                      first.name = "FirstName",
                      last.name = "LastName",
                      middle.name = "MiddleName_Initials",
                      orcid.name = "ORCID",
                      keywords = NULL,
                      rows = 10,
                      clean = TRUE) {
  
  ## check input
  if (!"data.frame" %in% class(x))
    stop ("Input object needs to be a data frame!")
  
  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")
  
  ## check the presence of input columns
  if (!orcid.name %in% names(x))
    x[, orcid.name] <- NA
  
    
  result <- vector("list", dim(x)[1])
  
  for (i in seq_len(length(result))) {
    
    dados <- x[i, ]
    first <- as.character(dados[, first.name])
    last <- as.character(dados[, last.name])
    full <- paste(first, last)
    
    if (middle.name %in% names(x)) {
      if (!is.na(dados[, middle.name]))
        last <- paste(as.character(dados[, middle.name]), last)
    }

    if (is.na(dados[, orcid.name])) {
      orcid.i <- as.data.frame(
        rorcid::orcid_search(given_name = first, 
                                      family_name = last,
                                      rows = rows))
      
      if (dim(orcid.i)[1] == 0) {
        result[[i]] <-
          cbind.data.frame(first = first,
                           last = last,
                           orcid = "not found",
                           row.names = full)
      }
      
      if (dim(orcid.i)[1] == 1) {
        row.names(orcid.i) <- full
        result[[i]] <- orcid.i
      }    
      
      if (dim(orcid.i)[1] > 1) {
        
        if (clean){
          keep <- orcid.i$last %in% last & orcid.i$first %in% first
          if (any(keep)) {
            orcid.i <- orcid.i[keep, ] 
          } else {
            orcid.i <- orcid.i
            warning("Attempt to clean records final excluded all of them; keeping the results from the non-exact match")            
          }
        }
          
        
        if (dim(orcid.i)[1] > 1) {
          orcid.ii <- as.data.frame(
            rorcid::orcid_search(
              given_name = first,
              family_name = last,
              keywords = keywords))
          
          if (dim(orcid.ii)[1] == 1) {
            row.names(orcid.ii) <- full
            result[[i]] <- orcid.ii
            
          } else {
            row.names(orcid.i) <- paste0(full, seq_len(dim(orcid.i)[1]))
            result[[i]] <- orcid.i
          }
          
        } else {
          row.names(orcid.i) <- full
          result[[i]] <- orcid.i
        }
        
        
      }
      
    } else {
      
      result[[i]] <- cbind.data.frame(
        first = first,
        last = last,
        orcid = dados$ORCID,
        row.names = full)
      
    }
    
  }
  
  output <- dplyr::bind_rows(result)
  full.names <- gsub("[0-9]", "", row.names(output), perl = TRUE)
  orcid.output <- 
    aggregate(output$orcid, list(full.names), paste0, collapse = "|")
  
  x[is.na(x[ , orcid.name]), orcid.name] <- 
    as.character(orcid.output$x[is.na(x[ , orcid.name])]) 
  
  return(x)
}
