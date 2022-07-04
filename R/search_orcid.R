#' @title Search ORCID
#'
#' @description Search for ORCID based on people's names.
#'
#' @param x a data frame containing at least two columns: first name and last
#'   name
#' @param first.name character. The name of the column containing the first
#'   name. Defaults to "FirstName".
#' @param last.name character. The name of the column containing the last name.
#'   Defaults to "LastName".
#' @param middle.name character. The name of the column containing the middle
#'   name (optional). Defaults to "MiddleName"
#' @param orcid.name character. The name of the column containing the ORCID
#'   number (optional). Defaults to "ORCID".
#' @param keywords character. one or more keywords to filter the search output,
#'   in the case of two or more ORCIDs
#' @param rows integer. The number of ORCID records to return, to be passed to
#'   function rorcid::orcid_search. Defaults to 20.
#' @param clean logical. Should the function return only the exact name search?
#'   Defaults to FALSE.
#' @param save.names logical. Should the people's names retrieved be returned as
#'   well? Defaults to TRUE.
#'
#' @return the same input data frame with the missing (i.e. NA) ORCID, and
#'   optionally the corresponding names, if found.
#'
#' @details 
#' 
#' If only one researcher is found the function returns a sole ORCID. If
#' there are two or more ORCID found, all ORCID are returned separated by a pipe
#' (i.e. "|"). 
#' 
#' The arguments 'keywords' can be used to narrow down the search if multiple
#' IDs are found. But it will only work if the reasearchers have included
#' keywords in their ORCID records, which is not always the case (see examples).
#' 
#' In addition, if argument 'clean' is TRUE, it will exclude those IDs which are
#' not exact matches, which can be useful to remove spurious hits. Also, you may
#' want to increase the number of results returned by each query if the names
#' provided are too common, to make sure that the name you are searching is
#' indeed returned in the query.
#' 
#' By default, the function performs queries only for missing DOIs in the in the
#' input column defined by `orcid.name` in the object `x`. It alsos returns two
#' new columns containing the first and last names as retrieved in ORCID, but
#' this option is controleed by the argument `save.names`. 
#' 
#' The package __rorcid__ which is used internally, requires an ORCID token or
#' that the user starts a ORCID session and authorise the package, so it can
#' query their API. So, make sure you have one of the options ready before using
#' the function.
#' 
#' @author Renato A. F. de Lima
#' 
#' @importFrom rorcid orcid_search
#' @importFrom dplyr bind_rows
#'
#' @examples
#'
#' \dontrun{
#'   # Single ORCID record with the name
#'   df <- data.frame(FirstName = "Nicolas", LastName = "Casajus")
#'   search_orcid(df)
#'   
#'   # Multiple ORCID entries
#'   df <- data.frame(FirstName = "Renato", LastName = "Lima")
#'   search_orcid(df)
#'   search_orcid(df, clean = TRUE)
#'   search_orcid(df, keywords = "ecology")
#' }   
#'   
#' @export search_orcid   
#'
#'
#'
search_orcid <- function(x,
                      first.name = "FirstName",
                      last.name = "LastName",
                      middle.name = "MiddleName",
                      orcid.name = "ORCID",
                      keywords = NULL,
                      rows = 20,
                      clean = FALSE,
                      save.names = TRUE) {
  
  ## check input
  if (!"data.frame" %in% class(x))
    stop ("Input object needs to be a data frame!")
  
  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")
  
  if (!first.name %in% names(x))
    stop("Input data frame does not contain the column with first names. Check input or redefine the column name")
  
  if (!last.name %in% names(x))
    stop("Input data frame does not contain the column with last names. Check input or redefine the column name")

  if (!orcid.name %in% names(x))
    x[, orcid.name] <- NA
  
  if (!middle.name %in% names(x))
    x[, middle.name] <- NA
  
  ## preparing for the query
  result <- vector("list", dim(x)[1])
  full.names.orig <- 
    apply(x[ , c(first.name, last.name)], 1, paste, collapse = " ")
  dup.names <- full.names.orig[duplicated(full.names.orig)]
  check_these <- full.names.orig %in% dup.names
  full.names.orig[check_these] <- stats::ave(
    full.names.orig[check_these], full.names.orig[check_these], 
    FUN = function(i) paste0(i, '', seq_along(i)))
  names(result) <- full.names.orig  
  
  ##querying
  for (i in seq_len(length(result))) {
    
    dados <- x[i, ]
    first <- as.character(dados[, first.name])
    last <- last.old <- as.character(dados[, last.name])
    full <- paste(first, last)
    
    cat("\r Running name", i, "of", length(result)) 
    utils::flush.console()

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

        if (middle.name %in% names(x)) {
          if (!is.na(dados[, middle.name]))
            orcid.i <- as.data.frame(
              rorcid::orcid_search(given_name = first, 
                                   family_name = last.old,
                                   rows = rows))
        }
        
        if (dim(orcid.i)[1] == 0) {
          result[[i]] <-
            cbind.data.frame(first = first,
                             last = last,
                             orcid = "not found",
                             row.names = full)
          next
        }
        
        if (dim(orcid.i)[1] == 1) {
          row.names(orcid.i) <- full
          result[[i]] <- orcid.i
          next
        }    

        if (dim(orcid.i)[1] > 1) {
          
          if (clean) {
            keep <- orcid.i$last %in% c(last, last.old) & orcid.i$first %in% first
            if (any(keep)) {
              orcid.i <- orcid.i[keep, ] 
            } else {
              orcid.i <- orcid.i
              warning("Attempt to clean records final excluded all of them; keeping the results from the non-exact match")            
            }
          }
          
          if (dim(orcid.i)[1] > 1) {
            row.names(orcid.i) <- paste0(full, seq_len(dim(orcid.i)[1]))
            result[[i]] <- orcid.i
            next
          } else {
            row.names(orcid.i) <- full
            result[[i]] <- orcid.i
            next
          }
        }
      }
      
      if (dim(orcid.i)[1] == 1) {
        row.names(orcid.i) <- full
        result[[i]] <- orcid.i
        next
      }    
      
      if (dim(orcid.i)[1] > 1) {
        
        if (clean) {
          keep <- tolower(orcid.i$last) %in% tolower(last) & 
                    tolower(orcid.i$first) %in% tolower(first)
          if (!any(keep))
            keep <- tolower(orcid.i$last) %in% tolower(last.old) & 
              tolower(orcid.i$first) %in% tolower(first)
          
          if (!any(keep))
            keep <- grepl(last.old, orcid.i$last, perl = TRUE, ignore.case = TRUE) &
                        grepl(first, orcid.i$first, perl = TRUE, ignore.case = TRUE)
          
          if (any(keep)) {
            orcid.i <- orcid.i[keep, ] 
          } else {
            orcid.i <- orcid.i
            warning("Attempt to clean records finally excluded all of them; keeping the results from the non-exact match")            
          }
        }

        if (dim(orcid.i)[1] > 1) {
          
          if (!is.null(keywords)) {
            orcid.ii <- as.data.frame(
              rorcid::orcid_search(
                given_name = first,
                family_name = last,
                keywords = keywords))
            
            if (clean) {
              keep <- tolower(orcid.i$last) %in% tolower(last) & 
                tolower(orcid.i$first) %in% tolower(first)
              if (!any(keep))
                keep <- tolower(orcid.i$last) %in% tolower(last.old) & 
                  tolower(orcid.i$first) %in% tolower(first)
              
              if (!any(keep))
                keep <- grepl(last.old, orcid.i$last, perl = TRUE, ignore.case = TRUE) &
                  grepl(first, orcid.i$first, perl = TRUE, ignore.case = TRUE)
              
              if (any(keep)) {
                orcid.i <- orcid.i[keep, ] 
              } else {
                orcid.i <- orcid.i
                warning("Attempt to clean records finally excluded all of them; keeping the results from the non-exact match")            
              }
            }

            if (dim(orcid.ii)[1] == 1) {
              row.names(orcid.ii) <- full
              result[[i]] <- orcid.ii
              next
            } else {
              row.names(orcid.i) <- paste0(full, seq_len(dim(orcid.i)[1]))
              result[[i]] <- orcid.i
              next
            }
          } else {
            row.names(orcid.i) <- paste0(full, seq_len(dim(orcid.i)[1]))
            result[[i]] <- orcid.i
            next
          }
        } else {
          row.names(orcid.i) <- full
          result[[i]] <- orcid.i
          next
        }
      }
      else {
        row.names(orcid.i) <- full
        result[[i]] <- orcid.i
        next
      }
    } else {
      result[[i]] <- cbind.data.frame(
        first = first,
        last = last,
        orcid = dados$ORCID,
        row.names = full)
      next
    }
  }
  cat("\n")
  
  output <- dplyr::bind_rows(result, .id = "full.names.orig")
  colunas <- c("first", "last", "orcid")
  orcid.output <- 
    stats::aggregate(output[, colunas], 
                     list(output$full.names.orig),
                     paste0, collapse = "|")
  orcid.output <- orcid.output[match(full.names.orig, orcid.output$Group.1),]
  
  new.orcid <- paste0(orcid.name, ".new")
  x[, new.orcid] <- NA_character_
  replace_these <- is.na(x[ , orcid.name])
  x[replace_these, new.orcid] <- 
    as.character(orcid.output$orcid[replace_these]) 
  x[!replace_these, new.orcid] <- "not queried" 
  
  if (save.names) {
    new.columns <- c("firstName.orcid", "lastName.orcid")
    check_these <- new.columns %in% names(x)
    if (any(check_these))
      new.columns[check_these] <- paste0(new.columns[check_these], ".new")
    
    x[, new.columns] <- orcid.output[, c("first", "last")]
  }
  
  return(x)
}
