#' @title Get Metadata of ROR
#'
#' @description Get all fields for a given ROR record
#'
#' @param ror a character conatinig the ROR IDs
#' @param output the type of output desired: record metadata or relationships.
#'
#' @return a data frame with the fields available for each ROR record.
#'
#' @details 
#' 
#' This function queries the ROR API (https://ror.readme.io/docs/rest-api) for
#' retrieving organization record by ROR ID.
#' 
#' 
#' @author Renato A. F. de Lima
#' 
#' @importFrom curl curl_download
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#'
#' @examples
#'
#' \dontrun{
#' 
#'   
#'   rors <- "05x5km989"
#'   fetch_ror(rors, output = "metadata")
#'   
#' }   
#'   
#' @export fetch_ror
#'
fetch_ror <- function(ror, output = NULL) {
  
  ## check input
  not.ror <- ror %in% c("", " ", NA, "NA")
  # if (any(not.ror))
  #   warning ("One or more input characters are not ORCID numbers")
  
  if (all(not.ror))
    stop ("Please provide at least one valid ROR record")
  
  ror.id <- ror[!not.ror] 
  ror.id <- gsub("https://ror.org/", "", ror.id, perl = TRUE)
  ror.id <- gsub("ror.org/", "", ror.id, perl = TRUE)

  ## preparing for the query
  result <- result.rel <- vector("list", length(ror.id))
  names(result) <- names(result.rel) <- ror.id
  
  #### CHECK: HOW TO DEAL WITH DUPLICATED ENTRIES ####
  
  for (i in seq_len(length(result))) {
    
    ## SEARCH STRING
    # basic ROR API url
    my_url <- "https://api.ror.org/organizations/"
    
    # basic query definition
    ror.i <- as.character(ror.id[i])
    my_url <- paste0(my_url, ror.i)
    
    cat("\r Running ROR ID", i, "of", length(result)) 
    utils::flush.console()
    
    ## QUERYING ROR
    tmp <- tempfile()
    check.error <- try(curl::curl_download(my_url, tmp), TRUE)
    if (class(check.error) == "try-error") {
      result.i <- data.frame(id = "download error")
    } else {
      result.list.i <- jsonlite::fromJSON(tmp, flatten = TRUE)
      file.remove(tmp)
      
      remove_these <- !sapply(result.list.i, class) %in% c("list", "data.frame") 
      result.i <- dplyr::bind_rows(result.list.i[remove_these])

      #Converting lists and data frames

      if ("addresses" %in% names(result.list.i)) {
        addresses <- result.list.i$addresses
        if (dim(addresses)[1] > 0)
          result.i <- cbind.data.frame(result.i, addresses[1,])
      }
      
      if ("aliases" %in% names(result.list.i))
        result.i$aliases <- 
          paste(unlist(.null_to_na(result.list.i$aliases)), collapse = ", ")
      
      if ("labels" %in% names(result.list.i)) {
        labels <- result.list.i$labels
        if (length(labels) > 0) {
          result.i <- cbind.data.frame(result.i, labels[1, ])
        }
      }
      
      if ("country" %in% names(result.list.i)) {
        country <- dplyr::bind_rows(result.list.i$country)
        if (dim(country)[1] > 0) {
          result.i <- cbind.data.frame(result.i, country[1, ])
        }
      }
      
      if ("external_ids" %in% names(result.list.i)) {
        external_ids <- result.list.i$external_ids
        
        if ("ISNI" %in% names(external_ids)) {
          isni <- lapply(external_ids$ISNI, paste, collapse = ", ")
          isni <- unlist(isni, use.names = TRUE)
          names(isni) <- paste("ISNI", names(isni), sep = ".") 
          result.i <- cbind.data.frame(result.i, t(isni))
        }
         
        if ("FundRef" %in% names(external_ids)) {
          fundref <- lapply(external_ids$FundRef, paste, collapse = ", ")
          fundref <- unlist(fundref, use.names = TRUE)
          names(fundref) <- paste("FundRef", names(fundref), sep = ".") 
          result.i <- cbind.data.frame(result.i, t(fundref))
        }
        
        if ("GRID" %in% names(external_ids)) {
          grid <- lapply(external_ids$GRID, paste, collapse = ", ")
          grid <- unlist(grid, use.names = TRUE)
          names(grid) <- paste("GRID", names(grid), sep = ".") 
          result.i <- cbind.data.frame(result.i, t(grid))
        }
      }

      if ("relationships" %in% names(result.list.i)) {
        relationships <- result.list.i$relationships
        if (length(relationships) > 0) {
          colunas <- names(result.i)[names(result.i) %in% 
                                        c("id", "name", "types", "label", 
                                          "country_name", "country_code")]
          relationships <- cbind.data.frame(relationships, 
                                            result.i[, colunas])
          result.rel[[i]] <- relationships
        }
      }
      
      
      # result.i <- data.frame(id = "zero results")
      # result.i <- data.frame(id = result.list.i$errors[[1]][1])
      
      result[[i]] <- result.i
    }      
  }
  
  if (output %in% "metadata")
    output.ror <- dplyr::bind_rows(result, .id = "query")
  
  if (output %in% "relationships")
    output.ror <- dplyr::bind_rows(result.rel, .id = "query")

    return(as.data.frame(output.ror))
}
