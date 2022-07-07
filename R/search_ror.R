#' @title Get ROR
#'
#' @description Get ROR ID based on the organisation name.
#'
#' @param x a data frame containing at least one column: the organization name
#' @param org.name character. The name of the column containing the organization
#'   name. Defaults to "OrganizationName".
#' @param country.name character. The name of the column containing the name of the organization country.
#'   Defaults to "CountryName".
#' @param type character. The type of organizations for matching (see details) 
#' @param matching.type character. The type of match performed (see details)
#' @param rows numerical. The row(s) number(s) to be returned up to 20. Default
#'   to the first one, which is the one with the best search score.
#'
#' @return the same input data frame with the missing (i.e. NA) ORCID, and
#'   optionally the corresponding names, if found.
#'
#' @details 
#' 
#' This function queries the ROR API (https://ror.readme.io/docs/rest-api)
#' 
#' If not NULL, the argument `type` can be one of the following: Education,
#' Healthcare, Company, Archive, Nonprofit, Government, Facility, and Other.
#' 
#' If not NULL, the argument `matching.type` can be one of the following:
#' - PHRASE: the entire phrase matched to a variant of the organization's name
#' - COMMON TERMS: the matching was done by comparing the words separately
#' - FUZZY: the matching was done by fuzzy-comparing the words separately
#' - HEURISTICS: "University of X" was matched to "X University"
#' - ACRONYM: matching by acronym

#' 
#' @author Renato A. F. de Lima
#' 
#' @importFrom curl curl_download
#' @importFrom jsonlite fromJSON flatten
#' @importFrom dplyr bind_rows
#'
#' @examples
#'
#' \dontrun{
#' 
#'   
#'   df <- data.frame(
#'   OrganizationName = c("USP", "Universidade de Sao Paulo", "USP"), 
#'   CountryName = c("Brazil", "Brazil", NA))
#'   search_ror(df)
#'   search_ror(df, matching.type = "ACRONYM")
#'   
#' }   
#'   
#' @export search_ror
#'
search_ror <- function(x,
                    org.name = "OrganizationName",
                    country.name = "CountryName",
                    type = NULL,
                    matching.type = NULL,
                    rows = 1) {
  
  ## check input
  if (!"data.frame" %in% class(x))
    stop ("Input object needs to be a data frame!")
  
  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")
  
  if (!org.name %in% names(x))
    stop("Input data frame must have the organization name. Check input or redefine the column name")
  
  if (!country.name %in% names(x)) {
    country.name = NULL
    warning("Input data frame should have the country name. Ignoring this filter")
  }
    
  ## preparing for the query
  result <- vector("list", dim(x)[1])
  full.names.orig <- x[, org.name]
  dup.names <- full.names.orig[duplicated(full.names.orig)]
  check_these <- full.names.orig %in% dup.names
  full.names.orig[check_these] <- stats::ave(
    full.names.orig[check_these], full.names.orig[check_these], 
    FUN = function(i) paste0(i, '', seq_along(i)))
  names(result) <- full.names.orig  

  for (i in seq_len(length(result))) {
    
    ## SEARCH STRING
    # basic ROR API url
    my_url <- "https://api.ror.org/organizations?query="
    
    # basic query definition and editing
    org.name.i <- as.character(x[i, org.name])
    org.name.i <- .remove_latin(org.name.i)
    org.name.i <- gsub("\\/", "%5C%2F", org.name.i, perl = TRUE)
    org.name.i <- gsub("\\&", "%26", org.name.i, perl = TRUE)
    org.name.i <- gsub("-", "%5C-", org.name.i, perl = TRUE)
    org.name.i <- gsub("\\(", "%5C%28", org.name.i, perl = TRUE)
    org.name.i <- gsub("\\)", "%5C%29", org.name.i, perl = TRUE)
    #### IL FAUT CONFIRMER SI C EST BIEN LES CODES URL ICI  ###
    org.name.i <- gsub("\u2019", "%E2%80%99", org.name.i, perl = TRUE)
    org.name.i <- gsub("\u2013", "%E2%80%93", org.name.i, perl = TRUE)
    
    my_url <- paste0(my_url, org.name.i)
    
    # matching type
    if (!is.null(matching.type)) {
      if (matching.type %in% c("PHRASE", "COMMON TERMS", "FUZZY", 
                               "HEURISTICS", "ACRONYM")) {
        my_url <- paste0(my_url, ",matching_type=", matching.type)
      } else {
        warning("Type of matching not found (check function details for valid matching types). Ignoring")
      }
    }
    
    ## FILTERS
    # type of organization
    if (!is.null(type)) {
      if (type %in% c("Education", "Healthcare", "Company", "Archive", 
                      "Nonprofit", "Government", "Facility", "Other")) {
        my_url <- paste0(my_url, "&filter=types:", type)
      } else {
        warning("Type of organization not found (see function details for valid type names). Ignoring")
      }
    }
    
    # country name
    if (!is.null(country.name)) {
      if (country.name %in% names(x))
        if (!is.na(x[i, country.name]))
          if (grepl("&filter", my_url)) {
            my_url <- paste0(my_url, ",country.country_name:", x[i, country.name])
          } else {
            my_url <- paste0(my_url, "&filter=country.country_name:", x[i, country.name])
          }
    }
    
    #replacing special characters
    my_url <- gsub(" ", "%20", my_url, fixed = TRUE)
    
    cat("\r Running organization name", i, "of", length(result)) 
    utils::flush.console()

    ## QUERYING ROR
    tmp <- tempfile()
    check.error <- try(curl::curl_download(my_url, tmp), TRUE)
    if (class(check.error) == "try-error") {
      result.i <- data.frame(id = "download error")
    } else {
      result.list.i <- jsonlite::fromJSON(tmp)
      file.remove(tmp)
      
      if (!"errors" %in% names(result.list.i)) {
        if (length(result.list.i$items) > 0) {
          result.i.flat <- jsonlite::flatten(result.list.i$items)
          remove_these <- !sapply(result.i.flat, class) %in% "list" 
          result.i <- result.i.flat[, remove_these]
          #Converting lists into data frame columns
          if ("types" %in% names(result.i.flat))
            result.i$types <- 
              as.character(do.call(rbind, .null_to_na(result.i.flat$types)))

          if ("links" %in% names(result.i.flat))
            result.i$links <- 
              as.character(do.call(rbind, .null_to_na(result.i.flat$links)))
          
          if ("acronyms" %in% names(result.i.flat))
            result.i$acronyms <-
              as.character(do.call(rbind, .null_to_na(result.i.flat$acronyms)))
          
          
          if ("addresses" %in% names(result.i.flat)) {
            addresses <- .null_to_na(result.i.flat$addresses)
            if (any(!is.na(addresses))) {
              addresses <- dplyr::bind_rows(addresses)
              result.i <- cbind.data.frame(result.i, addresses)
            }  
          }
          
          if ("labels" %in% names(result.i.flat)) {
            labels <- .null_to_na(result.i.flat$labels)
              
            if (any(!is.na(labels))) {
              labels <- dplyr::bind_rows(labels, .id = "id")
              labels <- labels[!duplicated(labels$id), -1]
              result.i <- cbind.data.frame(result.i, labels)
            }
          }

          if ("aliases" %in% names(result.i.flat))
            result.i$aliases <- 
              as.character(do.call(rbind,.null_to_na(result.i.flat$aliases)))
          
          if ("external_ids.ISNI.all" %in% names(result.i.flat))
            result.i$external_ids.ISNI.all <- 
              as.character(do.call(rbind,.null_to_na(result.i.flat$external_ids.ISNI.all)))
          
          if ("external_ids.FundRef.all" %in% names(result.i.flat))
            result.i$external_ids.FundRef.all <- 
              as.character(do.call(rbind,.null_to_na(result.i.flat$external_ids.FundRef.all)))
          
          if ("external_ids.OrgRef.all" %in% names(result.i.flat))
            result.i$external_ids.OrgRef.all <- 
              as.character(do.call(rbind,.null_to_na(result.i.flat$external_ids.OrgRef.all)))
          
          if ("external_ids.Wikidata.all" %in% names(result.i.flat))
            result.i$external_ids.Wikidata.all <- 
              as.character(do.call(rbind,.null_to_na(result.i.flat$external_ids.Wikidata.all)))
          
          #### CHECK HERE IF rows = 2 AND dim(result.i)[1] == 1 ####
          
          result.i <- result.i[rows, , drop = FALSE]
          
        } else {
          result.i <- data.frame(id = "zero results")
        }
      } else {
        result.i <- data.frame(id = result.list.i$errors[[1]][1])
      }      
    }
    result[[i]] <- result.i
  }
  
  output <- dplyr::bind_rows(result, .id = "query")
  # Do any type of extra filtering/celaning based on arguments?
  # ror.output <- stats::aggregate(output[, -1], 
  #                    list(output$query),
  #                    paste0, collapse = "|")
  # ror.output <- ror.output[match(full.names.orig, ror.output$Group.1),]

  return(as.data.frame(output))
}
