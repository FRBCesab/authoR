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
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#'
#' @examples
#'
#' \dontrun{
#' 
#'   df <- data.frame(OrganizationName = "USP", CountryName = "Brazil")
#'   search_ror(df)
#'   
#' }   
#'   
#' @export search_ror
#'
search_ror <- function(x,
                    org.name = "OrganizationName",
                    country.name = "CountryName",
                    type = NULL,
                    matching.type = NULL) {
  
  ## check input
  if (!"data.frame" %in% class(x))
    stop ("Input object needs to be a data frame!")
  
  if (dim(x)[1] == 0)
    stop("Input data frame is empty!")
  
  ## check the presence of input columns
  if (!org.name %in% names(x))
    stop("Input data frame must have the organization name. Check input or redefine the column name")
  
  result <- vector("list", dim(x)[1])
  names(result) <- x[, org.name]
  
  for (i in seq_len(length(result))) {
    
    ## SEARCH STRING
    # basic ROR API url
    my_url <- "https://api.ror.org/organizations?query="
    
    # basic query definition and editing
    org.name.i <- as.character(x[i, org.name])
    org.name.i <- gsub("\\/", "%5C%2F", org.name.i, perl = TRUE)
    org.name.i <- gsub("\\&", "%26", org.name.i, perl = TRUE)
    org.name.i <- gsub("-", "%5C-", org.name.i, perl = TRUE)
    org.name.i <- gsub("\\(", "%5C%28", org.name.i, perl = TRUE)
    org.name.i <- gsub("\\)", "%5C%29", org.name.i, perl = TRUE)
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
          my_url <- paste0(my_url, ",country.country_name:", x[i, country.name])
    }
    
    #replacing special characters
    my_url <- gsub(" ", "%20", my_url, fixed = TRUE)
    
    ## QUERYING ROR
    tmp <- tempfile()
    curl::curl_download(my_url1, tmp)
    result.list.i <- jsonlite::fromJSON(tmp)
    file.remove(tmp)
    
    if (!"errors" %in% names(result.list.i)) {
      if (dim(result.list.i$items)[1] > 0) {
        result.i <- result.list.i$items
      } else {
        result.i <- data.frame(id = "not found")
      }
    } else {
      result.i <- data.frame(id = result.list.i$errors[[1]][1])
    }
    result[[i]] <- result.i
  }
  
  output <- dplyr::bind_rows(result, .id = "query")
  # Do any type of extra filtering/celaning based on arguments?
  # Need to transform columns that are lists into character strings
  # ror.output <- 
  #   stats::aggregate(output, list(output$query), paste0, collapse = "|")
  # ror.output <- ror.output[match(x[, org.name], ror.output$Group.1),]
  
  return(output)
}
