#' @title Get Author Info
#'
#' @description Get and organize author's personal information obtained from
#'   ORCID.
#'
#' @param x character. 
#'    
#' @return the ...
#'
#' @details The function ...
#' 
#' If two or more ORCID are provided, ...
#'
#' @author Renato A. F. de Lima
#' 
#' @importFrom rorcid orcid_person
#'
#' @examples
#'
#' \dontrun{
#'   orcids <- c("0000-0002-5537-5294", "0000-0002-1048-0138")
#'   orcid_info(orcids)
#'   
#' @export orcid_info   
#'
orcid_info <- function(orcid) {
  
  ## check input
  not.orcid <- grepl("\\D", gsub("-", "", orcid, fixed = TRUE), perl = TRUE)
  if (any(not.orcid))
    warning ("One or more input objects are not ORCID numbers!")
  
  ## getting info from ORCID
  info <- rorcid::orcid_person(orcid[!not.orcid], details = FALSE)
  
  ## extracting relevant info
  given <- lapply(info, function(x) x[['name']][['given-names']][['value']])
  family <- lapply(info, function(x) x[['name']][['family-name']][['value']])
  credit <- lapply(info, function(x) x[['name']][['credit-name']])
  other <- 
    lapply(info, function(x) x[['other-names']][['other-name']][['content']])
  
  biography <- lapply(info, function(x) x[['biography']][['content']])
  
  urls.name <- 
    lapply(info, function(x) x[['researcher-urls']][['researcher-url']][['url-name']])
  urls.path <- 
    lapply(info, function(x) x[['researcher-urls']][['researcher-url']][['url.value']])
  # urls <- mapply(FUN = paste0, urls.name, " [[", urls.path,"]]")
  urls <- lapply(urls.path, function(x) paste0(x, collapse = "|"))

  emails <- lapply(info, function(x) paste0(x[['emails']][['email']], collapse = "|"))
  
  country <- 
    lapply(info, function(x) x[['addresses']][['address']][['country.value']])

  keywords <- 
    lapply(info, function(x) paste0(x[['keywords']][['keyword']][['content']], collapse = "|"))
  
  other.id.name <- 
    lapply(info, function(x) x[['external-identifiers']][['external-identifier']][['external-id-type']])
  other.id <- 
    lapply(info, function(x) x[['external-identifiers']][['external-identifier']][['external-id-value']])
  ids <- mapply(FUN = paste0, other.id.name, ": ", other.id)
  ids <- lapply(ids, function(x) paste0(x, collapse = "|"))
  other.id.url <- 
    lapply(info, function(x) x[['external-identifiers']][['external-identifier']][['external-id-url.value']])
  other.id.url <- lapply(other.id.url, function(x) paste0(x, collapse = "|"))

  ## editing extracted info
  result <- list(given, family, credit, other, biography, urls, emails, 
                 country, keywords, ids, other.id.url)
  output <- do.call(cbind, lapply(result, cbind))
  colnames(output) <- c("GivenName", "FamilyName", "CreditName", "OtherNames",
                        "Biography", "URLs", "Email", "Country", "Keywords", 
                        "OtherIDs", "OtherIDsURLs")
  rownames(output) <- NULL
  output[sapply(output, is.null)] <- ""
  output <- as.data.frame(output)
  output$OtherIDs[output$OtherIDs %in% ": "] <- ""

  return(output)
}
