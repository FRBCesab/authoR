#' @title Get Author Info
#'
#' @description Get and organize author's personal and employement information
#'   obtained from ORCID.
#'
#' @param orcid character. A vector of ORCID numbers. 
#'    
#' @return a data frame containing in the lines the information for each of the
#'   ORCID numbers provided.
#'
#' @details The functions obtains and organizes personal and employment
#'   information using the __rorcid__ functions 'orcid_person' and
#'   'orcid_employments'. Please check the help of these functions for more
#'   detail.
#' 
#' @author Renato A. F. de Lima
#' 
#' @importFrom rorcid orcid_person orcid_employments
#' @importFrom dplyr bind_rows
#'
#' @examples
#'
#'   orcids <- c("0000-0002-5537-5294", NA, "0000-0002-1048-0138", "not found")
#'   orcid_info(orcids)
#'   
#' @export orcid_info   
#'
orcid_info <- function(orcid) {
  
  ## check input
  not.orcid <- grepl("\\D", gsub("-", "", orcid, fixed = TRUE), perl = TRUE) |
                  orcid %in% c("", " ", NA)
  if (any(not.orcid))
    warning ("One or more input objects are not ORCID numbers!")
  
  if (all(not.orcid))
    stop ("Please provide at least one ORCID number!")

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

  employ <- rorcid::orcid_employments(orcid[!not.orcid], details = FALSE)
  summs <- lapply(
    lapply(employ, function(x) x[['affiliation-group']][['summaries']]), dplyr::bind_rows)
  columns <- paste0("employment-summary.", 
                    c("put-code", "department-name", "role-title", 
                      "start-date.year.value", "end-date.year.value", "organization.name", 
                      "organization.address.city", "organization.address.region",
                      "organization.address.country"))
  summs.clean <- lapply(summs, function (x) x[, names(x) %in% columns])
  for (i in 1:length(summs.clean)) {
    dados <- summs.clean[[i]]
    if (dim(dados)[1] > 1) {
      keep_these <- !(!is.na(dados["employment-summary.start-date.year.value"]) &
                        !is.na(dados["employment-summary.end-date.year.value"]))
      dados <- dados[keep_these, ]
    }
    
    if (dim(dados)[1] > 1)
      dados <- apply(dados, 2, paste0, collapse = "|")
    
    names(dados) <- gsub("employment-summary.", "", names(dados), fixed = TRUE)
    summs.clean[[i]] <- dados
  }
  output.adress <- dplyr::bind_rows(summs.clean)
  
  ## editing extracted info
  result <- list(given, family, credit, other, biography, urls, emails, 
                 country, keywords, ids, other.id.url)
  output.info <- do.call(cbind, lapply(result, cbind))
  colnames(output.info) <- c("GivenName", "FamilyName", "CreditName", "OtherNames",
                        "Biography", "URLs", "Email", "Country", "Keywords", 
                        "OtherIDs", "OtherIDsURLs")
  output <- cbind(output.info, output.adress) 
  output[sapply(output, is.null)] <- ""
  output <- as.data.frame(output)
  output$OtherIDs[output$OtherIDs %in% ": "] <- ""
  
  ## Saving only the results for valid ORCID numbers
  if (any(not.orcid)) {
    res.na <- matrix(NA, ncol = dim(output)[2], nrow = sum(not.orcid),
                     dimnames = list(orcid[not.orcid], colnames(output)))
    output.final <- rbind.data.frame(output, res.na)
    output.final <- output.final[match(orcid, row.names(output.final)),]
    row.names(output.final)[not.orcid] <- paste0("not.orcid", seq_len(sum(not.orcid))) 
  } else {
    output.final <- output
  }

  return(output.final)
}
