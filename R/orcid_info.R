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
#' \dontrun{
#'   orcids <- c("0000-0002-5537-5294", NA, "0000-0002-1048-0138", "not found")
#'   orcid_info(orcids)
#' }   
#'  
#' @export orcid_info   
#'
orcid_info <- function(orcid) {
  
  ## check input
  not.orcid <- grepl("\\D", gsub("-|X", "", orcid, perl = TRUE), perl = TRUE) |
                  orcid %in% c("", " ", NA)
  if (any(not.orcid))
    warning ("One or more input objects are not ORCID numbers!")
  
  if (all(not.orcid))
    stop ("Please provide at least one ORCID number!")

  ## getting personal info from ORCID
  info <- rorcid::orcid_person(orcid[!not.orcid], details = FALSE)
  
  #extracting relevant info
  given <- lapply(info, function(x) x[['name']][['given-names']][['value']])
  family <- lapply(info, function(x) x[['name']][['family-name']][['value']])
  credit <- lapply(info, function(x) x[['name']][['credit-name']][['value']])
  other <- 
    lapply(info, function(x) x[['other-names']][['other-name']][['content']])
  other <- lapply(other, function(x) paste0(x, collapse = "|"))
  
  biography <- lapply(info, function(x) x[['biography']][['content']])
  
  urls.name <- 
    lapply(info, function(x) x[['researcher-urls']][['researcher-url']][['url-name']])
  urls.path <- 
    lapply(info, function(x) x[['researcher-urls']][['researcher-url']][['url.value']])
  # urls <- mapply(FUN = paste0, urls.name, " [[", urls.path,"]]")
  urls <- lapply(urls.path, function(x) paste0(x, collapse = "|"))

  emails <- lapply(info, function(x) x[['emails']][['email']][['email']])
  
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

  ## getting Employment info from ORCID
  employ <- rorcid::orcid_employments(orcid[!not.orcid], details = FALSE)
  
  #editing employment info
  summs <- lapply(lapply(employ, 
                         function(x) x[['affiliation-group']][['summaries']]), dplyr::bind_rows)
  columns <- paste0("employment-summary.", 
                    c("put-code", "department-name", "role-title", 
                      "start-date.year.value", "end-date.year.value", "organization.name", 
                      "organization.address.city", "organization.address.region",
                      "organization.address.country"))
  summs.clean <- lapply(summs, function (x) x[, names(x) %in% columns])
  for (i in seq_len(length(summs.clean))) {
    dados <- summs.clean[[i]]
    if (dim(dados)[1] > 1) {
      start.date <- "employment-summary.start-date.year.value"
      end.data <- "employment-summary.end-date.year.value"
      if (start.date %in% names(dados) & end.data %in% names(dados)) {
        keep_these <- !(!is.na(dados[start.date]) & !is.na(dados[end.data]))
        dados <- dados[keep_these, ]
      }
    } else {
      dados <- as.data.frame(matrix(NA, ncol = length(columns),
                             dimnames = list(NULL, columns)))
    }
    
    if (dim(dados)[1] > 1)
      dados <- apply(dados, 2, paste0, collapse = "|")
    
    names(dados) <- gsub("employment-summary.", "", names(dados), fixed = TRUE)
    dados[] <- lapply(dados, as.character)
    summs.clean[[i]] <- dados
  }
  
  output.adress <- dplyr::bind_rows(summs.clean)
  
  ## editing extracted info
  output.info <- data.frame(ID = orcid[!not.orcid],
                            GivenName = as.character(given),
                            FamilyName = as.character(family),
                            CreditName = as.character(credit), 
                            OtherNames = as.character(other),
                            Biography = as.character(biography), 
                            URLs = as.character(urls), 
                            Email = as.character(emails), 
                            Country = as.character(country), 
                            Keywords = as.character(keywords), 
                            OtherIDs = as.character(ids), 
                            OtherIDsURLs = as.character(other.id.url))
  
  output <- cbind.data.frame(output.info, output.adress,
                             stringsAsFactors = FALSE) 
  output[sapply(output, is.null)] <- NA
  output[sapply(output, function(x) x %in% "NULL")] <- NA
  output[sapply(output, function(x) x %in% "")] <- NA
  output$OtherIDs[output$OtherIDs %in% ": "] <- NA
  
  ## Saving only the results for valid ORCID numbers
  if (any(not.orcid)) {
    res.na <- matrix(NA, ncol = dim(output)[2], nrow = sum(not.orcid),
                     dimnames = list(orcid[not.orcid], colnames(output)))
    res.na[,'ID'] <- orcid[not.orcid]
    output.final <- rbind.data.frame(output, res.na,
                                     stringsAsFactors = FALSE)
    output.final <- output.final[match(orcid, output.final$ID),]
  } else {
    output.final <- output
  }

  output.final[] <- lapply(output.final, as.character)

  return(output.final)
}
