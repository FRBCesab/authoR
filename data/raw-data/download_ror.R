#### DOWNLOAD AND PARSE LATEST ROR DATA ####

## Downloading latest data
url <- "https://github.com/ror-community/ror-api/tree/master/rorapi/data"
data.source <- readLines(url)

latest.source <- tail(data.source[grepl("data\\/ror-", data.source, perl = TRUE)], 1)
latest.source <- as.character(strsplit(latest.source, '\\\"')[[1]])
latest.source <- head(latest.source[grepl("ror", latest.source, perl = TRUE)], 1)

data.url <- "https://github.com/ror-community/ror-api/raw/master/rorapi/data/"
data.url <- paste0(paste0(file.path(url, latest.source), "/"), "ror.zip")
data.dest <- here::here("data-raw", "ror.zip")

# Step not working, downloaded manually from GitHub
# utils::download.file(data.url, method = "auto",
#                      destfile = data.dest, cacheOK = TRUE)
utils::unzip(zipfile = data.dest, exdir =  here::here("data-raw"))

ror <- jsonlite::fromJSON(here::here("data-raw", "ror.json"))
