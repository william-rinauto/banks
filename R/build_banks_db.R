#' Downloads quarterly call report data and saves it to local sqlite database.
#'
#' This function is the first one you should use upon downloading this package. It will
#' download all quarters of data from here, https://www5.fdic.gov/sdi/download_large_list_outside.asp,
#' store them as zip files in current working directory, unzip them, and write them to a sqlite
#' database given by db_path.
#'
#' @param db_path Path to sqlite database
#'
#' @export
#' @importFrom DBI dbConnect dbGetQuery
#' @importFrom dplyr mutate filter case_when select
#' @importFrom purrr reduce
#' @importFrom stringr str_match
#' @importFrom utils unzip write.csv download.file
#' @importFrom stats na.omit
build_banks_db <- function(db_path){
  zips <- 'zip location'
  page <- rvest::html('https://www5.fdic.gov/sdi/download_large_list_outside.asp')
  onClickURL <- 'https://www5.fdic.gov/sdi/'

  downloads <- page %>%
    rvest::html_nodes('a') %>%
    stringr::str_subset('\\.zip')

  downloads1 <- stringr::str_match(downloads,
                                   '<a href=\\\"(.+\\d{8}\\.zip)\\\">(All_Reports_\\d{8})\\.zip<\\/a>')

  #2007 has an extra "quarter" - 2007123 -- seems like error in data input from fdic
  downloads1 <- na.omit(downloads1)

  #downloads1 <- downloads1[1:2,]

  dir.create(zips)

  lapply(1:nrow(downloads1),function(i){
    download.file(paste(onClickURL,downloads1[i,2],
                        sep = ""),
                  file.path(zips,paste0(downloads1[i,3],".zip")))
  })

  write_banks(db_path) #Here files argument is blank becuase I will be using all files in directory

}
