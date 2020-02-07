#' Updates bank sqlite database
#'
#'Use this function to update your database of call report data. You should call this approximately quarterly, when
#'the FDIC provides new data for the previous quarter.
#'
#' @param db_path Full path to sqlite database, previously created with build_banks_db
#'
#'
#' @export
#'
#' @importFrom utils read.csv
update_banks_db <- function(db_path){
  zips <- 'zip location'
  #What currently exists in db
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  tables <- DBI::dbListTables(db)

  #What is available on website
  page <- rvest::html('https://www5.fdic.gov/sdi/download_large_list_outside.asp')
  onClickURL <- 'https://www5.fdic.gov/sdi/'

  downloads <- page %>%
    rvest::html_nodes('a') %>%
    stringr::str_subset('\\.zip')

  downloads1 <- stringr::str_match(downloads,
                                   '<a href=\\\"(.+\\d{8}\\.zip)\\\">(All_Reports_\\d{8})\\.zip<\\/a>')

  downloads1 <- na.omit(downloads1)

  alreadyWritten <- unique(stringr::str_match(tables, "(.+\\d{8}).+")[,2])

  #Only show quarters that haven't been written to DB yet.
  downloads1 <- subset(downloads1,  !(downloads1[,3] %in% alreadyWritten))


  if(!dir.exists(zips)) dir.create(zips)

  lapply(1:nrow(downloads1),function(i){
    download.file(paste(onClickURL,downloads1[i,2],
                        sep = ""),
                  file.path(zips,paste0(downloads1[i,3],".zip")))
  })

  write_banks(db_path, downloads1) #Here, files to be written only include new downlaods as of this run
}
