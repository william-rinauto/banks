#' Unzips files, reads in csv contents, writes to sqlite database, and then deletes csv files.
#'
#' @param db_path Path to sqlite database
#' @param files Specific files that will be written to sqlite database. Used when updating for new data.
#'
#'
#' @examples
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_extract
write_banks <- function(db_path, files){
  zips <- 'zip location'
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  if(!missing(files)){
    zipFiles <- str_match(files, ".+\\/.+\\/(.+\\d{8}\\.zip)")[,2]
  }else{
    zipFiles <- list.files(zips)
  }


  lapply(zipFiles, function(i){
    z <- unzip(file.path(zips,i), exdir = zips)
    #z <- str_remove(z, '^\\.\\/')
    z <- stringr::str_match(z, '\\/(.+)')[,2]

    #Don't include readme file
    z <- z[!stringr::str_detect(z, 'readme')]

    lapply(z, function(j){

      data <- read.csv(file.path(zips,j))
      file.remove(file.path(zips,j))
      name <- stringr::str_remove(j, '\\.csv$')

      DBI::dbWriteTable(db, name, data)

    })
  })


  #Create 'matching' table that will be used in easy query function

  #if(!missing(files)) browser()

  tables <- DBI::dbListTables(db)

  #If updating, only run query on new tables

  if(!missing(files)){

    matchingTable0 <- DBI::dbGetQuery(db,
                                      "SELECT * FROM matchingTable")

    #When I wrote matchingTables to db originally, I performed a distinct() on everything except tableName,
    # so with this method there will be some redundancy. However, it will be taken care of when
    # I run distinct again. It shouldn't add much time to the process.
    tables <- tables[!(tables %in% matchingTable0$tableName)]
  }

  matchingTable <- bind_rows(lapply(tables, function(i){

    data  <- DBI::dbGetQuery(db,
                             paste("SELECT * FROM [",i,"] LIMIT 1",sep = ""))
    dataNames <- names(data)

    date <- str_extract(i, "\\d{8}")
    year <- substr(date,1,4)
    quarter <- substr(date,5,8)


    tableOut <- data.frame(varNames = dataNames,
                           tableName = i,
                           year = year,
                           quarter = quarter)
  }))

  matchingTable <- dplyr::distinct(matchingTable, .data$varNames, .data$year, .data$quarter, .keep_all = T)


  DBI::dbWriteTable(db, "matchingTable", matchingTable, append = T)
}


