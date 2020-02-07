#' Helper function to simplify querying database
#'
#'Use this function to easily query the sqlite database that you created. Simpy provide a time range, and specify the
#'names of the fields that you want to query. This is useful becuase data is spread across multiple tables, and multiple
#'quarters, and within each quarter, spread across multiple tables. This function allows you to ignore that complexity.
#'
#'This is intended only to simplify more basic queries, and is by no means intended as the only way to access data
#'from this database.
#'
#' @param db_path string
#' @param vars character vector of desired data fields
#' @param year_start minimum year of data. 1992 through year corresponding to one quarter ago
#' @param quarter_start  minimum quarter of data. Possible values are 1, 2, 3, or 4
#' @param year_stop final year of data. 1992 through year corresponding to one quarter ago
#' @param quarter_stop final quarter of data. Possible values are 1, 2, 3, or 4
#'
#' @return tibble
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
query_banks_db <- function(db_path, vars = c(), year_start, quarter_start, year_stop, quarter_stop){

  db <- dbConnect(RSQLite::SQLite(), db_path)

  #browser()
  #Correspondence of var names to table
  matching <- dbGetQuery(db,
                         "SELECT * FROM matchingTable")

  matching <- matching %>%
    mutate(varNames = tolower(.data$varNames)) %>%
    #Remove fields that are going to be queried anyway
    filter(!(.data$varNames %in% c('cert','repdte','name','stalp')))


  matching <- matching %>%
    filter(.data$varNames %in% vars) %>%
    mutate(yearQuarter = as.numeric(paste0(.data$year,.data$quarter)))


  quarterConvert <- function(x){

    case_when(x == 1 ~ '0331',
              x == 2 ~ '0630',
              x == 3 ~ '0930',
              x == 4 ~ '1231')
  }

  quarter_start <- quarterConvert(quarter_start)
  quarter_stop <- quarterConvert(quarter_stop)

  dateRange <- as.numeric(paste0(year_start,quarter_start)):as.numeric(paste0(year_stop,quarter_stop))


  matching <- filter(matching, .data$yearQuarter %in% dateRange)

  datalist <- list()
  t <- 1
  for(i in unique(matching$yearQuarter)){

    data <- matching %>%
      dplyr::filter(.data$yearQuarter == i)

    datalist1 <- list()
    v <- 1
    for(j in unique(data$tableName)){

      innerVars <- data %>%
        filter(.data$tableName == j)


      data1 <- dplyr::tbl(db, j) %>%
        select(.data$cert, .data$repdte, .data$name, .data$stalp, innerVars$varNames) %>%
        as.data.frame()

      datalist1[[v]] <- data1
      v <- v + 1
    }

    quarterCrunch <- reduce(datalist1, dplyr::left_join)

    datalist[[t]] <- quarterCrunch
    t <- t+1
  }

  outData <- dplyr::bind_rows(datalist)

  return(outData)
}











