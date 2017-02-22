#' Map Traffic Accidents by State and Year
#'
#' A function that reads data from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System and maps the number of fatalities
#' for one state for a particular year
#'
#' @title MapByState
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @param state.num State number as defined in the csv file - should be an integer
#'    between 1 and 50
#' @param year Year to be queried for number of accidents - should be an integer
#'    for the years 2013 through 2015
#' @details Will report an error if there is an invalid state number or if there are
#'    no accidents in that state in the specified year or if multiple states or years
#'    are chosen
#' @return A map of a state, with points notating accidents for that year
#' @examples
#'    fars_map_state(12, 2015)
#' @export
#'

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

#' Summarize traffic fatalities by month and year
#'
#' A function that can summarize multiple years, by month. It takes the user input
#' of a year or multiple years and produces a table of traffic fatalities by month
#'
#' @title SummarizeByYears
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @param years User inputs the year needed - should be an integer
#'    between 2013 and 2015
#' @return Returns a table with number of accidents, by month, in the
#'    selected year
#' @examples
#'    fars_summarize_years(2015)
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Read data
#'
#' Takes the filename string and attempts to open the csv file as a data frame
#' Not user-facing
#'
#' @title ReadData
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @param filename Name of file to be input into the function. This is passed
#'    from the make_filename function (and can have the .bz2 extension)
#'    input should be a text string
#' @details will throw an error if the filename doesn't exist in the path
#' @return This function returns a data frame read from the input file
#' @examples
#'    fars_read("accident_2014.csv.bz2")

fars_read <- function(filename) {
        filename <- system.file("extdata", filename, package = "FarsMap")
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates a filename
#'
#' Take the year input by the user and creates a string to map to
#' the file to be read - for that year.
#' Not user-facing
#'
#' @title make_filename
#' @param year Parameter is passed from the fars_read_years function
#'   input that is passed should be able to be converted to an integer
#' @return Creates a string for the filename
#' @examples
#'    make_filename(2015)

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Gather data
#'
#' Compiles all of the months data for the year being selected by the user
#' Not user-facing
#'
#' @title SelectYearsFromData
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @param years Parameter is passed from the fars_summarize_years function - should be an integer
#' @details Will throw an error if the year being passed in not in the filenames available
#' @return Selects all of the months for the year being requested
#' @examples
#'    fars_read_years(2015)

fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}







