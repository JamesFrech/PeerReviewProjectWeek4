#' File Read
#'
#' If it exists, reads in a file to be used for the other functions. The data
#' in this file will be presented as a data frame.
#'
#' @param filename the name of the file you would like R to read
#' @return this function returns a data frame of the data present in the file
#' that was read in.
#' @note Will return an error if the file name does not exist in current working
#' directory.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")}
#' \dontrun{fars_read("accident_2014.csv.bz2")}
#' \dontrun{fars_read("accident_2015.csv.bz2")}
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create Filename
#'
#' Creates a name for a file dependent on the year of the data.
#'
#' @param year the year that the data in the file is collected to be used as
#' part of the created filename.
#' @return This function returns the filename that was generated from the function.
#' @note all inputs into the parameter year will become integers, so make sure to
#' use the numerical year.
#'
#' @examples
#' \dontrun{make_filename(2013)}
#' \dontrun{make_filename(2014)}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Fars Read Years
#'
#' reads in years from a data frame.
#'
#' @param years An integer vector of years you would like to take data from.
#' @return Returns a data frame with two columns "MONTH" and "Year". Returns
#' the month and year for each row in the inputted data frame. If multiple years
#' are selected, you will be returned a data frame for each year individually.
#' @note Will return an error if any of the given years do not contain any data.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{fars_read_years(2015)}
#' \dontrun{fars_read_years(c(2014,2013,2015))}
#'
#' @export
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

#' Fars Summary By Year
#'
#' Summarizes the number of fatalities from car crashes each year by month.
#'
#' @param years An integer vector of years you want to summarize data from.
#' @return This function returns a data frame with each column as a given year
#' in order of years given in the "years" parameter. Each row is representative
#' of a month in order from January to December.
#' @note You will get an error if you input any years not contained in a data set.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{fars_summarize_years(2013)}
#' \dontrun{fars_summarize_years(c(2013,2014,2015))}
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Fars Map State
#'
#' Creates a map of fatal car accidents in a given state.
#'
#' @param state.num The numerical number representing the state you would like to plot.
#' @param year The year of which you would like to plot accidents from.
#' @return returns a map of the requested state with dots representing each accident.
#' @note Will return an error if given an invalid state number.
#' @note Will return an error if a year with no data is given.
#' @note Will return a message saying "no accidents to plot" if there were no
#' accidents in the state in the year requested.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(10, 2013)}
#' \dontrun{fars_map_state(36, 2014)}
#'
#' @export
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
