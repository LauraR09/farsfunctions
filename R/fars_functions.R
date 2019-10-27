# --------------------------------------------------------#
# Assignment Week 2: Documenting Code
# Cours: Bulding R packages
# Laura Khil
# --------------------------------------------------------#

#' Read the fars data into R
#' This function reads fars data stored as csv files into the working directory
#' and returns a tibble or dataframe
#' if the data do not exist an error message is given
#' You can customize the data to read with the \code{filename} argument

#' @param filename a character string giving the filename to read
#' @param return returns the data as tibble/dataframe or an error message if the filename does not exist

#' @examples
#' \dontrun{
#' fars_read("accident_2015.csv.bz2")
#' or make the filename with: \code{\link{make_filename}}
#' make_filename(2015) %>% fars_read()
#' }
#' @seealso \code{\link{make_filename}}

#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make the filename of the fars data
#' This function takes the (\code{year} as a string or integer as input and
#' returns the filename of the fars data stored as csv files
#' The return value of this function can be used as input for the \code{fars_read function}
#' @param year integer or string giving the year of the data
#' @return a string giving the filename of the fars data

#' @examples
#' make_filename(2015)
#' make_filename("2015")

#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read year and month of all data
#' This function reads all fars data stored as csv files for the provided input years
#' (taken as list or vector), and selects the month and year of the data
#' it is an ancillary function used by  \code{\link{fars_summarize_years}
#' if the \code{year} is invalid, an error message is given
#' @param years list or vector of years

#' @importFrom magrittr "%>%"

#' @return a list of dataframes containing month and year of the fars data,
#'  or a NULL if  the \code{year} is invalid
#'
#' @seealso \code{\link{fars_read}}
#' @seealso \code{\link{make_filename}}
#' @seealso \code{\link{fars_summarize_years}}

#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014, 2015))
#' }
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

#' Number of entries by month and year
#' This function counts the number of fatal injuries by month and year
#' @param years a list or vector with the years to summarize
#'

#' @importFrom magrittr "%>%"
#' @return a dataframe containing the number of fatal injuries per month and year
#' @seealso \code{\link{fars_read_years}}
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Locate the place of fatal injuries on map
#' This function displays the place of the fata linjuries by state and year on a map
#' an error message is given if the \code{state.num} is invalid

#' @param state.num integer or character variable giving the state number
#' @param year integer or character variable giving the year

#' @return None

#' @seealso \code{\link{fars_read}}
#' @seealso \code{\link{make_filename}}

#' @examples
#' \dontrun{
#' fars_map_state(01, 2015)
#' fars_map_state("01", 2015)
#' }

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

