#' Loads csv file and converts it to dataframe of class tbl_df
#'
#' @description
#' This function loads the user imput csv file \code{filename} and returns a tibble, a data frame
#' that has the tbl_df class. An incorrect, or non existing path will result in an error.
#'
#' @param filename Path to csv file (character)
#'
#' @return A data frame that is based on the input csv file with class tbl_df
#'
#' @examples
#' \dontrun{accident_2013 <- fars_read("./accident_2013.csv.bz2")}
#'
#' @importFrom readr read_csv
#'
#' @importFrom dplyr tbl_df
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

#' Creates a filename according to the input year
#'
#' @description
#' This function creates a filename with the pattern "accident_%d.csv.bz2", where the "%d" is
#' replaced by the numerical or intiger function input \code{year}. Input that is not numerical
#' or an integer will lead to an error.
#'
#' @param year Numerical or integer value that corresponds to a year in the data set
#'
#' @return A character string with the pattern "accident_%d.csv.bz2", where "%d" is replaced with
#' a number/integer that corresponds to a year in the data set (i.e "2013", "2014", or "2015").
#' This output character string can be used as \code{filename} input for the fars_read function.
#'
#' @examples
#' \dontrun{make_filename(2013)}
#' \dontrun{fars_read(make_filename(2013))}
#'
#'@export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads accident file and returns tibble with month and year
#'
#' @description
#' This function takes one year, a vector of \code{years}, or a list of \code{years} as input for
#' which accident files are available. When accident files are not available the function will result
#' in a warning message indicating that the year is not valid. For valid input the function will
#' return a tibble that displays "Month" and "year" of the accident file.
#'
#' @param years A single year, or a vector/list of multiple \code{years} in numerical/integer class
#'
#' @return A tibble of each valid year that was chosen as input, diplaying "Month" and "year" of the
#'         accident file.
#'
#' @details This function implements the \code{make_filename()} and \code{fars_read()} function of this
#'          package
#'
#' @examples
#' \dontrun{fars_read_years(2013)}
#' \dontrun{fars_read_years(c(2013, 2014, 2015))}
#'
#' @importFrom dplyr mutate %>% select
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

#' Returns the number of accidents per month of select years
#'
#' @description
#' This function takes one year, a vector of \code{years}, or a list of \code{years} as input for
#' which accident files are available. Years for which no accident file is available will result in
#' an error and a worning message. For years that accident files are available the function summarizes
#' the accident data for each year based on "Month" and the corresponding total number of accidents for
#' that month. This function utilizes dplyr and tidyr, and returns the results as tibble.
#'
#' @param years A single year, or a vector/list of multiple \code{years} in numerical/integer class
#'
#' @return A summary in tibble format for each valid year, indicating the number of total accidents
#'         in a month for each month for the whole year.
#'
#' @details This function implements the \code{fars_read_years()}, \code{make_filename()} and
#'          \code{fars_read()} function of this package.
#'
#' @examples
#' \dontrun{fars_summarize_years(2013)}
#' \dontrun{fars_summarize_years(c(2013, 2014, 2015))}
#'
#' @importFrom dplyr bind_rows %>% group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Graphs accident locations on US state maps
#'
#' @description
#' This function plots locations of accidents based on longitude and latitude on select
#' US state maps for a particular year. The input for this function is a valid state number
#' \code{state.num} based on the available FARS data, and the \code{year} of interest. Both
#' values have to be valid numbers/integers for which FARS data is available, otherwise the
#' function results in an error. This function implements components of the maps package.
#'
#' @param state.num A valid state number between 1 and 51 as number or integer
#' @param year Numerical or integer value that corresponds to a valid year in the data set
#'
#' @return A graphical representation of a select US state with accident locations represented
#'         as dots on the map
#'
#' @details This function implements the \code{make_filename()} and \code{fars_read()} function
#'          of this package.
#'
#' @examples
#' \dontrun{fars_map_state(23, 2013)}
#' \dontrun{fars_map_state(1, 2014)}
#' \dontrun{fars_map_state(51, 2015)}
#'
#' #Errors will result in following cases:
#' \dontrun{fars_map_state(23, 2018)}
#' \dontrun{fars_map_state(52, 2013)}
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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


if(getRversion() >= "2.15.1")  utils::globalVariables(c("MONTH", "STATE", "n", "year"))
