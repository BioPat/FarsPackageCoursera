% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Loads csv file and converts it to dataframe of class tbl_df}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{Path to csv file (character)}
}
\value{
A data frame that is based on the input csv file with class tbl_df
}
\description{
This function loads the user imput csv file \code{filename} and returns a tibble, a data frame
that has the tbl_df class. An incorrect, or non existing path will result in an error.
}
\examples{
\dontrun{accident_2013 <- fars_read("./accident_2013.csv.bz2")}

}
