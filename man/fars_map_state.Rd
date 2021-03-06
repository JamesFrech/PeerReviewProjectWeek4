% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Fars Map State}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{The numerical number representing the state you would like to plot.}

\item{year}{The year of which you would like to plot accidents from.}
}
\value{
returns a map of the requested state with dots representing each accident.
}
\description{
Creates a map of fatal car accidents in a given state.
}
\note{
Will return an error if given an invalid state number.

Will return an error if a year with no data is given.

Will return a message saying "no accidents to plot" if there were no
accidents in the state in the year requested.
}
\examples{
\dontrun{fars_map_state(10, 2013)}
\dontrun{fars_map_state(36, 2014)}

}
