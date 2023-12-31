% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flagRecorder.R
\name{flagRecorder}
\alias{flagRecorder}
\title{Loads, appends, and saves occurrence flag data}
\usage{
flagRecorder(
  data = NULL,
  outPath = NULL,
  fileName = NULL,
  idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
  append = NULL,
  printSummary = FALSE
)
}
\arguments{
\item{data}{A data frame or tibble. Occurrence records as input.}

\item{outPath}{A character path. Where the file should be saved.}

\item{fileName}{Character. The name of the file to be saved}

\item{idColumns}{A character vector. The names of the columns that are to be kept along with the
flag columns. These columns should be useful for identifying unique records with flags.
Default = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource").}

\item{append}{Logical. If TRUE, this will find and append an existing file generated by this function.}

\item{printSummary}{Logical. If TRUE, print a \code{\link[=summary]{summary()}} of all filter columns - i.e. those which
tidyselect::starts_with(".")}
}
\value{
Saves a file with id and flag columns and returns this as an object.
}
\description{
This function is used to save the flag data for your occurrence data as you run the BeeBDC script.
It will read and append existing files, if asked to. Your flags should also be saved in the occurrence
file itself automatically.
}
\examples{
# Load the example data
data("beesFlagged")

  # Run the function
  OutPath_Report <- tempdir()
flagFile <- flagRecorder(
  data = beesFlagged,
  outPath = paste(OutPath_Report, sep =""),
  fileName = paste0("flagsRecorded_", Sys.Date(), ".csv"),
  # These are the columns that will be kept along with the flags
  idColumns = c("database_id", "id", "catalogNumber", "occurrenceID", "dataSource"),
  # TRUE if you want to find a file from a previous part of the script to append to
  append = FALSE)
}
