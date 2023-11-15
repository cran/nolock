\name{libr_called}
\alias{libr_called}
\title{'Get Packages Called in the Active 'R' Script'}
\usage{
  libr_called(script = NULL)
}
\description{
  Retrieves a vector of all package names which are called within an 'R' script.
}
\value{
  Retrieves a vector of package names.
}
\arguments{
  \item{script}{Character vector. 'R' script to be processed. If NULL, an active
    'R' script is used.}
}
\examples{
  script_content <- 'library(rstudioapi)
ActiveDocument <- getActiveDocumentContext()$path
min_c <- min(c(1,2,3))
require(dplyr)
pacman::p_load(tidymodels)'

  temp_script_path <- tempfile(fileext = ".R")
  writeLines(script_content, con = temp_script_path)

  libr_called(script = temp_script_path)

  unlink(temp_script_path)
}
