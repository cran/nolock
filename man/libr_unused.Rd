\name{libr_unused}
\alias{libr_unused}
\title{'Get Unused Packages in the Active 'R' Script'}
\usage{
  libr_unused(script = NULL)
}
\description{
  Retrieves a vector of package names that are called within an 'R' script but
  are never actually used in the script.
}
\value{
  Retrieves a vector of package names which are never really used in the code.
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

  libr_unused(script = temp_script_path)

  unlink(temp_script_path)
}
