\name{libr_used}
\alias{libr_used}
\title{'Get Packages Used in the Active Script'}
\usage{
libr_used(script = NULL)
}
\description{
The 'libr_used()' retrieves a vector of package names actively utilized within
an 'R' script; packages loaded using 'library()' but not actually used in the
script will not be included.
}
\value{
Returns the vector of all package names used in the active 'R' script, based
on all the functions used in the script with fully loaded namespaces in the
environment.
}
\arguments{
\item{script}{Character vector. 'R' script to be processed. If NULL, an active
'R' script is used.}
}
\examples{
script_content <- 'library(rstudioapi)
ActiveDocument <- getActiveDocumentContext()$path
min_c <- min(c(1,2,3))
'

temp_script_path <- tempfile(fileext = ".R")
writeLines(script_content, con = temp_script_path)

libr_used(script = temp_script_path)

unlink(temp_script_path)
}
