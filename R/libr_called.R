libr_called <- \(script = NULL) {
  if (is.null(script)) script <- rstudioapi::getActiveDocumentContext()$path
  pattern <- "(library|require|p_load)\\(['\"]?(.*?)['\"]?\\)"
  packages <- stringr::str_match(readLines(script), pattern)[,3]
  packages <- packages[!is.na(packages)]

  installed_packages <- installed.packages()[, "Package"]
  packages <- intersect(packages, installed_packages)

  return(packages)
}
