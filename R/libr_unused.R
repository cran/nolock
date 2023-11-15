libr_unused <- \(script = NULL) {
  if (is.null(script)) script <- rstudioapi::getActiveDocumentContext()$path

  called <- c(libr_called(script), "base")
  utilized <- libr_used(script)
  libr_unused <- setdiff(called, utilized)

  installed_packages <- installed.packages()[, "Package"]
  libr_unused <- intersect(libr_unused, installed_packages)

  return(libr_unused)
}
