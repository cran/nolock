libr_used <- \(script = NULL) {
  if (is.null(script)) script <- rstudioapi::getActiveDocumentContext()$path
  packages <-
      sub("package:", "",
          grep("package",
               summary(
                 script |>
                   NCmisc::list.functions.in.file()
               )[, 1] |>
                 names()
               , value = T)
      )

    installed_packages <- installed.packages()[, "Package"]
    packages <- intersect(packages, installed_packages)

  return(packages)
}

