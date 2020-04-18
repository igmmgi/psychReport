#' @title requiredPackages
#'
#' @description Installs (default if required) and loads specified packages.
#'
#' @param packages A list of packages
#' @param installPackages TRUE/FALSE Install package if not installed
#' @param lib character vector giving the library directories where to install the packages. Recycled as needed. If missing, defaults to the first element of .libPaths()
#' @param repos character vector, the base URL(s) of the repositories to use, e.g., the URL of a CRAN mirror such as "https://cloud.r-project.org". For more details on supported URL schemes see url. Can be NULL to install from local files, directories or URLs: this will be inferred by extension from pkgs if of length one.
#'
#' @return NULL
#'
#' @export
requiredPackages <- function(packages,
                             installPackages=FALSE,
                             lib = .libPaths()[1],
                             repos = "http://cran.us.r-project.org"){

  isPackageInstalled <- packages %in% rownames(utils::installed.packages())

  if (any(!isPackageInstalled) & installPackages) {
    isPackageAvailable <- packages %in% rownames(utils::available.packages(repos = repos))
    if (any(!isPackageAvailable)) {
      stop(paste0("Package ", packages[!isPackageAvailable], " not available!"))
    }
    utils::install.packages(packages[!isPackageInstalled],
                            lib = lib,
                            repos = repos,
                            dependencies = TRUE)
    isPackageInstalled <- packages %in% rownames(utils::installed.packages())
  } else if (any(!isPackageInstalled)) {
    stop(paste0("Package ", packages[!isPackageInstalled], " not installed!"))
  }

  lapply(packages[isPackageInstalled], library, character.only = TRUE)

}
