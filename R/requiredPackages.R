#' @title requiredPackages
#'
#' @description Installs (if required) and loads specified packages.
#'
#' @param packages A list of packages
#' @param lib character vector giving the library directories where to install the packages. Recycled as needed. If missing, defaults to the first element of .libPaths()
#' @param repos character vector, the base URL(s) of the repositories to use, e.g., the URL of a CRAN mirror such as "https://cloud.r-project.org". For more details on supported URL schemes see url. Can be NULL to install from local files, directories or URLs: this will be inferred by extension from pkgs if of length one.
#'
#' @return NULL
#'
#' @examples
#' # Example 1:
#' library(psychReport)
#'\dontrun{
#' requiredPackages(c("tidyverse", "ez"))
#'}
#'
#' @export
requiredPackages <- function(packages, lib = .libPaths()[1], repos = "http://cran.us.r-project.org"){
  isPackageInstalled <- packages %in% rownames(utils::installed.packages())
  isPackageAvailable <- packages %in% rownames(utils::available.packages(repos = repos))
  if (any(!isPackageAvailable)) {
    stop(paste0("Package ", packages[!isPackageAvailable], " not available!"))
  }
  if (any(!isPackageInstalled)) {
    utils::install.packages(packages[!isPackageInstalled],
                            lib = lib,
                            repos = repos,
                            dependencies = TRUE)
  }
  lapply(packages[isPackageAvailable], library, character.only = TRUE)
}
