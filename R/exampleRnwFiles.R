#' @title exampleRnwFiles(number)
#'
#' @description Open example *.Rnw files.
#'
#' @param exampleNumber Example file to open
#'
#' @return Creates a copy of an example *.Rnw file and opens it in the current
#' working directory. The report be generated via either:
#' 1) Compile PDF option within RStudio
#' 2) Command line:
#'    Step 1) R CMD Sweave filename.Rnw (creates the *.tex file)
#'    Step 2) pdflatex filename.tex (creates the *.pdf file)
#'
#' @examples
#' library(psychReport)
#' # Example 1:
#' exampleRnwFiles(1)

exampleRnwFiles <- function(exampleNumber = 1) {

  if (!exampleNumber %in% c(1:2)) {
    stop("exampleNumber must be 1 or 2!")
  }

  fileEx = system.file("examples",
                       paste0("exampleReport", exampleNumber, ".Rnw"),
                       package = "psychReport")
  file.copy(fileEx, getwd(), overwrite = TRUE)
  cat(paste0("run: file.edit(\"exampleReport", exampleNumber, ".Rnw\") in the console"))

}
