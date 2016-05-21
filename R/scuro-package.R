#' An R Markdown Beamer-Presentation Setup
#'
#' This package helps to produce beamer presentations with a dark-on-light
#' theme, as well as to generate speaker notes or scripts and audience slide
#' handouts from a single R markdown source. The included \code{lectures} R
#' markdown template has example files demonstrating how to use what's here.
#'
#' @examples \dontrun{
#' library(rmarkdown)
#' draft("my-talks", "lectures", "scuro", edit=F) # or RStudio "New R Markdown"
#' system("make -C my-talks") # a Makefile is supplied
#' }
#'
#' @name scuro
#' @docType package
NULL

# Initialize local storage
scuro_local <- new.env(parent=emptyenv())

.onLoad <- function (libname, pkgname) {
    op <- options()
    op_scuro <- list(
        scuro.invert_density=150
    )

    to_set <- !(names(op_scuro) %in% names(op))
    if (any(to_set)) options(op_scuro[to_set])
    invisible()
}
