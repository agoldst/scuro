#' Miscellaneous R Utilities for Beamer Presentations
#'
#' This package includes some utilities for styling and laying out beamer presentations.
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
