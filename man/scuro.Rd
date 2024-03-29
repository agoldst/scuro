% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scuro-package.R
\docType{package}
\name{scuro}
\alias{scuro}
\title{An R Markdown Beamer-Presentation Setup}
\description{
This package helps to produce beamer presentations with a dark-on-light
theme, as well as to generate speaker notes or scripts and audience slide
handouts from a single R markdown source. The included \code{lectures} R
markdown template has example files demonstrating how to use what's here.
}
\examples{
\dontrun{
library(rmarkdown)
draft("my-talks", "lectures", "scuro", edit=F) # or RStudio "New R Markdown"
system("make -C my-talks") # a Makefile is supplied
}

}
