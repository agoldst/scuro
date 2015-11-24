

#' @export
#' @rdname ragoldstBeamer_format
slides <- function (...) {
    ragoldstBeamer_format(output="slides", ...)
}

#' @export
#' @rdname ragoldstBeamer_format
notes <- function (...) {
    dots <- list(...)
    dots$pandoc_args <- c(rmarkdown::pandoc_variable_arg("fontsize", "8pt"),
                          dots$pandoc_args)
    dots$output="notes"
    do.call(ragoldstBeamer_format, dots)
}

#' @export
#' @rdname ragoldstBeamer_format
handout <- function (...) {
    dots <- list(...)
    dots$pandoc_args <- c("-V", "handout", dots$pandoc_args)
    dots$output="handout"
    do.call(ragoldstBeamer_format, dots)
}

#' Presentation output formats
#'
#' This package supplies three \pkg{rmarkdown} output formats,
#' \code{ragoldstBeamer_format::slides}, \code{ragoldstBeamer_format::notes},
#' and \code{ragoldstBeamer_format::handout}, which can be used in place of
#' \code{output: beamer_presentation} in the YAML block of an Rmd file or as the
#' \code{output_format} argument to \pkg{rmarkdown}'s
#' \code{\link[rmarkdown]{render}} function. The options these formats accept
#' are given by the parameters to these functions. \code{slides}, \code{notes},
#' and \code{handout} pass their arguments on to \code{ragoldstBeamer_format}.
#'
#' @param output one of \code{"slides"}, \code{"notes"}, or \code{"handout"}
#'
#' @param fig_width,fig_height default figure dimensions (inches). Note that
#'   beamer slides have small "physical" dimensions: 128 mm x 96 mm or about 5
#'   in x 3.75 in. The default values here are meant for slides with just one
#'   figure on them (and possible a title and a caption).
#'
#' @param fig_crop,fig_caption same as for the built-in
#'   \code{\link[rmarkdown]{beamer_presentation}}
#'
#' @param dev graphics device. \code{"tikz"} for TikZ graphics is the default,
#'   since one of the main points of this package is to transcend bad beamer
#'   fonts. This requires installing the \pkg{tikzDevice} package.
#'
#' @param highlight code syntax highlighting. I recommend "zenburn" (see
#'   \code{\link[rmarkdown]{beamer_presentation}} for other choices).
#'
#' @param keep_tex whether to keep the intermediate LaTeX file. Often needed for
#'   debugging errors.
#'
#' @param latex_engine I recommend xelatex (cf. above remarks about fonts).
#'
#' @param pandoc_args character vector of extra arguments to pandoc.
#'
#' @export
ragoldstBeamer_format <- function (
        output=c("slides", "notes", "handout"),
        fig_width=4.5,
        fig_height=2.75,
        fig_crop=TRUE,
        fig_caption=TRUE,
        dev="tikz",
        highlight="zenburn",
        keep_tex=FALSE,
        latex_engine="xelatex",
        pandoc_args=NULL) {

    output <- match.arg(output)

    hdr <- system.file(paste0("tex/preamble-", output, ".tex"),
                       package="ragoldstBeamer")
    tmpl <- system.file("pandoc/beamer.template", package="ragoldstBeamer")

    overlay_filter <- system.file("python/overlay_filter",
                                  package="ragoldstBeamer")

    result <- rmarkdown::beamer_presentation(
        toc=FALSE,
        slide_level=1,
        fig_width=fig_width,
        fig_height=fig_height,
        fig_crop=fig_crop,
        fig_caption=fig_caption,
        dev=dev,
        highlight=highlight,
        template=tmpl,
        keep_tex=keep_tex,
        latex_engine=latex_engine,
        includes=rmarkdown::includes(in_header=hdr),
        pandoc_args=c("--filter", overlay_filter, pandoc_args)
    )

    # this knit_hooks fiddling code is closely modeled on
    # https://github.com/rstudio/rmarkdown/blob/master/R/tufte_handout.R

    knitr_options <- rmarkdown::knitr_options_pdf(
        fig_width, fig_height, fig_crop, dev
    )
    if (is.null(knitr_options$opts_knit)) {
        knitr_options$opts_knit <- list()
    }
    if (is.null(knitr_options$knit_hooks)) {
        knitr_options$knit_hooks <- list()
    }

    # set some default options
    knitr_options$opts_knit$width <- 44

    knitr_options$opts_chunk$tidy <- FALSE
    knitr_options$opts_chunk$size <- "footnotesize"
    knitr_options$opts_chunk$dev.args <- list(pointsize=9)

    # custom package hook option: use dark_plot_theme everywhere
    knitr_options$opts_chunk$dark_theme <- TRUE

    # set hooks
    knitr_options$knit_hooks$plot <- plot_hook_textpos
    knitr_options$knit_hooks$output <- output_hook_routput
    knitr_options$knit_hooks$dark_theme <- set_dark_theme

    result$knitr <- knitr_options
    result
}

