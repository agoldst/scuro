
#' A format for article classes
#'
#' This format is meant to supply only the TikZ-wrangling features of the
#' package and to set up reasonable further defaults for an article rather than
#' a presentation. For slides that use scuro's font and figure-drawing setup
#' but not its color scheme, use \code{\link{scuro_md}} but with format option
#' \code{scuro=FALSE}. The built-in \code{\link[rmarkdown]{render}} is flexible
#' enough to handle all the processing from R markdown to PDF,
#' including citation processing where needed.
#'
#' Among defaults selected here, of particular note are centering alignment for
#' figures, a black-and-white ggplot theme, and stop-on-error. Errors,
#' warnings, and messages will all go to the console, not into the final
#' document. The intermediate LaTeX file will also be kept as this is often
#' necessary to diagnose typesetting errors.
#'
#' @param fig_width Natural figure width in inches. This is rescaled according
#' to the value of chunk option \code{out.width} (by default, configured to use
#' the natural width or the textwidth, whichever is smaller).
#'
#' @param fig_height Natural figure height in inches.
#' \code{fig_width/fig_height} is the aspect ratio of the graphic.
#'
#' @param fig_crop Apply \code{pdfcrop}? Set by default.
#'
#' @param latex_engine \code{xelatex} by default
#'
#' @param dev graphics device (\code{tikz}) by default.
#'
#' @param plot_font If both XeLaTeX and TikZ are used, the package will set the
#' font in graphics. Any system font name can be given. The default value
#' \code{mainfont} selects the document main font.
#'
#' @param plot_font_options \code{fontspec} options for the \code{plot_font}.
#'
#' @return An R Markdown format suitable for rendering.
#'
#' @export
chiaro_pdf <- function (
        fig_width=4,
        fig_height=2,
        fig_crop=TRUE,
        fig_caption=TRUE,
        dev="tikz",
        highlight="zenburn",
        keep_tex=TRUE,
        latex_engine="xelatex",
        plot_font="mainfont",
        plot_font_options=NULL) {

    tmpl <- system.file(file.path("memarticle", "memoir-article.latex"),
        package="scuro")

    meta <- extract_metadata(input)$metadata
    if (!is.null(meta) && !is.null(meta$biblatex) && meta$biblatex)
        cit_pkg <- "biblatex"
    else
        cit_pkg <- "none"


    result <- pdf_document(
        toc=FALSE, toc_depth=2, number_sections=FALSE,
        fig_width=fig_width,
        fig_height=fig_height,
        fig_crop=fig_crop,
        fig_caption=fig_caption,
        dev=dev,
        highlight=highlight,
        template=tmpl,
        keep_tex=keep_tex,
        latex_engine=latex_engine,
        citation_package=cit_pkg
    )
    result$knitr <- scuro_knitr(result$knitr,
        dev, latex_engine, plot_font, plot_font_options)

    result$knitr$opts_chunk$fig.path <- "figure/"
    result$knitr$opts_chunk$fig.align <- "center"
    # maxwidth macro defined in memoir-article.latex template
    result$knitr$opts_chunk$out.width <- "\\maxwidth"

    result$knitr$opts_chunk$theme_bw <- TRUE
    result$knitr$knit_hooks$theme_bw <- set_theme_bw

    result$knitr$opts_chunk$echo <- FALSE
    result$knitr$opts_chunk$error <- FALSE
    result$knitr$opts_chunk$warning <- FALSE
    result$knitr$opts_chunk$message <- FALSE
    result$knitr$opts_chunk$prompt <- FALSE
    result$knitr$opts_chunk$comment <- NA
    result$knitr$opts_chunk$autodep <- TRUE
    result$knitr$opts_chunk$cache <- TRUE

    result
}

