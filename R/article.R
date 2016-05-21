
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
#' @param ... remaining arguments (e.g. \code{include}) are passed on to
#' \code{\link[rmarkdown]{pdf_document}}, which is the base of this format.
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
        highlight="default",
        keep_tex=TRUE,
        citation_package=c("none", "biblatex"),
        latex_engine="xelatex",
        plot_font="mainfont",
        plot_font_options=NULL,
        pandoc_args=NULL,
        ...) {

    tmpl <- system.file(file.path("memarticle", "memoir-article.latex"),
        package="scuro")

    citation_package <- match.arg(citation_package)

    # default yaml (sets class option oneside and routput true)
    yaml <- system.file(file.path("pandoc", "chiaro.yaml"),
        package="scuro")

    result <- rmarkdown::pdf_document(
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
        citation_package=citation_package,
        pandoc_args=c(yaml, pandoc_args),
        ...
    )

    result$knitr <- scuro_knitr(result$knitr,
        dev, latex_engine, plot_font, plot_font_options)

    result$knitr$opts_chunk$fig.path <- "figure/"
    result$knitr$opts_chunk$fig.align <- "center"
    # maxwidth macro defined in memoir-article.latex template
    result$knitr$opts_chunk$out.width <- "\\maxwidth"

    result$knitr$opts_chunk$theme_bw <- TRUE
    result$knitr$knit_hooks$theme_bw <- set_theme_bw


    result
}

