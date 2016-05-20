
#' A format for article classes
#'
#' This format is meant to supply only the TikZ-wrangling features of the
#' package and to set up reasonable further defaults for an article rather than
#' a presentation. For slides that use scuro's font and figure-drawing setup
#' but not its color scheme, use \code{\link{scuro_md}} but with format option
#' \code{scuro=FALSE}.
#'
#' Among defaults selected here, of particular note are centering alignment for
#' figures, a black-and-white ggplot theme, and stop-on-error. Errors,
#' warnings, and messages will all go to the console, not into the final
#' document.
#'
#' @param fig_width Natural figure width in inches. This is rescaled according
#' to the value of chunk option `out.width` (configured to use the natural
#' width or the textwidth, whichever is smaller).
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
#' @return An R Markdown format suitable for rendering.
#'
#' @export
chiaro_md <- function (
        fig_width=4,
        fig_height=2,
        fig_crop=TRUE,
        latex_engine="xelatex",
        dev="tikz",
        plot_font="mainfont") {

    result <- scuro_md(fig_width, fig_height, fig_crop, latex_engine, dev,
        plot_font, scuro=FALSE)

    result$knitr$opts_chunk$fig.path <- "figure/"
    result$knitr$opts_chunk$fig.align <- "center"
    # maxwidth macro defined in memoir-article.latex template
    result$knitr$opts_chunk$out.width <- "\\maxwidth"

    result$knitr$opts_chunk$dark_theme <- FALSE
    result$knitr$opts_chunk$theme_bw <- TRUE
    result$knitr$knit_hooks$theme_bw <- set_theme_bw

    # don't need the textpos plot hook, but need TeX figures
    result$knitr$knit_hooks$plot <- knitr::hook_plot_tex

    result$pandoc$args <- result$pandoc$args[
        grep("\\.yaml$", result$pandoc$args, invert=TRUE)
    ]

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

#' @export
make_article <- function (target="all") { 
    makefile <- system.file(
        file.path("memarticle", "Makefile"), package="scuro")
    tmpl <- system.file(file.path("memarticle", "memoir-article.latex"),
        package="scuro")
    system2("make", c(
        paste0("PANDOC_TMPL=", tmpl), "out_dir=.",
        "-f", makefile,
        target
    ))

}
