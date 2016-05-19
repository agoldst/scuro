
#' A function for article classes
#'
#' To use scuro's font and figure-drawing setup with slides, use \code{\link{scuro_md}} but with format option \code{scuro=FALSE}.
#'
#' @export
chiaro_md <- function (
        fig_width=2,
        fig_height=2,
        fig_crop=TRUE,
        latex_engine="xelatex",
        dev="tikz",
        plot_font="mainfont") {

    result <- scuro_md(fig_width, fig_height, fig_crop, latex_engine, dev,
        plot_font, scuro=FALSE)

    result$knitr$opts_chunk$dark_theme <- FALSE
    result$knitr$opts_chunk$theme_bw <- TRUE
    knitr_options$knit_hooks$theme_bw <- set_theme_bw

    result$knitr$knit_hooks$plot <- NULL # revert to default

    result$pandoc$args <- result$pandoc$args[
        grepl("\\.yaml$", result$pandoc$args, invert=TRUE)
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
        file.path("memoirarticle", "Makefile"), package="scuro")
    tmpl <- system.file(file.path("memoirarticle", "memoir-article.latex"),
        package="scuro")
    system2("make", c(
        paste0("PANDOC_TMPL=", tmpl), "md_dir=md",
        "-f", makefile,
        target
    ))

}
