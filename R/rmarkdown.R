#' Intermediate markdown format
#'
#' The strategy adopted by this package is to knit R markdown to markdown
#' and then control the subsequent PDF generation using a Makefile to
#' govern pandoc and latex. That lets us generate multiple PDFs from a single
#' R markdown source without re-knitting.
#'
#' @param fig_width,fig_height default figure dimensions (inches). Note that
#'   beamer slides have small "physical" dimensions: 128 mm x 96 mm or about 5
#'   in x 3.75 in. The default values here are meant for slides with just one
#'   figure on them (and possibly a title and a caption).
#'
#' @param fig_crop,latex_engine same as for the built-in
#'   \code{\link[rmarkdown]{beamer_presentation}}, but XeLaTeX is the default
#'   engine
#'
#' @param dev graphics device. \code{"tikz"} for TikZ graphics is the default,
#'   since one of the main points of this package is to transcend bad beamer
#'   fonts. This requires installing the \pkg{tikzDevice} package.
#'
#' @param plot_font if TikZ and XeLaTeX are used, the name of the main font for
#' graphics. If \code{"sansfont"} (the default) or \code{"mainfont"}, the value
#' of the corresponding document metadata field is used. If unspecified, the
#' sans font is used if that has been specified. Otherwise we fall back to the
#' LaTeX default.
#'
#' @return an R Markdown output format used by
#'   \code{\link[rmarkdown]{render}}.
#'
#' @export
scuro_md <- function (
        fig_width=4.5,
        fig_height=2.75,
        fig_crop=TRUE,
        latex_engine="xelatex",
        dev="tikz",
        plot_font="sansfont") {

    # YAML defaults merged in by pandoc; can be overridden by source Rmd
    yaml_defaults <- system.file("pandoc/default.yaml", package="scuro")
    result <- rmarkdown::md_document(
        # variant="markdown" and --standalone (always set by md_document)
        # together entail the preservation of the YAML block *by pandoc*
        variant="markdown",
        # which means rmarkdown should not ALSO copy over the YAML block
        preserve_yaml=FALSE,
        toc=FALSE,
        fig_width=fig_width,
        fig_height=fig_height,
        dev=dev,
        pandoc_args=c(yaml_defaults, "--atx-headers")
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

    # put figures one level up, where they can be found by latex later on
    # overrides rmarkdown's default choice of "<filename>_files"
    knitr_options$opts_chunk$fig.path <- "../figure/"

    # custom package hook option: use dark_plot_theme everywhere
    knitr_options$opts_chunk$dark_theme <- TRUE
    if (dev == "tikz" && latex_engine == "xelatex") {
        knitr_options$opts_chunk$tikz_xelatex <- TRUE
        # custom option: plot_font (processed by tikz_setup_hook)
        knitr_options$opts_chunk$plot_font <- plot_font
    }


    # set hooks
    knitr_options$knit_hooks$plot <- plot_hook_textpos
    knitr_options$knit_hooks$output <- output_hook_routput
    knitr_options$knit_hooks$dark_theme <- set_dark_theme
    knitr_options$knit_hooks$tikz_xelatex <- tikz_setup_hook

    result$knitr <- knitr_options

    result
}

#' Convert scuro markdown to PDF slides, notes, scripts, and handouts
#'
#' Knitted markdown can be converted into the multiple forms that support a
#' lecture with this function. It is just a wrapper for an invocation of
#' \code{make}.
#'
#' The possible targets are defined by the Makefile included in this package
#' (\emph{not} the Makefile in the \code{lectures} template), which can be
#' found at \code{system.file("elsmd/Makefile", package="scuro")}. 
#'
#' @param target Which PDFs to make: for each file \code{notes/X.md} or
#' \code{scripts/X.md}, the options are \code{slides/X.pdf} (slides),
#' \code{lectures/X.pdf} (speaker notes), \code{handouts/X.pdf} (slide handout
#' for the audience). The default, \code{all}, generates all three for each
#' markdown file.
#'
#' @export
make <- function (target="all") {

    makefile <- system.file(file.path("elsmd", "Makefile"), package="scuro")
    overlay_filter <- system.file(file.path("elsmd", "overlay_filter"),
                                  package="scuro")
    slides_template <- system.file(file.path("elsmd", "elsmd-slides.latex"),
                                   package="scuro")
    script_template <- system.file(file.path("elsmd", "beamerarticle.latex"),
                                   package="scuro")
    system2("make", c(
        paste0("OVERLAY_FILTER=", overlay_filter),
        paste0("SLIDES_TMPL=", slides_template),
        paste0("SCRIPT_TMPL=", script_template), 
        "NOTES=notes_md", "SCRIPTS=scripts_md",
        "SCURO=\"\"",   # but rendering scuro_md ensure scuro: true in YAML
        "-f", makefile, # UNLESS scuro: false is explicit in the source Rmd
        target
    ))

}


# TODO invoke this in plot hook to generate file for handout
invert_plots <- function (indir, outdir) {
    if (Sys.which("convert") == "") return()

    in_files <- list.files(indir, full.names=TRUE)
    if (!file.exists(outdir)) {
        dir.create(outdir)
    }
    out_files <- gsub("\\.([^.]*)$", "-handout.\\1", in_files)

    den <- getOption("scuro.invert_density")
    for (i in seq_along(in_files)) {
        pos <- in_files[[i]]
        neg <- out_files[[i]]
        if (!file.exists(neg) ||
                file.info(pos)$mtime > file.info(neg)$mtime) {
            system2("convert", c(
                "-density", den, pos,
                "-negate", neg))
        }
    }
}
