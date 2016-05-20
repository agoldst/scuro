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
#' @param highlight syntax highlighting scheme. Note that this option is
#' currently only supported if specified in the R markdown source.
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
#' @param plot_font_options given a \code{plot_font}, font options. if
#' \code{plot_font} is \code{"sansfont"} or \code{"mainfont"}, the
#' corresponding font options will be used by default.
#'
#' @param scuro whether to apply the scuro color scheme to slides themselves.
#'
#' @return an R Markdown output format used by
#'   \code{\link[rmarkdown]{render}}.
#'
#' @seealso The included \code{lectures} R Markdown template for supporting
#' files and example source. In RStudio, choose "File > New File > R
#' Markdown...", choose "From Template," and select "Dark-on-Light Beamer
#' Slides."
#'
#' @export
scuro_md <- function (
        fig_width=4.5,
        fig_height=2.75,
        fig_crop=TRUE,
        highlight="zenburn",
        latex_engine="xelatex",
        dev="tikz",
        plot_font="sansfont",
        plot_font_options=NULL,
        scuro=TRUE) {

    # YAML defaults merged in by pandoc; can be overridden by source Rmd
    if (scuro) {
        yaml_defaults <- system.file("pandoc/default.yaml", package="scuro")
    } else {
        yaml_defaults <- system.file("pandoc/chiaro.yaml", package="scuro")
    }

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

    knitr_options <- rmarkdown::knitr_options_pdf(
        fig_width, fig_height, fig_crop, dev
    )

    result$knitr <- scuro_knitr(knitr_options,
        dev, latex_engine, plot_font, plot_font_options)
    # put figures one level up, where they can be found by latex later on
    # overrides rmarkdown's default choice of "<filename>_files"
    result$knitr$opts_chunk$fig.path <- "../figure/"

    # set textpos plotting hook
    result$knitr$knit_hooks$plot <- plot_hook_textpos

    # custom package hook option: use dark_plot_theme everywhere
    result$knitr$opts_chunk$dark_theme <- scuro
    result$knitr$knit_hooks$dark_theme <- set_dark_theme


    result
}

scuro_knitr <- function (knitr_options,
    dev, latex_engine, plot_font, plot_font_options) {
    # this knit_hooks fiddling code is closely modeled on
    # https://github.com/rstudio/rmarkdown/blob/master/R/tufte_handout.R
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

    if (dev == "tikz" && latex_engine == "xelatex") {
        knitr_options$opts_chunk$tikz_xelatex <- TRUE
        # custom option: plot_font (processed by tikz_setup_hook)
        knitr_options$opts_chunk$plot_font <- plot_font
        knitr_options$opts_chunk$plot_font_options <- plot_font_options
    }


    # set hooks
    knitr_options$knit_hooks$output <- output_hook_routput
    knitr_options$knit_hooks$tikz_xelatex <- tikz_setup_hook

    knitr_options
}


#' Convert scuro markdown to PDF slides, notes, scripts, and handouts
#'
#' Knitted markdown can be converted into the multiple forms that support a
#' lecture with this function. It is just a wrapper for an invocation of
#' \code{make}.
#'
#' @param input source markdown (\emph{not} R markdown) file, created by
#' \code{\link[rmarkdown]{render}}ing to \code{{\link{scuro_md}}}.
#'
#' @param type type of PDF to generate (any of \code{slides},
#' \code{handout[s]}, or \code{lecture[s]} (synonym: \code{notes})). Can be a
#' vector. Output goes to a correspondingly named subdirectory.
#'
#' @export
render_pdf <- function (input,
    type=c("slides", "handouts", "lectures", "notes")) {

    type <- match.arg(type, several.ok=TRUE)
    type <- ifelse(type == "notes", "lecture", type)

    meta <- extract_metadata(input)
    pandoc_opts <- c()
    if (!is.null(meta) && is.list(meta$output)
            && is.list(meta$output[["scuro::scuro_md"]])) {
        hlt <- meta$output[["scuro::scuro_md"]]$highlight
        if (is.character(hlt) && hlt != "default") {
            pandoc_opts <- paste0(
                'PANDOC_OPTIONS="--highlight-style=', hlt,'"')
        }
    }
    target <- file.path(type, sub("\\.md$", ".pdf", basename(input)))

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
        pandoc_opts,
        "NOTES=notes_md", "SCRIPTS=scripts_md",
        "SCURO=\"\"",   # but rendering scuro_md ensures scuro: true in YAML
        "-f", makefile, # UNLESS scuro: false is explicit in the source Rmd
        target
    ))
}

extract_metadata <- function (file) {
    ll <- readLines(file)
    # lifted from rmarkdown:::partition_yaml_front_matter
    delimiters <- grep("^(---|\\.\\.\\.)\\s*$", ll)
    valid <- FALSE
    if (length(delimiters) >= 2 && (delimiters[2] - delimiters[1] >
        1) && grepl("^---\\s*$", ll[delimiters[1]])) {
        if (delimiters[1] == 1)
            valid <- TRUE
        else
            valid <- all(!grepl("\\S", ll[1:delimiters[1] - 1]))
    }

    if (valid) {
        yaml::yaml.load(
            paste(ll[(delimiters[1] + 1):(delimiters[2] - 1)], collapse="\n")
        )
    } else
        NULL
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
