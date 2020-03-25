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
#' @param highlight syntax highlighting scheme to be used on slides. Note that
#' this option is currently only supported if specified in the R markdown
#' source.
#'
#' @param highlight_paper syntax highlighting scheme to be used on handouts and
#' notes. Note that this option is currently only supported if specified in the
#' R markdown source.
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
        highlight_paper="monochrome",
        latex_engine="xelatex",
        dev="tikz",
        plot_font="sansfont",
        plot_font_options=NULL,
        scuro=TRUE) {

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
        pandoc_args="--atx-headers"
    )

    result$post_processor <- function (
            metadata, input_file, output_file, clean, verbose) {
        partition <- extract_metadata(input_file)
        meta <- partition$metadata
        if (is.null(meta)) {
            meta <- list()
        }
        # inject routput flag used by the pandoc template
        if (is.null(meta[["routput"]])) {
            meta$routput <- TRUE
        }
        if (scuro) {
            # propagate scuro setting up to top-level metadata
            meta$scuro <- TRUE
        }
        # record highlighting choices in output md file metadata
        if (!is.list(meta$output)) {
            meta$output <- list()
        }
        if (!is.list(meta$output[["scuro::scuro_md"]])) {
            meta$output[["scuro::scuro_md"]] <- list()
        }
        meta$output[["scuro::scuro_md"]]$highlight <- highlight
        meta$output[["scuro::scuro_md"]]$highlight_paper <- highlight_paper
        writeLines(c(
                "---",
                yaml::as.yaml(meta),
                "...",
                "",
                partition$body
            ),
            output_file, useBytes=TRUE)
        output_file
    }

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

    knitr_options$opts_chunk$echo <- FALSE
    knitr_options$opts_chunk$error <- FALSE
    knitr_options$opts_chunk$warning <- FALSE
    knitr_options$opts_chunk$message <- FALSE
    knitr_options$opts_chunk$prompt <- FALSE
    knitr_options$opts_chunk$autodep <- TRUE
    knitr_options$opts_chunk$cache <- TRUE

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
    type <- ifelse(type == "notes", "lectures", type)
    type <- unique(type)

    meta <- extract_metadata(input)$metadata
    pandoc_opts <- c()
    if (!is.null(meta) && is.list(meta$output)
            && is.list(meta$output[["scuro::scuro_md"]])) {
        hlt <- meta$output[["scuro::scuro_md"]][["highlight"]]
        hltp <- meta$output[["scuro::scuro_md"]][["highlight_paper"]]
        if (!is.character(hlt) || hlt == "default") {
            hlt <- "kate"
        }
        if (!is.character(hltp)) {
            hltp <- hlt
        } else if (hltp == "default") {
            hltp <- "kate"
        }
        hlts <- type
        hlts[hlts == "slides"] <- hlt
        hlts[hlts %in% c("handouts", "lectures")] <- hltp

        pandoc_opts <- paste0(
            'PANDOC_OPTIONS="--highlight-style=', hlts,'"')

    }
    target <- file.path(type, sub("\\.md$", ".pdf", basename(input)))

    makefile <- system.file(file.path("elsmd", "Makefile"), package="scuro")
    noslide_filter <- system.file(file.path("elsmd", "noslide.lua"),
                                  package="scuro")
    notes_filter <- system.file(file.path("elsmd", "notes.lua"),
                                package="scuro")
    slides_template <- system.file(file.path("elsmd", "elsmd-slides.latex"),
                                   package="scuro")
    script_template <- system.file(file.path("elsmd", "beamerarticle.latex"),
                                   package="scuro")
    for (t in seq_along(target)) {
        system2("make", c(
            paste0("NOSLIDE_LUA=", noslide_filter),
            paste0("NOTES_LUA=", notes_filter),
            paste0("SLIDES_TMPL=", slides_template),
            paste0("SCRIPT_TMPL=", script_template),
            pandoc_opts[t],
            "NOTES=notes_md", "SCRIPTS=scripts_md",
            "SCURO=\"\"",   # but rendering scuro_md ensures scuro: true in YAML
            "-f", makefile, # UNLESS scuro: false is explicit in the source Rmd
            target[t]
        ))
    }
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
        meta <- yaml::yaml.load(
            paste(ll[(delimiters[1] + 1):(delimiters[2] - 1)], collapse="\n")
        )
        if (delimiters[2] < length(ll))
            body <- ll[-(1:delimiters[2])]
        list(metadata=meta, body=body)
    }
    else {
        list(body=ll)
    }
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
