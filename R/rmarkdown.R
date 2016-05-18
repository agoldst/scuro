#' Intermediate markdown format
#'
#' @param fig_width,fig_height default figure dimensions (inches). Note that
#'   beamer slides have small "physical" dimensions: 128 mm x 96 mm or about 5
#'   in x 3.75 in. The default values here are meant for slides with just one
#'   figure on them (and possibly a title and a caption).
#'
#' @param fig_crop,fig_caption same as for the built-in
#'   \code{\link[rmarkdown]{beamer_presentation}}
#'
#' @param dev graphics device. \code{"tikz"} for TikZ graphics is the default,
#'   since one of the main points of this package is to transcend bad beamer
#'   fonts. This requires installing the \pkg{tikzDevice} package.
#'
#' @export
scuro_md <- function (
        variant="markdown",
        preserve_yaml=TRUE,
        fig_width=4.5,
        fig_height=2.75,
        dev="tikz") {

    result <- rmarkdown::md_document(
        variant=variant,
        preserve_yaml=preserve_yaml,
        toc=FALSE,
        fig_width=fig_width,
        fig_height=fig_height,
        dev=dev
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
    if (dev == "tikz" && latex_engine == "xelatex") {
        knitr_options$opts_chunk$tikz_xelatex <- TRUE
    }

    # set hooks
    knitr_options$knit_hooks$plot <- plot_hook_textpos
    knitr_options$knit_hooks$output <- output_hook_routput
    knitr_options$knit_hooks$dark_theme <- set_dark_theme
    knitr_options$knit_hooks$tikz_xelatex <- tikz_setup_hook

    result$knitr <- knitr_options

    result
}

#' Process R markdown into slides, notes, handouts, or all three
#'
#' This function takes an R markdown file, knits it to ordinary markdown, and converts that markdown to one or all three of the supported PDF output types.
#'
#' It would be nice to make use all of the configurability of \code{rmarkdown::render} (and its RStudio integration), but that function expects to convert one input into one output. To create three outputs from one input, you have to define three output formats. This defeats knitr's cacheing and causes it to knit three times. If there are any serious computations in your R markdown, this is a drag. The alternative strategy adopted here is to knit once and do the rest with pandoc. This way if you render notes after having already rendered slides, your R chunks are not re-run.
#'
#' I recommend setting up Makefile rules invoking this with
#' \code{R -e 'scuro::render("$<", "slides")'} and so on.
#'
#' The conversion is still done with rmarkdown's copy of pandoc.
#'
#' Some rmarkdown and pandoc options can be specified in the YAML metadata under the key \code{scuro:}.
#
#' highlight code syntax highlighting. I recommend "zenburn" (see
#'   \code{\link[rmarkdown]{beamer_presentation}} for other choices).
#'
#' keep_tex whether to keep the intermediate LaTeX file. Often needed for
#'   debugging errors.
#'
#' latex_engine I recommend xelatex (cf. above remarks about fonts).
#'
#' pandoc_args character vector of extra arguments to pandoc.
#'
#' @param input R markdown file name
#' @param output_format one of \code{"slides", "notes", "handout"}
#'
#' @export
render <- function (input,
        output_format=c("slides", "notes", "handout")) {
    output_format <- match.arg(output_format)

    file_base <- gsub("\\.[^.]*$", "", input)
    md_file <- paste0(file_base, ".md")

    # merge in settings from YAML metadata
    settings <- rmarkdown::metadata$scuro
    md_params <- formals(scuro_md)
    md_override <- intersect(names(settings), names(md_params))
    md_params[md_override] <- settings[md_override]

    rmarkdown::render(input, output_file=md_file,
                      output_format=do.call(scuro_md, md_params))

    tmpl <- system.file("pandoc/beamer.template", package="scuro")
    overlay_filter <- system.file("python/overlay_filter",
                                  package="scuro")

    pandoc_options <- list(
        slide_level=1,
        highlight="zenburn",
        latex_engine="xelatex",
        template=tmpl,
        filter=overlay_filter,
        include_in_header=hdr
    )

    if (!is.null(settings$pandoc)) {
        pandoc_options[names(settings$pandoc)] <- settings$pandoc
    }
    keep_tex <- settings$keep_tex
    if (is.null(keep_tex)) keep_tex <- FALSE

    output_file <- paste0(file_base, "-", output_format, ".pdf")

    hdr <- system.file(paste0("tex/preamble-", output_format, ".tex"),
                       package="scuro")


    md_temp <- FALSE
    if (output_format == "handout") {
        if (!is.null(settings) {
            h_opt <- settings$handout
            if (!is.null(h_opt) && !is.null(h_opt$invert_plots)
                    && h_opt$invert_plots) {
                pdir <- paste0(file_base, "_files")
                invert_plots(file.path(pdir, "figure-markdown"),
                             file.path(pdir, "figure-scuro-handout"))
                # Now, just munge the rendered markdown to replace
                # figure file names
                # TODO would be a bit safer to walk the pandoc parse tree
                md_str <- gsub(
                    "(!\\[.*\\([^)]*)figure-markdown/",
                    "\\1figure-scuro-handout/",
                    readLines(md_file))
                md_file <- tempfile(fileext=".md")
                writeLines(md_str, md_file)
                on.exit(unlink(md_file), add=TRUE)
            }
        }
    }

    if (output_format == "notes") {
        pandoc_vars <- rmarkdown::pandoc_variable_arg("fontsize", "8pt")
    } else if (output_format == "handout") {
        pandoc_vars <- c("-V", "handout")
    } else {
        pandoc_vars <- NULL
    }

    pandoc_options <- paste0(
        "--",
        gsub("_", "-", names(pandoc_options)),
        " ",
        unlist(pandoc_options)
    )

    if (rmarkdown::scuro$keep_tex) {
        tex_file <- sub("pdf$", "tex", output_file)
        pandoc_convert(md_file, to="beamer", output=tex_file,
            options=c(pandoc_options, pandoc_vars)
        )
        pandoc_covert(tex_file, output_file)
    } else {
        pandoc_convert(md_file, to="beamer", output=output_file,
            options=c(pandoc_options, pandoc_vars)
        )
    }
}


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

#' Render all three presentation outputs
#'
#' A convenience function which simply calls \code{\link{render}} three times.
#'
#' @param input R markdown file.
#'
#' @export
#'
render_all <- function (input) {
    render(input, "slides")
    render(input, "notes")
    render(input, "handout")
}

