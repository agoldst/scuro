#' Code output environment hook
#'
#' Not called directly but automatically set as knitr's output hook. This puts
#' R output into an \code{ROutput} environment. If you use the included
#' templates this will be set up with a default style. You can adjust the
#' appearance of this output by adding an array of \code{routput-options} to
#' your document metadata. These are supplied to the definition of
#' \code{ROutput} via the \code{fancyvrb} package:
#'
#' \code{\\DefineVerbatimEnvironment{ROutput}{Verbatim}{...}}
#'
#' \code{...} is replaced with your \code{routput-options}. If you do not use
#' the included templates, either remove this output hook or define your own
#' \code{ROutput} environment.
#'
#' @export
#'
output_hook_routput <- function (x, options) {
    paste(
        c("\\begin{ROutput}",
          sub("\n$", "", x),
          "\\end{ROutput}"),
        collapse="\n"
    )
}

#' Plot placement and resizing hook
#'
#' Not called directly but automatically set as knitr's plot hook, so that
#' chunks with plots can process the chunk options \code{textblock_width} and
#' \code{inside_textblock}, which allow you to use the \code{textpos} grid for
#' placing and sizing plots on slides arbitrarily instead of in the flow of
#' text.
#'
#' This gives us some new options and changes the meaning of two others:
#'
#' \describe{
#'
#' \item{\code{out.width, out.height}}{reinterpreted as units on the textpos
#' grid. This sets the scaling by pdflatex of an image file whose generated
#' dimensions are set by \code{fig.width} and \code{fig.height} (which are
#' always in inches).}
#'
#' \item{\code{textblock_width}}{width of the block in \code{textpos} grid
#' units}
#'
#' \item{\code{textblock_pos}}{two-element vector of grid coordinates of the
#' textblock}
#'
#' \item{\code{center}}{Boolean: enclose graphic within \code{center}
#' environment?}
#'
#' \item{\code{inside_textblock}}{don't generate \code{textblock} environment,
#' just the \code{\\includegraphics} line (useful if, e.g., you write out the
#' environment \code{\\begin} and \code{\\end} yourself and stick the code chunk
#' between them)}
#'
#' }
#'
#' Grid positioning only happens if \code{inside_textblock=T} or
#' \code{textblock_width} is specified in the chunk positions; otherwise we
#' revert to knitr's normal behavior. Grid positioning also always makes
#' graphics inline rather than floating. Use \code{inside_textblock=T} and
#' enclose it in a \code{figure} environment yourself if you want to combine the
#' textblock and the figure environment (you probably don't).
#'
#' N.B. the default knitr plot output hook for LaTeX responds to many more
#' options than this does. But in my workflow we are knitting to markdown, not
#' LaTeX. That's a simplifying decision in some ways but with some costs in
#' flexibility.
#'
#' @export
#'
plot_hook_textpos <- function (x, options) {
    inside <- options$inside_textblock
    pos <- options$textblock_pos
    b <- ""
    e <- ""
    if (is.null(inside)) {
        inside <- F
    }
    if (is.numeric(options$textblock_width) || inside) {
        if(!inside && is.numeric(pos) && length(pos) == 2) {
            b <- paste0(
                "\\begin{textblock}{", options$textblock_width, "}(",
                pos[1], ",", pos[2], ")\n"
            )
            e <- "\\end{textblock}\n"
        }

        if (!is.null(options$center) && options$center) {
            b <- paste0(b, "\\begin{center}\n")
            e <- paste0("\n\\end{center}", e)
        }

        if (is.null(options$out.width) && is.null(options$out.height)) {
            opt <- ""
        } else if (is.null(options$out.width)) {
            opt <- paste0("[height=", options$out.height, "\\TPVertModule]")
        } else if (is.null(options$out.height)) {
            opt <- paste0("[width=", options$out.width, "\\TPHorizModule]")
        } else {
            opt <- paste0("[width=", options$out.width, "\\TPHorizModule],",
                          "[height=", options$out.height, "\\TPVertModule]")
        }

        # filename munging here taken from knitr:::hook_plot_md_base
        # just in case this will help with rmarkdown's file management
        base_url <- opts_knit$get("base.url")
        if (is.null(base_url)) {
            base_url <- ""
        }
        gfx <- paste0("\\includegraphics", opt, "{", base_url, x, "}\n")
        paste0(b, gfx, e)
        # TODO render inverted plot if requested and make it conditional
        # for the handout using beamer modes
    }
    else {
        knitr::hook_plot_tex(x, options)
    }
}

#' TikZ and XeLaTeX wrangling hook
#'
#' This hook ensures that the \pkg{tikzDevice} package options are correctly
#' set to use xelatex and the chosen font (chunk option \code{plotfont},
#' normally specified by the document metadata either explicitly or implicitly)
#' to generate graphics. It is set as the \code{tikz_xelatex} hook and by
#' default all chunks have \code{tikz_xelatex=T} among their options.
#'
#' @export
tikz_setup_hook <- function (before, options, envir) {
    if (before) {
        options(tikzDefaultEngine="xetex")

        # rmarkdown::metadata holds document yaml metadata when knitting

        plotfont <- options$plot_font
        plotfont_opt <- options$plot_font_options
        if (!is.character(plotfont) || plotfont %in% c("", "sansfont")) {
            plotfont <- rmarkdown::metadata$sansfont
            if (is.null(plotfont_opt))  {
                plotfont_opt <- rmarkdown::metadata$sansfontoptions
            }
        } else if (plotfont == "mainfont") {
            plotfont <- rmarkdown::metadata$mainfont
            if (is.null(plotfont_opt))  {
                plotfont_opt <- rmarkdown::metadata$mainfontoptions
            }
        }

        if (is.character(plotfont)) {
            fontline <- "\\setmainfont"
            if (!is.null(plotfont_opt)) {
                fontline <- paste0(fontline, "[",
                    paste(plotfont_opt, collapse=","),
                    "]"
                )
            }
            fontline <- paste0(fontline, "{", plotfont, "}\n")
        } else {
            fontline <- character()
        }

        # tikzDevice + xelatex
        options(tikzXelatexPackages=c(
            "\\usepackage{tikz}\n",
            "\\usepackage[active,tightpage,xetex]{preview}\n",
            "\\usepackage{fontspec,xunicode}\n",
            fontline,
            "\\PreviewEnvironment{pgfpicture}\n",
            "\\setlength\\PreviewBorder{0pt}\n"))
    }
}


