#' Code output environment hook
#' 
#' Not called directly but automatically set as knitr's output hook. This puts R
#' output into an \code{ROutput} environment which it is up to you to define
#' with something like 
#' \code{\\DefineVerbatimEnvironment{ROutput}{Verbatim}{frame=single,fontsize=\\footnotesize}} (which is what's used in the included \code{talk} RMarkdown template).
#' 
#' @export
#' 
output_hook_routput <- function (x, options) {
    paste(c("\\begin{ROutput}",
            sub("\n$", "", x),
            "\\end{ROutput}",
            ""),
          collapse="\n")
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
    }
    else {
        knitr::hook_plot_tex(x, options)
    }
}


tikz_setup_hook <- function (before, options, envir) {
    if (before) {
        options(tikzDefaultEngine="xetex")

        # rmarkdown::metadata holds document yaml metadata when knitting
        if (is.character(rmarkdown::metadata$mainfont)) {
            fontline <- paste0("\\setmainfont{", rmarkdown::metadata$mainfont,
                               "}\n")
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


