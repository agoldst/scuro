#' Code output environment hook
#'
#' Not called directly but automatically set as knitr's output hook.
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
#' Not called directly but automatically set as knitr's plot hook.
#' Allows you to use the textpos grid for placing and sizing plots on
#' slides. Example usage below.
#'
#' This gives us some new options and changes the meaning of two others:
#'
#' out.width, out.height: reinterpreted as units on the textpos grid.
#' N.B. this sets the scaling by pdflatex of an image file whose
#' generated dimensions are set by fig.width and fig.height (always
#' inches).
#'
#' textblock_width: width of the block in textpos grid units
#'
#' textblock_pos: 2-element vector of grid coordinates of the textblock
#'
#' center: Boolean: enclose graphic within center environment?
#'
#' inside_textblock: don't generate textblock environment, just the
#' \code{\\includegraphics} line (useful if, e.g., you write out the environment
#' \code{\\begin} and \code{\\end} yourself and stick the code chunk between them)
#'
#' Grid positioning only happens if inside_textblock=T or textblock_width
#' is specified. Otherwise we revert to knitr's normal behavior. Grid
#' positioning also always makes graphics inline. Use inside_textblock=T
#' and enclose it in a figure environment yourself if you want to combine
#' the textblock and the figure environment (you probably don't).
#'
#' The main use for this is probably to place a single figure on the grid
#' instead of in the normal flow of elements.
#'
#' N.B. the default knitr plot output hook for LaTeX responds to many
#' more options than this does. But in my workflow we are knitting to
#' markdown, not LaTeX. That's a simplifying decision in some ways but
#' with some costs in flexibility.
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
        ragoldst_local$plot_hook_default(x, options)
    }
}




#' Print LaTeX commands for typesetting a data frame as a table
#'
#' This is a helper function for using \code{xtable} in a knitting environment. 
#' It wrangles the bizarre division of options between 
#' \code{\link[xtable]{xtable}} and \code{\link[xtable]{print.xtable}} and 
#' ensures as-is output for knitting.
#'
#' @param x a data frame. Convert matrices and contingency tables yourself.
#'
#' @param digits the number of digits after the decimal point.
#'
#' @param alignment a single-element character vector giving the sequence of   
#' column alignments according to the \code{tabular} syntax in LaTeX (\code{l, c, r}   
#' for left, center, right, and \code{p{dim}} for text wrapped in a box of    
#' width \code{dim}---for example \code{p{2 in}}). By default numeric columns are  
#' right aligned and the rest are left aligned.                          
#'
#' @param include.colnames whether to print the data frame column names
#' as column headers. It often makes sense to assign something
#' human-friendly to the column names first. This function assumes you never
#' want to print rownames. Keeping anything interesting in the rownames
#' of a data frame is a bad idea anyway. If you want rownames, add a data
#' column on the front instead.
#'
#' @param floating whether just to drop the table in the flow of text
#' or to put it in a LaTeX environment like \code{table}. For slides, \code{floating=T} is
#' probably what you want; beamer centers
#' floating tables on slides as you'd hope.
#'
#' @param caption table caption (provided \code{floating=T})
#'
#' @param label the LaTeX label of the table (if \code{floating=T}), so that you can
#' refer to the table number---but why do that in a talk?
#'
#' @param ... the rest of the parameters are passed on to \code{\link[xtable]{print.xtable}}.
#'
#' @export
#'
print_tabular <- function (x, digits=0,
                           alignment=paste(ifelse(sapply(x, is.numeric),
                                                  "r", "l"),
                                           collapse=""),
                           include.colnames=T,
                           floating=F, caption=NULL, label=NULL,
                           ...) {
    if (length(alignment) != 1 || !is.character(alignment)) {
        stop("alignment must be a character vector of length 1")
    }
    if (nchar(alignment) == 1) {
        alignment <- paste(rep(alignment, length(x)), collapse="")
    }
    if (!is.data.frame(x)) {
        stop("x is not a data frame")
    }

    # xtable always wants an alignment char for the rownames
    alignment <- paste("l", alignment, sep="")
    xt <- xtable(x, digits=digits, align=alignment,
                 caption=caption, label=label)
    ll <- print(xt, comment=F, include.rownames=F,    # no row names, ever
                include.colnames=include.colnames,
                floating=floating, booktabs=T,
                tabular.environment=ifelse(floating, "tabular", "longtable"),
                print.results=F,
                ...)
    # xtable latex captured and returned this way so that you don't need to
    # set results="asis" in the chunk options
    knitr::asis_output(paste(ll, collapse="\n"))
}




#' A shortcut for printing out linear models as tables
#'
#' This routine wraps \code{\link[stargazer]{stargazer}}, adjusting some 
#' options and ensuring as-is output when knitting.
#'
#' @param ... passed on to \code{stargazer}. The first parameter is the model.
#'
#' @return LaTeX table code marked with class \code{asis_output} 
#'
#' @export 
#'
print_lm <- function (...) {
    stopifnot(requireNamespace("stargazer", quietly=TRUE))
    p <- list(dep.var.caption="",
              omit.stat=c("ser"),   # no residual SE, but keep F
              omit.table.layout="n",
              table.placement="H",
              single.row=T,
              header=F,
              report="vscp")
    dots <- list(...)
    p[names(dots)] <- dots

    # stargazer is irresponsible and directly `cat`s its output
    sgz <- capture.output(do.call(stargazer::stargazer, p))
    # this function, from knitr, shortcuts the need to set `results="asis"`
    knitr::asis_output(paste(sgz, collapse="\n"))
}

