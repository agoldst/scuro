% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/article.R
\name{chiaro_pdf}
\alias{chiaro_pdf}
\title{A format for article classes}
\usage{
chiaro_pdf(
  fig_width = 4,
  fig_height = 2,
  fig_crop = TRUE,
  fig_caption = TRUE,
  dev = "tikz",
  highlight = "default",
  keep_tex = TRUE,
  citation_package = c("default", "biblatex"),
  latex_engine = "xelatex",
  plot_font = "mainfont",
  plot_font_options = NULL,
  pandoc_args = NULL,
  ...
)
}
\arguments{
\item{fig_width}{Natural figure width in inches. This is rescaled according
to the value of chunk option \code{out.width} (by default, configured to use
the natural width or the textwidth, whichever is smaller).}

\item{fig_height}{Natural figure height in inches.
\code{fig_width/fig_height} is the aspect ratio of the graphic.}

\item{fig_crop}{Apply \code{pdfcrop}? Set by default.}

\item{dev}{graphics device (\code{tikz}) by default.}

\item{latex_engine}{\code{xelatex} by default}

\item{plot_font}{If both XeLaTeX and TikZ are used, the package will set the
font in graphics. Any system font name can be given. The default value
\code{mainfont} selects the document main font.}

\item{plot_font_options}{\code{fontspec} options for the \code{plot_font}.}

\item{...}{remaining arguments (e.g. \code{include}) are passed on to
\code{\link[rmarkdown]{pdf_document}}, which is the base of this format.}
}
\value{
An R Markdown format suitable for rendering.
}
\description{
This format is meant to supply only the TikZ-wrangling features of the
package and to set up reasonable further defaults for an article rather than
a presentation. For slides that use scuro's font and figure-drawing setup
but not its color scheme, use \code{\link{scuro_md}} but with format option
\code{scuro=FALSE}. For an article, \pkg{rmarkdown}'s built-in
\code{\link[rmarkdown]{render}} is flexible enough to handle all the
processing from R markdown to PDF, including citation processing where
needed. Simply set \code{output: scuro::chiaro_pdf} in your YAML header. The
package includes an R markdown template demonstrating this format as well.
}
\details{
Among defaults selected here, of particular note are centering alignment for
figures, a black-and-white ggplot theme, and stop-on-error. Errors,
warnings, and messages will all go to the console, not into the final
document. The intermediate LaTeX file will also be kept, as this is often
necessary to diagnose typesetting errors.

Additional typesetting customizations are as described in
\url{https://github.com/agoldst/memarticle} (the underlying pandoc
template).
}
\examples{
\dontrun{
# create article from the included template
rmarkdown::draft("my-essay", "article", "scuro", edit=F)
system("make -C my-essay") # a Makefile is supplied
}

}
