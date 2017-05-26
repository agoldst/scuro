
#' A dark ggplot2 theme suitable for slides
#'
#' Add this to your ggplots for a light-on-dark theme, suitable for matching to light-on-dark slides. Or use the chunk option \code{dark_theme=T}.
#'
#' @param base_size general text-size parameter in points (??)
#' @param base_family font family; to be overriden in TikZ graphics
#'
#' @param dark main background color (should probably match slide background).
#' Some intermediate colors will be interpolated between this and \code{light}.
#'
#' @param light main foreground color (should probably match slide text)
#'
#' @return a \code{\link[ggplot2]{theme}} object that can be added to a plot.
#'   Override settings by adding (or \code{\%replace+}ing) another
#' \code{theme()} call.
#'
#' @export
#'
dark_plot_theme <- function(base_size=9, base_family="",
                            dark="gray10", light="white") {
    if (!requireNamespace("ggplot2", quietly=TRUE)) {
        stop("ggplot2 must be installed to use dark_plot_theme.")
    }

    # color interpolator
    clr_rmp <- colorRamp(c(dark, light))
    ramp <- function (x) {
        clr <- clr_rmp(x)
        rgb(clr[1], clr[2], clr[3], maxColorValue=255)
    }

    ggplot2::`%+replace%`(
        # The default ggplot2 theme
        ggplot2::theme_grey(base_size=base_size, base_family=base_family),
        # with the following changes:
        ggplot2::theme(

    # axis options
    axis.line=ggplot2::element_blank(),
    axis.text.x=ggplot2::element_text(
        size=ggplot2::rel(0.8), color=light, lineheight=0.9, vjust=1,
        margin=ggplot2::margin(0.5, unit="lines")
    ),
    axis.text.y=ggplot2::element_text(
        size=ggplot2::rel(0.8), color=light, lineheight=0.9, vjust=1,
        margin=ggplot2::margin(0.5, unit="lines")
    ),
    axis.ticks=ggplot2::element_line(color=light, size=0.2),
    axis.title.x=ggplot2::element_text(
        size=ggplot2::rel(1), color=light, vjust=0
    ),
    axis.title.y=ggplot2::element_text(
        size=ggplot2::rel(1), color=light, angle=90, vjust=0.5
    ),
    axis.ticks.length=ggplot2::unit(0.3, "lines"),

    # legend options
    legend.background=ggplot2::element_rect(color=NA, fill=dark),
    legend.key=ggplot2::element_rect(color=light, fill=dark),
    legend.key.size=grid::unit(1.2, "lines"),
    legend.key.height=NULL,
    legend.key.width=NULL,
    legend.text=ggplot2::element_text(size=ggplot2::rel(0.8), color=light),
    legend.title=ggplot2::element_text(size=ggplot2::rel(0.8), color=light),
    legend.text.align=NULL,
    legend.title.align=NULL,
    legend.position="bottom",
    legend.box=NULL,

    # panel options
    panel.background=ggplot2::element_rect(fill=dark, color=NA),
    panel.border=ggplot2::element_rect(fill=NA, color=light),
    panel.grid.major=ggplot2::element_line(color=ramp(0.1)),
    panel.grid.minor=ggplot2::element_blank(),
    panel.spacing=ggplot2::unit(0.25, unit="lines"),

    # faceting options
    strip.background=ggplot2::element_rect(fill=ramp(0.1), color=light),
    strip.text.x=ggplot2::element_text(
        size=ggplot2::rel(0.8), color=light
    ),
    strip.text.y=ggplot2::element_text(
        size=ggplot2::rel(0.8), color=light, angle=-90
    ),

    # plot options
    plot.background=ggplot2::element_rect(color=dark, fill=dark),
    plot.title=ggplot2::element_text(size=ggplot2::rel(1.2), color=light),
    plot.margin=ggplot2::margin(1, 1, 0.5, 0.5, unit="lines")

        )
    )
}

set_dark_theme <- function (before, options, envir) {
    if (!options$dark_theme) return()

    if (requireNamespace("ggplot2", quietly=TRUE)) {
        if (before) {
            scuro_local$gg_theme <- ggplot2::theme_get()
            ggplot2::theme_set(dark_plot_theme())

        } else {
            ggplot2::theme_set(scuro_local$gg_theme)
        }
    }

    if (before) {
        scuro_local$par <- par("bg", "fg")
        par(bg="gray10", fg="white")
    } else {
        par(scuro_local$par)
    }
}

set_theme_bw <- function (before, options, envir) {
    if (!options$theme_bw) return()

    if (requireNamespace("ggplot2", quietly=TRUE)) {
        if (before) {
            scuro_local$gg_theme <- ggplot2::theme_get()
            ggplot2::theme_set(ggplot2::theme_bw())

        } else {
            ggplot2::theme_set(scuro_local$gg_theme)
        }
    }
}
