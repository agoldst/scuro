
#' A dark ggplot2 theme suitable for slides
#'
#' Add this to your ggplots.
#'
#' @param base_size general text-size parameter in ?? units
#' @param base_family font family; to be overriden in tikz graphics
#' @param dark main background color (normally, match slide background)
#' @param light main foreground color (normally, match slide text)
#'
#' @return a \code{\link[ggplot2]{theme}} object that can be added to a plot. 
#'   Override settings by adding another \code{theme()} call.
#'
#' @export
#'
dark_plot_theme <- function(base_size=9, base_family="",
                            dark="gray10", light="white") {
  theme_grey(base_size=base_size, base_family=base_family) %+replace%
    theme(
      # Specify axis options
      axis.line=element_blank(), 
      axis.text.x=element_text(size=base_size*0.8, color=light,
                               lineheight=0.9, vjust=1), 
      axis.text.y=element_text(size=base_size*0.8, color=light,
                               lineheight=0.9, hjust=1), 
      axis.ticks=element_line(color=light, size = 0.2), 
      axis.title.x=element_text(size=base_size, color=light, vjust=0),
      axis.title.y=element_text(size=base_size, color=light, angle=90,
                                vjust=0.5), 
      axis.ticks.length=grid::unit(0.3, "lines"), 
      axis.ticks.margin=grid::unit(0.5, "lines"),
      # Specify legend options
      legend.background=element_rect(color=NA, fill=dark), 
      legend.key=element_rect(color=light, fill=dark), 
      legend.key.size=grid::unit(1.2, "lines"), 
      legend.key.height=NULL, 
      legend.key.width=NULL,     
      legend.text=element_text(size=base_size * 0.8, color=light), 
      legend.title=element_text(size=base_size * 0.8, color=light), 
      legend.text.align=NULL, 
      legend.title.align=NULL, 
      legend.position="bottom",
      legend.box=NULL,
      # Specify panel options
      panel.background=element_rect(fill=dark, color = NA), 
      panel.border=element_rect(fill=NA, color=light), 
      panel.grid.major=element_line(color="gray20"), 
      panel.grid.minor=element_blank(), 
      panel.margin=grid::unit(0.25, "lines"),  
      # Specify facetting options
      # TODO color mixing instead of hard-coding here
      strip.background=element_rect(fill="gray40",color="gray20"), 
      strip.background=element_rect(fill=dark,color=light), 
      strip.text.x=element_text(size=base_size * 0.8, color=light), 
      strip.text.y=element_text(size=base_size * 0.8, color=light,
                                angle=-90), 
      # Specify plot options
      plot.background=element_rect(color=dark,fill=dark), 
      plot.title=element_text(size=base_size*1.2,color=light), 
      plot.margin=grid::unit(c(1,1,0.5,0.5),"lines")
    )
}
