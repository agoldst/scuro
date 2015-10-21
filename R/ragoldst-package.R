
ragoldst_local <- new.env(parent=emptyenv())

.onLoad <- function (libname, pkgname) {
    ragoldst_local$plot_hook_default <- knitr::knit_hooks$get("plot")
    ragoldst_local$output_hook_default <- knitr::knit_hooks$get("output")
    knitr::knit_hooks$set(plot=plot_hook_textpos,
                          output=output_hook_routput)
}

.onUnload <- function (libpath) {
    knitr::knit_hooks$set(plot=ragoldst_local$plot_hook_default,
                          output=ragoldst_local$output_hook_default)
}
