#' save_ggplot
#'
#' @param p "plot" to be saved. Has to be a ggplot object
#' @param dest Destination directory of the plot
#' @param plot_name Name of the plot to be exported
#' @param ext Extension for saving the plot. e.g "png" or "eps" etc.
#' @param units,width,height Plot size in units ("in", "cm", "mm", or "px").
#' If not supplied, uses the size of current graphics device.
#' @param dpi Plot resolution. Also accepts a string input: "retina" (320),
#' "print" (300), or "screen" (72). Applies only to raster output types
#'
#' @return NULL. The function only saves a ggplot object
#' @importFrom ggplot2 ggsave
#' @export
#'
#' @examples
#' \dontrun{
#' save_ggplot()
#' }
save_ggplot <- function(p,
                        dest = tempdir(), plot_name = "dummy", ext = "png",
                        units = "in", width = 7, height = 7, dpi = 300) {
    filename <- paste0(dest, "/", plot_name, ".", ext)
    ggsave(plot = p,
           filename = filename,
           width = width, height = height,
           units = units,
           dpi = dpi)
    cat("Saved as", filename, "\n")
}
