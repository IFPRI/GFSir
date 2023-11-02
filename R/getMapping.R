#' getMapping
#'
#' @param type "region" or "crops". Region returns six CGIAR regions + developed
#' or developing countries + world. Crops returns IMPACT mapping for crops in
#' their more user friendly name.
#' @param file mapping file name
#'
#' @return mapping according to the type selected
#' @importFrom readxl read_xlsx
#' @export
#'
#' @examples
#' \dontrun{
#' getMapping()
#' }
#' @author Abhijeet Mishra
getMapping <- function(type = "region", file = "mapping.xlsx") {
    mapping <- NULL
    mapping_file <- system.file("extdata", file, package = "GFSir")

    duplicated_message <- function(mapping) {
        if (any(duplicated(mapping))) {
            warning("Duplicates in mapping")
            warning("Returning DUPLICATED mapping\n")
        } else {
            message("working with unique mapping\n")
        }
    }

    if (type == "region") {
        mapping <- read_xlsx(path = mapping_file,
                             sheet = "regions",
                             trim_ws = TRUE,
                             progress = TRUE,
                             .name_repair = "unique")
    }
    if (type == "crops") {
        mapping <- read_xlsx(path = mapping_file,
                                     sheet = "crops",
                                     trim_ws = TRUE,
                                     progress = TRUE,
                                     .name_repair = "unique")
    }
    if (!is.null(mapping)) duplicated_message(mapping)
    if (is.null(mapping)) message("No mapping created. Returning NULL.")
    return(mapping)
}
