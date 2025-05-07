#' add_crops
#'
#' @param df dataframe with "j" or "c" column
#' @param mapping Mapping file which contains crops/activities
#'
#' @return Cleaned scenario name
#' @importFrom collapse join
#' @export
#' @examples
#' \dontrun{
#' add_crops()
#' }
#' @author Abhijeet Mishra
add_crops <- function(df, mapping = "mapping.xlsx") {
    origin_cols <- colnames(df)

    jfish <- c("Shrimp", "Crust", "Mllscs", "Salmon",
               "ODmrsl", "Tuna", "Cobswf", "Eelstg",
               "Tilapia", "Pangas", "Carp", "Mullet",
               "OFrshD", "OCarp", "OPelag", "OMarn")

    cfish <- c("c-shrimp", "c-Crust", "c-Mllsc", "c-Salmon",
               "c-FrshD", "c-Tuna", "c-OPelag", "c-ODmrsl", "c-OMarn")

    # Find regional mapping
    map <- getMapping(type = "crops", file = mapping)

    # Find if "j" or "c" exist in original columns
    if ("j" %in% origin_cols && !("c" %in% origin_cols)) {
        map <- map[startsWith(map$j_c, "j") | map$j_c %in% jfish, ]
        colnames(map)[1] <- "j"
    }

    if ("c" %in% origin_cols && !("j" %in% origin_cols)) {
        map <- map[startsWith(map$j_c, "c") | map$j_c %in% cfish, ]
        colnames(map)[1] <- "c"
    }

    # Merge with data on cty level
    df <- join(df, map, multiple = TRUE)

    return(df)
}
